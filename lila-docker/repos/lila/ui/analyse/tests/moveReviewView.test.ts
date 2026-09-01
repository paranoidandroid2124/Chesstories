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
  '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/occurrence-explanation-produced.json',
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

const producerExplanationId = verdictReview(completedJob).explanations[0]!.id;
const producerFirstBranchProofId = `${producerExplanationId}-branch-0`;

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
    assert.match(renderedText(findNode(panel, 'section.move-review__summary')), /b2b4/);
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
  assert.ok(candidates.some(candidate => renderedText(candidate).includes(`${copy.best} d4d5`)));
  assert.ok(candidates.some(candidate => renderedText(candidate).includes(`${copy.played} b2b4`)));
  assert.match(renderedText(findNode(panel, 'section.move-review__summary')), /Blunder/);
  assert.match(renderedText(findNode(panel, 'dl.move-review__metrics')), /90\.1% → 50\.0%/);
  assert.match(renderedText(panel), /-40\.1%p/);
  const headlines = findNodes(panel, 'button.move-review__proof-entry-button').map(renderedText).join(' ');
  assert.doesNotMatch(headlines, /capture_exclusion_move_order/);
});

test('renders the immutable BestChoice producer fixture with its runner-up', () => {
  const panel = renderMoveReview(props({ job: completedFrom(fixture(bestChoiceFixtureUrl)) }));
  const summary = findNode(panel, 'section.move-review__summary');
  assert.match(renderedText(summary), /Best .* · Runner-up .*:/);
  assert.equal(findNodes(summary, 'span.move-review__verdict-badge').length, 0);
});

test('renders the producer occurrence as ordered typed paths and provenance-owned branches', () => {
  const panel = renderMoveReview(
    props({
      view: { evidenceExpanded: true, expandedProofId: producerExplanationId },
    }),
  );
  const text = renderedText(panel);
  assert.match(text, /Proof path 1/);
  assert.match(text, /Premises/);
  assert.match(text, /Closed absence/);
  assert.match(text, /Closed state/);
  assert.match(text, /Captured target/);
  assert.match(text, /d4/);
  assert.match(text, /b2b4/);
  assert.match(text, /e5d4/);

  assert.deepEqual(findNodes(panel, 'header.move-review__branch-header').map(renderedText), [
    'Analyzed alternative d4d5',
    'Actual move · Analysis continuation b2b4',
  ]);
  assert.deepEqual(findNodes(panel, 'span.move-review__proof-stage').map(renderedText), [
    'Analyzed alternative',
    'Analyzed alternative',
    'Analyzed alternative',
    'Analyzed alternative',
    'Analyzed alternative',
    'Actual move',
    'Analysis continuation',
  ]);
  assert.deepEqual(
    findNodes(panel, 'button.move-review__proof-entry-button').map(button =>
      renderedText(findNode(button, 'strong')),
    ),
    ['Verified candidate line', moveReviewCopy('en-US').familyLabels.capture_exclusion_move_order],
  );
  assert.equal(findNodes(panel, 'section.move-review__participants.move-review__typed-section').length, 1);
  assert.equal(findNodes(panel, 'section.move-review__participant').length, 4);
  assert.equal(findNodes(panel, 'section.move-review__proof-path.move-review__typed-section').length, 1);
  assert.equal(findNodes(panel, 'section.move-review__premise').length, 4);
  assert.equal(findNodes(panel, 'section.move-review__closed-absence').length, 1);
  assert.equal(findNodes(panel, 'section.move-review__closed-state').length, 1);
  assert.equal(findNodes(panel, 'section.move-review__later-consumer.move-review__typed-section').length, 1);
  const provenance = findNode(panel, 'details.move-review__provenance');
  assert.equal(provenance.data?.attrs?.open, undefined);
  const decodedExplanation = verdictReview(completedJob).explanations[0]!;
  const visible = renderedTextWithout(panel, 'details.move-review__provenance');
  assert.doesNotMatch(visible, /capture_exclusion_move_order|proofKind|causeEvidenceId|capturedTarget/);
  assert.doesNotMatch(visible, new RegExp(decodedExplanation.proof.occurrenceId));
  assert.match(renderedText(provenance), new RegExp(decodedExplanation.proof.occurrenceId));
  assert.equal(findNodes(panel, 'div.cg-wrap.is2d').length, 2);
});

test('omits a move stage when the transmitted step provenance is absent', () => {
  const job = structuredClone(completedJob);
  const explanation = verdictReview(job).explanations[0]!;
  assert.equal(explanation.proofKind, 'capture_exclusion_move_order');
  if (explanation.proofKind !== 'capture_exclusion_move_order') return;
  delete (explanation.proof.immediateCaptureBranch.steps[0] as { provenance?: unknown }).provenance;

  const panel = renderMoveReview(
    props({ job, view: { evidenceExpanded: true, expandedProofId: explanation.id } }),
  );
  const stages = findNodes(panel, 'span.move-review__proof-stage').map(renderedText);
  assert.equal(stages.filter(stage => stage === 'Actual move').length, 0);
  assert.equal(stages.filter(stage => stage === 'Analyzed alternative').length, 5);
  assert.equal(stages.filter(stage => stage === 'Analysis continuation').length, 1);
});

test('preserves multiple independent occurrence explanations in wire order', () => {
  const raw = fixture();
  const occurrences = objects(commentary(raw).occurrence_explanations);
  const second = structuredClone(occurrences[0]!);
  second.cause_evidence_id = 'second-independent-cause';
  const secondProof = object(second.proof);
  secondProof.occurrence_id = 'e'.repeat(64);
  object(objects(secondProof.proof_paths)[0]).path_occurrence_id = 'f'.repeat(64);
  commentary(raw).occurrence_explanations = [occurrences[0]!, second];

  const panel = renderMoveReview(props({ job: completedFrom(raw), view: { evidenceExpanded: true } }));
  assert.deepEqual(
    findNodes(panel, 'button.move-review__proof-entry-button').map(button =>
      renderedText(findNode(button, 'strong')),
    ),
    [
      'Verified candidate line',
      moveReviewCopy('en-US').familyLabels.capture_exclusion_move_order,
      moveReviewCopy('en-US').familyLabels.capture_exclusion_move_order,
    ],
  );
  assert.deepEqual(
    findNodes(panel, 'article.move-review__proof-entry').map(entry => entry.key),
    [
      `pv:${playedUci}`,
      `occurrence-explanation-${object(occurrences[0]!.proof).occurrence_id}`,
      `occurrence-explanation-${secondProof.occurrence_id}`,
    ],
  );
});

test('keeps Assessment comparison proof and Explanation branches separate', () => {
  const assessment = renderMoveReview(
    props({ view: { evidenceExpanded: true, expandedProofId: `pv:${playedUci}` } }),
  );
  assert.equal(findNodes(assessment, 'div.cg-wrap.is2d').length, 1);
  assert.equal(findNodes(assessment, 'section.move-review__branch').length, 0);

  const explanation = renderMoveReview(
    props({ view: { evidenceExpanded: true, expandedProofId: producerExplanationId } }),
  );
  assert.equal(findNodes(explanation, 'div.cg-wrap.is2d').length, 2);
  assert.equal(findNodes(explanation, 'section.move-review__branch').length, 2);
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
      view: { evidenceExpanded: true, expandedProofId: producerExplanationId },
    }),
  );
  const moves = findNodes(panel, 'button.move-review__proof-san');
  assert.equal(renderedText(moves[0]!), 'd4d5');
  assert.equal(renderedText(moves[1]!), 'c7c6');
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
  assert.deepEqual(renderedBoardConfig(panel).lastMove, ['d4', 'd5']);

  const pinned = renderMoveReview(
    props({
      view: {
        evidenceExpanded: true,
        expandedProofId: producerExplanationId,
        pinnedFrame: { proofId: producerFirstBranchProofId, ply: 2 },
      },
    }),
  );
  assert.match(renderedText(findNode(pinned, 'figcaption')), /Step 2: c7c6/);
  assert.deepEqual(renderedBoardConfig(pinned).lastMove, ['c7', 'c6']);
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

test('shows a verdict-only explanation absence without inventing a cause', () => {
  const job = structuredClone(completedJob);
  verdictReview(job).explanations = [];
  const panel = renderMoveReview(props({ job, view: { evidenceExpanded: true } }));
  assert.match(renderedText(panel), /no cause .* was verified/i);
  assert.equal(findNodes(panel, 'section.move-review__branch').length, 0);
});

test('limits Korean explanation copy to finite structural labels', () => {
  const panel = renderMoveReview(
    props({
      locale: 'ko-KR',
      view: { evidenceExpanded: true, expandedProofId: producerExplanationId },
    }),
  );
  const text = renderedText(panel);
  assert.match(text, /근거 경로 1/);
  assert.match(text, /분석 대안 d4d5/);
  assert.match(text, /실제 수 · 분석 후속 b2b4/);
  assert.match(text, /전제/);
  assert.match(text, /폐쇄 부재/);
  assert.match(text, /폐쇄 상태/);
  assert.deepEqual(moveReviewCopy('ko-KR').familyLabels, {
    unique_check_reply_defender_displacement_before_capture: '유일 체크 응수로 수비수 이동 후 포획',
    sole_recapturer_removal_before_target_capture: '유일 재포획 기물 제거 후 목표 포획',
    vacated_gate_enables_unrecapturable_slider_capture: '관문 비움 후 재포획 불가 장거리 포획',
    square_release_route: '칸 해방 후 기물 경로',
    capture_exclusion_move_order: '포획 응수를 배제한 수순',
    passed_pawn_progress_realized_after_only_legal_reply: '유일 합법 응수 후 통과폰 전진',
  });
  assert.deepEqual(moveReviewCopy('en-US').familyLabels, {
    unique_check_reply_defender_displacement_before_capture:
      'Unique-check-reply defender displacement before capture',
    sole_recapturer_removal_before_target_capture: 'Sole-recapturer removal before target capture',
    vacated_gate_enables_unrecapturable_slider_capture:
      'Unrecapturable slider capture through a vacated gate',
    square_release_route: 'Route through a released square',
    capture_exclusion_move_order: 'Move order excluding the capture reply',
    passed_pawn_progress_realized_after_only_legal_reply: 'Passed-pawn progress after the only legal reply',
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
  assert.match(renderedText(findNode(panel, 'figcaption')), /Step 1: b2b4/);
  assert.deepEqual(renderedBoardConfig(panel).lastMove, ['b2', 'b4']);
});

test('offers the added Study line without duplicating its add action', () => {
  const proofId = producerFirstBranchProofId;
  let viewed: string | undefined;
  const panel = renderMoveReview(
    props({
      canWrite: true,
      addedProofId: proofId,
      actions: { viewAddedLine: id => (viewed = id) },
      view: { evidenceExpanded: true, expandedProofId: producerExplanationId },
    }),
  );
  assert.match(renderedText(panel), /Line added to Study/);
  assert.equal(findNodes(panel, 'button.button.button-thin.move-review__add').length, 1);
  const viewButton = findNode(panel, 'button.button.button-thin.move-review__view-added');
  (viewButton.data?.on?.click as () => void)();
  assert.equal(viewed, proofId);
});

test('keeps forced and position-action outcomes separate from explanations', () => {
  const forced = structuredClone(completedJob);
  const candidate = forced.snapshot.evidence.candidates.find(item => item.roles.includes('played'))!;
  candidate.review = {
    kind: 'forced-single-move',
    lineUcis: [playedUci, 'e5d4' as Uci],
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
