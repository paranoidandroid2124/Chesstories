import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { moveReviewEngineProfile } from 'lib/ceval/types';
import type { VNode } from 'snabbdom';
import {
  decodeMoveReviewSnapshot,
  moveReviewCopy,
  moveReviewObservations,
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
import {
  initialFen,
  rawResponse,
  observationFixtureJob,
  linkedHighlightFixture,
  type CertifiedObservationFixture,
} from './moveReviewTestSupport';
import { mountMoveReviewFixturePreview } from './moveReviewFixturePreview';
import {
  targetResponseDetail,
  lineContinuationDetail,
  continuationComparisonDetail,
} from '../src/moveReviewSelection';

type JsonObject = Record<string, unknown>;
type CompletedJob = Extract<MoveReviewJobState, { kind: 'completed' }>;
type MoveReviewPanelActions = MoveReviewPanelProps['actions'];
type VerdictReview = Extract<MoveReviewCandidateReview, { kind: 'move-verdict' }>;

const object = (value: unknown): JsonObject => value as JsonObject;
const bestChoiceFixtureUrl = new URL(
  '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/best-choice-produced.json',
  import.meta.url,
);

const certifiedFixtures = fixture(
  new URL('certified-observations-produced.json', bestChoiceFixtureUrl),
) as unknown as CertifiedObservationFixture[];

function certifiedProps(name: string, fixtures = certifiedFixtures): MoveReviewPanelProps {
  const job = observationFixtureJob(structuredClone(fixtures.find(fixture => fixture.name === name)!));
  const panelProps = props({
    job,
    actions: {
      previewFrame: frame => (panelProps.view.hoveredFrame = frame),
      clearPreview: () => (panelProps.view.hoveredFrame = undefined),
      pinFrame: frame => {
        panelProps.view.pinnedFrame = frame;
        panelProps.view.hoveredFrame = undefined;
      },
      clearPin: () => (panelProps.view.pinnedFrame = undefined),
      toggleProof: id => (panelProps.view.expandedProofId = id),
    },
  });
  return panelProps;
}

test('role text uses the existing occurrence board and square inspection', () => {
  const items = structuredClone(certifiedFixtures);
  const item = items.find(fixture => fixture.name === 'Bf3')!;
  const raw = item.response.observations as JsonObject;
  const occurrence = (raw.statements as JsonObject[])[0]!.occurrence_id;
  raw.highlights = [{ occurrence_id: occurrence, text: 'A local role of the bishop.', squares: ['f3'] }];
  const panel = certifiedProps('Bf3', items);
  const tree = renderMoveReview(panel);
  const role = findNodes(tree, 'section.move-review__work')[0];
  assert.ok(role);
  assert.ok(renderedText(role).includes('A local role of the bishop.'));
  const button = findNodes(role, 'button')[0]!;
  (button.data!.on!.click as () => void)();
  assert.equal(panel.view.pinnedFrame?.phase, 'after');
  assert.equal(panel.view.pinnedFrame?.inspectSquare, 'f3');
  assert.ok(renderedText(renderMoveReview(panel)).includes('A local role of the bishop.'));
});

test('linked role evidence previews and pins exact before/after frames, then supports square inspection', () => {
  const { fixture, highlight } = linkedHighlightFixture(certifiedFixtures.find(f => f.name === 'Bf3')!);
  for (const locale of ['en-US', 'ko-KR'] as const) {
    const panel = certifiedProps('Bf3', [fixture]);
    panel.locale = locale;
    panel.copy = moveReviewCopy(locale);
    const role = () => findNode(renderMoveReview(panel), 'section.move-review__work');
    const frames = () => findNodes(role(), 'button.move-review__highlight-frame');
    assert.deepEqual(frames().map(renderedText), [
      panel.copy.frameLabels.before,
      panel.copy.frameLabels.after,
    ]);
    assert.deepEqual(findNodes(role(), 'button').map(renderedText), highlight.frames[1].squares);
    assert.ok(renderedText(role()).includes(highlight.text));
    for (const id of [
      highlight.occurrence_id,
      ...highlight.premise_ids,
      ...highlight.frames.flatMap(f => [f.position_id, ...f.source_ids]),
    ]) {
      assert.ok(!renderedText(role()).includes(id));
      assert.ok(findNodes(role(), 'button').every(b => !String(b.data?.attrs?.['aria-label']).includes(id)));
    }
    const board = () => renderedBoardConfig(renderMoveReview(panel));
    const afterFen = board().fen;
    (frames()[0].data!.on!.focus as () => void)();
    assert.equal(board().fen, fixture.fen);
    assert.equal(board().lastMove, undefined);
    assert.deepEqual(
      board().drawable?.autoShapes,
      highlight.frames[0].squares.map(orig => ({ orig, brush: 'blue' })),
    );
    (frames()[0].data!.on!.blur as () => void)();
    assert.equal(board().fen, afterFen);
    (frames()[0].data!.on!.click as () => void)();
    assert.equal(frames()[0].data!.attrs!['aria-pressed'], 'true');
    (frames()[1].data!.on!.mouseenter as () => void)();
    assert.equal(board().fen, afterFen);
    assert.deepEqual(board().lastMove, ['g2', 'f3']);
    assert.deepEqual(
      board().drawable?.autoShapes,
      highlight.frames[1].squares.map(orig => ({ orig, brush: 'blue' })),
    );
    (role().data!.on!.mouseleave as () => void)();
    assert.equal(board().fen, fixture.fen, 'Leaving a preview restores the pinned before frame');
    for (const phase of ['before', 'after'] as const) {
      (frames()[phase === 'before' ? 0 : 1].data!.on!.click as () => void)();
      assert.deepEqual(
        findNodes(role(), 'button').map(renderedText),
        highlight.frames[phase === 'before' ? 0 : 1].squares,
      );
      const square = findNodes(role(), 'button').find(b => renderedText(b) === 'h2')!;
      assert.equal(
        square.data!.attrs!['aria-label'],
        `${panel.copy.frameLabels[phase]} · h2: ${highlight.text}`,
      );
      (square.data!.on!.click as () => void)();
      assert.equal(panel.view.pinnedFrame?.phase, phase);
      assert.equal(panel.view.pinnedFrame?.inspectSquare, 'h2');
      assert.equal(board().fen, phase === 'before' ? fixture.fen : afterFen);
      assert.equal(
        board().drawable!.autoShapes!.some(s => s.orig === 'c2' && s.dest === 'h2'),
        phase === 'after',
      );
    }
    inspectSquare(panel, 'g2');
    assert.equal(panel.view.pinnedFrame?.highlightKey, undefined);
    assert.equal(board().fen, afterFen);
    const proof = findNode(renderMoveReview(panel), 'div.move-review__proof');
    (proof.data!.on!.keydown as (event: unknown) => void)({
      key: 'Escape',
      preventDefault() {},
      stopPropagation() {},
    });
    assert.equal(panel.view.pinnedFrame, undefined);
    assert.deepEqual(board().drawable?.autoShapes, []);
  }
});

test('linked frames draw only cited control pairs while square inspection still shows every controller', () => {
  const { fixture, highlight } = linkedHighlightFixture(certifiedFixtures.find(f => f.name === 'Bf3')!);
  const positions = moveReviewObservations(observationFixtureJob(fixture))!.positions!;
  for (const evidence of highlight.frames) {
    const position = positions.find(p => p.id === evidence.position_id)!;
    const queen = position.controls.pieces.find(entry => entry.piece.square === 'c2')!;
    const destination = evidence.phase === 'before' ? 'g2' : 'h2';
    const index = queen.squares.indexOf(destination);
    assert.ok(index >= 0);
    // Keep the ray citation too; only the explicitly cited control pair creates an arrow.
    evidence.source_ids.push(queen.source_ids[index]);
  }
  const panel = certifiedProps('Bf3', [fixture]);
  for (const [index, phase] of (['before', 'after'] as const).entries()) {
    const button = findNodes(renderMoveReview(panel), 'button.move-review__highlight-frame')[index];
    (button.data!.on!.click as () => void)();
    const board = renderedBoardConfig(renderMoveReview(panel));
    assert.equal(board.fen, positions.find(p => p.id === highlight.frames[index].position_id)!.fen);
    assert.deepEqual(board.drawable?.autoShapes, [
      ...highlight.frames[index].squares.map(orig => ({ orig, brush: 'blue' })),
      { orig: 'c2', dest: phase === 'before' ? 'g2' : 'h2', brush: 'blue' },
    ]);
  }
  const inspected = renderedBoardConfig(inspectSquare(panel, 'h2'));
  assert.deepEqual(
    inspected
      .drawable!.autoShapes!.filter(shape => shape.dest === 'h2')
      .map(shape => shape.orig)
      .sort(),
    ['c2', 'd6', 'h1', 'h4'],
    'Independent square inspection retains uncited controllers, including opponents',
  );
});

test('removed or replaced highlight links clear stale selections without hiding facts or crossing histories', () => {
  const { fixture, highlight } = linkedHighlightFixture(certifiedFixtures.find(f => f.name === 'Bf3')!);
  const panel = certifiedProps('Bf3', [fixture]);
  const observations = moveReviewObservations(panel.job)!;
  const selectBefore = () => {
    const before = findNode(renderMoveReview(panel), 'button.move-review__highlight-frame');
    (before.data!.on!.click as () => void)();
    assert.equal(renderedBoardConfig(renderMoveReview(panel)).fen, fixture.fen);
  };
  const independentRows = findNodes(renderMoveReview(panel), 'details.move-review__controller').map(
    renderedText,
  );
  selectBefore();
  observations.highlights![0].frames![0].sourceIds = [];
  assert.deepEqual(renderedBoardConfig(renderMoveReview(panel)).drawable?.autoShapes, []);
  selectBefore();
  const path = observations.paths[0];
  observations.paths.push({
    ...structuredClone(path),
    id: 'same-fen-other-history',
    occurrenceIds: ['another-occurrence'],
    positionIds: [null, null],
  });
  panel.actions.toggleProof('same-fen-other-history');
  const other = renderMoveReview(panel);
  assert.equal(renderedBoardConfig(other).fen, path.moves[0].fenAfter);
  assert.ok(!renderedText(other).includes(highlight.text));
  panel.actions.toggleProof(path.id);
  observations.highlights = undefined;
  const without = renderMoveReview(panel);
  assert.deepEqual(renderedBoardConfig(without).drawable?.autoShapes, []);
  assert.deepEqual(findNodes(without, 'details.move-review__controller').map(renderedText), independentRows);
});

test('unsearched grade has an explicit action and missing occurrence never displays another move', () => {
  const panel = certifiedProps('Bf3');
  const original = panel.job;
  assert.ok(original.kind !== 'idle');
  const subject =
    original.kind === 'completed' || original.kind === 'position-action'
      ? original.snapshot.subject
      : original.subject;
  const observations = moveReviewObservations(original)!;
  let requested = 0;
  panel.actions.retry = () => requested++;
  panel.job = {
    kind: 'unsearched',
    subject,
    observationsPending: false,
    observations,
    observationFrame: { proofId: observations.paths[0].id, ply: 1, phase: 'after' },
  };
  const tree = renderMoveReview(panel);
  assert.ok(renderedText(tree).includes(panel.copy.gradeNotRequested));
  const action = findNodes(tree, 'button.button.button-thin.move-review__request-grade')[0];
  assert.ok(action);
  (action.data!.on!.click as () => void)();
  assert.equal(requested, 1);
  panel.job.observationFrame = null;
  const missing = renderMoveReview(panel);
  assert.ok(renderedText(missing).includes(panel.copy.observationsMissing));
  assert.equal(findNodes(missing, 'section.move-review__work').length, 0);
  assert.equal(findNodes(missing, 'div.move-review__proof').length, 0);
  panel.job = { kind: 'abstained', subject, stopCondition: 'cancelled', observations };
  const cancelled = renderMoveReview(panel);
  assert.ok(renderedText(cancelled).includes(panel.copy.stopReasons.cancelled));
  assert.equal(findNodes(cancelled, 'button.button.button-thin.move-review__request-grade').length, 1);
});

test('commentary uses the selected historical anchor and existing line inspection actions', () => {
  const panel = certifiedProps('Bf3');
  const observations = moveReviewObservations(panel.job)!;
  const path = observations.paths[0];
  const root = path.occurrenceIds[0]!;
  // Renderer contract only: the selector/decoder own certification tests.
  observations.selection = {
    version: 1,
    candidates: [
      {
        id: 'comparison:view',
        kind: 'comparison',
        state: 'ready',
        selection: 'selected',
        rootOccurrenceId: root,
        occurrenceIds: [root],
        positionIds: [],
        lineIds: [path.id],
        evidenceIds: [],
        sourceRefs: [],
        detail: { type: 'observed_material_comparison', text: 'Only the exchanges in these finite lines.' },
      },
    ],
    scenes: [
      {
        id: 'scene:view',
        rootOccurrenceId: root,
        occurrenceIds: [root],
        lineIds: [path.id],
        candidateIds: ['comparison:view'],
        state: 'ready',
      },
    ],
  };
  panel.job.observationFrame = { proofId: path.id, ply: 1, phase: 'after' };
  const scene = findNode(renderMoveReview(panel), 'article.move-review__scene');
  assert.match(renderedText(scene), /Only the exchanges in these finite lines/);
  (findNodes(scene, 'button')[0].data!.on!.click as () => void)();
  assert.deepEqual(panel.view.pinnedFrame, { proofId: path.id, ply: 1, phase: 'after' });
  assert.equal(panel.view.expandedProofId, path.id);
  panel.job.observationFrame = null;
  assert.equal(findNodes(renderMoveReview(panel), 'article.move-review__scene').length, 0);
  assert.ok(observations.statements.length > 0);
});

test('moment navigation follows game order from an unfinished move and reports a paused review', () => {
  const panel = certifiedProps('Bf3');
  const original = panel.job;
  assert.ok(original.kind !== 'idle');
  const subject =
    original.kind === 'completed' || original.kind === 'position-action'
      ? original.snapshot.subject
      : original.subject;
  panel.job = {
    kind: 'unsearched',
    subject: { ...subject, after: { ...subject.after, path: 'aabb' } },
    observationsPending: false,
    observationFrame: null,
    recordReview: {
      total: 4,
      assessed: 3,
      completed: 2,
      stopped: 1,
      status: 'paused',
      scenes: [
        { path: 'aa', label: '1. e4', state: 'ready' },
        { path: 'aabb', label: '1... e5', state: 'partial' },
        { path: 'aabbcc', label: '2. Nf3', state: 'ready' },
      ],
    },
  };
  const jumps: string[] = [];
  panel.actions.jumpToScene = path => jumps.push(path);
  const tree = renderMoveReview(panel);
  const nav = findNode(tree, 'nav');
  const buttons = findNodes(nav, 'button.button.button-thin');
  (buttons[0].data!.on!.click as () => void)();
  (buttons[1].data!.on!.click as () => void)();
  assert.deepEqual(jumps, ['aa', 'aabbcc']);
  assert.match(renderedText(tree), /Game review is paused/);
  assert.match(renderedText(tree), /still need explanatory evidence/);
  assert.equal(findNodes(tree, 'article.move-review__scene').length, 0);
});

function selectObservation(panelProps: MoveReviewPanelProps, text: RegExp, phase: 'before' | 'after'): VNode {
  const row = descendants(renderMoveReview(panelProps)).find(
    node =>
      (node.sel === 'div.move-review__observation' || node.sel === 'details.move-review__observation') &&
      text.test(renderedText(node)),
  );
  assert.ok(row, `Expected observation ${text}`);
  const button = findNodes(row, 'button.move-review__observation-frame').find(
    node => renderedText(node) === panelProps.copy.frameLabels[phase],
  );
  assert.ok(button, `Expected phase ${phase}`);
  (button.data!.on!.click as () => void)();
  return renderMoveReview(panelProps);
}

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
  if (review.core.kind !== 'move-verdict') throw new Error('Expected played verdict');
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

test('incomplete investigations keep the earlier assessment visible and restrict pair labels', () => {
  for (const kind of ['loading', 'fault', 'abstained'] as const) {
    const baseline = completedJob.snapshot;
    const shared = {
      subject: baseline.subject,
      assessment: { kind: 'completed' as const, evidence: baseline.evidence },
      assessmentState: 'needs_refresh' as const,
      assessmentScope: 'root_search' as const,
    };
    const job: MoveReviewJobState =
      kind === 'loading'
        ? { ...shared, kind }
        : kind === 'fault'
          ? { ...shared, kind, message: 'Offline', retryable: true }
          : { ...shared, kind, stopCondition: 'deadline_exceeded' };
    const tree = renderMoveReview(props({ job }));
    assert.match(renderedText(tree), /Previously established assessment/);
    assert.match(renderedText(tree), /previous reference, and new candidate still need a matched search/);
    assert.match(renderedText(tree), /Comparison reference/);
    assert.ok(findNode(tree, 'div.move-review__body'));
  }
  const pair = structuredClone(completedJob);
  pair.snapshot.assessmentScope = 'investigation_pair';
  const text = renderedText(renderMoveReview(props({ job: pair })));
  assert.match(text, /does not establish the best of all legal moves/);
  assert.match(text, /Comparison reference/);
  assert.ok(!text.includes(moveReviewCopy('en-US').runnerUp));
});

test('producer investigation states expose quiet observations, source lines and both comparison paths', () => {
  const fixtures = fixture(
    new URL('commentary-investigation-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const rootFixtures = fixtures.filter(
    source => !source.name.startsWith('response-') && !source.name.startsWith('continuation-'),
  );
  assert.equal(rootFixtures.length, 8);
  for (const source of rootFixtures) {
    const job = observationFixtureJob(source);
    const observations = moveReviewObservations(job)!;
    assert.ok(observations.selection?.inquiries?.length, source.name);
    const panel = props({ job });
    panel.actions.toggleProof = id => {
      panel.view.expandedProofId = id;
    };
    panel.actions.pinFrame = frame => {
      panel.view.pinnedFrame = frame;
    };
    let tree = renderMoveReview(panel);
    const inquiries = findNodes(tree, 'article.move-review__inquiry');
    assert.equal(inquiries.length, observations.selection.inquiries.length, source.name);
    const shared = findNode(tree, 'section.move-review__shared-observations');
    const sharedMoves = findNodes(shared, 'details.move-review__inquiry-observations');
    assert.ok(sharedMoves.length);
    assert.equal(sharedMoves.length, new Set(sharedMoves.map(node => node.key)).size);
    assert.equal(findNodes(inquiries[0], 'details.move-review__inquiry-observations').length, 0);
    if (source.move === 'g1f3') {
      assert.match(renderedText(shared), /White knight.*g1.*f3/);
      assert.match(renderedText(shared), /d4, e5/);
      assert.match(renderedText(shared), /support|attack/i);
    }
    for (const [index, inquiry] of observations.selection.inquiries.entries()) {
      const article = inquiries[index];
      const originalLine = inquiry.reasons.find(reason => reason.source_line_id)?.source_line_id;
      if (originalLine) {
        const button = findNodes(article, 'button.button.button-thin').find(button =>
          renderedText(button).includes('earlier line behind this question'),
        )!;
        assert.ok(button);
        (button.data!.on!.click as () => void)();
        assert.equal(panel.view.expandedProofId, originalLine);
        assert.equal(panel.view.pinnedFrame?.proofId, originalLine);
      }
      if (inquiry.answer) {
        const linkedReading = inquiry.explanation_candidate_ids?.[0];
        const answerArea = linkedReading
          ? findNodes(tree, 'article.move-review__pv-reading').find(
              node => node.data?.attrs?.id === `move-review-pv-${linkedReading}`,
            )!
          : article;
        assert.ok(answerArea);
        const links = findNodes(answerArea, 'button.button.button-thin').filter(button =>
          renderedText(button).includes('Inspect the comparison line'),
        );
        assert.equal(links.length, 2);
        for (const [i, lineId] of [
          inquiry.answer.reference_line_id,
          inquiry.answer.alternative_line_id,
        ].entries()) {
          (links[i].data!.on!.click as () => void)();
          assert.equal(panel.view.expandedProofId, lineId);
          assert.equal(panel.view.pinnedFrame?.proofId, lineId);
          tree = renderMoveReview(panel);
          assert.equal(
            renderedBoardConfig(tree).fen,
            observations.paths.find(path => path.id === lineId)!.moves[0].fenAfter,
          );
        }
      }
      assert.ok(!renderedText(article).includes(inquiry.id));
      assert.ok(!renderedText(article).includes(inquiry.position_id));
    }
    if (source.name === 'awaiting-probe')
      assert.match(renderedText(tree), /Previously established assessment/);
    if (source.name === 'awaiting-refresh' || source.name === 'stopped-refresh')
      assert.match(renderedText(tree), /assessment has not been refreshed/);
    if (source.name === 'refreshed-assessment')
      assert.match(renderedText(tree), /does not establish the best of all legal moves/);
    if (source.name === 'deadline-before-answer')
      assert.match(renderedText(tree), /Additional comparison stopped/);
  }
});

test('producer investigation DOM opens quiet evidence and comparison boards, then preserves a stopped refresh', t => {
  const fixtures = fixture(
    new URL('commentary-investigation-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const root = document.createElement('div');
  document.body.append(root);
  mountMoveReviewFixturePreview(root, fixtures);
  const main = document.getElementById('move-review-fixture-preview')!;
  const ground = () =>
    (
      main.querySelector('.cg-wrap') as HTMLElement & {
        _cg: { state: { lastMove?: Key[] }; destroy(): void };
      }
    )._cg;
  t.after(() => {
    ground().destroy();
    main.remove();
  });
  const details = main.querySelector<HTMLDetailsElement>('.move-review__inquiry-observations')!;
  assert.ok(details.textContent?.includes('d4, e5'));
  assert.equal(details.open, false);
  details.querySelector('summary')!.click();
  assert.equal(details.open, true);
  const source = observationFixtureJob(fixtures[0]);
  const questions = moveReviewObservations(source)!.selection!.inquiries!;
  const answered = questions.find(inquiry => inquiry.answer)!;
  const answerArea = answered.explanation_candidate_ids?.[0]
    ? document.getElementById(`move-review-pv-${answered.explanation_candidate_ids[0]}`)!
    : main.querySelectorAll('.move-review__inquiry')[questions.indexOf(answered)];
  const comparison = [...answerArea.querySelectorAll<HTMLButtonElement>('button')].filter(button =>
    button.textContent?.includes('Inspect the comparison line'),
  );
  comparison[1].click();
  assert.deepEqual(ground().state.lastMove, [
    answered.alternative_move.slice(0, 2),
    answered.alternative_move.slice(2, 4),
  ]);
  const picker = main.querySelector<HTMLSelectElement>('select[aria-label="Runtime fixture"]')!;
  picker.value = 'stopped-refresh';
  picker.dispatchEvent(new window.Event('change', { bubbles: true }));
  assert.ok(main.textContent?.includes('assessment has not been refreshed'));
  assert.ok(main.querySelector('.move-review__request-grade'));
  assert.equal(main.querySelector('.move-review__status')?.textContent, 'Assessment refresh incomplete');
});

test('opening PV readings distinguish close evaluations, observed differences and unmet evidence needs', () => {
  const fixtures = fixture(
    new URL('commentary-investigation-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const job = observationFixtureJob(fixtures.find(f => f.name === 'opening-equal-alternative')!);
  const observations = moveReviewObservations(job)!;
  const panel = props({ job });
  const tree = renderMoveReview(panel);
  const readings = findNodes(tree, 'article.move-review__pv-reading');
  assert.equal(readings.length, 4);
  for (const reading of readings) {
    const text = renderedText(reading);
    const candidate = observations.selection!.candidates.find(candidate => candidate.id === reading.key)!;
    assert.match(text, candidate.detail.quality_band === 'close' ? /moves are close/ : /moves are separated/);
    assert.match(text, /piece-role counts are the same/);
    assert.match(text, /events match in timing/);
    assert.match(text, /different recorded development or control changes/);
    assert.doesNotMatch(text, /alternative scored better|best move/i);
  }
  assert.match(renderedText(tree), /do not establish a forced result or the cause/);
  assert.match(renderedText(tree), /Evidence used to order this question/);
  assert.match(renderedText(tree), /does not mean another search is scheduled/);
  assert.match(renderedText(tree), /observed differences in the existing lines/);
  const primary = observations.selection!.candidates.find(
    c => c.kind === 'comparison' && c.detail.type !== 'pv_comparison',
  )!;
  assert.equal(
    findNodes(tree, 'article.move-review__scene').length,
    0,
    'The matching material scope is shown in its PV reading',
  );
  [primary.detail.reference_line_id, primary.detail.candidate_line_id] = [
    primary.detail.candidate_line_id,
    primary.detail.reference_line_id,
  ];
  assert.equal(
    findNodes(renderMoveReview(panel), 'article.move-review__scene').length,
    1,
    'A different ordered comparison must keep its separate scope',
  );
});

test('opening reading links preserve game history and root ownership while inspecting another line', t => {
  const fixtures = fixture(
    new URL('commentary-investigation-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const source = fixtures.find(f => f.name === 'opening-pv-priority')!;
  const job = observationFixtureJob(source);
  const observations = moveReviewObservations(job)!;
  const root = document.createElement('div');
  document.body.append(root);
  mountMoveReviewFixturePreview(root, [source]);
  const main = document.getElementById('move-review-fixture-preview')!;
  const ground = () =>
    (
      main.querySelector('.cg-wrap') as HTMLElement & {
        _cg: { state: { lastMove?: Key[] }; getFen(): string; destroy(): void };
      }
    )._cg;
  t.after(() => {
    ground().destroy();
    main.remove();
  });
  const cards = () => main.querySelectorAll<HTMLElement>('.move-review__pv-reading');
  const questions = () => main.querySelectorAll('.move-review__inquiry');
  const originalCount = questions().length;
  const ordering = main.querySelector<HTMLElement>('.move-review__inquiry-reading-links')!;
  const target = cards()[0];
  let scrolled = 0;
  target.scrollIntoView = () => {
    scrolled++;
  };
  const locationBefore = window.location.href;
  ordering.querySelector<HTMLButtonElement>('button')!.click();
  assert.equal(scrolled, 1);
  assert.equal(document.activeElement, target);
  assert.equal(
    window.location.href,
    locationBefore,
    'A reading link must not replace the analysis ply in the URL',
  );
  const alternative = [...target.querySelectorAll<HTMLButtonElement>('button')].filter(button =>
    button.textContent?.includes('Inspect the comparison line'),
  )[1];
  alternative.click();
  assert.notDeepEqual(ground().state.lastMove, ['f1', 'b5']);
  assert.equal(questions().length, originalCount);
  assert.equal(cards().length, 2);
  const historyIndex = observations.paths.findIndex(path => path.kind === 'history');
  assert.ok(historyIndex >= 0);
  main.querySelectorAll<HTMLButtonElement>('.move-review__observation-paths button')[historyIndex].click();
  main.querySelectorAll<HTMLButtonElement>('.move-review__proof-san')[1].click();
  assert.deepEqual(ground().state.lastMove, ['f1', 'b5']);
  assert.equal(questions().length, originalCount);
  const originalPosition = [...cards()[0].querySelectorAll<HTMLButtonElement>('button')].find(button =>
    button.textContent?.includes('Inspect at the original position'),
  )!;
  originalPosition.click();
  assert.equal(ground().getFen(), source.fen.split(' ')[0]);
  assert.equal(ground().state.lastMove, undefined);
  assert.equal(questions().length, originalCount);
});

test('continuation views retain searched scope and navigate delayed captures and paired functional differences', () => {
  const fixtures = fixture(
    new URL('commentary-investigation-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  for (const name of [
    'continuation-deep-awaiting',
    'continuation-deep-complete',
    'continuation-deep-stopped',
    'continuation-bare-capture',
    'continuation-move-order',
    'continuation-other-target',
    'continuation-discovery-awaiting',
    'continuation-discovery-complete',
    'continuation-recovery-relost',
    'continuation-route-awaiting',
    'continuation-route-sampled',
    'continuation-route-compared',
    'continuation-route-cancelled',
    'continuation-support-discovery-awaiting',
    'continuation-support-class-awaiting',
    'continuation-support-application',
    'continuation-support-use-unobserved',
    'continuation-support-frontier-awaiting',
    'continuation-support-frontier-complete',
    'continuation-exposure-awaiting',
    'continuation-exposure-other-arm',
    'continuation-exposure-complete',
    'continuation-direct-support-awaiting',
    'continuation-direct-support-frontier-awaiting',
    'continuation-direct-support-target-lost',
    'continuation-direct-support-recaptured',
    'continuation-supplied-support-current-reply',
    'continuation-supplied-support-other-current-arm',
    'continuation-quiet-exposure-awaiting',
    'response-answered',
  ]) {
    for (const locale of ['en-US', 'ko-KR'] as const) {
      const panel = certifiedProps(name, fixtures);
      panel.locale = locale;
      panel.copy = moveReviewCopy(locale);
      const observations = moveReviewObservations(panel.job)!;
      const selection = observations.selection!;
      let tree = renderMoveReview(panel);
      const questions = findNodes(tree, 'article.move-review__continuation-inquiry');
      assert.equal(questions.length, selection.continuation_inquiries?.length ?? 0, name);
      assert.equal(
        findNodes(tree, 'div.move-review__continuation-unresolved').length,
        selection.continuation_inquiries?.filter(question => question.unresolved_source_line_ids?.length)
          .length ?? 0,
      );
      assert.equal(
        findNodes(tree, 'div.move-review__continuation-followups').length,
        selection.continuation_inquiries?.filter(question => question.followup_question_ids?.length).length ??
          0,
      );
      const profiles = findNodes(tree, 'article.move-review__continuation-profile');
      const routeCards = findNodes(tree, 'div.move-review__route-challenge');
      if (name.startsWith('continuation-exposure-') || name === 'continuation-quiet-exposure-awaiting') {
        const exposure = findNodes(tree, 'div.move-review__capture-exposure')[0];
        assert.ok(exposure);
        assert.match(
          renderedText(exposure),
          locale === 'ko-KR' ? /이득 여부는 후속 수순/ : /material outcome depends on the continuation/,
        );
        const fact = selection.candidates
          .map(lineContinuationDetail)
          .find(detail => detail?.capture_exposures?.length)!.capture_exposures![0];
        (findNodes(exposure, 'button.button.button-thin')[0].data!.on!.click as () => void)();
        assert.equal(
          renderedBoardConfig(renderMoveReview(panel)).fen,
          observations.positions!.find(position => position.id === fact.position_id)!.fen,
        );
      }
      if (name.startsWith('continuation-direct-support-')) {
        const uses = findNodes(tree, 'div.move-review__direct-support-recapture');
        assert.ok(uses.length);
        assert.match(
          renderedText(uses[0]),
          locale === 'ko-KR' ? /실제로 되잡았습니다/ : /later recaptures its capturer in this line/,
        );
        if (name.endsWith('target-lost'))
          assert.match(
            renderedText(tree),
            locale === 'ko-KR' ? /후속 결과의 확인이 남은 수순/ : /follow-up remains unresolved/,
          );
      }
      assert.equal(
        routeCards.length,
        selection.continuation_inquiries?.filter(q => q.route_challenge).length ?? 0,
      );
      if (name.startsWith('continuation-route-')) {
        assert.ok(routeCards.length);
        const text = renderedText(routeCards[0]);
        if (name.endsWith('awaiting'))
          assert.match(
            text,
            locale === 'ko-KR'
              ? /묶음 전체와 기준 응수를 함께 조사 중/
              : /full missing reply classes together/,
          );
        if (name.endsWith('sampled'))
          assert.match(
            text,
            locale === 'ko-KR' ? /같은 검색의 대조 미확정/ : /same-search comparison unresolved/,
          );
        if (name.endsWith('cancelled'))
          assert.match(text, locale === 'ko-KR' ? /보완 조사 중단/ : /follow-up search stopped/);
        if (name.endsWith('compared')) {
          assert.match(
            text,
            locale === 'ko-KR' ? /같은 검색의 대조 있음/ : /same-search comparison available/,
          );
          const q = selection.continuation_inquiries!.find(q => q.route_challenge?.state === 'compared')!;
          const comparison = q.route_challenge!.move_classes[0].comparison!;
          for (const [index, id] of [comparison.reference_line_id, comparison.challenge_line_id].entries()) {
            (findNodes(routeCards[0], 'button.button.button-thin')[index].data!.on!.click as () => void)();
            const branch = observations.paths.find(path => path.id === id)!;
            assert.equal(
              renderedBoardConfig(renderMoveReview(panel)).fen,
              branch.moves[branch.moves.length - 1].fenAfter,
            );
          }
        }
      }
      if (name.startsWith('continuation-support-')) {
        assert.match(
          renderedText(tree),
          locale === 'ko-KR' ? /재포획할 기물이 여러 개/ : /Several pieces can recapture/,
        );
        assert.ok(findNodes(tree, 'div.move-review__support-application').length);
        if (name.includes('frontier-'))
          assert.match(
            renderedText(tree),
            locale === 'ko-KR' ? /이 대안의 정확한 후속 수순/ : /this recapture’s exact continuation/,
          );
        for (const profile of profiles)
          assert.ok(
            findNodes(profile, 'div.move-review__support-application').length <= 1,
            'the same knight support use has one display even when two comparisons cite it',
          );
        if (name.endsWith('use-unobserved'))
          assert.match(
            renderedText(tree),
            locale === 'ko-KR' ? /후속 결과의 확인이 남은 수순/ : /follow-up remains unresolved/,
          );
        if (name.endsWith('class-awaiting'))
          assert.match(
            renderedText(tree),
            locale === 'ko-KR' ? /재포획 대안 미관측/ : /recapture alternative not observed/,
          );
        if (name.endsWith('application')) {
          const fixed = findNodes(tree, 'div.move-review__recapturer-support');
          assert.ok(fixed.some(card => /f3/.test(renderedText(card))));
          const uses = findNodes(tree, 'p.move-review__king-capture');
          assert.ok(uses.length >= 2);
          assert.ok(
            uses.some(use =>
              (locale === 'ko-KR' ? /합법수 있음/ : /has a legal capture/).test(renderedText(use)),
            ),
          );
          assert.ok(
            uses.some(use =>
              (locale === 'ko-KR' ? /합법수 없음/ : /has no legal capture/).test(renderedText(use)),
            ),
          );
          const single = selection.candidates
            .map(lineContinuationDetail)
            .find(detail => detail?.support_applications?.length)!;
          const application = single.support_applications![0];
          (
            findNodes(
              findNodes(tree, 'div.move-review__support-application')[0],
              'button.button.button-thin',
            )[0].data!.on!.click as () => void
          )();
          assert.equal(
            renderedBoardConfig(renderMoveReview(panel)).fen,
            observations.positions!.find(position => position.id === application.position_id)!.fen,
          );
        }
      }
      assert.equal(
        profiles.length,
        selection.candidates.filter(candidate => lineContinuationDetail(candidate)).length,
        name,
      );
      const pairs = findNodes(tree, 'article.move-review__continuation-comparison');
      assert.equal(
        pairs.length,
        selection.candidates.filter(candidate => continuationComparisonDetail(candidate)).length,
        name,
      );
      if (name === 'continuation-deep-complete')
        assert.match(
          renderedText(tree),
          locale === 'ko-KR' ? /전체 합법 범위를 검색함/ : /full legal scope was searched/,
        );
      if (name === 'continuation-deep-stopped')
        assert.match(
          renderedText(tree),
          locale === 'ko-KR' ? /후속 조사 중단/ : /Continuation investigation stopped/,
        );
      if (name === 'continuation-bare-capture') {
        assert.match(
          renderedText(tree),
          locale === 'ko-KR' ? /관측한 다음 수: 없음/ : /Observed next moves: none/,
        );
        assert.match(
          renderedText(tree),
          locale === 'ko-KR'
            ? /지원 대상의 변화|이후의 후속 수순은 아직 없습니다/
            : /Changes in supported targets|has no recorded continuation yet/,
        );
      }
      if (name === 'continuation-other-target') {
        const shared = findNodes(tree, 'div.move-review__shared-attacker');
        assert.ok(shared.length, 'the same queen connects both observed capture targets');
        assert.match(renderedText(shared[0]), locale === 'ko-KR' ? /모두 통제하며/ : /controls both/);
        const sharedDetail = selection.candidates
          .flatMap(candidate => continuationComparisonDetail(candidate)?.differences ?? [])
          .find(difference => difference.kind === 'loss_followup' && difference.shared_attackers?.length)!;
        assert.equal(sharedDetail.kind, 'loss_followup');
        if (sharedDetail.kind === 'loss_followup') {
          (findNodes(shared[0], 'button.button.button-thin')[0].data!.on!.click as () => void)();
          assert.equal(
            renderedBoardConfig(renderMoveReview(panel)).fen,
            observations.positions!.find(
              position => position.id === sharedDetail.shared_attackers![0].position_id,
            )!.fen,
          );
        }
        assert.match(
          renderedText(tree),
          locale === 'ko-KR' ? /수순 끝까지 미관측/ : /not observed through line end/,
        );
        assert.match(
          renderedText(tree),
          locale === 'ko-KR' ? /이 손실에 연결된 다른 대상 포획/ : /Other-target gains linked to this loss/,
        );
      }
      if (name === 'continuation-discovery-awaiting') {
        assert.match(
          renderedText(tree),
          locale === 'ko-KR'
            ? /합법적인 포획 또는 체크 후보가 있어 그 위치의 전체 응수를 조사함/
            : /A legal capture or check at this line’s end prompts a search of all choices/,
        );
        assert.match(
          renderedText(tree),
          locale === 'ko-KR'
            ? /앞서 포획한 기물에 상대의 통제가 닿는 위치/
            : /earlier capturer is under opposing control/,
        );
        assert.match(
          renderedText(tree),
          locale === 'ko-KR' ? /후속 결과의 확인이 남은 수순/ : /Lines whose follow-up remains unresolved/,
        );
      }
      if (name === 'continuation-recovery-relost') {
        assert.match(renderedText(tree), locale === 'ko-KR' ? /에서 다시 잡혔습니다/ : /is then captured by/);
        assert.match(
          renderedText(tree),
          locale === 'ko-KR' ? /후속 결과의 확인이 남은 수순/ : /Lines whose follow-up remains unresolved/,
        );
      }
      if (name === 'response-answered')
        assert.match(renderedText(tree), locale === 'ko-KR' ? /원래 기물/ : /original piece/);
      assert.match(renderedText(tree), locale === 'ko-KR' ? /충분한 보상/ : /sufficient compensation/);
      for (const [index, card] of questions.entries()) {
        const question = selection.continuation_inquiries![index];
        const button = findNodes(card, 'button.button.button-thin').find(button =>
          renderedText(button).includes(locale === 'ko-KR' ? '조사 위치 보기' : 'Inspect this choice point'),
        )!;
        (button.data!.on!.click as () => void)();
        assert.equal(
          renderedBoardConfig(renderMoveReview(panel)).fen,
          observations.positions!.find(position => position.id === question.position_id)!.fen,
        );
        const unresolvedId = question.unresolved_source_line_ids?.[0];
        if (unresolvedId) {
          const unresolved = observations.paths.find(path => path.id === unresolvedId)!;
          const control = findNodes(card, 'button.button.button-thin').find(
            button => renderedText(button) === unresolved.moves.map(move => move.label).join(' '),
          )!;
          (control.data!.on!.click as () => void)();
          assert.equal(
            renderedBoardConfig(renderMoveReview(panel)).fen,
            unresolved.moves[unresolved.moves.length - 1].fenAfter,
          );
        }
        for (const id of question.followup_question_ids ?? [])
          assert.ok(questions.some(card => card.data?.attrs?.id === `continuation-question-${id}`));
      }
      const profile = selection.candidates.find(
        candidate => lineContinuationDetail(candidate)?.capture_links.length,
      );
      if (profile) {
        const detail = lineContinuationDetail(profile)!;
        const link = detail.capture_links.find(link => link.elapsed_plies > 1) ?? detail.capture_links[0];
        const event = detail.events.find(event => event.id === link.capture_event_id)!;
        tree = renderMoveReview(panel);
        const card = findNodes(tree, 'article.move-review__continuation-profile').find(
          card => card.key === profile.id,
        )!;
        const path = observations.paths.find(path => path.id === detail.line_id)!;
        const button = findNodes(card, 'button.button.button-thin').find(
          button => renderedText(button) === path.moves[event.ply_offset].label,
        )!;
        (button.data!.on!.click as () => void)();
        assert.equal(renderedBoardConfig(renderMoveReview(panel)).fen, path.moves[event.ply_offset].fenAfter);
        assert.equal(
          findNodes(renderMoveReview(panel), 'article.move-review__continuation-inquiry').length,
          questions.length,
        );
      }
    }
  }
});

test('producer response states separate reused observations, child evaluations and stopped work', () => {
  const fixtures = fixture(
    new URL('commentary-investigation-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  for (const name of [
    'response-awaiting',
    'response-answered',
    'response-stopped',
    'response-observed',
    'response-pinned-recapture',
  ]) {
    const source = fixtures.find(fixture => fixture.name === name);
    assert.ok(source, name);
    const job = observationFixtureJob(source);
    const observations = moveReviewObservations(job)!;
    const inquiries = observations.selection!.response_inquiries!;
    assert.ok(inquiries.length, name);
    for (const locale of ['en-US', 'ko-KR'] as const) {
      const panel = props({ job, locale });
      panel.actions.toggleProof = id => {
        panel.view.expandedProofId = id;
      };
      panel.actions.pinFrame = frame => {
        panel.view.pinnedFrame = frame;
      };
      const cards = findNodes(renderMoveReview(panel), 'article.move-review__response-inquiry');
      assert.equal(cards.length, inquiries.length, name);
      if (name === 'response-answered') {
        const comparisons = findNodes(renderMoveReview(panel), 'section.move-review__related-responses');
        assert.equal(comparisons.length, 1, `${locale}: symmetric links produce one comparison`);
        const rows = findNodes(comparisons[0], 'tr');
        const pawnRow = rows.find(row => row.key === 'd2d3')!;
        const bishopRow = rows.find(row => row.key === 'f1d3')!;
        assert.ok(pawnRow && bishopRow, `${locale}: both root arms are compared`);
        assert.match(
          renderedText(pawnRow),
          locale === 'ko-KR' ? /기록된 지원 대상 없음/ : /No recorded support targets/,
        );
        assert.doesNotMatch(
          renderedText(bishopRow),
          locale === 'ko-KR' ? /기록된 지원 대상 없음/ : /No recorded support targets/,
        );
        assert.match(renderedText(bishopRow), /f3/);
        assert.match(
          renderedText(comparisons[0]),
          locale === 'ko-KR' ? /강제 응수나 첫 수의 우열/ : /does not establish forced replies or rank/,
        );
      }
      for (const [index, inquiry] of inquiries.entries()) {
        const card = cards[index];
        const text = renderedText(card);
        assert.ok(!text.includes(inquiry.id));
        assert.ok(!text.includes(inquiry.child_position_id));
        const childButton = findNodes(card, 'button.button.button-thin').find(button =>
          renderedText(button).includes(
            locale === 'ko-KR' ? '응수를 두기 전 대상' : 'target before the reply',
          ),
        )!;
        assert.ok(childButton, `${name}: ${locale} child board action`);
        (childButton.data!.on!.click as () => void)();
        assert.equal(
          renderedBoardConfig(renderMoveReview(panel)).fen,
          observations.positions!.find(position => position.id === inquiry.child_position_id)!.fen,
        );
        assert.equal(
          findNodes(renderMoveReview(panel), 'article.move-review__response-inquiry').length,
          inquiries.length,
        );
        if (inquiry.state === 'observed') {
          assert.equal(inquiry.answer, null);
          assert.match(text, locale === 'ko-KR' ? /기존 관측을 재사용/ : /Existing observations were reused/);
          assert.doesNotMatch(text, /Evaluation comparison obtained/);
          assert.doesNotMatch(text, /from White’s side/);
        }
        if (name === 'response-stopped' && inquiry.root_move === 'd2d3') {
          assert.equal(inquiry.state, 'stopped');
          const candidate = observations.selection!.candidates.find(
            candidate => targetResponseDetail(candidate)?.response_inquiry_id === inquiry.id,
          )!;
          assert.equal(candidate.state, 'partial');
          const branch = targetResponseDetail(candidate)!.branches.find(
            branch => branch.reply_move === 'd5e4',
          )!;
          assert.equal(branch.target_state, 'captured');
          assert.equal(branch.observed_recapture.state, 'not_observed');
          assert.match(text, locale === 'ko-KR' ? /원래 대상이 잡힘/ : /original target was captured/);
          assert.match(text, locale === 'ko-KR' ? /합법적인 즉시 재포획/ : /Legal immediate recaptures/);
          assert.match(
            text,
            locale === 'ko-KR'
              ? /회수 뒤 지원과 지연 회수는 미확정/
              : /Support after a recapture and later recovery remain unresolved/,
          );
          assert.doesNotMatch(
            text,
            locale === 'ko-KR' ? /기록된 지원 대상 없음/ : /No recorded support targets/,
          );
        }
        if (inquiry.answer) {
          assert.match(text, locale === 'ko-KR' ? /원래 착수의 평가/ : /original move assessment/);
          for (const lineId of [inquiry.answer.reference_line_id, inquiry.answer.alternative_line_id]) {
            const path = observations.paths.find(path => path.id === lineId)!;
            assert.ok(path.moves.length >= 2);
            assert.equal(path.moves[0].uci, inquiry.root_move);
            assert.ok(text.includes(path.moves[1].label));
          }
          if (name === 'response-answered' && locale === 'en-US') {
            assert.equal(inquiry.answer.evaluation_relation, 'worse');
            assert.match(text, /lower for the responding side/);
            assert.match(text, /-1.00 from White’s side/);
            assert.match(text, /\+0.40 from White’s side/);
          }
        }
      }
      if (locale === 'en-US' && name === 'response-stopped')
        assert.match(renderedText(renderMoveReview(panel)), /Additional comparison stopped/);
    }
  }
});

test('response selection scope stays distinct from complete observations and comparisons across searches', () => {
  const fixtures = fixture(
    new URL('commentary-investigation-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  for (const locale of ['en-US', 'ko-KR'] as const) {
    for (const name of ['response-observed', 'response-third-quiet']) {
      const job = observationFixtureJob(fixtures.find(source => source.name === name)!);
      const text = renderedText(renderMoveReview(props({ job, locale })));
      assert.match(text, locale === 'ko-KR' ? /응수 선택 근거/ : /Why these replies were selected/);
      assert.match(text, locale === 'ko-KR' ? /전체 합법 응수/ : /all legal replies/);
      assert.match(
        text,
        locale === 'ko-KR'
          ? /반환하지 않은 응수가 약하다는 뜻은 아닙니다/
          : /absent from the returned sample are not thereby weak/,
      );
      if (name === 'response-observed') {
        assert.match(text, locale === 'ko-KR' ? /1개의 지정 응수/ : /1 specified reply/);
        assert.doesNotMatch(
          text,
          locale === 'ko-KR' ? /응수하는 쪽의 검색 평가값이 더/ : /for the responding side/,
        );
      }
    }
  }
});

test('related response summary preserves incomparable recorded recapturers and each exact board path', () => {
  const fixtures = fixture(
    new URL('commentary-investigation-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const job = observationFixtureJob(
    fixtures.find(f => f.name === 'continuation-supplied-support-other-current-arm')!,
  );
  const observations = moveReviewObservations(job)!;
  const selection = observations.selection!;
  const inquiry = selection.response_inquiries!.find(
    q => q.root_move === 'g1h1' && q.target.square === 'e4',
  )!;
  const retained = selection.candidates.find(
    c => targetResponseDetail(c)?.response_inquiry_id === inquiry.id,
  )!;
  const knight = targetResponseDetail(retained)!.branches[0];
  const decline = selection.candidates
    .flatMap(c => targetResponseDetail(c)?.branches ?? [])
    .find(b => b.reply_move === 'g8g7')!;
  const shortPath = structuredClone(observations.paths.find(p => p.id === knight.line_id)!);
  shortPath.id += ':short';
  shortPath.moves = shortPath.moves.slice(0, 2);
  shortPath.occurrenceIds = shortPath.occurrenceIds.slice(0, 2);
  shortPath.positionIds = shortPath.positionIds!.slice(0, 3);
  delete shortPath.lineSummary;
  observations.paths.push(shortPath);

  // Arrange the independently certified pawn path beside the existing knight facts, after decoding.
  // This exercises the renderer's observation selection, not a new producer or chess evaluator.
  const donor = moveReviewObservations(
    observationFixtureJob(fixtures.find(f => f.name === 'continuation-direct-support-recaptured')!),
  )!;
  const pawnPath = donor.paths.find(p => p.moves.map(m => m.uci).join(' ') === 'g1h1 d5e4 f3e4')!;
  const pawnProfile = donor
    .selection!.candidates.map(lineContinuationDetail)
    .find(d => d?.line_id === pawnPath.id)!;
  const pawnEvent = pawnProfile.events.find(e => e.ply_offset === 2)!;
  assert.equal(pawnEvent.actor_end.state, 'present');
  assert.ok(
    !pawnProfile.support_comparisons.find(c => c.kind === 'replacement_after_capture')!.targets.length,
  );
  if (!observations.paths.some(p => p.id === pawnPath.id)) observations.paths.push(pawnPath);
  const pawn = structuredClone(knight);
  pawn.line_id = pawnPath.id;
  pawn.observed_recapture = {
    state: 'complete',
    occurrence_id: pawnEvent.occurrence_id,
    actor: { original: pawnEvent.original_actor!, current: pawnEvent.actor_end.piece! },
    supported_targets: [],
    source_ids: [],
  };
  const current = structuredClone(retained);
  current.id += ':short';
  current.state = 'partial';
  const currentDetail = targetResponseDetail(current)!;
  currentDetail.branches = [structuredClone(knight), structuredClone(decline)];
  currentDetail.branches[0].line_id = shortPath.id;
  currentDetail.branches[0].observed_recapture = {
    state: 'not_observed',
    occurrence_id: null,
    actor: null,
    supported_targets: [],
    source_ids: [],
  };
  const other = structuredClone(retained);
  other.id += ':pawn';
  targetResponseDetail(retained)!.branches = [knight, structuredClone(decline)];
  targetResponseDetail(other)!.branches = [pawn, structuredClone(decline)];
  inquiry.alternative_move = decline.reply_move;
  selection.candidates = [current, retained, other, ...selection.candidates.filter(c => c !== retained)];
  const score = structuredClone(inquiry.answer);
  for (const locale of ['en-US', 'ko-KR'] as const) {
    const panel = props({ job, locale });
    panel.actions.toggleProof = id => {
      panel.view.expandedProofId = id;
    };
    panel.actions.pinFrame = frame => {
      panel.view.pinnedFrame = frame;
    };
    const table = findNodes(renderMoveReview(panel), 'section.move-review__related-responses').find(n =>
      renderedText(n).includes('e4'),
    )!;
    const rows = findNodes(table, 'tr').filter(n => String(n.key).startsWith('g1h1'));
    assert.equal(rows.length, 2);
    assert.match(
      renderedText(table),
      locale === 'ko-KR' ? /여러 후속 관측/ : /Multiple recorded continuations/,
    );
    for (const branch of [knight, pawn]) {
      const row = rows.find(row =>
        renderedText(row).includes(branch.observed_recapture.actor!.original.square),
      )!;
      assert.ok(row);
      assert.doesNotMatch(renderedText(row), locale === 'ko-KR' ? /기록되지 않음/ : /Not recorded/);
      const path = observations.paths.find(p => p.id === branch.line_id)!;
      const pathButton = findNodes(row, 'button.button.button-thin').find(
        b =>
          renderedText(b) ===
          path.moves
            .slice(2)
            .map(m => m.label)
            .join(' '),
      )!;
      (pathButton.data!.on!.click as () => void)();
      assert.equal(panel.view.pinnedFrame?.proofId, path.id);
      assert.equal(renderedBoardConfig(renderMoveReview(panel)).fen, path.moves.at(-1)!.fenAfter);
      const recapturer = findNodes(row, 'button.button.button-thin').at(-1)!;
      (recapturer.data!.on!.click as () => void)();
      assert.equal(panel.view.pinnedFrame?.proofId, path.id);
      assert.equal(renderedBoardConfig(renderMoveReview(panel)).fen, path.moves[2].fenAfter);
    }
    assert.deepEqual(inquiry.answer, score);
    assert.equal(current.state, 'partial');
  }
});

test('related response summary keeps complete capture facts when only the decline branch is partial', () => {
  const fixtures = fixture(
    new URL('commentary-investigation-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const job = observationFixtureJob(fixtures.find(f => f.name === 'response-answered')!);
  const observations = moveReviewObservations(job)!;
  const inquiry = observations.selection!.response_inquiries!.find(q => q.root_move === 'd2d3')!;
  const retained = observations.selection!.candidates.find(
    c => c.state === 'ready' && targetResponseDetail(c)?.response_inquiry_id === inquiry.id,
  )!;
  const detail = targetResponseDetail(retained)!;
  const capture = detail.branches.find(b => b.reply_move === inquiry.reference_move)!;
  const decline = detail.branches.find(b => b.reply_move !== inquiry.reference_move)!;
  const score = structuredClone(inquiry.answer);
  // Only the decline's control evidence is missing; the capture's certified board and support stay known.
  retained.state = 'partial';
  decline.providers = { state: 'unknown', pieces: [], source_ids: [] };
  assert.equal(capture.observed_recapture.state, 'complete');
  const path = observations.paths.find(p => p.id === capture.line_id)!;
  for (const locale of ['en-US', 'ko-KR'] as const) {
    const panel = props({ job, locale });
    panel.actions.toggleProof = id => {
      panel.view.expandedProofId = id;
    };
    panel.actions.pinFrame = frame => {
      panel.view.pinnedFrame = frame;
    };
    const table = findNode(renderMoveReview(panel), 'section.move-review__related-responses');
    const row = findNodes(table, 'tr').find(n => n.key === 'd2d3')!;
    assert.match(
      renderedText(row),
      locale === 'ko-KR' ? /기록된 지원 대상 없음/ : /No recorded support targets/,
    );
    assert.doesNotMatch(
      renderedText(row),
      locale === 'ko-KR' ? /기록되지 않음|미확정/ : /Not recorded|Unresolved/,
    );
    const recapturer = findNodes(row, 'button.button.button-thin').at(-1)!;
    (recapturer.data!.on!.click as () => void)();
    assert.equal(panel.view.pinnedFrame?.proofId, path.id);
    assert.equal(renderedBoardConfig(renderMoveReview(panel)).fen, path.moves[2].fenAfter);
    assert.deepEqual(inquiry.answer, score);
    assert.equal(retained.state, 'partial');
    assert.equal(decline.providers.state, 'unknown');
  }
});

test('response DOM keeps the original question while navigating replies, recaptures and original targets', t => {
  const fixtures = fixture(
    new URL('commentary-investigation-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const source = fixtures.find(fixture => fixture.name === 'response-answered')!;
  const job = observationFixtureJob(source);
  const observations = moveReviewObservations(job)!;
  const readings = observations.selection!.response_inquiries!.flatMap(inquiry =>
    observations.selection!.candidates.flatMap(candidate => {
      const detail = targetResponseDetail(candidate);
      return detail?.response_inquiry_id === inquiry.id ? [detail] : [];
    }),
  );
  assert.ok(readings.length);
  const root = document.createElement('div');
  document.body.append(root);
  mountMoveReviewFixturePreview(root, [source]);
  const main = document.getElementById('move-review-fixture-preview')!;
  const ground = () =>
    (
      main.querySelector('.cg-wrap') as HTMLElement & {
        _cg: { state: { lastMove?: Key[] }; getFen(): string; destroy(): void };
      }
    )._cg;
  t.after(() => {
    ground().destroy();
    main.remove();
  });
  const initialCount = main.querySelectorAll('.move-review__response-inquiry').length;
  assert.ok(initialCount);
  const comparison = () => main.querySelector<HTMLElement>('.move-review__related-responses')!;
  assert.equal(main.querySelectorAll('.move-review__related-responses').length, 1);
  assert.match(comparison().textContent!, /Original target · White pawn e4/);
  const relatedRows = () => [...comparison().querySelectorAll<HTMLTableRowElement>('tbody tr')];
  assert.equal(relatedRows().length, 2);
  for (const [index, inquiry] of observations.selection!.response_inquiries!.entries()) {
    const detail = readings.find(
      reading =>
        reading.response_inquiry_id === inquiry.id &&
        reading.branches.some(
          branch => branch.reply_move === inquiry.reference_move && branch.observed_recapture.actor,
        ),
    )!;
    const branch = detail.branches.find(branch => branch.reply_move === inquiry.reference_move)!;
    const path = observations.paths.find(path => path.id === branch.line_id)!;
    const rowText = relatedRows()[index].textContent!;
    assert.match(rowText, /Captured/);
    if (inquiry.root_move === 'd2d3') {
      assert.match(rowText, /White pawn e4 \(originally White pawn d2\)/);
      assert.match(rowText, /No recorded support targets/);
    } else {
      assert.equal(inquiry.root_move, 'f1d3');
      assert.match(rowText, /White bishop e4 \(originally White bishop f1\)/);
      assert.match(rowText, /White knight f3/);
    }
    for (const [buttonIndex, ply] of [
      [0, 0],
      [1, 1],
      [2, 2],
    ]) {
      relatedRows()[index].querySelectorAll<HTMLButtonElement>('button')[buttonIndex].click();
      assert.equal(ground().getFen(), path.moves[ply].fenAfter.split(' ')[0]);
      assert.equal(main.querySelectorAll('.move-review__response-inquiry').length, initialCount);
      assert.equal(main.querySelectorAll('.move-review__related-responses').length, 1);
    }
  }
  comparison().querySelector<HTMLButtonElement>('button')!.click();
  assert.equal(ground().getFen(), source.fen.split(' ')[0]);
  assert.equal(ground().state.lastMove, undefined);
  for (const [index, branch] of readings.flatMap(reading => reading.branches).entries()) {
    main
      .querySelectorAll<HTMLElement>('.move-review__response-branch')
      [index].querySelector<HTMLButtonElement>('button')!
      .click();
    const path = observations.paths.find(path => path.id === branch.line_id)!;
    const ply = path.occurrenceIds.indexOf(branch.reply_occurrence_id);
    assert.equal(ground().getFen(), path.moves[ply].fenAfter.split(' ')[0]);
    assert.deepEqual(ground().state.lastMove, [branch.reply_move.slice(0, 2), branch.reply_move.slice(2, 4)]);
    assert.equal(main.querySelectorAll('.move-review__response-inquiry').length, initialCount);
    assert.equal(
      main
        .querySelectorAll<HTMLButtonElement>('.move-review__observation-paths button')
        [observations.paths.findIndex(value => value.id === branch.line_id)].getAttribute('aria-pressed'),
      'true',
    );
  }
  const recaptured = readings
    .flatMap(reading => reading.branches)
    .find(branch => branch.observed_recapture.state === 'complete');
  assert.ok(recaptured, 'The producer must exercise actual recapture ownership');
  [...main.querySelectorAll<HTMLButtonElement>('.move-review__response-branch button')]
    .find(button => button.textContent?.includes('Inspect after the recorded recapture'))!
    .click();
  const recapturePath = observations.paths.find(path => path.id === recaptured.line_id)!;
  const recapturePly = recapturePath.occurrenceIds.indexOf(recaptured.observed_recapture.occurrence_id!);
  assert.equal(ground().getFen(), recapturePath.moves[recapturePly].fenAfter.split(' ')[0]);
  [...main.querySelectorAll<HTMLButtonElement>('.move-review__response-inquiry button')]
    .find(button => button.textContent?.includes('Original target'))!
    .click();
  assert.equal(ground().getFen(), source.fen.split(' ')[0]);
  assert.equal(ground().state.lastMove, undefined);
  assert.equal(main.querySelectorAll('.move-review__response-inquiry').length, initialCount);
});

test('target response view distinguishes unknown evidence from a certified missing target or legal recapture', () => {
  const fixtures = fixture(
    new URL('commentary-investigation-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const source = fixtures.find(fixture => fixture.name === 'response-pinned-recapture')!;
  const job = observationFixtureJob(source);
  const observations = moveReviewObservations(job)!;
  const detail = observations
    .selection!.candidates.flatMap(candidate => {
      const detail = targetResponseDetail(candidate);
      return detail ? [detail] : [];
    })
    .find(detail =>
      detail.branches.some(
        branch =>
          branch.target_state === 'captured' &&
          branch.recaptures.state === 'complete' &&
          !branch.recaptures.moves.length,
      ),
    )!;
  assert.ok(detail, 'Pinned recapture must produce known legal emptiness');
  const branch = detail.branches.find(
    branch => branch.target_state === 'captured' && !branch.recaptures.moves.length,
  )!;
  const panel = props({ job });
  const known = renderedText(renderMoveReview(panel));
  assert.match(known, /original target was captured/i);
  assert.match(known, /no legal immediate recapture/);
  // This is a renderer-only partial-state check, after the real producer fixture has been admitted.
  branch.target_state = 'unknown';
  branch.target = null;
  branch.providers = { state: 'unknown', pieces: [], source_ids: [] };
  branch.capturer = null;
  branch.recaptures = { state: 'unknown', moves: [] };
  branch.observed_recapture = {
    state: 'unknown',
    occurrence_id: null,
    actor: null,
    supported_targets: [],
    source_ids: [],
  };
  const card = findNodes(renderMoveReview(panel), 'section.move-review__target-response-reading').find(
    card =>
      card.key ===
      observations.selection!.candidates.find(candidate => targetResponseDetail(candidate) === detail)!.id,
  )!;
  const unknown = renderedText(
    findNodes(card, 'div.move-review__response-branch').find(node => node.key === branch.line_id)!,
  );
  assert.match(unknown, /target’s state is not established/);
  assert.match(unknown, /Legal immediate recaptures are not established/);
  assert.doesNotMatch(unknown, /target was captured|no legal immediate recapture/i);
});

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
  assert.match(renderedText(summary), /Initial search #1 .* · Initial search #2 .*:/);
  assert.equal(renderedText(findNode(summary, 'h3')), 'Played the search reference');
  assert.match(renderedText(panel), /51\.8% → 52\.8%.*\+0\.9%p/);
  assert.equal(findNodes(summary, 'span.move-review__verdict-badge').length, 0);
});

test('a poor runner-up does not label the played reference a blunder, and both paths stay navigable', () => {
  const raw = fixture(bestChoiceFixtureUrl);
  const primary = object(
    object((object(raw.result).selected_move_reviews as JsonObject[])[0]!.commentary).primary,
  );
  // Controlled score variation tests display direction, not engine analysis quality.
  primary.runner_up_verdict_code = 'blunder';
  object(primary.runner_up_endpoint).win_percent_for_mover = 10;
  object(primary.delta).candidate_win_percent_delta_for_mover = -42.8;
  const job = completedFrom(raw);
  const review = verdictReview(job);
  const [playedProof, runnerUpProof] = review.comparisonProofs;
  assert.ok(playedProof && runnerUpProof);
  assert.notEqual(playedProof.id, runnerUpProof.id);
  const calls: string[] = [];
  const panel = renderMoveReview(
    props({
      job,
      canWrite: true,
      view: {
        evidenceExpanded: true,
        expandedProofId: runnerUpProof.id,
        pinnedFrame: { proofId: runnerUpProof.id, ply: 2 },
      },
      actions: {
        toggleProof: id => calls.push(`open:${id}`),
        addProof: id => calls.push(`add:${id}`),
      },
    }),
  );
  const summary = findNode(panel, 'section.move-review__summary');
  assert.equal(renderedText(findNode(summary, 'h3')), 'Played the search reference');
  assert.match(renderedText(summary), /Initial search #2 d2d4: Blunder/);
  assert.match(renderedText(panel), /10\.0% → 52\.8%.*\+42\.8%p/);
  const entries = findNodes(panel, 'button.move-review__proof-entry-button');
  assert.equal(entries.length, 2);
  entries[0]!.data!.on!.click({} as MouseEvent, entries[0]!);
  const add = findNode(panel, 'button.button.button-thin.move-review__add');
  add.data!.on!.click({} as MouseEvent, add);
  assert.deepEqual(calls, [`open:${playedProof.id}`, `add:${runnerUpProof.id}`]);
  assert.equal(renderedBoardConfig(panel).fen, runnerUpProof.moves[1]!.fenAfter);
  assert.deepEqual(renderedBoardConfig(panel).lastMove, ['d7', 'd5']);
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
          job: { kind: 'abstained', subject, stopCondition: 'browser_unsupported' },
        }),
      ),
    ),
    /browser environment does not meet the engine requirements/,
  );
  assert.match(
    renderedText(
      renderMoveReview(props({ job: { kind: 'abstained', subject, stopCondition: 'cancelled' } })),
    ),
    /Analysis was cancelled/i,
  );
});

test('producer observations survive stopped searches and navigate the recapture history', () => {
  for (const [name, prefix] of [
    ['development-cancelled-produced', []],
    ['development-engine_unavailable-produced', []],
    ['development-browser_unsupported-produced', []],
    ['recapture-stopped-produced', ['e2e4', 'd7d5', 'e4d5']],
    ['capture-unknown-stopped-produced', []],
  ] as const) {
    const raw = fixture(new URL(`${name}.json`, bestChoiceFixtureUrl));
    const observations = object(raw.observations);
    const paths = observations.paths as JsonObject[];
    const subject: MoveReviewSubject = {
      ...subjectFor(raw),
      initialFen: prefix.length ? initialFen : (paths[0]!.start_fen as FEN),
      movePrefixUci: [...prefix],
      before: { path: '' as Tree.Path, fen: paths[0]!.start_fen as FEN },
    };
    const snapshot = decodeMoveReviewSnapshot(raw, { requestId: raw.request_id as string, subject });
    assert.ok(snapshot?.kind === 'abstained' && snapshot.observations);
    if (snapshot?.kind !== 'abstained') throw new Error('Expected stopped producer observation');
    const job: MoveReviewJobState = {
      kind: 'abstained',
      subject,
      stopCondition: snapshot.stopCondition,
      observations: snapshot.observations,
    };
    const history = snapshot.observations!.paths.find(path => path.kind === 'history');
    const view = history ? { expandedProofId: history.id, pinnedFrame: { proofId: history.id, ply: 2 } } : {};
    const panel = renderMoveReview(props({ job, view, canWrite: true }));
    const text = renderedText(panel);
    assert.match(text, /Verified move observations/);
    assert.equal(renderedBoardConfig(panel).fen, subject.after.fen);
    if (name.startsWith('development-')) {
      assert.match(text, /leaves its starting square g1 for f3 for the first time/);
      assert.match(text, /central squares d4, e5/);
      assert.doesNotMatch(text, /Best move|Brilliant/);
      if (name === 'development-engine_unavailable-produced')
        assert.match(text, /could not be loaded or started.*Retry/);
      if (name === 'development-browser_unsupported-produced') assert.doesNotMatch(text, /Retry/);
    } else if (name === 'recapture-stopped-produced') {
      assert.match(text, /recaptures the opposing piece/);
      assert.deepEqual(
        history!.moves.map(move => move.uci),
        ['e4d5', 'd8d5'],
      );
      assert.equal(findNodes(panel, 'button.button.button-thin.move-review__add').length, 0);
    } else {
      assert.match(text, /captures the pawn on d5/);
      assert.match(text, /Whether this capture was a recapture is unknown/);
      assert.equal(history, undefined);
    }
  }
});

test('producer transposed captures retain their targets, board paths and partial-input issue in the renderer', () => {
  const shared = ['b8c6', 'h6f4', 'b5b4', 'e4e5'];
  const pawn = [...shared, 'c6e5', 'f3e5', 'd6e5', 'f4e5', 'c7b7'] as Uci[];
  const knight = [...shared, 'd6e5', 'f3e5', 'c6e5', 'f4e5', 'c7b7'] as Uci[];
  for (const name of ['transposed-captures', 'partial-input']) {
    const raw = fixture(
      new URL(
        `../../../../../../judgment-evaluation/fixtures/public-commentary-v6/${name}-produced.json`,
        import.meta.url,
      ),
    );
    const subject = subjectFor({ ...raw, current_fen: object(raw.issued_engine_work).search_fen });
    subject.initialFen = initialFen;
    subject.movePrefixUci =
      'e2e4 c7c5 g1f3 d7d6 d2d4 c5d4 f3d4 g8f6 b1c3 a7a6 f1c4 e7e6 c4b3 b7b5 e1g1 f8e7 d1f3 d8c7 f3g3 e8g8 c1h6 f6e8 a1d1 c8d7 d4f3'.split(
        ' ',
      ) as Uci[];
    subject.inputLines = [
      { source: 'analysis', moves: pawn },
      { source: 'analysis', moves: knight },
    ];
    if (name === 'partial-input')
      subject.inputLines.push({ source: 'study', moves: [...pawn.slice(0, 6), 'a1a8'] });
    const context = { requestId: raw.request_id as string, subject, engineProfile: moveReviewEngineProfile };
    const snapshot = decodeMoveReviewSnapshot(raw, context);
    assert(snapshot?.observations);
    const paths = snapshot.observations.paths.filter(path => path.moves.length === 9);
    assert.equal(paths.length, 2);
    assert.notEqual(paths[0].occurrenceIds[7], paths[1].occurrenceIds[7]);
    assert.equal(paths[0].moves.at(-1)!.fenAfter, paths[1].moves.at(-1)!.fenAfter);
    assert.ok(
      paths.every(path => path.positionIds),
      'Regenerate ProduceSuppliedCommentaryFixtures with position catalogues',
    );
    assert.notEqual(paths[0].positionIds![9], paths[1].positionIds![9]);
    const aliasedPosition = structuredClone(raw);
    const aliasedPaths = (object(aliasedPosition.observations).paths as JsonObject[]).filter(
      path => (path.moves as unknown[]).length === 9,
    );
    (aliasedPaths[1].position_ids as string[])[9] = (aliasedPaths[0].position_ids as string[])[9];
    assert.equal(
      decodeMoveReviewSnapshot(aliasedPosition, context),
      undefined,
      'Distinct incoming occurrences cannot share one historical position ID',
    );
    const job: MoveReviewJobState = { kind: 'loading', subject, observations: snapshot.observations };
    for (const [index, role] of ['pawn', 'knight'].entries()) {
      const path = paths[index];
      const panel = renderMoveReview(
        props({
          job,
          view: {
            evidenceExpanded: false,
            expandedProofId: path.id,
            pinnedFrame: { proofId: path.id, ply: 8 },
          },
        }),
      );
      const text = renderedText(panel);
      assert.match(text, new RegExp(`captures the ${role} on e5`));
      assert.match(text, /Submitted analysis branch/);
      assert.deepEqual(renderedBoardConfig(panel).lastMove, ['f4', 'e5']);
      if (name === 'partial-input') assert.match(text, /Input line 3: preserved 6 plies/);
    }
    const drift = structuredClone(raw);
    object((object(drift.observations).paths as unknown[])[1]).sources = ['pgn_record'];
    assert.equal(decodeMoveReviewSnapshot(drift, context), undefined);
  }
});

test('observation selection uses the matching phase and payload, with independent keys for repeated kinds', () => {
  const raw = fixture(new URL('development-cancelled-produced.json', bestChoiceFixtureUrl));
  const subject = subjectFor({ ...raw, current_fen: initialFen });
  const snapshot = decodeMoveReviewSnapshot(raw, { requestId: raw.request_id as string, subject });
  assert.ok(snapshot?.observations);
  const observations = structuredClone(snapshot.observations);
  const development = observations.statements.find(statement => statement.kind === 'development')!;
  assert.equal(development.kind, 'development');
  if (development.kind !== 'development') throw new Error('Expected transmitted development');
  // Split supplied targets to exercise independent rows of one kind, including an exact duplicate.
  observations.statements = [
    { ...development, centralSquares: ['d4'] },
    { ...development, centralSquares: ['e5'] },
    { ...development, centralSquares: ['e5'] },
  ];
  const panelProps = props({
    job: { kind: 'loading', subject, observations },
    actions: {
      previewFrame: frame => (panelProps.view.hoveredFrame = frame),
      clearPreview: () => (panelProps.view.hoveredFrame = undefined),
      pinFrame: frame => {
        panelProps.view.pinnedFrame = frame;
        panelProps.view.hoveredFrame = undefined;
      },
      clearPin: () => (panelProps.view.pinnedFrame = undefined),
    },
  });
  const render = () => renderMoveReview(panelProps);
  const buttons = () => findNodes(render(), 'button.move-review__observation-frame');
  const rows = findNodes(render(), 'div.move-review__observation').filter(
    row => findNodes(row, 'button.move-review__observation-frame').length > 0,
  );
  assert.equal(rows.length, 3);
  assert.equal(new Set(rows.map(row => row.key)).size, 3);
  assert.deepEqual(renderedBoardConfig(render()).drawable?.autoShapes, []);

  (buttons()[1].data!.on!.click as () => void)();
  const firstAfter = renderedBoardConfig(render());
  assert.equal(firstAfter.fen, subject.after.fen);
  assert.deepEqual(firstAfter.lastMove, ['g1', 'f3']);
  assert.deepEqual(firstAfter.drawable?.autoShapes, [
    { orig: 'f3', brush: 'blue' },
    { orig: 'f3', dest: 'd4', brush: 'blue' },
  ]);
  assert.equal(buttons()[1].data!.attrs!['aria-pressed'], 'true');

  (buttons()[0].data!.on!.focus as () => void)();
  const before = renderedBoardConfig(render());
  assert.equal(before.fen, initialFen);
  assert.equal(before.lastMove, undefined);
  assert.deepEqual(before.drawable?.autoShapes, [{ orig: 'g1', brush: 'blue' }]);
  assert.match(renderedText(findNode(render(), 'figcaption')), /Before move/);
  (buttons()[0].data!.on!.blur as () => void)();
  assert.deepEqual(renderedBoardConfig(render()), firstAfter);

  (buttons()[3].data!.on!.mouseenter as () => void)();
  assert.deepEqual(renderedBoardConfig(render()).drawable?.autoShapes, [
    { orig: 'f3', brush: 'blue' },
    { orig: 'f3', dest: 'e5', brush: 'blue' },
  ]);
  const list = findNode(render(), 'div.move-review__observation-statements');
  (list.data!.on!.mouseleave as () => void)();
  assert.deepEqual(renderedBoardConfig(render()), firstAfter);

  const move = findNode(render(), 'button.move-review__proof-san');
  (move.data!.on!.mouseenter as () => void)();
  assert.deepEqual(renderedBoardConfig(render()).drawable?.autoShapes, []);
  (move.data!.on!.blur as () => void)();
  assert.deepEqual(renderedBoardConfig(render()), firstAfter);

  // A replaced payload cannot inherit a selection just because kind and occurrence still match.
  observations.statements = observations.statements.slice(1);
  assert.deepEqual(renderedBoardConfig(render()).drawable?.autoShapes, []);
  (buttons()[0].data!.on!.click as () => void)();
  const proof = findNode(render(), 'div.move-review__proof');
  proof.data!.on!.keydown(
    {
      key: 'Escape',
      preventDefault() {},
      stopPropagation() {},
    } as KeyboardEvent,
    proof,
  );
  assert.equal(panelProps.view.pinnedFrame, undefined);
  assert.equal(panelProps.view.hoveredFrame, undefined);
  assert.deepEqual(renderedBoardConfig(render()).drawable?.autoShapes, []);
});

test('a capture before frame uses the preceding replay board and transmitted captured square', () => {
  const raw = fixture(new URL('recapture-stopped-produced.json', bestChoiceFixtureUrl));
  const paths = object(raw.observations).paths as JsonObject[];
  const subject: MoveReviewSubject = {
    ...subjectFor({ ...raw, current_fen: paths[0].start_fen }),
    initialFen,
    movePrefixUci: ['e2e4', 'd7d5', 'e4d5'],
  };
  const snapshot = decodeMoveReviewSnapshot(raw, { requestId: raw.request_id as string, subject });
  assert.ok(snapshot?.observations);
  const observations = snapshot.observations;
  const history = observations.paths.find(path => path.kind === 'history')!;
  const panelProps = props({
    job: { kind: 'fault', subject, message: 'transport failed', retryable: true, observations },
    view: { expandedProofId: history.id, pinnedFrame: { proofId: history.id, ply: 2 } },
    actions: { pinFrame: frame => (panelProps.view.pinnedFrame = frame) },
  });
  const panel = renderMoveReview(panelProps);
  const before = findNodes(panel, 'button.move-review__observation-frame')[0];
  (before.data!.on!.click as () => void)();
  const selected = renderedBoardConfig(renderMoveReview(panelProps));
  assert.equal(selected.fen, history.moves[0].fenAfter);
  assert.equal(selected.fen, subject.before.fen);
  assert.equal(selected.lastMove, undefined);
  assert.deepEqual(selected.drawable?.autoShapes, [
    { orig: 'd8', brush: 'blue' },
    { orig: 'd5', brush: 'red' },
  ]);
  assert.match(renderedText(renderMoveReview(panelProps)), /recaptures/);
});

test('all twelve runtime fixtures render with unique observation rows and grouped relationship details', () => {
  for (const fixture of certifiedFixtures) {
    const panelProps = certifiedProps(fixture.name);
    const observations = moveReviewObservations(panelProps.job)!;
    const panel = renderMoveReview(panelProps);
    const rows = descendants(panel).filter(
      node => node.sel === 'div.move-review__observation' || node.sel === 'details.move-review__observation',
    );
    const visible = observations.statements.filter(
      statement => statement.kind !== 'structural_change' || statement.changes.length > 0,
    );
    assert.equal(rows.length, visible.length, fixture.name);
    assert.equal(new Set(rows.map(row => row.key)).size, rows.length, fixture.name);
    for (const group of findNodes(panel, 'details.move-review__controller'))
      assert.equal(group.data?.attrs?.open, undefined);
    assert.deepEqual(renderedBoardConfig(panel).drawable?.autoShapes, [], fixture.name);
    assert.doesNotMatch(renderedText(panel), /undefined|all chess unchanged|no chess changes/i);
    panelProps.locale = 'ko-KR';
    panelProps.copy = moveReviewCopy('ko-KR');
    assert.doesNotMatch(renderedText(renderMoveReview(panelProps)), /undefined/);
  }
});

test('runtime moving-target relationships follow both endpoints and clear when switching paths or moves', () => {
  const panelProps = certifiedProps('moving-target');
  const before = selectObservation(panelProps, /Before move: White rook a1 → Black rook a7/, 'before');
  assert.equal(
    renderedBoardConfig(before).fen,
    certifiedFixtures.find(fixture => fixture.name === 'moving-target')!.fen,
  );
  assert.equal(renderedBoardConfig(before).lastMove, undefined);
  assert.deepEqual(renderedBoardConfig(before).drawable?.autoShapes, [
    { orig: 'a1', dest: 'a7', brush: 'red' },
    { orig: 'a7', brush: 'red' },
  ]);
  const after = selectObservation(panelProps, /Before move: White rook a1 → Black rook a7/, 'after');
  assert.deepEqual(renderedBoardConfig(after).drawable?.autoShapes, [
    { orig: 'a1', dest: 'a6', brush: 'red' },
    { orig: 'a6', brush: 'red' },
  ]);
  const other = selectObservation(panelProps, /Before move: Black rook a7 → White rook a1/, 'after');
  assert.deepEqual(renderedBoardConfig(other).drawable?.autoShapes, [
    { orig: 'a6', dest: 'a1', brush: 'red' },
    { orig: 'a1', brush: 'red' },
  ]);
  (findNode(other, 'button.move-review__proof-san').data!.on!.click as () => void)();
  assert.deepEqual(renderedBoardConfig(renderMoveReview(panelProps)).drawable?.autoShapes, []);
  const paths = findNode(renderMoveReview(panelProps), 'div.move-review__observation-paths');
  (findNode(paths, 'button').data!.on!.click as () => void)();
  assert.equal(panelProps.view.pinnedFrame, undefined);
  assert.equal(panelProps.view.hoveredFrame, undefined);
});

test('capture summaries use one board phase and empty structural targets do not imply a lost file', () => {
  const panelProps = certifiedProps('pinned-recapture');
  let text = renderedText(renderMoveReview(panelProps));
  assert.match(text, /White pawn e4 → Black pawn d5/);
  assert.doesNotMatch(text, /White pawn d5 → Black pawn d5/);
  assert.doesNotMatch(text, /file e → None/i);
  panelProps.locale = 'ko-KR';
  panelProps.copy = moveReviewCopy('ko-KR');
  text = renderedText(renderMoveReview(panelProps));
  assert.match(text, /백 폰 e4 → 흑 폰 d5/);
  assert.doesNotMatch(text, /백 폰 d5 → 흑 폰 d5/);
  assert.doesNotMatch(text, /열린 파일 e → 없음/);
});

test('runtime pinned recapture keeps known legal emptiness separate from unknown capture history', () => {
  const panelProps = certifiedProps('pinned-recapture');
  const panel = selectObservation(panelProps, /Geometric recapturers: knight e7/, 'after');
  const text = renderedText(panel);
  assert.match(text, /Whether this capture was a recapture is unknown/);
  assert.match(text, /Legal responses: None/);
  assert.match(
    text,
    /If knight e7 recaptures on d5, rook e1 would attack its king on e8; this recapture is illegal/,
  );
  assert.match(text, /Absolute pin paths: rook e1 → knight e7 → King e8/);
  assert.deepEqual(renderedBoardConfig(panel).drawable?.autoShapes, [
    { orig: 'e7', dest: 'd5', brush: 'blue' },
    { orig: 'e7', brush: 'red' },
  ]);
});

test('runtime root response selects only the before board and retains simultaneous response modes', () => {
  const panelProps = certifiedProps('root-response');
  const panel = selectObservation(panelProps, /Selected check response/, 'before');
  const row = findNodes(panel, 'details.move-review__observation').find(row =>
    /Selected check response/.test(renderedText(row)),
  )!;
  assert.deepEqual(findNodes(row, 'button.move-review__observation-frame').map(renderedText), [
    'Before move',
  ]);
  assert.match(renderedText(row), /Capture checker \+ King move/);
  assert.equal(
    renderedBoardConfig(panel).fen,
    certifiedFixtures.find(fixture => fixture.name === 'root-response')!.fen,
  );
  assert.equal(renderedBoardConfig(panel).lastMove, undefined);
  assert.deepEqual(renderedBoardConfig(panel).drawable?.autoShapes, [
    { orig: 'e1', brush: 'red' },
    { orig: 'e2', dest: 'e1', brush: 'red' },
    { orig: 'e1', dest: 'e2', brush: 'green' },
  ]);
});

test('runtime double check, mate and stalemate retain different causes and response inventories', () => {
  const panel = selectObservation(certifiedProps('double-check'), /Checking pieces:/, 'after');
  const shapes = renderedBoardConfig(panel).drawable?.autoShapes;
  assert.ok(shapes?.some(shape => shape.orig === 'b5' && shape.dest === 'e8'));
  assert.ok(shapes?.some(shape => shape.orig === 'e1' && shape.dest === 'e8'));
  assert.match(renderedText(panel), /Controlled king destinations:.*e7.*rook e1/);
  const mate = renderedText(renderMoveReview(certifiedProps('mate')));
  assert.match(mate, /Checkmate/);
  assert.match(mate, /Legal responses: None/);
  const stalemate = selectObservation(certifiedProps('stalemate'), /Stalemate\./, 'after');
  assert.match(renderedText(stalemate), /not in check.*no legal moves/);
  assert.deepEqual(renderedBoardConfig(stalemate).drawable?.autoShapes, [{ orig: 'h8', brush: 'blue' }]);
});

test('runtime pawn connections display an unmoved pawn and preserve support direction versus phalanx', () => {
  const panelProps = certifiedProps('pawn-connections');
  const before = selectObservation(panelProps, /Before move: White pawn d5/, 'before');
  assert.match(renderedText(before), /Geometrically supported by c4/);
  assert.deepEqual(renderedBoardConfig(before).drawable?.autoShapes, [
    { orig: 'd5', brush: 'blue' },
    { orig: 'c4', dest: 'd5', brush: 'green' },
  ]);
  const after = selectObservation(panelProps, /Before move: White pawn d5/, 'after');
  assert.match(renderedText(after), /In phalanx with c5/);
  assert.deepEqual(renderedBoardConfig(after).drawable?.autoShapes, [
    { orig: 'd5', brush: 'blue' },
    { orig: 'c5', brush: 'blue' },
  ]);
});

test('runtime castling and underpromotion keep physical movement and removed pawn frames', () => {
  const castleProps = certifiedProps('castle');
  const castle = selectObservation(castleProps, /Castling ·/, 'after');
  assert.match(renderedText(castle), /king e1 → g1; rook h1 → f1/);
  assert.deepEqual(renderedBoardConfig(castle).drawable?.autoShapes, [
    { orig: 'g1', brush: 'blue' },
    { orig: 'f1', brush: 'blue' },
  ]);
  const promotionProps = certifiedProps('underpromotion');
  const promoted = selectObservation(promotionProps, /Promotion ·/, 'after');
  assert.match(renderedText(promoted), /pawn a7 → knight b8/);
  assert.match(renderedBoardConfig(promoted).fen, /^1N5k\//);
  const removed = selectObservation(promotionProps, /Before move: White pawn a7/, 'after');
  assert.match(renderedText(removed), /After move: Removed piece/);
  assert.deepEqual(renderedBoardConfig(removed).drawable?.autoShapes, []);
});

test('quiet move relationships are equally accessible by controller, including the unmoved queen', () => {
  for (const name of ['Bf3', 'Bc3', 'Bc4'])
    for (const locale of ['en-US', 'ko-KR'] as const) {
      const panelProps = certifiedProps(name);
      panelProps.locale = locale;
      panelProps.copy = moveReviewCopy(locale);
      const panel = renderMoveReview(panelProps);
      const groups = findNodes(panel, 'details.move-review__controller');
      const controllers = new Set(
        moveReviewObservations(panelProps.job)!.statements.flatMap(s =>
          s.kind === 'piece_control' ? [(s.before.controller ?? s.after.controller)!.square] : [],
        ),
      );
      assert.equal(groups.length, controllers.size);
      for (const group of groups) {
        assert.equal(group.data?.attrs?.open, undefined);
        assert.doesNotMatch(renderedText(findNode(group, 'summary')), /best|safe|최선|안전/i);
      }
      if (name === 'Bf3') {
        const queen = groups.find(group => renderedText(findNode(group, 'summary')).includes('c2'))!;
        assert.ok(queen, 'Unmoved queen must have its own first-level disclosure');
        assert.match(renderedText(queen), /h2/);
        assert.match(
          renderedText(queen),
          locale === 'en-US'
            ? /Before move:.*Absent.*After move:.*Present/
            : /수 이전:.*성립하지 않음.*수 이후:.*성립/,
        );
      }
    }
});

function inspectSquare(panelProps: MoveReviewPanelProps, square: Key): VNode {
  const inspector = findNode(renderMoveReview(panelProps), 'div.move-review__position-resources');
  const select = findNode(inspector, 'select');
  (select.data!.on!.change as (event: unknown) => void)({ target: { value: square } });
  return renderMoveReview(panelProps);
}

test('square inspection follows Bf3 before/after evidence without drawing attacks through blockers', () => {
  const panelProps = certifiedProps('Bf3');
  const after = inspectSquare(panelProps, 'h2');
  const afterText = renderedText(findNode(after, 'div.move-review__position-resources'));
  assert.match(afterText, /First blocker: White pawn h2/);
  assert.ok(
    renderedBoardConfig(after).drawable!.autoShapes!.some(
      shape => shape.orig === 'c2' && shape.dest === 'h2',
    ),
  );
  const beforeButton = findNodes(findNode(after, 'div.move-review__resource-frames'), 'button')[0];
  (beforeButton.data!.on!.mouseenter as () => void)();
  const before = renderMoveReview(panelProps);
  assert.match(
    renderedText(findNode(before, 'div.move-review__position-resources')),
    /First blocker: White bishop g2.*Pieces behind it \(nearest first\): White pawn h2/,
  );
  assert.equal(
    renderedBoardConfig(before).fen,
    certifiedFixtures.find(fixture => fixture.name === 'Bf3')!.fen,
  );
  assert.equal(renderedBoardConfig(before).lastMove, undefined);
  assert.ok(
    !renderedBoardConfig(before).drawable!.autoShapes!.some(
      shape => shape.orig === 'c2' && shape.dest === 'h2',
    ),
  );
  panelProps.actions.clearPreview();
  assert.equal(renderedBoardConfig(renderMoveReview(panelProps)).fen, renderedBoardConfig(after).fen);
  (beforeButton.data!.on!.click as () => void)();
  panelProps.actions.clearPreview();
  assert.equal(renderedBoardConfig(renderMoveReview(panelProps)).fen, renderedBoardConfig(before).fen);
  panelProps.actions.clearPin();
  assert.deepEqual(renderedBoardConfig(renderMoveReview(panelProps)).drawable!.autoShapes, []);
});

test('resource emptiness, partial evidence and an unavailable historical position stay distinct', () => {
  const panelProps = certifiedProps('Bf3');
  const observations = moveReviewObservations(panelProps.job)!;
  const path = observations.paths[0];
  const position = observations.positions!.find(p => p.id === path.positionIds![1])!;
  position.controls.pieces = [];
  position.rays.lines = [];
  position.pawn_advances.paths = [];
  let inspector = findNode(inspectSquare(panelProps, 'd5'), 'div.move-review__position-resources');
  const geometrySections = findNodes(inspector, 'section').filter(section =>
    [
      panelProps.copy.resourceLabels.controls,
      panelProps.copy.resourceLabels.friendlySupport,
      panelProps.copy.resourceLabels.opposingControl,
      panelProps.copy.resourceLabels.rays,
      panelProps.copy.resourceLabels.advances,
    ].includes(renderedText(findNode(section, 'h4'))),
  );
  assert.equal(geometrySections.length, 4);
  for (const section of geometrySections) assert.match(renderedText(section), /None/);
  const legalSection = findNodes(inspector, 'section').find(
    section => renderedText(findNode(section, 'h4')) === panelProps.copy.resourceLabels.legalMoves,
  )!;
  assert.match(renderedText(legalSection), /d5/);
  position.controls.complete = false;
  inspector = findNode(renderMoveReview(panelProps), 'div.move-review__position-resources');
  const controls = findNodes(inspector, 'section')[0];
  assert.match(renderedText(controls), /Only partial evidence/);
  assert.doesNotMatch(renderedText(controls), /None/);
  path.positionIds![1] = null;
  inspector = findNode(renderMoveReview(panelProps), 'div.move-review__position-resources');
  assert.match(renderedText(inspector), /No evidence is available for this position/);
  assert.equal(findNodes(inspector, 'section').length, 0);
});

test('inspection never borrows a same-FEN catalogue from another historical path', () => {
  const panelProps = certifiedProps('Bf3');
  const observations = moveReviewObservations(panelProps.job)!;
  const path = observations.paths[0];
  inspectSquare(panelProps, 'h2');
  const fork = {
    ...structuredClone(path),
    id: 'another-path',
    occurrenceIds: ['another-transition'],
    positionIds: [null, null],
  };
  observations.paths.push(fork);
  panelProps.actions.toggleProof(fork.id);
  const panel = inspectSquare(panelProps, 'h2');
  assert.equal(renderedBoardConfig(panel).fen, path.moves[0].fenAfter);
  assert.match(
    renderedText(findNode(panel, 'div.move-review__position-resources')),
    /No evidence is available/,
  );
  assert.deepEqual(renderedBoardConfig(panel).drawable?.autoShapes, [{ orig: 'h2', brush: 'blue' }]);
});

test('actual pinned pawn paths distinguish illegal, legal, opposite-turn and unknown evidence', () => {
  const fixtures = fixture(
    new URL('resources-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const panelProps = certifiedProps('pawn-pinned-but-geometric-routes-open', fixtures);
  const observations = moveReviewObservations(panelProps.job)!;
  const path = observations.paths[0];
  const sectionText = (panel: VNode): string =>
    renderedText(
      findNodes(findNode(panel, 'div.move-review__position-resources'), 'section').find(
        section => renderedText(findNode(section, 'h4')) === panelProps.copy.resourceLabels.advances,
      )!,
    );
  panelProps.actions.pinFrame({ proofId: path.id, ply: 1, phase: 'before' });
  let panel = inspectSquare(panelProps, 'd2');
  assert.match(sectionText(panel), /d2 – d3.*Move is not legal/);
  assert.match(sectionText(panel), /d2 – d3 – d4.*Move is not legal/);
  panel = inspectSquare(panelProps, 'a2');
  assert.match(sectionText(panel), /Legal move \(a2a3\)/);
  assert.match(sectionText(panel), /Legal move \(a2a4\)/);
  panelProps.actions.pinFrame({ proofId: path.id, ply: 1, phase: 'after' });
  panel = inspectSquare(panelProps, 'd2');
  assert.match(sectionText(panel), /Not this piece’s turn/);
  assert.doesNotMatch(sectionText(panel), /Move is not legal/);
  const position = observations.positions!.find(p => p.id === path.positionIds![1])!;
  for (const shape of renderedBoardConfig(panel).drawable?.autoShapes ?? [])
    if (shape.dest)
      assert.ok(
        position.controls.pieces.some(
          entry => entry.piece.square === shape.orig && entry.squares.includes(shape.dest!),
        ),
      );
  panelProps.actions.pinFrame({ proofId: path.id, ply: 1, phase: 'before', inspectSquare: 'd2' });
  const before = observations.positions!.find(p => p.id === path.positionIds![0])!;
  before.legal_moves.complete = false;
  assert.match(sectionText(renderMoveReview(panelProps)), /Legality unknown/);
});

test('selected piece inspection separates actual legal moves, outgoing controls and incomplete evidence', () => {
  const fixtures = fixture(
    new URL('resources-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const resourceSection = (panel: VNode, label: string): string =>
    renderedText(
      findNodes(findNode(panel, 'div.move-review__position-resources'), 'section').find(
        section => renderedText(findNode(section, 'h4')) === label,
      )!,
    );
  const pinned = certifiedProps('knight-pinned', fixtures);
  const pinnedPanel = inspectSquare(pinned, 'e7');
  assert.match(resourceSection(pinnedPanel, pinned.copy.resourceLabels.legalMoves), /has no legal move/);
  const controls = resourceSection(pinnedPanel, pinned.copy.resourceLabels.outgoingControls);
  for (const square of ['c6', 'c8', 'd5', 'f5', 'g6', 'g8']) assert.ok(controls.includes(square));
  assert.doesNotMatch(controls, /safe|loses|best|pinned|legal/i);

  const released = certifiedProps('knight-released', fixtures);
  const after = inspectSquare(released, 'e7');
  const moves = resourceSection(after, released.copy.resourceLabels.legalMoves);
  for (const square of ['c6', 'c8', 'd5', 'f5', 'g6', 'g8'])
    assert.ok(moves.includes(`${square} (e7${square})`));
  const observations = moveReviewObservations(released.job)!;
  const path = observations.paths[0];
  released.actions.pinFrame({ proofId: path.id, ply: 1, phase: 'before', inspectSquare: 'e7' });
  const beforePanel = renderMoveReview(released);
  assert.match(
    resourceSection(beforePanel, released.copy.resourceLabels.legalMoves),
    /Not this piece’s turn/,
  );
  assert.equal(renderedText(beforePanel).split('Not this piece’s turn').length - 1, 1);

  released.actions.pinFrame({ proofId: path.id, ply: 1, phase: 'after', inspectSquare: 'e7' });
  const position = observations.positions!.find(p => p.id === path.positionIds![1])!;
  position.legal_moves.moves = position.legal_moves.moves.filter(move => move.uci !== 'e7c6');
  position.legal_moves.complete = false;
  const partial = renderMoveReview(released);
  const partialMoves = resourceSection(partial, released.copy.resourceLabels.legalMoves);
  assert.match(partialMoves, /e7c8.*Only partial evidence/);
  assert.doesNotMatch(partialMoves, /has no legal move|e7c6/);
  assert.equal(resourceSection(partial, released.copy.resourceLabels.outgoingControls), controls);
});

test('actual finite line summaries expose every material event and navigate its exact occurrence', () => {
  const fixtures = fixture(
    new URL('resources-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const panelProps = certifiedProps('finite-material-branches', fixtures);
  const paths = moveReviewObservations(panelProps.job)!.paths.filter(path => path.moves.length > 1);
  assert.equal(paths.length, 4);
  for (const path of paths) {
    panelProps.actions.toggleProof(path.id);
    const panel = renderMoveReview(panelProps);
    const summary = findNode(panel, 'details.move-review__line-summary');
    assert.match(renderedText(summary), /Within this line.*not engine evaluations/);
    assert.match(renderedText(summary), /do not establish a forced or lasting gain/);
    assert.match(renderedText(summary), /supplied or analyzed line stops here/);
    const line = path.lineSummary!;
    assert.ok(line.material);
    assert.equal(
      findNodes(summary, 'li').length,
      new Set([...line.events, ...line.material.events].map(event => event.ply_offset)).size,
    );
    for (const event of line.material.events) {
      const button = findNodes(summary, 'button.move-review__line-event').find(button =>
        renderedText(button).startsWith(`${panelProps.copy.proofStep} ${event.ply_offset + 1}:`),
      )!;
      assert.ok(button, `Event button for ${event.move_uci}`);
      (button.data!.on!.click as () => void)();
      assert.equal(panelProps.view.pinnedFrame!.ply, event.ply_offset + 1);
      assert.equal(path.occurrenceIds[event.ply_offset], event.occurrence_id);
      assert.equal(
        renderedBoardConfig(renderMoveReview(panelProps)).fen,
        path.moves[event.ply_offset].fenAfter,
      );
    }
    if (line.material.recovered_after_loss && line.material.net_cp < 0) {
      assert.match(renderedText(summary), /returned to at least zero.*Further losses may follow/);
      assert.match(renderedText(summary), /Change at line end: -1/);
    }
  }
  assert.ok(
    paths.some(
      path => path.lineSummary!.material!.recovered_after_loss && path.lineSummary!.material!.net_cp < 0,
    ),
  );
});

test('runtime quiet-line zero material is distinct from missing material evidence', () => {
  const fixtures = fixture(
    new URL('resources-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const panelProps = certifiedProps('quiet-line', fixtures);
  const path = moveReviewObservations(panelProps.job)!.paths.find(path => path.moves.length === 2)!;
  panelProps.actions.toggleProof(path.id);
  let summary = findNode(renderMoveReview(panelProps), 'details.move-review__line-summary');
  assert.match(renderedText(summary), /No material-changing events in this line/);
  assert.match(renderedText(summary), /Change at line end: 0/);
  delete path.lineSummary!.material;
  summary = findNode(renderMoveReview(panelProps), 'details.move-review__line-summary');
  assert.match(renderedText(summary), /Material evidence is unavailable/);
  assert.doesNotMatch(renderedText(summary), /Change at line end: 0|No material-changing events/);
});

test('line event buttons use short factual sentences, with kind-only fallbacks in both languages', () => {
  const cases = [
    ['pinned-recapture', 'White pawn captures the pawn on d5.'],
    ['double-check', 'White gives check to the king on e8.'],
    ['root-response', 'White king captures the rook on e2. White gets out of check.'],
    ['castle', 'White castles, placing the king on g1.'],
    ['mate', 'White checkmates the king on h8.'],
    ['stalemate', 'White stalemates the opposing king on h8.'],
    ['underpromotion', 'White pawn captures the rook on b8. White promotes to a knight on b8.'],
  ];
  for (const [name, expected] of cases) {
    const panelProps = certifiedProps(name);
    const buttons = findNodes(renderMoveReview(panelProps), 'button.move-review__line-event');
    assert.ok(renderedText(buttons[0]).endsWith(expected), name);
    assert.doesNotMatch(renderedText(buttons[0]), /Related targets|White bishop gives check/);
    for (const locale of ['en-US', 'ko-KR'] as const) {
      const fallbackProps = certifiedProps(name);
      fallbackProps.locale = locale;
      fallbackProps.copy = moveReviewCopy(locale);
      const line = moveReviewObservations(fallbackProps.job)!.paths[0].lineSummary!;
      for (const event of line.events) {
        delete event.side;
        delete event.piece_role;
        delete event.target_role;
        delete event.square;
      }
      const fallback = findNodes(renderMoveReview(fallbackProps), 'button.move-review__line-event')[0];
      assert.ok(
        renderedText(fallback).endsWith(
          line.events.map(event => `${fallbackProps.copy.lineEventLabels[event.kind]}.`).join(' '),
        ),
      );
      assert.doesNotMatch(renderedText(fallback), /undefined|\{.*\}|Related targets|관련 대상/);
    }
  }
  const fixtures = fixture(
    new URL('resources-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const panelProps = certifiedProps('finite-material-branches', fixtures);
  const path = moveReviewObservations(panelProps.job)!.paths.find(path => path.moves.length === 8)!;
  panelProps.actions.toggleProof(path.id);
  const buttons = findNodes(renderMoveReview(panelProps), 'button.move-review__line-event');
  assert.ok(buttons.some(button => renderedText(button).includes('White knight recaptures the pawn on e4.')));
  assert.ok(
    buttons.some(button =>
      renderedText(button).includes(
        'Black queen captures the pawn on d2. Black gives check to the king on e1.',
      ),
    ),
  );
});

test('actual empty-square controllers change by phase and unavailable history is not reconstructed from FEN', () => {
  const fixtures = fixture(
    new URL('resources-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const panelProps = certifiedProps('empty-d5-loss-and-maintenance', fixtures);
  let panel = inspectSquare(panelProps, 'd5');
  const controlText = (panel: VNode): string =>
    renderedText(findNodes(findNode(panel, 'div.move-review__position-resources'), 'section')[0]);
  assert.match(controlText(panel), /White knight c3/);
  assert.match(controlText(panel), /Black pawn e6/);
  assert.doesNotMatch(controlText(panel), /White pawn e4/);
  const before = findNodes(findNode(panel, 'div.move-review__resource-frames'), 'button')[0];
  (before.data!.on!.click as () => void)();
  panel = renderMoveReview(panelProps);
  assert.match(controlText(panel), /White pawn e4/);
  assert.match(controlText(panel), /White knight c3/);
  assert.match(controlText(panel), /Black pawn e6/);

  const historyProps = certifiedProps('en-passant-empty-target-is-not-legality', fixtures);
  const history = moveReviewObservations(historyProps.job)!.paths.find(path => path.kind === 'history')!;
  assert.equal(history.positionIds![0], null);
  historyProps.actions.toggleProof(history.id);
  historyProps.actions.pinFrame({ proofId: history.id, ply: 1, phase: 'before', inspectSquare: 'f6' });
  const first = renderMoveReview(historyProps);
  assert.equal(renderedBoardConfig(first).fen, history.startFen);
  assert.match(
    renderedText(findNode(first, 'div.move-review__position-resources')),
    /No evidence is available/,
  );
  historyProps.actions.pinFrame({ proofId: history.id, ply: 2, phase: 'before', inspectSquare: 'f6' });
  const next = renderMoveReview(historyProps);
  assert.equal(renderedBoardConfig(next).fen, history.moves[0].fenAfter);
  assert.match(controlText(next), /White pawn g5/);
  assert.doesNotMatch(
    renderedText(findNode(next, 'div.move-review__position-resources')),
    /No evidence is available/,
  );
});

test('unchanged pawn facts and current rook files remain inspectable without a change event', () => {
  const fixtures = fixture(
    new URL('resources-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const pawnProps = certifiedProps('unchanged-pawn-states', fixtures);
  const sectionText = (panel: VNode, title: string): string =>
    renderedText(
      findNodes(findNode(panel, 'div.move-review__position-resources'), 'section').find(
        section => renderedText(findNode(section, 'h4')) === title,
      )!,
    );
  let panel = inspectSquare(pawnProps, 'd5');
  assert.match(
    sectionText(panel, pawnProps.copy.resourceLabels.pawnState),
    /Passed: Yes.*Isolated: No.*Doubled: No/,
  );
  assert.match(sectionText(panel, pawnProps.copy.resourceLabels.friendlySupport), /White pawn c4/);
  assert.match(sectionText(panel, pawnProps.copy.resourceLabels.opposingControl), /None/);
  const observations = moveReviewObservations(pawnProps.job)!;
  const position = observations.positions!.find(p => p.id === observations.paths[0].positionIds![1])!;
  position.pawn_states.pawns.find(p => p.square === 'd5')!.passed = null;
  position.pawn_states.complete = false;
  panel = renderMoveReview(pawnProps);
  assert.match(
    sectionText(panel, pawnProps.copy.resourceLabels.pawnState),
    /Passed: Not established.*Isolated: No.*Doubled: No/,
  );
  for (const [name, expected] of [
    ['rook-enters-open-file', /Open file: neither side has a pawn/],
    ['rook-enters-black-pawns-only-file', /Semi-open for this rook/],
    ['rook-enters-white-pawns-only-file', /This rook’s side has pawns/],
    ['rook-enters-both-sides-file', /This rook’s side has pawns/],
  ] as const) {
    const rookProps = certifiedProps(name, fixtures);
    assert.match(
      sectionText(inspectSquare(rookProps, 'e1'), rookProps.copy.resourceLabels.rookFile),
      expected,
    );
    const o = moveReviewObservations(rookProps.job)!;
    const p = o.positions!.find(p => p.id === o.paths[0].positionIds![1])!;
    p.pawn_files.files = p.pawn_files.files.filter(f => f.file !== 'e');
    p.pawn_files.complete = false;
    const missing = sectionText(renderMoveReview(rookProps), rookProps.copy.resourceLabels.rookFile);
    assert.ok(missing.includes(rookProps.copy.resourceLabels.partial));
    assert.doesNotMatch(missing, expected);
  }
});

test('legal en passant displays its actual captured square, separately from its destination', () => {
  const fixtures = fixture(
    new URL('resources-produced.json', bestChoiceFixtureUrl),
  ) as unknown as CertifiedObservationFixture[];
  const panelProps = certifiedProps('en-passant-two-blockers-disappear', fixtures);
  const o = moveReviewObservations(panelProps.job)!;
  const path = o.paths[0];
  const before = o.positions!.find(p => p.id === path.positionIds![0])!;
  const move = before.legal_moves.moves.find(m => m.captured_square && m.captured_square !== m.destination)!;
  assert.ok(move);
  inspectSquare(panelProps, move.uci.slice(0, 2) as Key);
  panelProps.actions.pinFrame({
    proofId: path.id,
    ply: 1,
    phase: 'before',
    inspectSquare: move.uci.slice(0, 2) as Key,
  });
  const panel = renderMoveReview(panelProps);
  const text = renderedText(findNode(panel, 'div.move-review__position-resources'));
  assert.ok(text.includes(`${move.destination} (${move.uci}) · Captured on: ${move.captured_square}`), text);
});

test('the browser harness mounts the real view and Chessground, including native details and frame clearing', t => {
  const root = document.createElement('div');
  document.body.append(root);
  mountMoveReviewFixturePreview(root, certifiedFixtures);
  const main = document.getElementById('move-review-fixture-preview')!;
  const ground = () =>
    (
      main.querySelector('.cg-wrap') as HTMLElement & {
        _cg: {
          state: { lastMove?: Key[]; drawable: { autoShapes: { orig: Key; dest?: Key; brush: string }[] } };
          destroy(): void;
        };
      }
    )._cg;
  t.after(() => {
    ground().destroy();
    main.remove();
  });
  const picker = main.querySelector<HTMLSelectElement>('select[aria-label="Runtime fixture"]')!;
  picker.value = 'moving-target';
  picker.dispatchEvent(new window.Event('change', { bubbles: true }));
  assert.equal(main.getAttribute('data-fixture'), 'moving-target');
  const details = [...main.querySelectorAll<HTMLDetailsElement>('.move-review__controller')].find(group =>
    group.querySelector('summary')!.textContent!.includes('White rook a1'),
  )!;
  assert.equal(details.open, false);
  details.querySelector('summary')!.click();
  assert.equal(details.open, true);
  const row = [...details.querySelectorAll<HTMLElement>('.move-review__observation')].find(row =>
    row.textContent?.includes('Before move: White rook a1 → Black rook a7'),
  )!;
  row.querySelector('summary')?.click();
  const before = row.querySelector<HTMLButtonElement>('button')!;
  const after = row.querySelectorAll<HTMLButtonElement>('button')[1];
  before.click();
  assert.equal(ground().state.lastMove, undefined);
  assert.deepEqual(ground().state.drawable.autoShapes, [
    { orig: 'a1', dest: 'a7', brush: 'red' },
    { orig: 'a7', brush: 'red' },
  ]);
  after.click();
  assert.deepEqual(ground().state.lastMove, ['a7', 'a6']);
  assert.deepEqual(ground().state.drawable.autoShapes, [
    { orig: 'a1', dest: 'a6', brush: 'red' },
    { orig: 'a6', brush: 'red' },
  ]);
  assert.equal(details.open, true, 'native disclosure remains open across product redraws');
  main.querySelector<HTMLButtonElement>('.move-review__proof-san')!.click();
  assert.deepEqual(ground().state.drawable.autoShapes, []);
  picker.value = 'root-response';
  picker.dispatchEvent(new window.Event('change', { bubbles: true }));
  const response = [...main.querySelectorAll<HTMLDetailsElement>('details.move-review__observation')].find(
    row => row.textContent?.includes('Selected check response'),
  )!;
  response.querySelector('summary')!.click();
  response.querySelector<HTMLButtonElement>('button')!.click();
  assert.equal(ground().state.lastMove, undefined);
  assert.ok(ground().state.drawable.autoShapes.some(shape => shape.orig === 'e2' && shape.dest === 'e1'));
});
