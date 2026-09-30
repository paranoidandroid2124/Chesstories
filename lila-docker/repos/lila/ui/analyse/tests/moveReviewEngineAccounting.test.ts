import assert from 'node:assert/strict';
import { test } from 'node:test';
import { JSDOM } from 'jsdom';
import { makeTree } from 'lib/tree';
import { treeReconstruct } from '../src/util';
import { Protocol } from '../../lib/src/ceval/protocol';
import {
  makeMoveReviewWork,
  moveReviewEngineProfile,
  type MoveReviewWorkInput,
} from '../../lib/src/ceval/types';
import {
  decodeMoveReviewSnapshot,
  type IssuedMoveReviewEngineWork,
  type MoveReviewEngineOutcome,
  type MoveReviewJobState,
  type MoveReviewProof,
} from '../src/moveReview';
import { rawResponse, rawSnapshot, rawIssuedWork, rawProgress, decodeContext } from './moveReviewTestSupport';

const fen = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';
const issued: IssuedMoveReviewEngineWork = {
  engineProfile: moveReviewEngineProfile,
  workId: 'work:1',
  purpose: 'root_search',
  executionKeySha256: 'a'.repeat(64),
  variant: 'standard',
  enginePositionInitialFen: fen,
  enginePositionMovesUci: [],
  searchFen: fen,
  rootRestriction: { kind: 'unrestricted' },
  searchLimits: { depth: 16, nodes: 5_000_000, movetimeMs: 5_000, multiPv: 1 },
  maxSearchElapsedMs: 6_000,
};

type Owner = {
  ceval: EngineHost;
  executeMoveReviewEngineWork: (
    work: IssuedMoveReviewEngineWork,
    received: number,
    signal: AbortSignal,
    deadlineAtMs?: number,
  ) => Promise<MoveReviewEngineOutcome>;
  resumeLiveEngineAfterMoveReview: () => void;
  moveReviewAccounting?: Promise<void>;
  moveReviewEngineFailure?: () => void;
};

test('AnalyseCtrl closes stopped engine accounting without delaying cancellation UI', async t => {
  const dom = new JSDOM('<!doctype html>', { url: 'https://chesstory.test/', pretendToBeVisual: true });
  const clock = new TestClock();
  const originals = new Map<string, PropertyDescriptor | undefined>();
  for (const [name, value] of Object.entries({
    window: dom.window,
    document: dom.window.document,
    navigator: dom.window.navigator,
    location: dom.window.location,
    performance: { now: () => clock.now },
    $html: String.raw,
  })) {
    originals.set(name, Object.getOwnPropertyDescriptor(globalThis, name));
    Object.defineProperty(globalThis, name, { configurable: true, value });
  }
  dom.window.setTimeout = clock.setTimeout as typeof dom.window.setTimeout;
  dom.window.clearTimeout = clock.clearTimeout;
  try {
    const { default: AnalyseCtrl } = await import('../src/ctrl');
    await t.test(
      'retained assessment proofs remain available while investigation is loading or stopped',
      () => {
        const baseline = decodeMoveReviewSnapshot(rawResponse(), decodeContext());
        assert.ok(baseline?.kind === 'completed');
        const owner = Object.create(AnalyseCtrl.prototype) as {
          moveReviewJob: MoveReviewJobState;
          currentMoveReviewProof(id: string): { proof: MoveReviewProof } | undefined;
        };
        const shared = {
          subject: baseline.subject,
          assessment: { kind: 'completed' as const, evidence: baseline.evidence },
        };
        for (const job of [
          { ...shared, kind: 'loading' as const },
          { ...shared, kind: 'abstained' as const, stopCondition: 'deadline_exceeded' as const },
        ]) {
          owner.moveReviewJob = job;
          assert.equal(owner.currentMoveReviewProof('pv:e7e5')?.proof.moves[0].uci, 'e7e5');
          assert.equal(owner.currentMoveReviewProof('line:c7c5')?.proof.moves[0].uci, 'c7c5');
        }
      },
    );
    function harness(deadlineAtMs?: number) {
      clock.reset();
      const events: string[] = [];
      const engine = new EngineHost();
      const owner = Object.assign(Object.create(AnalyseCtrl.prototype), {
        ceval: engine,
        cevalEnabled: () => true,
        startCeval: () => events.push('live-resumed'),
        redraw: () => events.push('redraw'),
      }) as Owner;
      const abort = new AbortController();
      const result = owner.executeMoveReviewEngineWork(issued, clock.now, abort.signal, deadlineAtMs);
      engine.protocol.received('readyok');
      return { owner, engine, abort, result, events };
    }

    await t.test('late final usage belongs to cancellation, never late depth16 success', async () => {
      const h = harness();
      let settled = false;
      void h.result.then(() => (settled = true));
      h.engine.protocol.received('info depth 15 multipv 1 score cp 12 nodes 191876 time 146 pv e2e4');
      h.abort.abort();
      h.owner.resumeLiveEngineAfterMoveReview();
      assert.deepEqual(h.events, ['live-resumed', 'redraw']);
      assert.equal(h.engine.destroyed, 0, 'UI resumption must not dispose the accounting owner');
      await Promise.resolve();
      assert.equal(settled, false);
      assert.equal(clock.nextDelay(), 250);
      h.engine.protocol.received('info depth 16 multipv 1 score cp 25 nodes 286932 time 204 pv d2d4');
      h.engine.protocol.received('bestmove d2d4');
      const result = await h.result;
      assert.equal(result.kind, 'executor_failed');
      if (result.kind !== 'executor_failed') return;
      assert.equal(result.failureCode, 'cancelled');
      assert.equal(result.observedNodes, 286932);
      assert.equal(result.engineTimeMs, 204);
      assert.doesNotMatch(result.diagnostic, /incomplete/);
      assert.equal(h.engine.destroyed, 1);
      assert.equal(h.owner.moveReviewAccounting, undefined);
      assert.equal(clock.size, 0);
    });

    await t.test(
      'missing stop acknowledgement closes at 250 ms with explicit lower-bound counters',
      async () => {
        const h = harness();
        h.engine.protocol.received('info nodes 700 time 4');
        h.abort.abort();
        h.owner.resumeLiveEngineAfterMoveReview();
        clock.fireNext();
        const result = await h.result;
        assert.equal(result.kind, 'executor_failed');
        if (result.kind !== 'executor_failed') return;
        assert.equal(result.failureCode, 'cancelled');
        assert.equal(result.executorElapsedMs, 250);
        assert.equal(result.observedNodes, 700);
        assert.match(result.diagnostic, /incomplete.*lower bounds/);
        assert.equal(h.engine.destroyed, 1);
        h.engine.input!.observe?.(9000, 90);
        h.engine.input!.emit({ fen, depth: 16, nodes: 9000, millis: 90, bestmove: 'e2e4', pvs: [] });
        assert.equal(result.observedNodes, 700, 'closed accounting is immutable');
        assert.equal(h.owner.moveReviewAccounting, undefined);
      },
    );

    await t.test(
      'lease expiry keeps its reason and final usage while natural search exhaustion stays separate',
      async () => {
        const h = harness();
        h.engine.protocol.received('info depth 10 multipv 1 score cp 10 nodes 100 time 2 pv e2e4');
        clock.fireNext();
        assert.equal(clock.nextDelay(), 250);
        h.engine.protocol.received('info nodes 600 time 5900');
        h.engine.protocol.received('bestmove e2e4');
        const lease = await h.result;
        assert.equal(lease.kind, 'executor_failed');
        if (lease.kind !== 'executor_failed') return;
        assert.equal(lease.failureCode, 'lease_expired');
        assert.equal(lease.observedNodes, 600);
        assert.equal(lease.engineTimeMs, 5900);
        const natural = harness();
        natural.engine.protocol.received(
          'info depth 10 multipv 1 score cp 10 nodes 5000000 time 4000 pv e2e4',
        );
        natural.engine.protocol.received('bestmove e2e4');
        const exhausted = await natural.result;
        assert.equal(exhausted.kind, 'executor_failed');
        if (exhausted.kind !== 'executor_failed') return;
        assert.equal(exhausted.failureCode, 'search_budget_exhausted');
        assert.equal(exhausted.observedNodes, 5000000);
        assert.equal(natural.owner.moveReviewAccounting, undefined);
      },
    );

    await t.test(
      'request deadline shortens execution without rewriting the issued search or skipping cleanup',
      async () => {
        const h = harness(1_150);
        assert.equal(clock.nextDelay(), 150);
        assert.deepEqual(h.engine.input!.searchLimits, issued.searchLimits);
        clock.fireNext();
        assert.equal(clock.nextDelay(), 250, 'deadline requests stop; it does not claim immediate disposal');
        assert.ok(h.owner.moveReviewAccounting);
        h.engine.protocol.received('info nodes 123 time 145');
        h.engine.protocol.received('bestmove e2e4');
        const result = await h.result;
        assert.equal(result.kind, 'executor_failed');
        if (result.kind !== 'executor_failed') return;
        assert.equal(result.failureCode, 'lease_expired');
        assert.equal(result.observedNodes, 123);
        assert.equal(h.engine.destroyed, 1);
        assert.equal(h.owner.moveReviewAccounting, undefined);
      },
    );

    await t.test('replacement review waits for disposal and cannot inherit old counters', async () => {
      const h = harness();
      const oldInput = h.engine.input!;
      h.abort.abort();
      h.owner.resumeLiveEngineAfterMoveReview();
      const next = h.owner.executeMoveReviewEngineWork(
        { ...issued, workId: 'work:0' },
        clock.now,
        new AbortController().signal,
      );
      await Promise.resolve();
      assert.equal(h.engine.preflights, 0);
      h.engine.protocol.received('info nodes 999999 time 55');
      h.engine.protocol.received('bestmove e2e4');
      await h.result;
      await Promise.resolve();
      assert.equal(h.engine.preflights, 1);
      assert.equal(h.engine.destroyed, 1);
      const nextProtocol = h.engine.protocol;
      nextProtocol.received('readyok');
      oldInput.observe?.(9999999, 999);
      oldInput.emit({ fen, depth: 16, nodes: 9999999, millis: 999, bestmove: 'd2d4', pvs: [] });
      nextProtocol.received('info depth 1 multipv 1 score cp 5 nodes 33 time 2 pv g1f3');
      nextProtocol.received('bestmove g1f3');
      const outcome = await next;
      assert.equal(outcome.kind, 'executor_failed');
      if (outcome.kind !== 'executor_failed') return;
      assert.equal(outcome.failureCode, 'search_budget_exhausted');
      assert.equal(outcome.observedNodes, 33);
      assert.equal(outcome.engineTimeMs, 2);
      assert.equal(h.engine.destroyed, 1, 'old accounting cannot dispose a replacement');
    });

    await t.test('stop failure preserves cancellation and disposes only the captured owner', async () => {
      const h = harness();
      h.engine.throwOnStop = true;
      const replacement = new EngineHost();
      h.owner.ceval = replacement;
      h.abort.abort();
      const outcome = await h.result;
      assert.equal(outcome.kind, 'executor_failed');
      if (outcome.kind !== 'executor_failed') return;
      assert.equal(outcome.failureCode, 'cancelled');
      assert.match(outcome.diagnostic, /incomplete/);
      assert.equal(h.engine.destroyed, 1);
      assert.equal(replacement.destroyed, 0);
      assert.equal(clock.size, 0);
    });

    await t.test(
      'engine error during draining cannot change lease expiry into success or another reason',
      async () => {
        const h = harness();
        clock.fireNext();
        h.owner.moveReviewEngineFailure!();
        const outcome = await h.result;
        assert.equal(outcome.kind, 'executor_failed');
        if (outcome.kind !== 'executor_failed') return;
        assert.equal(outcome.failureCode, 'lease_expired');
        assert.match(outcome.diagnostic, /incomplete/);
        assert.equal(h.engine.destroyed, 1);
        assert.equal(clock.size, 0);
      },
    );

    await t.test('completion uses maximum observed usage beyond the last exact PV frame', async () => {
      const outcomes: MoveReviewEngineOutcome[] = [];
      for (const usage of [
        'info nodes 5000001 time 4',
        'info depth 17 score cp 12 lowerbound nodes 300 time 5001 pv e2e4',
        'info nodes 300 time 4',
      ]) {
        const h = harness();
        for (const depth of [15, 16])
          h.engine.protocol.received(`info depth ${depth} multipv 1 score cp 10 nodes 100 time 2 pv e2e4`);
        h.engine.protocol.received(usage);
        h.engine.protocol.received('bestmove e2e4');
        outcomes.push(await h.result);
        assert.equal(h.owner.moveReviewAccounting, undefined);
        assert.equal(clock.size, 0);
      }
      assert.deepEqual(
        outcomes.map(outcome => outcome.kind),
        ['executor_failed', 'executor_failed', 'completed'],
      );
      for (const [index, nodes, millis] of [
        [0, 5000001, 4],
        [1, 300, 5001],
      ] as const) {
        const outcome = outcomes[index]!;
        assert.equal(outcome.kind, 'executor_failed');
        if (outcome.kind !== 'executor_failed') continue;
        assert.equal(outcome.failureCode, 'search_budget_exhausted');
        assert.equal(outcome.observedNodes, nodes);
        assert.equal(outcome.engineTimeMs, millis);
      }
      const complete = outcomes[2]!;
      assert.equal(complete.kind, 'completed');
      if (complete.kind === 'completed') {
        assert.equal(complete.nodes, 300);
        assert.equal(complete.engineTimeMs, 4);
      }
    });

    await t.test('an exact natural completion still succeeds without a stop delay', async () => {
      const h = harness();
      for (const depth of [15, 16])
        h.engine.protocol.received(`info depth ${depth} multipv 1 score cp 10 nodes 100 time 2 pv e2e4`);
      h.engine.protocol.received('bestmove e2e4');
      const outcome = await h.result;
      assert.equal(outcome.kind, 'completed');
      assert.equal(h.owner.moveReviewAccounting, undefined);
      assert.equal(clock.size, 0);
    });
  } finally {
    for (const [name, descriptor] of originals) {
      if (descriptor) Object.defineProperty(globalThis, name, descriptor);
      else Reflect.deleteProperty(globalThis, name);
    }
    dom.window.close();
  }
});

test('AnalyseCtrl imports a complete PGN, delivers progressive reviews across focus changes, and reuses completed moves', async () => {
  const dom = new JSDOM('<!doctype html>', { url: 'https://chesstory.test/import', pretendToBeVisual: true });
  const clock = new TestClock();
  const originals = new Map<string, PropertyDescriptor | undefined>();
  for (const [name, value] of Object.entries({
    window: dom.window, document: dom.window.document, navigator: dom.window.navigator,
    location: dom.window.location, performance: { now: () => clock.now }, $html: String.raw,
  })) {
    originals.set(name, Object.getOwnPropertyDescriptor(globalThis, name));
    Object.defineProperty(globalThis, name, { configurable: true, value });
  }
  dom.window.setTimeout = clock.setTimeout as typeof dom.window.setTimeout;
  dom.window.clearTimeout = clock.clearTimeout;
  const originalFetch = globalThis.fetch;
  const requests: Record<string, any>[] = [];
  const workDepths: number[] = [];
  let owner: any;
  try {
    const { default: AnalyseCtrl } = await import('../src/ctrl');
    // Existing controller methods run unchanged; board/explorer effects are outside this connection test.
    owner = Object.assign(Object.create(AnalyseCtrl.prototype), {
      opts: { moveReview: { mode: 'runtime' } }, isEmbed: false, ongoing: false,
      data: { orientation: 'white', pref: {}, game: { variant: { key: 'chess960' } } },
      moveReviewAnnotations: new Map(), moveReviewView: { evidenceExpanded: false }, cgVersion: { js: 0 },
      ceval: { stopMoveReview: () => {} }, cevalEnabled: () => false,
      redraw: () => {}, syncWorkspacePrefs: () => {}, initCeval: () => {}, startCeval: () => {},
      mergeIdbThenShowTreeView: () => {}, suspendLiveEngineForMoveReview: () => {},
      initialize(data: any) {
        this.data = data;
        this.tree = makeTree(treeReconstruct(data.treeParts, data.sidelines));
      },
      setPath(path: Tree.Path) {
        this.path = path;
        this.nodeList = this.tree.getNodeList(path);
      },
      mainlinePlyToPath() {
        let node = this.tree.root, path = '';
        while (node.children[0]) { node = node.children[0]; path += node.id; }
        return path;
      },
      userJump(path: Tree.Path) { this.setPath(path); this.scheduleMoveReviewForCurrentEdge(); },
      async executeMoveReviewEngineWork(work: IssuedMoveReviewEngineWork, _at: number, signal: AbortSignal): Promise<MoveReviewEngineOutcome> {
        workDepths.push(work.searchLimits.depth);
        if (work.searchLimits.depth === 16) {
          assert.equal(this.moveReviewJob.recordReview.assessed, requests.length);
          assert.equal(this.moveReviewJob.recordReview.completed, requests.length - 1);
          dom.window.dispatchEvent(new dom.window.Event('blur'));
          Object.defineProperty(dom.window.document, 'visibilityState', { configurable: true, value: 'hidden' });
          dom.window.document.dispatchEvent(new dom.window.Event('visibilitychange'));
          assert.equal(signal.aborted, false, 'changing windows must not cancel a submitted review');
        }
        const move = requests.at(-1)!.focus.played_move_uci;
        return {
          kind: 'completed', completedDepth: work.searchLimits.depth, selectiveDepth: work.searchLimits.depth,
          nodes: 1, engineTimeMs: 1, executorElapsedMs: 1, bestmoveUci: move,
          lineSuffixes: [{ moves: [move], depth: work.searchLimits.depth, whiteScore: { kind: 'cp', value: 0 } }],
        };
      },
    });
    const jobs = new Map<string, { request: Record<string, any>; reports: number }>();
    globalThis.fetch = async (input, init) => {
      const url = String(input);
      assert.equal(init?.method, 'post');
      const body = JSON.parse(String(init?.body));
      let jobId: string;
      if (url.endsWith('/position-commentary-jobs')) {
        requests.push(body);
        jobId = requests.length.toString().padStart(32, '0');
        jobs.set(jobId, { request: body, reports: 0 });
      } else {
        assert.ok(url.endsWith('/engine-work-reports'));
        jobId = url.split('/').at(-2)!;
        const job = jobs.get(jobId)!;
        assert.equal(body.work_id, `work:${job.reports}`);
        job.reports++;
      }
      const { request, reports } = jobs.get(jobId)!;
      const common = { request_id: request.request_id, job_id: jobId, focus: request.focus };
      const depth = reports === 2 ? 16 : 8;
      const result = {
        kind: 'selected_move_choices', selected_move_reviews: [{
          legal_move_index: 0, move_uci: request.focus.played_move_uci,
          selection: { roles: ['best', 'played'], root_rank: 1 },
          line_insight: { endpoint: { kind: 'engine_search', moves: [request.focus.played_move_uci],
            win_percent_for_mover: 50, depth } },
        }],
      };
      const verification = { state: reports === 2 ? 'complete' : 'pending', target_depth: 16,
        reason: 'initial_verification', trigger_work_ids: [], completed_depth: reports === 2 ? 16 : null,
        stop_reason: null };
      const response = reports === 2
        ? rawResponse({ ...common, current_fen: request.current_fen, result, verification })
        : {
            ...rawSnapshot(reports === 0 ? 'awaiting_core' : 'awaiting_evidence'), ...common,
            progress: { ...rawProgress('root_search', reports + 1, reports), selected_commentaries_completed: reports },
            issued_engine_work: {
              ...rawIssuedWork(), work_id: `work:${reports}`, execution_key_sha256: String(reports + 1).repeat(64),
              engine_position_initial_fen: request.initial_fen, engine_position_moves_uci: request.move_prefix_uci,
              search_fen: request.current_fen,
              search_limits: { depth: reports === 0 ? 8 : 16, nodes: reports === 0 ? 2_000_000 : 5_000_000,
                movetime_ms: reports === 0 ? 2_500 : 5_000, multi_pv: 1 },
              max_search_elapsed_ms: reports === 0 ? 3_500 : 6_000,
            },
            ...(reports === 1 ? { assessment: result, verification } : {}),
          };
      return new Response(JSON.stringify(response), { status: reports === 0 ? 201 : 200 });
    };
    for (const [mode, isEmbed, ongoing, variant] of [
      ['off', false, false, 'standard'], ['runtime', true, false, 'standard'],
      ['runtime', false, true, 'standard'], ['runtime', false, false, 'chess960'],
    ]) {
      owner.opts.moveReview.mode = mode;
      owner.isEmbed = isEmbed;
      owner.ongoing = ongoing;
      owner.data.game.variant.key = variant;
      owner.scheduleMoveReviewForCurrentEdge();
      assert.equal(owner.moveReviewAvailable(), false, 'disabled or unsupported context does not initialize review');
    }
    const pgn = '1. f3 (1. e4) e5 2. g4 Qh4# 0-1';
    assert.ok(owner.changePgn(pgn, true));
    assert.equal(owner.moveReviewAvailable(), true, 'ordinary PGN import initializes the configured runtime');
    assert.equal(owner.opts.study, undefined);
    const expected = ['f2f3', 'e7e5', 'g2g4', 'd8h4'];
    for (let i = 0; i < expected.length; i++) {
      assert.equal(clock.nextDelay(), 250);
      clock.fireNext();
      // Drain only the asynchronous mock HTTP chain; no real engine or wall-clock delay is involved.
      for (let j = 0; j < 100 && owner.moveReviewJob.recordReview.completed < i + 1; j++) await Promise.resolve();
      assert.equal(owner.moveReviewJob.recordReview.completed, i + 1);
      assert.equal(requests[i].focus.played_move_uci, expected[i]);
      assert.deepEqual(requests[i].move_prefix_uci, expected.slice(0, i));
    }
    assert.deepEqual(workDepths, [8, 16, 8, 16, 8, 16, 8, 16]);
    assert.deepEqual(requests[0].input_lines, [
      { source: 'pgn_record', moves: expected }, { source: 'analysis', moves: ['e2e4'] },
    ]);
    assert.deepEqual(owner.moveReviewJob.recordReview, {
      total: 4, assessed: 4, completed: 4, stopped: 0, scenes: [], status: 'finished',
    });
    let node = owner.tree.root, path = '';
    for (const move of expected) {
      node = node.children[0]; path += node.id;
      owner.userJump(path);
      assert.equal(owner.moveReviewJob.kind, 'completed');
      assert.equal(owner.moveReviewJob.snapshot.subject.played.uci, move);
      assert.equal(owner.moveReviewJob.snapshot.evidence.candidates[0].review.proof.moves[0].uci, move);
    }
    assert.equal(requests.length, 4, 'navigation reuses completed reviews instead of issuing new jobs');
    assert.equal(clock.size, 0);
    // Leaving the document still cancels; a replacement PGN cannot inherit the previous game's grades.
    assert.ok(owner.changePgn('1. d4 d5 *', true));
    assert.equal(owner.moveReviewJob.recordReview.completed, 0);
    dom.window.dispatchEvent(new dom.window.Event('pagehide'));
    assert.equal(owner.moveReviewJob.recordReview.status, 'paused');
    assert.equal(clock.size, 0);
  } finally {
    owner?.moveReviewCoordinator?.destroy();
    globalThis.fetch = originalFetch;
    for (const [name, descriptor] of originals) {
      if (descriptor) Object.defineProperty(globalThis, name, descriptor);
      else Reflect.deleteProperty(globalThis, name);
    }
    dom.window.close();
  }
});

class EngineHost {
  protocol = this.freshProtocol();
  input?: MoveReviewWorkInput;
  destroyed = 0;
  preflights = 0;
  throwOnStop = false;
  private freshProtocol(): Protocol {
    const protocol = new Protocol();
    protocol.connected(() => {});
    protocol.received('uciok');
    protocol.received('readyok');
    return protocol;
  }
  async preflightMoveReviewSelection() {
    this.preflights++;
    this.protocol = this.freshProtocol();
    return { ok: true, profile: moveReviewEngineProfile };
  }
  startMoveReview(_profile: string, input: MoveReviewWorkInput): boolean {
    this.input = input;
    this.protocol.compute(makeMoveReviewWork(moveReviewEngineProfile, input));
    return true;
  }
  stopMoveReviewWork(): void {
    if (this.throwOnStop) throw new Error('stop failed');
    this.protocol.compute(undefined);
  }
  stopMoveReview(): void {
    this.destroyed++;
  }
}

class TestClock {
  now = 1000;
  private nextId = 1;
  private tasks = new Map<number, { at: number; callback: () => void }>();
  get size() {
    return this.tasks.size;
  }
  reset() {
    assert.equal(this.tasks.size, 0);
    this.now = 1000;
  }
  setTimeout = (handler: TimerHandler, delay = 0): number => {
    const id = this.nextId++;
    this.tasks.set(id, { at: this.now + delay, callback: handler as () => void });
    return id;
  };
  clearTimeout = (id?: number): void => {
    if (id !== undefined) this.tasks.delete(id);
  };
  nextDelay(): number {
    return Math.min(...[...this.tasks.values()].map(t => t.at)) - this.now;
  }
  fireNext(): void {
    const [id, task] = [...this.tasks.entries()].sort((a, b) => a[1].at - b[1].at)[0]!;
    assert.ok(task);
    this.tasks.delete(id);
    this.now = task.at;
    task.callback();
  }
}
