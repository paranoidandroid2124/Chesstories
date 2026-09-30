import assert from 'node:assert/strict';
import { test } from 'node:test';
import { readFileSync } from 'node:fs';
import pgnImport from '../src/pgnImport';
import { treeReconstruct } from '../src/util';
import { moveReviewEngineProfile } from 'lib/ceval/types';
import {
  type MoveReviewEngineOutcome,
  type MoveReviewJobState,
  type MoveReviewSnapshot,
  type MoveReviewSource,
  type MoveReviewSourceRequest,
  type MoveReviewObservations,
  decodeMoveReviewSnapshot,
  moveReviewCopy,
  moveReviewSubjectFromNodeList,
  moveReviewRecordFromTree,
  moveReviewFrameForSubject,
  moveReviewObservations,
  type MoveReviewSubject,
  type IssuedMoveReviewEngineWork,
} from '../src/moveReview';
import { MoveReviewCoordinator, type MoveReviewCoordinatorHost } from '../src/moveReviewCoordinator';
import { MoveReviewRuntimeError, createMoveReviewRuntimeSource } from '../src/moveReviewRuntimeSource';
import { decodeCommentarySelection, type CommentaryProposal } from '../src/moveReviewSelection';
import {
  rawResponse,
  rawSnapshot,
  rawInvestigation,
  requestId,
  subject,
  observationFixtureJob,
} from './moveReviewTestSupport';

type CompletedSnapshot = Extract<MoveReviewSnapshot, { kind: 'completed' }>;

function suppliedGame() {
  const base = new URL('../../../../../../research/ordinary-move-progress-2026-09-06/', import.meta.url);
  const game = JSON.parse(readFileSync(new URL('pc-games.json', base), 'utf8'))[0];
  const data = pgnImport(game.san.join(' ') + ' *');
  const root = treeReconstruct(data.treeParts!, data.sidelines);
  const record = moveReviewRecordFromTree('standard', root, data.recordedMoves, 'study/chapter')!;
  const fixture = JSON.parse(readFileSync(new URL('whole-game-public/fixtures.json', base), 'utf8'))[0];
  const observations = moveReviewObservations(observationFixtureJob(fixture))!;
  const selected: MoveReviewSubject[] = [];
  const nodes = [root];
  let path = '';
  while (nodes.at(-1)!.children[0]) {
    const node = nodes.at(-1)!.children[0]!;
    nodes.push(node);
    path += node.id;
    selected.push(moveReviewSubjectFromNodeList('standard', path, nodes, data.recordedMoves, 'study')!);
  }
  return { root, data, record, observations, selected };
}

test('one supplied record survives 109 jumps, late completion and an explicit grade failure', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  let coordinator: MoveReviewCoordinator | undefined;
  t.after(() => {
    coordinator?.destroy();
    environment.restore();
  });
  const { record, observations, selected } = suppliedGame();
  const runs: {
    request: MoveReviewSourceRequest;
    emit: (s: MoveReviewSnapshot) => void;
    signal: AbortSignal;
    finish: () => void;
  }[] = [];
  const harness = createHost({
    run: async (request, emit, signal) => {
      const common = snapshotCommon(request);
      emit({
        ...common,
        kind: 'awaiting-core',
        observations: runs.length ? undefined : observations,
        issuedEngineWork: {} as IssuedMoveReviewEngineWork,
      }); // Executor is not used in this coordinator test.
      await new Promise<void>(finish => runs.push({ request, emit, signal, finish }));
    },
  });
  coordinator = new MoveReviewCoordinator('en-US', harness.host);
  coordinator.activate();
  coordinator.settle(undefined, record); // Actual root entry starts the supplied record.
  environment.timers.runAll();
  await settleAsync();
  assert.equal(runs.length, 1);
  assert.equal(runs[0].request.subject.movePrefixUci.length, 0);
  for (const [i, selection] of selected.entries()) {
    coordinator.settle(selection, record);
    environment.timers.runAll();
    await settleAsync();
    const state = harness.states.at(-1)!;
    assert.equal(state.kind, i === 0 ? 'loading' : 'unsearched');
    assert.deepEqual(
      state.observationFrame,
      moveReviewFrameForSubject(observations, record.subject, selection),
    );
    assert.ok(state.observationFrame, `frame ${i + 1}`);
  }
  assert.equal(runs.length, 1);
  assert.equal(runs[0].signal.aborted, false);
  runs[0].emit({ ...completedSnapshot(runs[0].request), observations });
  runs[0].finish();
  await settleAsync();
  assert.equal(harness.states.at(-1)!.kind, 'unsearched'); // First grade cannot migrate to ply 109.
  coordinator.settle(selected[0], record);
  assert.equal(harness.states.at(-1)!.kind, 'completed');
  coordinator.settle(selected[40], record);
  coordinator.retry();
  environment.timers.runAll();
  await settleAsync();
  assert.equal(runs.length, 2);
  assert.deepEqual(runs[1].request.subject.movePrefixUci, selected[40].movePrefixUci);
  runs[1].emit({
    ...snapshotCommon(runs[1].request),
    kind: 'abstained',
    stopCondition: 'engine_unavailable',
  });
  runs[1].finish();
  await settleAsync();
  const failed = harness.states.at(-1)!;
  assert.equal(failed.kind, 'abstained');
  assert.equal(moveReviewObservations(failed), observations);
  assert.ok(failed.observationFrame);
  coordinator.settle(selected[41], record);
  assert.equal(harness.states.at(-1)!.kind, 'unsearched');
  assert.equal(moveReviewObservations(harness.states.at(-1)!), observations);
  assert.deepEqual(harness.counts(), { suspended: 2, resumed: 2 });
});

test('record input revisions invalidate observations and reject old emissions even at the same focus', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  t.after(environment.restore);
  const { record, root, data, selected, observations } = suppliedGame();
  const runs: {
    request: MoveReviewSourceRequest;
    emit: (s: MoveReviewSnapshot) => void;
    signal: AbortSignal;
    finish: () => void;
  }[] = [];
  const harness = createHost({
    run: async (request, emit, signal) => {
      await new Promise<void>(finish => runs.push({ request, emit, signal, finish }));
    },
  });
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => {
    coordinator.destroy();
    runs.forEach(r => r.finish());
  });
  coordinator.activate();
  coordinator.settle(selected[0], record);
  environment.timers.runAll();
  await settleAsync();
  root.children[0].children[0].children = []; // Even an edit masked by immutable pgn_record revises the tree.
  const revised = moveReviewRecordFromTree('standard', root, data.recordedMoves, 'study/chapter')!;
  assert.notEqual(revised.key, record.key);
  coordinator.settle(selected[0], revised);
  environment.timers.runAll();
  await settleAsync();
  assert.equal(runs.length, 2);
  assert.equal(runs[0].signal.aborted, true);
  runs[0].emit({ ...completedSnapshot(runs[0].request), observations });
  runs[0].finish();
  await settleAsync();
  assert.equal(harness.states.at(-1)!.kind, 'loading');
  assert.equal(moveReviewObservations(harness.states.at(-1)!), undefined);
  coordinator.deactivate();
  await settleAsync();
  assert.equal(harness.states.at(-1)!.kind, 'abstained');
  assert.equal(harness.states.at(-1)!.recordReview?.status, 'paused');
  assert.equal(runs[1].signal.aborted, true);
  coordinator.activate();
  environment.timers.runAll();
  await settleAsync();
  assert.equal(runs.length, 2); // Cancellation is not disguised as an automatic new attempt.
  coordinator.settle(selected[0], revised);
  assert.equal(harness.states.at(-1)!.recordReview?.status, 'paused');
});

function snapshotCommon(request: MoveReviewSourceRequest) {
  return {
    requestId: request.requestId,
    jobId: 'a'.repeat(32),
    engineProfile: request.engineProfile,
    judgmentRevision: 'chesstory.position-commentary.response.v6',
    annotationPolicyRevision: 'chesstory.verdict-threshold-policy.v2',
    subject: request.subject,
  };
}

function completedSnapshot(request: MoveReviewSourceRequest): CompletedSnapshot {
  return {
    ...snapshotCommon(request),
    kind: 'completed',
    evidence: {
      candidates: [
        {
          uci: request.subject.played.uci,
          label: request.subject.played.san,
          roles: ['best', 'played'],
          review: {
            kind: 'single-candidate-insight',
            proof: {
              id: 'line.test',
              startFen: request.subject.before.fen,
              moves: [
                {
                  uci: request.subject.played.uci,
                  label: request.subject.played.uci,
                  fenAfter: request.subject.after.fen,
                },
              ],
            },
          },
        },
      ],
    },
  };
}

class TestTimers {
  private nextId = 1;
  private readonly callbacks = new Map<number, () => void>();

  readonly window = {
    setTimeout: (callback: TimerHandler): number => {
      const id = this.nextId++;
      this.callbacks.set(id, callback as () => void);
      return id;
    },
    clearTimeout: (id?: number): void => {
      if (id !== undefined) this.callbacks.delete(id);
    },
  } as unknown as Window & typeof globalThis;

  runAll(): void {
    const callbacks = [...this.callbacks.values()];
    this.callbacks.clear();
    callbacks.forEach(callback => callback());
  }
}

class TestLockManager {
  private tail = Promise.resolve();

  readonly api = {
    request: async (name: string, _options: { signal?: AbortSignal }, callback: () => unknown) => {
      assert.equal(name, 'chesstory.position-commentary.v6');
      const previous = this.tail;
      let release = () => {};
      this.tail = new Promise<void>(resolve => (release = resolve));
      await previous;
      try {
        return await callback();
      } finally {
        release();
      }
    },
  } as unknown as LockManager;
}

function replaceGlobal(name: string, value: unknown): () => void {
  const previous = Object.getOwnPropertyDescriptor(globalThis, name);
  Object.defineProperty(globalThis, name, { configurable: true, writable: true, value });
  return () => {
    if (previous) Object.defineProperty(globalThis, name, previous);
    else delete (globalThis as Record<string, unknown>)[name];
  };
}

function installCoordinatorEnvironment({ webLocks = true } = {}) {
  const timers = new TestTimers();
  const lockManager = new TestLockManager();
  const restoreWindow = replaceGlobal('window', timers.window);
  const restoreNavigator = replaceGlobal('navigator', webLocks ? { locks: lockManager.api } : {});
  return {
    timers,
    restore: () => {
      restoreNavigator();
      restoreWindow();
    },
  };
}

function createHost(source: MoveReviewSource) {
  const states: MoveReviewJobState[] = [];
  let suspended = 0;
  let resumed = 0;
  const host: MoveReviewCoordinatorHost = {
    engineProfile: moveReviewEngineProfile,
    source,
    suspendLiveEngine: () => suspended++,
    resumeLiveEngine: () => resumed++,
    stateChanged: state => states.push(state),
  };
  return { host, states, counts: () => ({ suspended, resumed }) };
}

async function settleAsync(): Promise<void> {
  for (let index = 0; index < 4; index++) await Promise.resolve();
}

test('coordinator preempts the live engine once and publishes one completed v6 review', async t => {
  const environment = installCoordinatorEnvironment();
  t.after(environment.restore);
  const source: MoveReviewSource = {
    run: async (request, emit) => emit(completedSnapshot(request)),
  };
  const harness = createHost(source);
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => coordinator.destroy());

  coordinator.activate();
  coordinator.settle(subject);
  await settleAsync();
  environment.timers.runAll();
  await settleAsync();

  assert.equal(harness.states.at(-1)?.kind, 'completed');
  assert.deepEqual(harness.counts(), { suspended: 1, resumed: 1 });
  assert.equal(coordinator.isPreemptingLiveEngine(), false);
});

test('coordinator preserves each decoded stop reason and releases the live engine', async t => {
  const environment = installCoordinatorEnvironment();
  t.after(environment.restore);
  for (const stopCondition of Object.keys(moveReviewCopy('en-US').stopReasons)) {
    const source: MoveReviewSource = {
      run: async (request, emit) => {
        const raw = rawSnapshot('stopped');
        raw.request_id = request.requestId;
        raw.stop_condition = stopCondition;
        const snapshot = decodeMoveReviewSnapshot(raw, {
          requestId: request.requestId,
          subject: request.subject,
          engineProfile: request.engineProfile,
        });
        assert.ok(snapshot);
        emit(snapshot);
      },
    };
    const harness = createHost(source);
    const coordinator = new MoveReviewCoordinator('en-US', harness.host);
    try {
      coordinator.activate();
      coordinator.settle(subject);
      await settleAsync();
      environment.timers.runAll();
      await settleAsync();
      const state = harness.states.at(-1);
      assert.equal(state?.kind, 'abstained');
      assert.equal(state?.kind === 'abstained' && state.stopCondition, stopCondition);
      assert.deepEqual(harness.counts(), { suspended: 1, resumed: 1 });
    } finally {
      coordinator.destroy();
    }
  }
});

test('coordinator retains producer observations after transport failure and panel deactivation', async t => {
  const environment = installCoordinatorEnvironment();
  t.after(environment.restore);
  const { readFileSync } = await import('node:fs');
  const raw = JSON.parse(
    readFileSync(
      new URL(
        '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/development-awaiting-produced.json',
        import.meta.url,
      ),
      'utf8',
    ),
  );
  const observedSubject = {
    ...subject,
    initialFen: raw.issued_engine_work.engine_position_initial_fen,
    movePrefixUci: [],
    before: { path: '' as Tree.Path, fen: raw.issued_engine_work.search_fen },
    played: { uci: 'g1f3', san: 'Nf3' },
    after: { path: 'aa' as Tree.Path, fen: raw.focus.resulting_fen },
  };
  let received: MoveReviewSnapshot | undefined;
  const harness = createHost({
    run: async (request, emit) => {
      raw.request_id = request.requestId;
      received = decodeMoveReviewSnapshot(raw, { requestId: request.requestId, subject: request.subject });
      assert.ok(received?.observations);
      emit(received!);
      throw new MoveReviewRuntimeError('offline', true);
    },
  });
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => coordinator.destroy());
  coordinator.activate();
  coordinator.settle(observedSubject);
  await settleAsync();
  environment.timers.runAll();
  await settleAsync();
  const last = harness.states.at(-1);
  assert.equal(last?.kind, 'fault');
  assert.deepEqual(last?.observations, received?.observations);
  assert.ok(harness.states.some(state => state.kind === 'loading' && state.observations?.statements.length));
  coordinator.deactivate();
  assert.deepEqual(harness.states.at(-1)?.observations, received?.observations);
});

test('Web Lock serializes coordinators and hands off when work ends without leaving the page', async t => {
  const environment = installCoordinatorEnvironment();
  t.after(environment.restore);
  let firstRuns = 0;
  let secondRuns = 0;
  let finish = () => {};
  const first = new MoveReviewCoordinator(
    'en-US',
    createHost({
      run: async (request, emit) => {
        firstRuns++;
        await new Promise<void>(resolve => (finish = resolve));
        emit(completedSnapshot(request));
      },
    }).host,
  );
  const coordinators = [first];
  t.after(() => {
    coordinators.forEach(coordinator => coordinator.destroy());
    finish();
  });

  first.activate();
  first.settle(subject);
  await settleAsync();
  environment.timers.runAll();
  await settleAsync();
  assert.equal(firstRuns, 1);

  const second = new MoveReviewCoordinator(
    'en-US',
    createHost({
      run: async (request, emit) => {
        secondRuns++;
        emit(completedSnapshot(request));
      },
    }).host,
  );
  coordinators.push(second);
  second.activate();
  second.settle(subject);
  await settleAsync();
  environment.timers.runAll();
  await settleAsync();
  assert.equal(secondRuns, 0);

  finish();
  await settleAsync();
  await settleAsync();
  environment.timers.runAll();
  await settleAsync();
  assert.equal(secondRuns, 1);
  first.retry();
  await settleAsync();
  environment.timers.runAll();
  await settleAsync();
  assert.equal(firstRuns, 2, 'a completed page can acquire authority again on an explicit retry');
});

test('Web Lock is not held by an empty page or a record blocked by an unavailable engine', async t => {
  const environment = installCoordinatorEnvironment();
  t.after(environment.restore);
  for (const blocked of [false, true]) {
    const firstHost = createHost({
      run: async (request, emit) => emit({
        ...snapshotCommon(request), kind: 'abstained', stopCondition: 'engine_unavailable',
      }),
    });
    let secondRuns = 0;
    const secondHost = createHost({
      run: async (request, emit) => { secondRuns++; emit(completedSnapshot(request)); },
    });
    const first = new MoveReviewCoordinator('en-US', firstHost.host);
    const second = new MoveReviewCoordinator('en-US', secondHost.host);
    try {
      first.activate();
      if (blocked) first.settle(undefined, threePlyRecord().record);
      await settleAsync();
      environment.timers.runAll();
      await settleAsync();
      if (blocked) assert.equal(firstHost.states.at(-1)!.recordReview?.status, 'blocked');
      second.activate();
      second.settle(subject);
      await settleAsync();
      environment.timers.runAll();
      await settleAsync();
      assert.equal(secondRuns, 1, blocked ? 'blocked page released authority' : 'empty page never claimed authority');
      assert.equal(secondHost.states.at(-1)!.kind, 'completed');
    } finally {
      first.destroy();
      second.destroy();
    }
  }
});

test('coordinators compute independently when Web Locks are unavailable', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  t.after(environment.restore);
  let firstRuns = 0;
  let secondRuns = 0;
  const first = new MoveReviewCoordinator(
    'en-US',
    createHost({
      run: async (request, emit) => {
        firstRuns++;
        emit(completedSnapshot(request));
      },
    }).host,
  );
  const second = new MoveReviewCoordinator(
    'en-US',
    createHost({
      run: async (request, emit) => {
        secondRuns++;
        emit(completedSnapshot(request));
      },
    }).host,
  );
  t.after(() => {
    first.destroy();
    second.destroy();
  });

  first.activate();
  first.settle(subject);
  second.activate();
  second.settle(subject);
  environment.timers.runAll();
  await settleAsync();

  assert.deepEqual([firstRuns, secondRuns], [1, 1]);
});

test('job creation delivers producer facts before engine preparation and preserves them on preparation failure', async () => {
  const { readFileSync } = await import('node:fs');
  const read = (name: string) =>
    JSON.parse(
      readFileSync(
        new URL(
          `../../../../../../judgment-evaluation/fixtures/public-commentary-v6/${name}.json`,
          import.meta.url,
        ),
        'utf8',
      ),
    );
  for (const code of ['engine_unavailable', 'browser_unsupported'] as const) {
    const environment = installCoordinatorEnvironment();
    const awaiting = read('development-awaiting-produced');
    const stopped = read(`development-${code}-produced`);
    const observedSubject = {
      ...subject,
      initialFen: awaiting.issued_engine_work.engine_position_initial_fen,
      movePrefixUci: [],
      before: { path: '' as Tree.Path, fen: awaiting.issued_engine_work.search_fen },
      played: { uci: 'g1f3', san: 'Nf3' },
      after: { path: 'aa' as Tree.Path, fen: awaiting.focus.resulting_fen },
    };
    const order: string[] = [];
    const restoreFetch = replaceGlobal(
      'fetch',
      async (_input: string | URL | Request, init?: RequestInit) => {
        const body = JSON.parse(init!.body as string);
        if (order.length === 0) {
          order.push('create');
          awaiting.request_id = stopped.request_id = body.request_id;
          return new Response(JSON.stringify(awaiting), { status: 201 });
        }
        order.push('report');
        assert.equal(body.outcome.failure_code, code);
        return new Response(JSON.stringify(stopped), { status: 200 });
      },
    );
    let prepared = 0;
    const source = createMoveReviewRuntimeSource(async () => {
      const state = harness.states.at(-1);
      assert.ok(
        state?.kind === 'loading' &&
          state.observations?.statements.some(statement => statement.kind === 'development'),
      );
      order.push('prepare');
      prepared++;
      return {
        kind: 'executor_failed',
        executorElapsedMs: 1,
        observedNodes: 0,
        engineTimeMs: 0,
        failureCode: code,
        diagnostic: 'Controlled preparation failure',
      };
    });
    const harness = createHost(source);
    const coordinator = new MoveReviewCoordinator('en-US', harness.host);
    try {
      coordinator.activate();
      coordinator.settle(observedSubject);
      await settleAsync();
      environment.timers.runAll();
      for (let i = 0; i < 8; i++) await settleAsync();
      const state = harness.states.at(-1);
      assert.equal(state?.kind, 'abstained');
      assert.equal(state?.kind === 'abstained' && state.stopCondition, code);
      const development = state?.observations?.statements.find(statement => statement.kind === 'development');
      assert.ok(development?.kind === 'development');
      assert.deepEqual(development.centralSquares, ['d4', 'e5']);
      assert.equal(prepared, 1);
      assert.deepEqual(order, ['create', 'prepare', 'report']);
    } finally {
      coordinator.destroy();
      restoreFetch();
      environment.restore();
    }
  }
});

test('runtime source executes root and focus searches only through the browser executor', async t => {
  const bodies = [rawSnapshot('awaiting_core'), rawSnapshot('awaiting_evidence'), rawResponse()];
  const requests: Array<{ url: string; method: string; body?: string }> = [];
  const restoreFetch = replaceGlobal('fetch', async (input: string | URL | Request, init?: RequestInit) => {
    requests.push({
      url: String(input),
      method: init?.method ?? 'get',
      body: init?.body as string | undefined,
    });
    if (init?.method === 'delete') {
      assert.equal(new Headers(init.headers).get('Prefer'), 'return=minimal');
      return new Response(null, { status: 204 });
    }
    const body = bodies.shift();
    assert.ok(body);
    return new Response(JSON.stringify(body), { status: requests.length === 1 ? 201 : 200 });
  });
  t.after(restoreFetch);

  const executedPurposes: string[] = [];
  const source = createMoveReviewRuntimeSource(async work => {
    executedPurposes.push(work.purpose);
    const rootMoves =
      work.rootRestriction.kind === 'restricted'
        ? work.rootRestriction.movesUci
        : (['c7c5', 'e7e5', 'g8f6'] as Uci[]);
    return {
      kind: 'completed',
      completedDepth: 16,
      selectiveDepth: 20,
      nodes: 100_000,
      engineTimeMs: 500,
      executorElapsedMs: 550,
      bestmoveUci: rootMoves[0]!,
      lineSuffixes: rootMoves.map(move => ({
        moves: [move],
        depth: 16,
        whiteScore: { kind: 'cp', value: 0 },
      })),
    } satisfies MoveReviewEngineOutcome;
  });
  const emitted: string[] = [];
  await source.run(
    { requestId, subject, engineProfile: moveReviewEngineProfile },
    snapshot => emitted.push(snapshot.kind),
    new AbortController().signal,
  );

  assert.deepEqual(executedPurposes, ['root_search', 'focus_comparison']);
  assert.deepEqual(emitted, ['awaiting-core', 'awaiting-evidence', 'completed']);
  assert.equal(requests.filter(request => request.url.endsWith('/engine-work-reports')).length, 2);
  assert.equal(requests.filter(request => request.method === 'delete').length, 1);
  for (const request of requests.slice(1).filter(request => request.method === 'post')) {
    const report = JSON.parse(request.body!) as Record<string, unknown>;
    assert.deepEqual(Object.keys(report), [
      'schema_version',
      'engine_profile',
      'work_id',
      'execution_key_sha256',
      'outcome',
    ]);
  }
});

test('runtime source rejects a repeated work id before spending browser CPU and cancels the job', async t => {
  const repeated = rawSnapshot('awaiting_evidence');
  (repeated.issued_engine_work as Record<string, unknown>).work_id = 'work:0';
  const bodies = [rawSnapshot('awaiting_core'), repeated];
  const methods: string[] = [];
  const restoreFetch = replaceGlobal('fetch', async (_input: string | URL | Request, init?: RequestInit) => {
    methods.push(init?.method ?? 'get');
    const body = init?.method === 'delete' ? { ok: true } : bodies.shift();
    return new Response(JSON.stringify(body), {
      status: init?.method === 'post' && methods.length === 1 ? 201 : 200,
    });
  });
  t.after(restoreFetch);
  let executions = 0;
  const source = createMoveReviewRuntimeSource(async work => {
    executions++;
    return {
      kind: 'executor_failed',
      executorElapsedMs: 1,
      observedNodes: 0,
      engineTimeMs: 0,
      failureCode: 'engine_failure',
      diagnostic: work.workId,
    };
  });
  await assert.rejects(
    source.run(
      { requestId, subject, engineProfile: moveReviewEngineProfile },
      () => {},
      new AbortController().signal,
    ),
    (error: unknown) => error instanceof MoveReviewRuntimeError && error.code === 'malformed-response',
  );
  assert.equal(executions, 1);
  assert.ok(methods.includes('delete'));
});

test('explorations, mandatory repairs and later work share the original source deadline without a duplicate quota', async t => {
  for (const extra of [false, true]) {
    const bodies = [
      rawSnapshot('awaiting_core'),
      rawSnapshot('awaiting_evidence'),
      rawInvestigation(2),
      rawInvestigation(3, true),
      rawInvestigation(4, true),
      rawInvestigation(5, true),
      rawInvestigation(6, true),
      ...(extra ? [rawInvestigation(7, true)] : []),
      rawResponse({ assessment_state: 'current', assessment_scope: 'investigation_pair' }),
    ];
    const methods: string[] = [];
    const restoreFetch = replaceGlobal('fetch', async (_input: unknown, init: RequestInit) => {
      methods.push(init.method!);
      return new Response(JSON.stringify(init.method === 'delete' ? {} : bodies.shift()), {
        status: methods.length === 1 ? 201 : 200,
      });
    });
    try {
      const deadlineAtMs = performance.now() + 10_000;
      const works: IssuedMoveReviewEngineWork[] = [];
      const source = createMoveReviewRuntimeSource(async (work, _received, _signal, deadline) => {
        works.push(work);
        assert.equal(deadline, deadlineAtMs);
        return {
          kind: 'executor_failed',
          executorElapsedMs: 0,
          observedNodes: 0,
          engineTimeMs: 0,
          failureCode: 'search_budget_exhausted',
          diagnostic: 'fake',
        };
      });
      const emitted: MoveReviewSnapshot[] = [];
      await source.run(
        { requestId, subject, engineProfile: moveReviewEngineProfile, deadlineAtMs },
        s => emitted.push(s),
        new AbortController().signal,
      );
      assert.equal(emitted.at(-1)?.kind, 'completed');
      assert.deepEqual(
        works.map(w => w.purpose),
        [
          'root_search',
          'focus_comparison',
          'investigation_comparison',
          'investigation_comparison',
          'investigation_comparison',
          'investigation_comparison',
          'investigation_comparison',
          ...(extra ? ['investigation_comparison'] : []),
        ],
      );
      assert.equal(methods.filter(m => m === 'post').length, extra ? 9 : 8);
      assert.equal(methods.filter(method => method === 'delete').length, 1);
    } finally {
      restoreFetch();
    }
  }
});

test('admitted assessments survive investigation transport failure, deadline and cancellation without becoming final', async t => {
  for (const stop of ['offline', 'deadline', 'cancel', 'ended'] as const) {
    const environment = installCoordinatorEnvironment({ webLocks: false });
    let now = 1_000;
    const restoreClock = replaceGlobal('performance', { now: () => now });
    let release: () => void = () => {};
    const harness = createHost({
      run: async (request, emit) => {
        const raw = rawInvestigation(2, true);
        raw.request_id = request.requestId;
        const incoming = decodeMoveReviewSnapshot(raw, {
          requestId: request.requestId,
          subject: request.subject,
        })!;
        emit(incoming);
        await new Promise<void>(resolve => {
          release = resolve;
        });
        if (stop === 'offline') throw new MoveReviewRuntimeError('offline', true);
        if (stop === 'ended') return;
        if (stop === 'deadline') now = request.deadlineAtMs!;
        emit(completedSnapshot(request));
      },
    });
    const coordinator = new MoveReviewCoordinator('en-US', harness.host);
    try {
      coordinator.activate();
      coordinator.settle(subject, { key: 'investigation-assessment', subject, subjects: [subject] });
      environment.timers.runAll();
      await settleAsync();
      assert.equal(harness.states.at(-1)?.assessment?.kind, 'completed');
      assert.equal(harness.states.at(-1)?.kind, 'loading');
      assert.equal(harness.states.at(-1)?.recordReview?.assessed, 1);
      assert.equal(harness.states.at(-1)?.recordReview?.completed, 0);
      assert.equal(harness.states.at(-1)?.recordReview?.status, 'running');
      if (stop === 'cancel') coordinator.deactivate();
      release();
      await settleAsync();
      const last = harness.states.at(-1)!;
      assert.notEqual(last.kind, 'completed');
      assert.equal(last.assessment?.kind, 'completed');
      assert.equal(last.assessmentState, 'needs_refresh');
      assert.equal(last.recordReview?.assessed, 1);
      assert.equal(last.recordReview?.completed, 0);
      assert.equal(last.recordReview?.stopped, 1);
      assert.equal(last.recordReview?.status, 'finished');
      if (stop === 'offline' || stop === 'ended') assert.equal(last.kind, 'fault');
      if (stop === 'deadline')
        assert.ok(last.kind === 'abstained' && last.stopCondition === 'deadline_exceeded');
    } finally {
      coordinator.destroy();
      release();
      restoreClock();
      environment.restore();
    }
  }
});

test('runtime source never duplicates a job-creation POST after a transport failure', async t => {
  let calls = 0;
  const restoreFetch = replaceGlobal('fetch', async () => {
    calls++;
    throw new TypeError('network unavailable');
  });
  t.after(restoreFetch);
  const source = createMoveReviewRuntimeSource(async () => {
    throw new Error('engine must not run before job creation');
  });

  await assert.rejects(
    source.run(
      { requestId, subject, engineProfile: moveReviewEngineProfile },
      () => {},
      new AbortController().signal,
    ),
    (error: unknown) => error instanceof MoveReviewRuntimeError && error.code === 'offline',
  );
  assert.equal(calls, 1);
});

test('one source deadline includes every HTTP round trip and rejects a late completed response', async t => {
  let now = 1_000;
  t.after(replaceGlobal('performance', { now: () => now }));
  const bodies = [rawSnapshot('awaiting_core'), rawSnapshot('awaiting_evidence'), rawResponse()];
  const methods: string[] = [];
  const executionDeadlines: number[] = [];
  t.after(
    replaceGlobal('fetch', async (_input: unknown, init: RequestInit) => {
      methods.push(init.method!);
      if (init.method === 'delete') return new Response('{}', { status: 200 });
      if (methods.length === 1) assert.equal(JSON.parse(init.body as string).remaining_budget_ms, 1_000);
      now = bodies.length === 1 ? 2_000 : now + 100;
      return new Response(JSON.stringify(bodies.shift()), { status: methods.length === 1 ? 201 : 200 });
    }),
  );
  const source = createMoveReviewRuntimeSource(async (work, _received, _signal, deadlineAtMs) => {
    executionDeadlines.push(deadlineAtMs!);
    now += 200;
    // An execution cap must not rewrite the issued work or its report identity.
    assert.equal(work.searchLimits.movetimeMs, work.purpose === 'root_search' ? 5_000 : 2_500);
    return {
      kind: 'executor_failed',
      executorElapsedMs: 200,
      observedNodes: 0,
      engineTimeMs: 0,
      failureCode: 'engine_failure',
      diagnostic: 'Controlled executor result',
    };
  });
  const emitted: string[] = [];
  await assert.rejects(
    source.run(
      { requestId, subject, engineProfile: moveReviewEngineProfile, deadlineAtMs: 2_000 },
      snapshot => emitted.push(snapshot.kind),
      new AbortController().signal,
    ),
    (error: unknown) => error instanceof MoveReviewRuntimeError && error.code === 'deadline-exceeded',
  );
  assert.deepEqual(executionDeadlines, [2_000, 2_000]);
  assert.deepEqual(emitted, ['awaiting-core', 'awaiting-evidence']);
  assert.deepEqual(methods, ['post', 'post', 'post', 'delete']);
});

test('a source deadline aborts a stalled creation POST without creating a second job', async t => {
  let now = 1_000;
  const timers = new TestTimers();
  t.after(replaceGlobal('performance', { now: () => now }));
  t.after(replaceGlobal('setTimeout', timers.window.setTimeout));
  t.after(replaceGlobal('clearTimeout', timers.window.clearTimeout));
  let calls = 0;
  let receivedSignal: AbortSignal | undefined;
  t.after(
    replaceGlobal('fetch', async (_input: unknown, init: RequestInit) => {
      calls++;
      receivedSignal = init.signal!;
      return new Promise<Response>((_resolve, reject) => {
        receivedSignal!.addEventListener('abort', () => reject(new DOMException('Aborted', 'AbortError')));
      });
    }),
  );
  const source = createMoveReviewRuntimeSource(async () => {
    throw new Error('A timed-out creation must never run the engine');
  });
  const run = source.run(
    { requestId, subject, engineProfile: moveReviewEngineProfile, deadlineAtMs: 1_100 },
    () => assert.fail('No server snapshot was received'),
    new AbortController().signal,
  );
  now = 1_100;
  timers.runAll();
  await assert.rejects(
    run,
    (error: unknown) => error instanceof MoveReviewRuntimeError && error.code === 'deadline-exceeded',
  );
  assert.equal(receivedSignal?.aborted, true);
  assert.equal(calls, 1);
});

test('record scheduling preserves an active deadline through navigation and investigation, with a fresh budget on retry', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  let now = 1_000;
  const restorePerformance = replaceGlobal('performance', { now: () => now });
  const { record, subjects: selected } = threePlyRecord();
  const requests: MoveReviewSourceRequest[] = [];
  let send: (snapshot: MoveReviewSnapshot) => void = () => {};
  let finish = () => {};
  const harness = createHost({
    run: async (request, emit) => {
      requests.push(request);
      if (requests.length === 1) {
        send = emit;
        await new Promise<void>(resolve => (finish = resolve));
      }
      emit(completedSnapshot(request));
    },
  });
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => {
    coordinator.destroy();
    finish();
    restorePerformance();
    environment.restore();
  });
  coordinator.activate();
  coordinator.settle(selected[0], record);
  now = 201_000; // A queued record does not expire before its first job starts.
  environment.timers.runAll();
  await settleAsync();
  assert.equal(requests[0].deadlineAtMs, 381_000);
  for (const state of ['pending', 'complete'] as const) {
    now += 10_000;
    send({
      ...snapshotCommon(requests[0]),
      kind: 'awaiting-evidence',
      issuedEngineWork: {} as IssuedMoveReviewEngineWork,
      assessment: { kind: 'completed', evidence: completedSnapshot(requests[0]).evidence },
      verification: {
        state, targetDepth: 16, reason: 'initial_verification', triggerWorkIds: [],
        ...(state === 'complete' ? { completedDepth: 16 } : {}),
      },
    });
    assert.equal(harness.states.at(-1)!.assessment?.kind, 'completed', 'first assessment is delivered');
    assert.equal(harness.states.at(-1)!.recordReview?.completed, 0);
    coordinator.settle(selected[2], record);
    environment.timers.runAll();
    await settleAsync();
    assert.equal(requests.length, 1, 'verification and investigation finish before the next move');
    assert.equal(requests[0].deadlineAtMs, 381_000, 'navigation does not replenish an active budget');
    coordinator.settle(selected[0], record);
  }
  finish();
  await settleAsync();
  now = 261_000;
  coordinator.settle(selected[2], record);
  environment.timers.runAll();
  await settleAsync();
  assert.equal(requests.length, 2);
  assert.equal(requests[1].deadlineAtMs, 441_000);
  now = 271_000;
  coordinator.retry();
  environment.timers.runAll();
  await settleAsync();
  assert.equal(requests[2].deadlineAtMs, 451_000);
});

function threePlyRecord() {
  const data = pgnImport('1. e4 e5 2. Nf3 *');
  const root = treeReconstruct(data.treeParts!, data.sidelines);
  const record = moveReviewRecordFromTree('standard', root, data.recordedMoves, 'three-ply')!;
  assert.equal(record.subjects?.length, 3);
  return { record, subjects: record.subjects! };
}

function investigationObservations(request: MoveReviewSourceRequest): MoveReviewObservations {
  const proof = completedSnapshot(request).evidence.candidates[0].review!;
  assert.equal(proof.kind, 'single-candidate-insight');
  const requirement = {
    kind: 'compare_moves', id: 'strength', prefix_moves_uci: [],
    reference_move: 'e2e4', alternative_move: 'd2d4', state: 'deferred',
    stop_reason: 'investigation_limit', answer: null, line_id: null,
    occurrence_id: null, position_id: null, work_id: null,
    result: { state: 'partial', samples: [], proofs: [], reason: 'missing_comparison',
      needs: [{ kind: 'comparison', reason: 'missing_comparison', prefix_moves_uci: [],
        requirement_id: 'strength', horizon: null }] },
  } as CommentaryProposal['requirements'][number];
  const origin = { kind: 'generator', producer: 'inline-memory-check', source_refs: [] } as const;
  return {
    paths: [{ ...proof.proof, kind: 'played', occurrenceIds: ['played'], positionIds: [] }],
    statements: [], positions: [], highlights: [],
    selection: { version: 1, candidates: [], scenes: [], proposals: [{
      id: 'preparation', admission: 'generated', question_text: 'Does the preparation help?',
      answer_text: 'The comparison remains unverified.',
      origin: { question: { ...origin, source_refs: [] }, candidates: { ...origin, source_refs: [] },
        priority: { ...origin, source_refs: [] }, evidence: { ...origin, source_refs: [] } },
      requirements: [requirement],
    }] },
  };
}

test('memory ownership: end-only retirement preserves previous moves, unresolved scope and explicit release', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  const { record, subjects } = threePlyRecord();
  const original: MoveReviewObservations[] = [];
  const harness = createHost({ run: async (request, emit) => {
    const observations = investigationObservations(request);
    original.push(observations);
    emit({ ...snapshotCommon(request), kind: 'awaiting-evidence', observations,
      issuedEngineWork: {} as IssuedMoveReviewEngineWork });
    assert.ok(observations.selection?.proposals, 'work validation keeps its inputs while the source is active');
    emit({ ...completedSnapshot(request), observations,
      assessmentState: 'needs_refresh', verification: { state: 'stopped', reason: 'child_challenge',
        targetDepth: 17, completedDepth: 16, triggerWorkIds: ['work:3'], stopReason: 'search_budget_exhausted' } });
  } });
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => { coordinator.destroy(); environment.restore(); });
  coordinator.settle(subjects[0], record);
  coordinator.activate();
  for (let index = 0; index < 3; index++) { environment.timers.runAll(); await settleAsync(); }
  for (let index = 2; index >= 0; index--) {
    coordinator.settle(subjects[index], record);
    const state = harness.states.at(-1)!;
    const observations = moveReviewObservations(state)!;
    assert.equal(state.assessmentState, 'needs_refresh');
    assert.equal(state.verification?.reason, 'child_challenge');
    assert.equal(state.recordReview?.stopped, 3);
    assert.equal(observations, original[index], 'all aliases share the retired observation owner');
    assert.equal(observations.selection?.proposals, undefined);
    assert.equal(observations.paths[0].moves[0].fenAfter, subjects[index].after.fen);
    const outcome = observations.selection!.questionOutcomes![0];
    assert.equal(outcome.answer_text, 'The comparison remains unverified.');
    assert.deepEqual(outcome.conditions, [{ id: 'strength', state: 'deferred', stop_reason: 'investigation_limit',
      reason: 'missing_comparison' }]);
    assert.equal(outcome.unresolved[0].id, 'strength');
    assert.equal(outcome.needs[0].reason, 'missing_comparison');
    assert.equal('result' in outcome.unresolved[0], false);
    assert.ok(moveReviewFrameForSubject(observations, subjects[index], subjects[index]));
  }
  const grades = (coordinator as unknown as { record: { grades: Map<string, unknown> } }).record.grades;
  coordinator.destroy();
  assert.equal(grades.size, 0, 'destroy releases even a separately held grade map');
  assert.equal(moveReviewObservations(harness.states.at(-1)!), undefined);
});

test('memory ownership: cancellation retires investigation data and late responses cannot restore it', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  const { record, subjects } = threePlyRecord();
  let finish = () => {};
  const harness = createHost({ run: async (request, emit) => {
    const observations = investigationObservations(request);
    observations.selection!.proposals![0].requirements[0].stop_reason = null;
    emit({ ...snapshotCommon(request), kind: 'awaiting-evidence', observations,
      issuedEngineWork: {} as IssuedMoveReviewEngineWork });
    await new Promise<void>(resolve => (finish = resolve));
    emit({ ...completedSnapshot(request), observations: investigationObservations(request) });
  } });
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => { finish(); coordinator.destroy(); environment.restore(); });
  coordinator.settle(subjects[0], record);
  coordinator.activate(); environment.timers.runAll(); await settleAsync();
  coordinator.deactivate();
  const stopped = harness.states.at(-1)!;
  const observations = moveReviewObservations(stopped)!;
  assert.equal(stopped.kind, 'abstained');
  assert.equal(observations.selection?.proposals, undefined);
  assert.equal(observations.selection?.questionOutcomes?.[0].conditions[0].stop_reason, 'cancelled');
  finish(); await settleAsync();
  assert.equal(harness.states.at(-1), stopped);
});

test('record scheduling lets later moves finish after 180 seconds of total elapsed time', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  let now = 1_000;
  const restorePerformance = replaceGlobal('performance', { now: () => now });
  const { record } = threePlyRecord();
  const requests: MoveReviewSourceRequest[] = [];
  const harness = createHost({
    run: async (request, emit) => {
      requests.push(request);
      now += 75_000;
      emit(completedSnapshot(request));
    },
  });
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => {
    coordinator.destroy();
    restorePerformance();
    environment.restore();
  });
  coordinator.activate();
  coordinator.settle(undefined, record);
  for (let i = 0; i < 3; i++) {
    environment.timers.runAll();
    await settleAsync();
  }
  assert.equal(requests.length, 3);
  assert.equal(harness.states.at(-1)!.recordReview?.completed, 3);
  assert.equal(harness.states.at(-1)!.recordReview?.stopped, 0);
  assert.equal(harness.states.at(-1)!.recordReview?.status, 'finished');
});

test('record scheduling starts its safety deadline after waiting for browser authority', async t => {
  const environment = installCoordinatorEnvironment();
  let now = 1_000;
  const restorePerformance = replaceGlobal('performance', { now: () => now });
  const { record } = threePlyRecord();
  const requests: MoveReviewSourceRequest[] = [];
  const waiting = createHost({
    run: async (request, emit) => {
      requests.push(request);
      emit(completedSnapshot(request));
    },
  });
  let finish = () => {};
  const first = new MoveReviewCoordinator('en-US', createHost({
    run: () => new Promise<void>(resolve => (finish = resolve)),
  }).host);
  const second = new MoveReviewCoordinator('en-US', waiting.host);
  t.after(() => {
    first.destroy();
    second.destroy();
    finish();
    restorePerformance();
    environment.restore();
  });
  first.activate();
  first.settle(record.subject);
  await settleAsync();
  environment.timers.runAll();
  await settleAsync();
  second.activate();
  second.settle(undefined, record);
  now = 201_000;
  environment.timers.runAll();
  await settleAsync();
  assert.equal(requests.length, 0);
  first.deactivate();
  await settleAsync();
  environment.timers.runAll();
  await settleAsync();
  assert.equal(requests.length, 1);
  assert.equal(requests[0].deadlineAtMs, 381_000);
  assert.equal(waiting.states.at(-1)!.recordReview?.completed, 1);
  assert.equal(waiting.states.at(-1)!.recordReview?.status, 'running');
});

test('record retention shares the latest root observations with unsearched moves and the completed grade', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  const { record, subjects } = threePlyRecord();
  const first: MoveReviewObservations = { statements: [], paths: [] };
  const latest: MoveReviewObservations = {
    statements: [],
    paths: [{
      id: 'record-path', kind: 'played', startFen: subjects[0].before.fen,
      occurrenceIds: ['occ:1', 'occ:2', 'occ:3'],
      moves: subjects.map(s => ({ uci: s.played.uci, label: s.played.san, fenAfter: s.after.fen })),
    }],
  };
  let send: (snapshot: MoveReviewSnapshot) => void = () => {};
  let current: MoveReviewSourceRequest;
  let finish = () => {};
  const harness = createHost({
    run: async (request, emit) => {
      current = request;
      send = emit;
      emit({ ...snapshotCommon(request), kind: 'awaiting-core', observations: first,
        issuedEngineWork: {} as IssuedMoveReviewEngineWork });
      await new Promise<void>(resolve => (finish = resolve));
    },
  });
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => {
    coordinator.destroy();
    finish();
    environment.restore();
  });
  coordinator.activate();
  coordinator.settle(undefined, record);
  environment.timers.runAll();
  await settleAsync();
  send({ ...completedSnapshot(current!), observations: latest });
  coordinator.settle(subjects[1], record);
  const unsearched = harness.states.at(-1)!;
  assert.equal(unsearched.kind, 'unsearched');
  assert.equal(moveReviewObservations(unsearched), latest);
  assert.deepEqual(unsearched.observationFrame, { proofId: 'record-path', ply: 2, phase: 'after' });
  finish();
  await settleAsync();
  coordinator.settle(subjects[0], record);
  assert.equal(harness.states.at(-1)!.kind, 'completed');
  assert.equal(moveReviewObservations(harness.states.at(-1)!), latest);
});

test('record completion waits for the last move investigation after its D8 and D16 assessments', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  t.after(environment.restore);
  const { record, subjects } = threePlyRecord();
  let lastRequest: MoveReviewSourceRequest;
  let send: (snapshot: MoveReviewSnapshot) => void = () => {};
  let finish = () => {};
  let runs = 0;
  const harness = createHost({
    run: async (request, emit) => {
      runs++;
      if (runs < 3) return emit(completedSnapshot(request));
      lastRequest = request;
      send = emit;
      emit({
        ...snapshotCommon(request),
        kind: 'awaiting-evidence',
        issuedEngineWork: {} as IssuedMoveReviewEngineWork,
        assessment: { kind: 'completed', evidence: completedSnapshot(request).evidence },
        verification: {
          state: 'pending', targetDepth: 16, reason: 'initial_verification', triggerWorkIds: [],
        },
      });
      await new Promise<void>(resolve => (finish = resolve));
    },
  });
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => {
    coordinator.destroy();
    finish();
  });
  coordinator.activate();
  coordinator.settle(undefined, record);
  for (let i = 0; i < 3; i++) {
    environment.timers.runAll();
    await settleAsync();
  }
  assert.equal(runs, 3);
  assert.deepEqual(harness.states.at(-1)!.recordReview, {
    total: 3, assessed: 3, completed: 2, stopped: 0, scenes: [], status: 'running',
  });
  coordinator.settle(subjects[2], record);
  assert.equal(harness.states.at(-1)!.kind, 'loading');
  assert.equal(harness.states.at(-1)!.verification?.state, 'pending');
  send({
    ...snapshotCommon(lastRequest!),
    kind: 'awaiting-evidence',
    issuedEngineWork: {} as IssuedMoveReviewEngineWork,
    assessment: { kind: 'completed', evidence: completedSnapshot(lastRequest!).evidence },
    verification: {
      state: 'complete', targetDepth: 16, completedDepth: 16,
      reason: 'initial_verification', triggerWorkIds: [],
    },
  });
  assert.equal(harness.states.at(-1)!.recordReview?.completed, 2);
  assert.equal(harness.states.at(-1)!.recordReview?.status, 'running');
  const generator = { kind: 'generator' as const, producer: 'test', source_refs: [] };
  const observations: MoveReviewObservations = {
    statements: [], paths: [],
    selection: {
      version: 1, candidates: [], scenes: [],
      proposals: [{
        id: 'preparation', admission: 'generated',
        question_text: 'How does Nc3 compare with Nf3?',
        answer_text: 'The comparison remains unverified.',
        origin: { question: generator, candidates: generator, priority: generator, evidence: generator },
        requirements: [{
          id: 'alternative', kind: 'compare_moves', prefix_moves_uci: [],
          reference_move: 'g1f3', alternative_move: 'b1c3',
          state: 'stopped', stop_reason: 'investigation_limit',
          answer: null, line_id: null, occurrence_id: null, position_id: null, work_id: null,
        }],
      }],
    },
  };
  send({ ...completedSnapshot(lastRequest!), observations });
  assert.equal(harness.states.at(-1)!.recordReview?.status, 'running', 'the source is still active');
  finish();
  await settleAsync();
  assert.equal(harness.states.at(-1)!.recordReview?.completed, 3);
  assert.equal(harness.states.at(-1)!.recordReview?.status, 'finished');
  assert.deepEqual(moveReviewObservations(harness.states.at(-1)!), observations,
    'ending the job does not turn an unverified question into an answer');
  assert.deepEqual(harness.counts(), { suspended: 3, resumed: 3 });
});

test('record completion preserves an interrupted verification and its assessment in a terminal response', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  t.after(environment.restore);
  for (const incomplete of ['verification', 'refresh', 'stop-status'] as const) {
    const verification = {
      state: 'stopped', target_depth: 24, reason: 'child_challenge', trigger_work_ids: ['work:2'],
      completed_depth: 16, stop_reason: 'invalid_engine_work_report',
    };
    const harness = createHost({
      run: async (request, emit) => {
        const raw = rawInvestigation();
        raw.request_id = request.requestId;
        const context = { requestId: request.requestId, subject: request.subject };
        emit(decodeMoveReviewSnapshot(raw, context)!);
        const terminal = incomplete === 'stop-status'
          ? { ...rawSnapshot('stopped'), verification, stop_condition: 'invalid_engine_work_report' }
          : rawResponse({
              assessment_state: incomplete === 'refresh' ? 'needs_refresh' : 'current',
              ...(incomplete === 'verification' ? { verification } : {}),
            });
        terminal.request_id = request.requestId;
        const decoded = decodeMoveReviewSnapshot(terminal, context);
        assert.ok(decoded);
        emit(decoded);
      },
    });
    const coordinator = new MoveReviewCoordinator('en-US', harness.host);
    try {
      coordinator.activate();
      coordinator.settle(subject, { key: incomplete, subject, subjects: [subject] });
      environment.timers.runAll();
      await settleAsync();
      const last = harness.states.at(-1)!;
      assert.equal(last.assessment?.kind, 'completed');
      assert.deepEqual(last.recordReview, {
        total: 1, assessed: 1, completed: 0, stopped: 1, scenes: [], status: 'finished',
      });
      if (incomplete !== 'refresh') {
        assert.equal(last.verification?.state, 'stopped');
        assert.equal(last.verification?.targetDepth, 24);
        assert.equal(last.verification?.stopReason, 'invalid_engine_work_report');
      }
      if (incomplete === 'stop-status')
        assert.ok(last.kind === 'abstained' && last.stopCondition === 'invalid_engine_work_report');
    } finally {
      coordinator.destroy();
    }
  }
});

test('record scene navigation indexes only each cached grade actual root and reuses that grade on selection', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  t.after(environment.restore);
  const { record, subjects } = threePlyRecord();
  const observations: MoveReviewObservations = {
    statements: [],
    paths: [
      {
        id: 'played',
        kind: 'played',
        startFen: subjects[0].before.fen,
        occurrenceIds: ['occ:1', 'occ:2', 'occ:3'],
        moves: subjects.map(s => ({ uci: s.played.uci, label: s.played.san, fenAfter: s.after.fen })),
      },
    ],
  };
  const rawSelection = {
    version: 1,
    candidates: [1, 2].map(n => ({
      id: `candidate:${n}`,
      kind: 'comparison',
      state: 'partial',
      selection: 'selected',
      root_occurrence_id: `occ:${n}`,
      occurrence_ids: [`occ:${n}`],
      position_ids: [],
      line_ids: ['played'],
      evidence_ids: [`evidence:${n}`],
      source_refs: [],
      detail: {
        type: 'evaluation_comparison',
        reference_line_id: 'played',
        candidate_line_id: null,
        missing_line_ids: [`missing:${n}`],
        comparison_evidence_id: `evidence:${n}`,
        comparison_kind: 'played_vs_best',
        scope: 'observed_lines',
        material_relation: 'unavailable',
        explanation_gap: 'missing_path',
      },
    })),
    scenes: [1, 2].map(n => ({
      id: `scene:${n}`,
      root_occurrence_id: `occ:${n}`,
      occurrence_ids: [`occ:${n}`],
      line_ids: ['played'],
      candidate_ids: [`candidate:${n}`],
      state: 'partial',
    })),
  };
  observations.selection = decodeCommentarySelection(rawSelection, observations);
  assert.ok(observations.selection, 'the coordinator receives references accepted by the selection decoder');
  let runs = 0;
  const harness = createHost({
    run: async (request, emit) => {
      runs++;
      if (runs === 1) emit({ ...completedSnapshot(request), observations });
      else emit({ ...snapshotCommon(request), kind: 'abstained', stopCondition: 'search_budget_exhausted' });
    },
  });
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => coordinator.destroy());
  coordinator.activate();
  coordinator.settle(undefined, record);
  for (let i = 0; i < 4; i++) {
    environment.timers.runAll();
    await settleAsync();
  }
  const rootState = harness.states.at(-1)!;
  assert.equal(rootState.kind, 'idle');
  assert.deepEqual(rootState.recordReview, {
    total: 3,
    assessed: 1,
    completed: 1,
    stopped: 2,
    status: 'finished',
    scenes: [{ path: subjects[0].after.path, label: '1. e4', state: 'partial' }],
  });
  coordinator.settle(subjects[0], record);
  const selected = harness.states.at(-1)!;
  assert.equal(selected.kind, 'completed');
  assert.equal(moveReviewObservations(selected), observations);
  assert.deepEqual(selected.recordReview, rootState.recordReview);
  environment.timers.runAll();
  await settleAsync();
  assert.equal(runs, 3);
});

test('PGN basic coverage visits each ungraded focus once in order without changing the selected scene', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  t.after(environment.restore);
  let now = 1_000;
  t.after(replaceGlobal('performance', { now: () => now }));
  const { record, subjects } = threePlyRecord();
  const requests: MoveReviewSourceRequest[] = [];
  const harness = createHost({
    run: async (request, emit) => {
      requests.push(request);
      now += 1_000;
      if (requests.length === 2) throw new Error('Controlled position-specific failure');
      if (requests.length === 3)
        emit({ ...snapshotCommon(request), kind: 'abstained', stopCondition: 'search_budget_exhausted' });
      else emit(completedSnapshot(request));
    },
  });
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => coordinator.destroy());
  coordinator.activate();
  coordinator.settle(subjects[2], record);
  for (let i = 0; i < 5; i++) {
    environment.timers.runAll();
    await settleAsync();
  }
  assert.deepEqual(
    requests.map(request => request.subject.played.uci),
    ['e2e4', 'e7e5', 'g1f3'],
  );
  assert.deepEqual(
    requests.map(request => request.deadlineAtMs),
    [181_000, 182_000, 183_000],
  );
  assert.equal(harness.states.at(-1)!.kind, 'abstained');
  for (const state of harness.states) {
    if ('subject' in state) assert.deepEqual(state.subject, subjects[2]);
    if (state.kind === 'completed' || state.kind === 'position-action')
      assert.deepEqual(state.snapshot.subject, subjects[2]);
  }
  coordinator.settle(subjects[0], record);
  assert.equal(harness.states.at(-1)!.kind, 'completed');
  coordinator.settle(subjects[1], record);
  assert.equal(harness.states.at(-1)!.kind, 'fault');
  environment.timers.runAll();
  await settleAsync();
  assert.equal(requests.length, 3, 'cached results and failures do not silently restart the queue');
});

test('explicit retry preempts basic coverage without shortening its budget or reissuing the cancelled focus', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  t.after(environment.restore);
  t.after(replaceGlobal('performance', { now: () => 1_000 }));
  const { record, subjects } = threePlyRecord();
  const runs: {
    request: MoveReviewSourceRequest;
    emit: (s: MoveReviewSnapshot) => void;
    signal: AbortSignal;
    finish: () => void;
  }[] = [];
  const harness = createHost({
    run: async (request, emit, signal) => {
      await new Promise<void>(finish => runs.push({ request, emit, signal, finish }));
    },
  });
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => {
    coordinator.destroy();
    runs.forEach(run => run.finish());
  });
  coordinator.activate();
  coordinator.settle(subjects[2], record);
  environment.timers.runAll();
  await settleAsync();
  coordinator.retry();
  environment.timers.runAll();
  await settleAsync();
  assert.equal(runs[0].signal.aborted, true);
  runs[1].emit(completedSnapshot(runs[1].request));
  runs[1].finish();
  await settleAsync();
  environment.timers.runAll();
  await settleAsync();
  assert.deepEqual(
    runs.map(run => run.request.subject.played.uci),
    ['e2e4', 'g1f3', 'e7e5'],
  );
  assert.deepEqual(
    runs.map(run => run.request.deadlineAtMs),
    [181_000, 181_000, 181_000],
  );
  runs[0].emit(completedSnapshot(runs[0].request));
  runs[0].finish();
  runs[2].emit(completedSnapshot(runs[2].request));
  runs[2].finish();
  await settleAsync();
  environment.timers.runAll();
  await settleAsync();
  assert.equal(runs.length, 3);
  coordinator.settle(subjects[0], record);
  const cancelled = harness.states.at(-1)!;
  assert.equal(cancelled.kind, 'abstained');
  assert.equal(cancelled.kind === 'abstained' && cancelled.stopCondition, 'cancelled');
});

test('systemic failures block the PGN queue until an explicit retry, then allow fresh job budgets', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  t.after(environment.restore);
  let now = 1_000;
  t.after(replaceGlobal('performance', { now: () => now }));
  const { record, subjects } = threePlyRecord();
  for (const failure of [
    'engine_unavailable',
    'browser_unsupported',
    'offline',
    'rate-limit',
    'runtime-unavailable',
    'malformed-response',
  ] as const) {
    now = 1_000;
    const requests: MoveReviewSourceRequest[] = [];
    const harness = createHost({
      run: async (request, emit) => {
        requests.push(request);
        if (requests.length === 1) {
          if (failure === 'engine_unavailable' || failure === 'browser_unsupported')
            emit({ ...snapshotCommon(request), kind: 'abstained', stopCondition: failure });
          else throw new MoveReviewRuntimeError(failure, true);
        } else emit(completedSnapshot(request));
      },
    });
    const coordinator = new MoveReviewCoordinator('en-US', harness.host);
    try {
      coordinator.activate();
      coordinator.settle(undefined, record);
      for (let i = 0; i < 3; i++) {
        environment.timers.runAll();
        await settleAsync();
      }
      assert.equal(requests.length, 1, failure);
      assert.equal(harness.states.at(-1)!.recordReview?.status, 'blocked');
      assert.equal(harness.states.at(-1)!.recordReview?.stopped, 1);
      coordinator.settle(subjects[2], record);
      environment.timers.runAll();
      await settleAsync();
      assert.equal(harness.states.at(-1)!.kind, 'unsearched');
      assert.equal(harness.states.at(-1)!.recordReview?.status, 'blocked');
      assert.equal(requests.length, 1, 'navigation cannot clear the blocking failure');
      coordinator.settle(undefined, record);
      now = 3_000;
      coordinator.retry(); // The record root retries the stopped focus, without requiring a selected move.
      for (let i = 0; i < 4; i++) {
        environment.timers.runAll();
        await settleAsync();
      }
      assert.deepEqual(
        requests.map(request => request.subject.played.uci),
        ['e2e4', 'e2e4', 'e7e5', 'g1f3'],
      );
      assert.deepEqual(
        requests.map(request => request.deadlineAtMs),
        [181_000, 183_000, 183_000, 183_000],
      );
      assert.equal(harness.states.at(-1)!.recordReview?.status, 'finished');
      assert.equal(harness.states.at(-1)!.recordReview?.completed, 3);
    } finally {
      coordinator.destroy();
    }
  }
});

test('record scheduling preserves a timed out move and continues later moves before an explicit retry', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  let now = 1_000;
  const restorePerformance = replaceGlobal('performance', { now: () => now });
  const { record, subjects } = threePlyRecord();
  const requests: MoveReviewSourceRequest[] = [];
  const harness = createHost({
    run: async (request, emit) => {
      requests.push(request);
      if (requests.length === 1) {
        emit({
          ...snapshotCommon(request), kind: 'awaiting-evidence',
          issuedEngineWork: {} as IssuedMoveReviewEngineWork,
          assessment: { kind: 'completed', evidence: completedSnapshot(request).evidence },
          verification: {
            state: 'pending', targetDepth: 16, reason: 'initial_verification', triggerWorkIds: [],
          },
        });
        now = request.deadlineAtMs!;
      }
      emit(completedSnapshot(request));
    },
  });
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => {
    coordinator.destroy();
    restorePerformance();
    environment.restore();
  });
  coordinator.activate();
  coordinator.settle(subjects[0], record);
  environment.timers.runAll();
  await settleAsync();
  assert.equal(harness.states.at(-1)!.kind, 'abstained');
  assert.equal(harness.states.at(-1)!.verification?.state, 'stopped');
  assert.equal(harness.states.at(-1)!.verification?.stopReason, 'deadline_exceeded');
  assert.equal(harness.states.at(-1)!.assessment?.kind, 'completed');
  assert.equal(harness.states.at(-1)!.recordReview?.status, 'running');
  for (let i = 0; i < 2; i++) {
    environment.timers.runAll();
    await settleAsync();
  }
  assert.equal(requests.length, 3);
  assert.deepEqual(requests.map(request => request.deadlineAtMs), [181_000, 361_000, 361_000]);
  assert.deepEqual(harness.states.at(-1)!.recordReview, {
    total: 3, assessed: 3, completed: 2, stopped: 1, scenes: [], status: 'finished',
  });
  assert.equal(harness.states.at(-1)!.kind, 'abstained', 'a later completion does not overwrite the stopped move');
  coordinator.settle(subjects[0], record);
  now = 191_000;
  coordinator.retry();
  environment.timers.runAll();
  await settleAsync();
  assert.equal(requests[3].deadlineAtMs, 371_000);
  assert.equal(harness.states.at(-1)!.kind, 'completed');
  environment.timers.runAll();
  await settleAsync();
  assert.equal(requests.length, 4);
  assert.equal(harness.states.at(-1)!.recordReview?.completed, 3);
  assert.equal(harness.states.at(-1)!.recordReview?.stopped, 0);
  coordinator.settle(subjects[2], record);
  assert.equal(harness.states.at(-1)!.kind, 'completed');
});

test('late completion preserves earlier facts but cannot become a completed review', async t => {
  const environment = installCoordinatorEnvironment({ webLocks: false });
  t.after(environment.restore);
  let now = 1_000;
  t.after(replaceGlobal('performance', { now: () => now }));
  const raw = JSON.parse(
    readFileSync(
      new URL(
        '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/development-awaiting-produced.json',
        import.meta.url,
      ),
      'utf8',
    ),
  );
  const observations = moveReviewObservations(
    observationFixtureJob({
      name: 'development',
      fen: raw.issued_engine_work.search_fen,
      move: raw.focus.played_move_uci,
      response: raw,
    }),
  )!;
  assert.ok(observations.statements.length);
  const harness = createHost({
    run: async (request, emit) => {
      emit({
        ...snapshotCommon(request),
        kind: 'awaiting-core',
        observations,
        issuedEngineWork: {} as IssuedMoveReviewEngineWork,
      });
      now = request.deadlineAtMs!;
      emit(completedSnapshot(request));
    },
  });
  const coordinator = new MoveReviewCoordinator('en-US', harness.host);
  t.after(() => coordinator.destroy());
  coordinator.activate();
  coordinator.settle(subject);
  environment.timers.runAll();
  await settleAsync();
  const state = harness.states.at(-1)!;
  assert.equal(state.kind, 'abstained');
  assert.equal(state.kind === 'abstained' && state.stopCondition, 'deadline_exceeded');
  assert.equal(moveReviewObservations(state), observations);
  assert.equal(
    harness.states.some(state => state.kind === 'completed'),
    false,
  );
  assert.deepEqual(harness.counts(), { suspended: 1, resumed: 1 });
});
