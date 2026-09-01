import assert from 'node:assert/strict';
import { test } from 'node:test';
import { moveReviewEngineProfile } from 'lib/ceval/types';
import {
  type MoveReviewEngineOutcome,
  type MoveReviewJobState,
  type MoveReviewSnapshot,
  type MoveReviewSource,
  type MoveReviewSourceRequest,
} from '../src/moveReview';
import { MoveReviewCoordinator, type MoveReviewCoordinatorHost } from '../src/moveReviewCoordinator';
import { MoveReviewRuntimeError, createMoveReviewRuntimeSource } from '../src/moveReviewRuntimeSource';
import { rawResponse, rawSnapshot, requestId, subject } from './moveReviewTestSupport';

type CompletedSnapshot = Extract<MoveReviewSnapshot, { kind: 'completed' }>;

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
              annotations: [],
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
    prepare: async () => ({ ok: true, engineProfile: moveReviewEngineProfile, source }),
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

test('Web Lock serializes a late-joining coordinator and hands off after deactivation', async t => {
  const environment = installCoordinatorEnvironment();
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
  const coordinators = [first];
  t.after(() => coordinators.forEach(coordinator => coordinator.destroy()));

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

  first.deactivate();
  await settleAsync();
  environment.timers.runAll();
  await settleAsync();
  assert.equal(secondRuns, 1);
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

test('runtime source executes root and focus searches only through the browser executor', async t => {
  const bodies = [rawSnapshot('awaiting_core'), rawSnapshot('awaiting_evidence'), rawResponse()];
  const requests: Array<{ url: string; method: string; body?: string }> = [];
  const restoreFetch = replaceGlobal('fetch', async (input: string | URL | Request, init?: RequestInit) => {
    requests.push({
      url: String(input),
      method: init?.method ?? 'get',
      body: init?.body as string | undefined,
    });
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
  for (const request of requests.slice(1)) {
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
      failureCode: 'fixture',
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
