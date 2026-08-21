import assert from 'node:assert/strict';
import { test } from 'node:test';
import { moveReviewEngineProfile, type MoveReviewEngineProfile } from 'lib/ceval/types';
import {
  type MoveReviewJobState,
  type MoveReviewEngineOutcome,
  type MoveReviewSnapshot,
  type MoveReviewSource,
  type MoveReviewSourceRequest,
  type MoveReviewSubject,
} from '../src/moveReview';
import {
  MoveReviewCoordinator,
  type MoveReviewCoordinatorHost,
  type MoveReviewPreparation,
} from '../src/moveReviewCoordinator';
import { MoveReviewRuntimeError, createMoveReviewRuntimeSource } from '../src/moveReviewRuntimeSource';
import {
  compactReceipt,
  failedCompactReceipt,
  playedMoveBudget,
  rawCompactReceipt,
  rawMetrics,
  rawProgress,
  rawResponse,
} from './moveReviewTestSupport';

const moveReviewFallbackEngineProfile: MoveReviewEngineProfile = 'sf18-smallnet-single-t1-h16-v1';
const initialFen = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1' as FEN;
const beforeFen = 'rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1' as FEN;
const afterE5Fen = 'rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2' as FEN;
const afterC5Fen = 'rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2' as FEN;
const jobId = 'a'.repeat(32);

const subjectE5: MoveReviewSubject = {
  variant: 'standard',
  initialFen,
  movePrefixUci: ['e2e4'],
  before: { path: 'aa', fen: beforeFen },
  played: { uci: 'e7e5', san: 'e5' },
  after: { path: 'aabb', fen: afterE5Fen },
};

const subjectC5: MoveReviewSubject = {
  variant: 'standard',
  initialFen,
  movePrefixUci: ['e2e4'],
  before: { path: 'aa', fen: beforeFen },
  played: { uci: 'c7c5', san: 'c5' },
  after: { path: 'aacc', fen: afterC5Fen },
};

type CompletedSnapshot = Extract<MoveReviewSnapshot, { kind: 'completed' }>;

interface SnapshotIdentity {
  jobId?: string;
}

function snapshotCommon(request: MoveReviewSourceRequest, identity: SnapshotIdentity = {}) {
  return {
    requestId: request.requestId,
    jobId: identity.jobId ?? jobId,
    engineProfile: request.engineProfile,
    judgmentRevision: 'chesstory.position-commentary.response.v6',
    annotationPolicyRevision: 'chesstory.verdict-threshold-policy.v2',
    subject: request.subject,
  };
}

function awaitingCoreSnapshot(
  request: MoveReviewSourceRequest,
  identity: SnapshotIdentity = {},
): Extract<MoveReviewSnapshot, { kind: 'awaiting-core' }> {
  return {
    ...snapshotCommon(request, identity),
    kind: 'awaiting-core',
    issuedEngineWork: {
      engineProfile: request.engineProfile,
      workId: 'work:0',
      generation: 0,
      executionKeySha256: 'a'.repeat(64),
      variant: request.subject.variant,
      enginePositionInitialFen: request.subject.initialFen,
      enginePositionMovesUci: [...request.subject.movePrefixUci],
      searchFen: request.subject.before.fen,
      rootRestriction: { kind: 'unrestricted' },
      searchLimits: { depth: 16, nodes: 5_000_000, movetimeMs: 5_000, multiPv: 1 },
      maxSearchElapsedMs: 6_000,
    },
  };
}

function completedSnapshot(
  request: MoveReviewSourceRequest,
  identity: SnapshotIdentity = {},
): CompletedSnapshot {
  const { played } = request.subject;
  return {
    ...snapshotCommon(request, identity),
    kind: 'completed',
    evidence: {
      candidates: [
        {
          uci: played.uci,
          label: played.san,
          roles: ['best', 'played'],
          review: {
            kind: 'single-candidate-insight',
            proof: {
              id: 'line.test',
              startFen: request.subject.before.fen,
              moves: [{ uci: played.uci, label: played.uci, fenAfter: request.subject.after.fen }],
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
      assert.equal(typeof callback, 'function');
      const id = this.nextId++;
      this.callbacks.set(id, callback as () => void);
      return id;
    },
    clearTimeout: (id?: number): void => {
      if (id !== undefined) this.callbacks.delete(id);
    },
  } as unknown as Window & typeof globalThis;

  runAll(): void {
    const pending = [...this.callbacks.values()];
    this.callbacks.clear();
    for (const callback of pending) callback();
  }

  get size(): number {
    return this.callbacks.size;
  }
}

class TestBroadcastChannel {
  private static readonly channels = new Map<string, Set<TestBroadcastChannel>>();

  onmessage: ((event: MessageEvent) => void) | null = null;

  constructor(readonly name: string) {
    const peers = TestBroadcastChannel.channels.get(name) ?? new Set<TestBroadcastChannel>();
    peers.add(this);
    TestBroadcastChannel.channels.set(name, peers);
  }

  postMessage(message: unknown): void {
    for (const peer of TestBroadcastChannel.channels.get(this.name) ?? [])
      if (peer !== this) peer.onmessage?.({ data: message } as MessageEvent);
  }

  close(): void {
    const peers = TestBroadcastChannel.channels.get(this.name);
    peers?.delete(this);
    if (!peers?.size) TestBroadcastChannel.channels.delete(this.name);
  }

  static reset(): void {
    TestBroadcastChannel.channels.clear();
  }
}

function replaceGlobal(name: string, value: unknown): () => void {
  const previous = Object.getOwnPropertyDescriptor(globalThis, name);
  Object.defineProperty(globalThis, name, { configurable: true, writable: true, value });
  return () => {
    if (previous) Object.defineProperty(globalThis, name, previous);
    else delete (globalThis as Record<string, unknown>)[name];
  };
}

function installCoordinatorEnvironment(): { timers: TestTimers; restore(): void } {
  const timers = new TestTimers();
  const restoreWindow = replaceGlobal('window', timers.window);
  const restoreBroadcastChannel = replaceGlobal(
    'BroadcastChannel',
    TestBroadcastChannel as unknown as typeof BroadcastChannel,
  );
  return {
    timers,
    restore: () => {
      TestBroadcastChannel.reset();
      restoreBroadcastChannel();
      restoreWindow();
    },
  };
}

function createHost(prepare: MoveReviewCoordinatorHost['prepare']): {
  host: MoveReviewCoordinatorHost;
  states: MoveReviewJobState[];
  readonly suspendCount: number;
  readonly resumeCount: number;
} {
  const states: MoveReviewJobState[] = [];
  let suspendCount = 0;
  let resumeCount = 0;
  return {
    host: {
      prepare,
      suspendLiveEngine: () => {
        suspendCount++;
      },
      resumeLiveEngine: () => resumeCount++,
      stateChanged: state => states.push(state),
    },
    states,
    get suspendCount() {
      return suspendCount;
    },
    get resumeCount() {
      return resumeCount;
    },
  };
}

test('live-engine preemption follows the active review even when the user toggles Engine mid-review', async t => {
  const environment = installCoordinatorEnvironment();
  t.after(environment.restore);

  let engineEnabled = false;
  let engineRunning = false;
  let releaseSource: (() => void) | undefined;
  const source: MoveReviewSource = {
    run: async (_request, _emit, signal) => {
      await new Promise<void>(resolve => {
        releaseSource = resolve;
        signal.addEventListener('abort', resolve, { once: true });
      });
    },
  };
  const states: MoveReviewJobState[] = [];
  const coordinator = new MoveReviewCoordinator('en-US', {
    prepare: async () => successfulPreparation(source),
    suspendLiveEngine: () => {
      engineRunning = false;
    },
    resumeLiveEngine: () => {
      engineRunning = engineEnabled;
    },
    stateChanged: state => states.push(state),
  });
  t.after(() => {
    releaseSource?.();
    coordinator.destroy();
  });

  coordinator.activate();
  coordinator.settle(subjectE5);
  environment.timers.runAll();
  await eventually(() => coordinator.isPreemptingLiveEngine());
  assert.equal(lastState(states)?.kind, 'loading');
  assert.equal(coordinator.isPreemptingLiveEngine(), true);
  assert.equal(engineRunning, false);

  engineEnabled = true;
  if (!coordinator.isPreemptingLiveEngine()) engineRunning = true;
  assert.equal(engineRunning, false, 'turning Engine on must not start it beside review work');

  coordinator.settle(undefined);
  assert.equal(coordinator.isPreemptingLiveEngine(), false);
  assert.equal(engineRunning, true, 'the current Engine preference is restored after cancellation');
});

function successfulPreparation(
  source: MoveReviewSource,
  engineProfile: MoveReviewEngineProfile = moveReviewEngineProfile,
): MoveReviewPreparation {
  return { ok: true, engineProfile, source };
}

function completedSource(
  options: SnapshotIdentity & {
    afterFirstSnapshot?: (signal: AbortSignal) => void;
  } = {},
): MoveReviewSource {
  return {
    async run(request, emit, signal): Promise<void> {
      emit(awaitingCoreSnapshot(request, options));
      options.afterFirstSnapshot?.(signal);
      if (signal.aborted) return;
      emit(completedSnapshot(request, options));
    },
  };
}

async function eventually(predicate: () => boolean, message = 'condition was not reached'): Promise<void> {
  for (let attempt = 0; attempt < 50; attempt++) {
    if (predicate()) return;
    await new Promise<void>(resolve => setImmediate(resolve));
  }
  assert.fail(message);
}

function lastState(states: MoveReviewJobState[]): MoveReviewJobState | undefined {
  return states.at(-1);
}

test('cancels stale edge work and ignores emissions from the superseded source', async t => {
  const environment = installCoordinatorEnvironment();
  t.after(environment.restore);

  let staleEmit: ((snapshot: MoveReviewSnapshot) => void) | undefined;
  let staleSnapshot: MoveReviewSnapshot | undefined;
  let staleSignal: AbortSignal | undefined;
  let releaseStale: (() => void) | undefined;
  let firstSource!: MoveReviewSource;
  const staleEntered = new Promise<void>(resolve => {
    firstSource = {
      async run(request, emit, signal): Promise<void> {
        staleEmit = emit;
        staleSnapshot = completedSnapshot(request);
        staleSignal = signal;
        resolve();
        await new Promise<void>(done => (releaseStale = done));
      },
    };
  });
  const freshSource = completedSource();
  const tracked = createHost(async current =>
    successfulPreparation(current.played.uci === subjectE5.played.uci ? firstSource : freshSource),
  );
  const coordinator = new MoveReviewCoordinator('en-US', tracked.host);
  t.after(() => {
    releaseStale?.();
    coordinator.destroy();
  });

  coordinator.activate();
  coordinator.settle(subjectE5);
  environment.timers.runAll();
  await staleEntered;

  coordinator.settle(subjectC5);
  assert.equal(staleSignal?.aborted, true);
  environment.timers.runAll();
  await eventually(() => lastState(tracked.states)?.kind === 'completed');

  assert.ok(staleEmit && staleSnapshot);
  staleEmit(staleSnapshot);
  releaseStale?.();
  await new Promise<void>(resolve => setImmediate(resolve));

  const completed = lastState(tracked.states);
  assert.equal(completed?.kind, 'completed');
  if (completed?.kind === 'completed') assert.equal(completed.snapshot.subject.played.uci, 'c7c5');
  assert.equal(
    tracked.states.some(
      state => state.kind === 'completed' && state.snapshot.subject.played.uci === subjectE5.played.uci,
    ),
    false,
  );
  assert.equal(tracked.suspendCount, 2);
  assert.equal(tracked.resumeCount, 2);
});

test('keeps pending snapshots internal and reports a source failure without a partial result', async t => {
  const environment = installCoordinatorEnvironment();
  t.after(environment.restore);

  const source: MoveReviewSource = {
    async run(request, emit): Promise<void> {
      emit(awaitingCoreSnapshot(request));
      throw new Error('source-failed');
    },
  };
  const tracked = createHost(async () => successfulPreparation(source));
  const coordinator = new MoveReviewCoordinator('en-US', tracked.host);
  t.after(() => coordinator.destroy());

  coordinator.activate();
  coordinator.settle(subjectE5);
  environment.timers.runAll();
  await eventually(() => lastState(tracked.states)?.kind === 'fault');

  const fault = lastState(tracked.states);
  assert.equal(fault?.kind, 'fault');
  if (fault?.kind === 'fault') assert.equal(fault.retryable, true);
  assert.deepEqual(
    tracked.states.map(state => state.kind),
    ['loading', 'fault'],
  );
  assert.equal(tracked.suspendCount, 1);
  assert.equal(tracked.resumeCount, 1);
});

test('hits cache only for the exact v6 engine profile and contract revisions', async t => {
  const environment = installCoordinatorEnvironment();
  t.after(environment.restore);

  const attempts = [
    { profile: moveReviewEngineProfile },
    { profile: moveReviewEngineProfile, exactHit: true },
    { profile: moveReviewFallbackEngineProfile },
  ] as const;
  let attemptIndex = 0;
  let cacheHitSignalWasAborted = false;
  const tracked = createHost(async () => {
    const attempt = attempts[attemptIndex++];
    assert.ok(attempt);
    return successfulPreparation(
      completedSource({
        afterFirstSnapshot: signal => {
          if (attempt.exactHit) cacheHitSignalWasAborted = signal.aborted;
        },
      }),
      attempt.profile,
    );
  });
  const coordinator = new MoveReviewCoordinator('en-US', tracked.host);
  t.after(() => coordinator.destroy());
  coordinator.activate();

  const run = async (): Promise<Extract<MoveReviewJobState, { kind: 'completed' }>> => {
    const completedBefore = tracked.states.filter(state => state.kind === 'completed').length;
    coordinator.settle(subjectE5);
    environment.timers.runAll();
    await eventually(
      () => tracked.states.filter(state => state.kind === 'completed').length > completedBefore,
      'coordinator did not complete the cache attempt',
    );
    const state = lastState(tracked.states);
    assert.equal(state?.kind, 'completed');
    return state as Extract<MoveReviewJobState, { kind: 'completed' }>;
  };

  const initial = await run();
  const exact = await run();
  assert.equal(exact.snapshot, initial.snapshot);
  assert.equal(cacheHitSignalWasAborted, true);

  const differentProfile = await run();
  assert.equal(differentProfile.snapshot.engineProfile, moveReviewFallbackEngineProfile);
  assert.notEqual(differentProfile.snapshot, initial.snapshot);
  assert.equal(attemptIndex, attempts.length);
});

test('BroadcastChannel lease allows only the most recently activated visible tab to work', async t => {
  const environment = installCoordinatorEnvironment();
  const originalNow = Date.now;
  let now = 1;
  Date.now = () => now;
  t.after(() => {
    Date.now = originalNow;
    environment.restore();
  });

  let firstPrepares = 0;
  let secondPrepares = 0;
  const firstHost = createHost(async () => {
    firstPrepares++;
    return successfulPreparation(completedSource());
  });
  const secondHost = createHost(async () => {
    secondPrepares++;
    return successfulPreparation(completedSource());
  });
  const first = new MoveReviewCoordinator('en-US', firstHost.host);
  const second = new MoveReviewCoordinator('en-US', secondHost.host);
  t.after(() => {
    first.destroy();
    second.destroy();
  });

  first.activate();
  first.settle(subjectE5);
  now = 2;
  second.activate();
  second.settle(subjectE5);
  environment.timers.runAll();
  await eventually(() => lastState(secondHost.states)?.kind === 'completed');

  assert.equal(firstPrepares, 0);
  assert.equal(secondPrepares, 1);

  second.deactivate();
  environment.timers.runAll();
  await eventually(() => lastState(firstHost.states)?.kind === 'completed');
  assert.equal(firstPrepares, 1);
  assert.equal(secondPrepares, 1);
});

test('deactivation used by hidden and pagehide cancels debounce/work and restarts the current edge on activate', async t => {
  const environment = installCoordinatorEnvironment();
  t.after(environment.restore);

  let prepareCount = 0;
  let pendingSignal: AbortSignal | undefined;
  let finishPending: (() => void) | undefined;
  const tracked = createHost(async () => {
    prepareCount++;
    if (prepareCount === 1) {
      const source: MoveReviewSource = {
        async run(_request, _emit, signal): Promise<void> {
          pendingSignal = signal;
          await new Promise<void>(resolve => (finishPending = resolve));
        },
      };
      return successfulPreparation(source);
    }
    return successfulPreparation(completedSource());
  });
  const coordinator = new MoveReviewCoordinator('en-US', tracked.host);
  t.after(() => {
    finishPending?.();
    coordinator.destroy();
  });

  coordinator.activate();
  coordinator.settle(subjectE5);
  environment.timers.runAll();
  await eventually(() => pendingSignal !== undefined);
  coordinator.deactivate();
  assert.equal(pendingSignal?.aborted, true);
  assert.equal(tracked.resumeCount, 1);
  assert.equal(lastState(tracked.states)?.kind, 'loading');

  coordinator.activate();
  environment.timers.runAll();
  await eventually(() => lastState(tracked.states)?.kind === 'completed');
  assert.equal(prepareCount, 2);

  coordinator.settle(subjectC5);
  assert.equal(environment.timers.size, 1);
  coordinator.deactivate();
  assert.equal(environment.timers.size, 0);
  environment.timers.runAll();
  assert.equal(prepareCount, 2);

  coordinator.activate();
  environment.timers.runAll();
  await eventually(() => prepareCount === 3 && lastState(tracked.states)?.kind === 'completed');
});

function runtimeRequest(): MoveReviewSourceRequest {
  return {
    requestId: 'runtime-request',
    subject: subjectE5,
    engineProfile: moveReviewEngineProfile,
  };
}

function wireFocus(subject: MoveReviewSubject): Record<string, unknown> {
  return {
    kind: 'played_move',
    played_move_uci: subject.played.uci,
    resulting_fen: subject.after.fen,
  };
}

function wireIssuedWork(
  request: MoveReviewSourceRequest,
  state: 'awaiting_core' | 'awaiting_evidence',
): Record<string, unknown> {
  const comparison = state === 'awaiting_evidence';
  return {
    work_id: comparison ? 'work:1' : 'work:0',
    generation: 0,
    engine_profile: request.engineProfile,
    execution_key_sha256: (comparison ? 'b' : 'a').repeat(64),
    variant: request.subject.variant,
    engine_position_initial_fen: request.subject.initialFen,
    engine_position_moves_uci: request.subject.movePrefixUci,
    search_fen: request.subject.before.fen,
    root_restriction: comparison
      ? { kind: 'restricted', moves_uci: ['c7c5', request.subject.played.uci] }
      : { kind: 'unrestricted' },
    search_limits: {
      depth: 16,
      nodes: comparison ? 2_000_000 : 5_000_000,
      movetime_ms: comparison ? 2_500 : 5_000,
      multi_pv: 2,
    },
    admission: { minimum_completed_depth: 16 },
    max_search_elapsed_ms: comparison ? 3_500 : 6_000,
  };
}

function wireSnapshot(
  request: MoveReviewSourceRequest,
  state: 'awaiting_core' | 'awaiting_evidence',
  identity: SnapshotIdentity = {},
  receiptOverride?: readonly ReturnType<typeof compactReceipt>[],
): Record<string, unknown> {
  const receipts =
    receiptOverride ?? (state === 'awaiting_evidence' ? [compactReceipt('work:0')] : []);
  const pending = wireIssuedWork(request, state);
  return {
    schema_version: 'chesstory.position-commentary.job-status.v6',
    engine_profile: request.engineProfile,
    variant: request.subject.variant,
    request_id: request.requestId,
    job_id: identity.jobId ?? jobId,
    generation: 0,
    state: 'awaiting_engine_work',
    deadline_epoch_ms: 1_900_000_000_000,
    focus: wireFocus(request.subject),
    budget: playedMoveBudget,
    progress: rawProgress(
      state === 'awaiting_core' ? 'root_search' : 'evidence_acquisition',
      receipts,
      pending,
    ),
    decision_trace: { events: [] },
    issued_engine_work: pending,
  };
}

function wireStopped(
  request: MoveReviewSourceRequest,
  options: {
    receipts?: readonly ReturnType<typeof compactReceipt>[];
    generation?: number;
    stopCondition?: 'deadline_exceeded' | 'engine_execution_failed';
  } = {},
): Record<string, unknown> {
  const receipts = options.receipts ?? [failedCompactReceipt('work:0')];
  return {
    schema_version: 'chesstory.position-commentary.job-status.v6',
    engine_profile: request.engineProfile,
    variant: request.subject.variant,
    request_id: request.requestId,
    job_id: jobId,
    generation: options.generation ?? 0,
    state: 'stopped',
    deadline_epoch_ms: 1_900_000_000_000,
    focus: wireFocus(request.subject),
    budget: playedMoveBudget,
    progress: rawProgress('stopped', receipts),
    metrics: rawMetrics(receipts),
    work_receipts: receipts.map(rawCompactReceipt),
    decision_trace: { events: [] },
    stop_condition: options.stopCondition ?? 'engine_execution_failed',
  };
}

function wireCompleted(request: MoveReviewSourceRequest): Record<string, unknown> {
  return rawResponse({
    request_id: request.requestId,
    job_id: jobId,
    engine_profile: request.engineProfile,
  });
}

function completedOutcome(): MoveReviewEngineOutcome {
  const lines = [
    { multipvIndex: 1, moves: ['c7c5' as Uci], depth: 16, whiteScore: { kind: 'cp' as const, value: 20 } },
    { multipvIndex: 2, moves: ['e7e5' as Uci], depth: 16, whiteScore: { kind: 'cp' as const, value: 8 } },
  ];
  return {
    kind: 'completed',
    completedDepth: 16,
    selectiveDepth: 20,
    nodes: 100_000,
    engineTimeMs: 500,
    executorElapsedMs: 550,
    bestmoveUci: 'c7c5',
    lineSuffixes: lines.map(line => ({ ...line, terminalEndpoint: { kind: 'none' } })),
    previousIteration: {
      depth: 15,
      orderedLines: lines.map(line => ({ ...line, depth: 15, terminalEndpoint: { kind: 'none' } })),
    },
  };
}

function failedOutcome(): Extract<import('../src/moveReview').MoveReviewEngineOutcome, { kind: 'executor_failed' }> {
  return {
    kind: 'executor_failed',
    executorElapsedMs: 1,
    observedNodes: 0,
    engineTimeMs: 0,
    failureCode: 'fixture_failure',
    diagnostic: 'Fixture engine failure.',
  };
}

function response(status: number, value: unknown, headers: Record<string, string> = {}): Response {
  return { status, headers: new Headers(headers), json: async () => value } as Response;
}

function runtimeError(code: MoveReviewRuntimeError['code'], retryable: boolean): (error: unknown) => boolean {
  return error =>
    error instanceof MoveReviewRuntimeError && error.code === code && error.retryable === retryable;
}

test('runtime retries a network failure once with the same idempotent payload and honours cache abort', async t => {
  const request = runtimeRequest();
  const calls: Array<{ url: string; method?: string; body?: BodyInit | null; keepalive?: boolean }> = [];
  let postAttempts = 0;
  const restoreFetch = replaceGlobal('fetch', (async (
    input: RequestInfo | URL,
    init?: RequestInit,
  ): Promise<Response> => {
    calls.push({ url: String(input), method: init?.method, body: init?.body, keepalive: init?.keepalive });
    if (init?.method === 'delete') return response(204, undefined);
    postAttempts++;
    if (postAttempts === 1) throw new TypeError('network unavailable');
    return response(201, wireSnapshot(request, 'awaiting_core'));
  }) as typeof fetch);
  t.after(restoreFetch);

  let executeCount = 0;
  const source = createMoveReviewRuntimeSource(async () => {
    executeCount++;
    return failedOutcome();
  });
  const abort = new AbortController();
  const emitted: MoveReviewSnapshot[] = [];
  await source.run(
    request,
    snapshot => {
      emitted.push(snapshot);
      abort.abort();
    },
    abort.signal,
  );

  const posts = calls.filter(call => call.method === 'post');
  assert.equal(posts.length, 2);
  assert.equal(posts[0]?.body, posts[1]?.body);
  const deletes = calls.filter(call => call.method === 'delete');
  assert.equal(deletes.length, 1);
  assert.equal(deletes[0]?.keepalive, true);
  assert.deepEqual(
    emitted.map(snapshot => snapshot.kind),
    ['awaiting-core'],
  );
  assert.equal(executeCount, 0);
});

test('runtime cancels a created server job when abort wins the create-response continuation race', async t => {
  const request = runtimeRequest();
  let resolveCreate: ((value: Response) => void) | undefined;
  let createCalls = 0;
  let deleteCalls = 0;
  const createResponse = new Promise<Response>(resolve => (resolveCreate = resolve));
  const restoreFetch = replaceGlobal('fetch', (async (
    _input: RequestInfo | URL,
    init?: RequestInit,
  ): Promise<Response> => {
    if (init?.method === 'delete') {
      deleteCalls++;
      throw new TypeError('page is unloading');
    }
    createCalls++;
    return createResponse;
  }) as typeof fetch);
  t.after(restoreFetch);

  let executeCount = 0;
  let emissionCount = 0;
  const abort = new AbortController();
  const running = createMoveReviewRuntimeSource(async () => {
    executeCount++;
    return failedOutcome();
  }).run(request, () => emissionCount++, abort.signal);

  await eventually(() => createCalls === 1);
  assert.ok(resolveCreate);
  resolveCreate(response(201, wireSnapshot(request, 'awaiting_core')));
  abort.abort();
  await running;

  assert.equal(deleteCalls, 1);
  assert.equal(emissionCount, 0);
  assert.equal(executeCount, 0);
});

test('runtime caps network recovery at one retry', async t => {
  let attempts = 0;
  const restoreFetch = replaceGlobal('fetch', (async (): Promise<Response> => {
    attempts++;
    throw new TypeError('still offline');
  }) as typeof fetch);
  t.after(restoreFetch);

  await assert.rejects(
    createMoveReviewRuntimeSource(async () => failedOutcome()).run(
      runtimeRequest(),
      () => {},
      new AbortController().signal,
    ),
    runtimeError('offline', true),
  );
  assert.equal(attempts, 2);
});

test('runtime retries report-processing once with the identical body and stops after failure', async t => {
  const request = runtimeRequest();
  const reportBodies: BodyInit[] = [];
  let reportCalls = 0;
  let deleteCalls = 0;
  const restoreFetch = replaceGlobal('fetch', (async (
    input: RequestInfo | URL,
    init?: RequestInit,
  ): Promise<Response> => {
    if (init?.method === 'delete') {
      deleteCalls++;
      return response(204, {});
    }
    if (String(input).endsWith('/engine-work-reports')) {
      reportBodies.push(init?.body ?? '');
      reportCalls++;
      return reportCalls === 1
        ? response(
            503,
            {
              schema_version: 'chesstory.position-commentary.job-error.v6',
              job_id: jobId,
              error: 'engine_work_report_processing',
            },
            { 'Retry-After': '0' },
          )
        : response(200, wireStopped(request));
    }
    return response(201, wireSnapshot(request, 'awaiting_core'));
  }) as typeof fetch);
  t.after(restoreFetch);

  const emitted: MoveReviewSnapshot[] = [];
  await createMoveReviewRuntimeSource(async () => failedOutcome()).run(
    request,
    snapshot => emitted.push(snapshot),
    new AbortController().signal,
  );

  assert.equal(reportCalls, 2);
  assert.equal(reportBodies[0], reportBodies[1]);
  assert.deepEqual(emitted.map(snapshot => snapshot.kind), ['awaiting-core', 'abstained']);
  assert.equal(deleteCalls, 0);
});

test('runtime executes root then comparison work and stops after the completed response', async t => {
  const request = runtimeRequest();
  const executed: string[] = [];
  let reportCalls = 0;
  let deleteCalls = 0;
  const restoreFetch = replaceGlobal('fetch', (async (
    input: RequestInfo | URL,
    init?: RequestInit,
  ): Promise<Response> => {
    if (init?.method === 'delete') {
      deleteCalls++;
      return response(204, {});
    }
    if (String(input).endsWith('/engine-work-reports')) {
      reportCalls++;
      return response(
        200,
        reportCalls === 1 ? wireSnapshot(request, 'awaiting_evidence') : wireCompleted(request),
      );
    }
    return response(201, wireSnapshot(request, 'awaiting_core'));
  }) as typeof fetch);
  t.after(restoreFetch);

  const emitted: MoveReviewSnapshot[] = [];
  await createMoveReviewRuntimeSource(async work => {
    executed.push(work.workId);
    return completedOutcome();
  }).run(request, snapshot => emitted.push(snapshot), new AbortController().signal);

  assert.deepEqual(executed, ['work:0', 'work:1']);
  assert.equal(reportCalls, 2);
  assert.equal(deleteCalls, 0);
  assert.deepEqual(emitted.map(snapshot => snapshot.kind), [
    'awaiting-core',
    'awaiting-evidence',
    'completed',
  ]);
});

test('runtime rejects work out of order without starting an extra engine search', async t => {
  const request = runtimeRequest();
  const duplicateKeyComparison = wireSnapshot(request, 'awaiting_evidence');
  (duplicateKeyComparison.issued_engine_work as Record<string, unknown>).execution_key_sha256 =
    'a'.repeat(64);
  const advancedGenerationComparison = wireSnapshot(request, 'awaiting_evidence');
  advancedGenerationComparison.generation = 1;
  (advancedGenerationComparison.issued_engine_work as Record<string, unknown>).generation = 1;
  const cases = [
    {
      name: 'comparison first',
      create: wireSnapshot(request, 'awaiting_evidence'),
      reports: [] as Record<string, unknown>[],
      expectedExecutions: [] as string[],
      expectedDeletes: 0,
    },
    {
      name: 'repeated root',
      create: wireSnapshot(request, 'awaiting_core'),
      reports: [wireSnapshot(request, 'awaiting_core', {}, [compactReceipt('work:0')])],
      expectedExecutions: ['work:0'],
      expectedDeletes: 1,
    },
    {
      name: 'repeated comparison',
      create: wireSnapshot(request, 'awaiting_core'),
      reports: [
        wireSnapshot(request, 'awaiting_evidence'),
        wireSnapshot(request, 'awaiting_evidence', {}, [
          compactReceipt('work:0'),
          compactReceipt('work:1'),
        ]),
      ],
      expectedExecutions: ['work:0', 'work:1'],
      expectedDeletes: 1,
    },
    {
      name: 'duplicate execution key',
      create: wireSnapshot(request, 'awaiting_core'),
      reports: [duplicateKeyComparison],
      expectedExecutions: ['work:0'],
      expectedDeletes: 1,
    },
    {
      name: 'generation advanced while awaiting',
      create: wireSnapshot(request, 'awaiting_core'),
      reports: [advancedGenerationComparison],
      expectedExecutions: ['work:0'],
      expectedDeletes: 1,
    },
  ];

  for (const scenario of cases) {
    const executed: string[] = [];
    let reportIndex = 0;
    let deleteCalls = 0;
    const restoreFetch = replaceGlobal('fetch', (async (
      input: RequestInfo | URL,
      init?: RequestInit,
    ): Promise<Response> => {
      if (init?.method === 'delete') {
        deleteCalls++;
        return response(204, {});
      }
      if (String(input).endsWith('/engine-work-reports'))
        return response(200, scenario.reports[reportIndex++]);
      return response(201, scenario.create);
    }) as typeof fetch);
    try {
      await assert.rejects(
        createMoveReviewRuntimeSource(async work => {
          executed.push(work.workId);
          return completedOutcome();
        }).run(request, () => {}, new AbortController().signal),
        runtimeError('malformed-response', false),
        scenario.name,
      );
    } finally {
      restoreFetch();
    }
    assert.deepEqual(executed, scenario.expectedExecutions, scenario.name);
    assert.equal(reportIndex, scenario.reports.length, scenario.name);
    assert.equal(deleteCalls, scenario.expectedDeletes, scenario.name);
  }
});

test('runtime preserves the awaiting-response receipt time through synchronous UI work', async t => {
  const request = runtimeRequest();
  let now = 100;
  const restorePerformance = replaceGlobal('performance', { now: () => now });
  t.after(restorePerformance);
  const restoreFetch = replaceGlobal('fetch', (async (
    input: RequestInfo | URL,
    init?: RequestInit,
  ): Promise<Response> => {
    if (String(input).endsWith('/engine-work-reports')) return response(200, wireStopped(request));
    return {
      status: 201,
      headers: new Headers(),
      json: async () => {
        now = 250;
        return wireSnapshot(request, 'awaiting_core');
      },
    } as Response;
  }) as typeof fetch);
  t.after(restoreFetch);

  const receivedTimes: number[] = [];
  await createMoveReviewRuntimeSource(async (_work, receivedAtMs) => {
    receivedTimes.push(receivedAtMs);
    assert.equal(now, 400);
    return failedOutcome();
  }).run(
    request,
    snapshot => {
      if (snapshot.kind === 'awaiting-core') now = 400;
    },
    new AbortController().signal,
  );

  assert.deepEqual(receivedTimes, [100]);
});

test('runtime accepts authoritative generation invalidation before or after receipt admission', async t => {
  const request = runtimeRequest();
  for (const admitted of [false, true]) {
    let reportCalls = 0;
    let deleteCalls = 0;
    const receipts = admitted ? [compactReceipt('work:0')] : [];
    const restoreFetch = replaceGlobal('fetch', (async (
      input: RequestInfo | URL,
      init?: RequestInit,
    ): Promise<Response> => {
      if (init?.method === 'delete') {
        deleteCalls++;
        return response(204, {});
      }
      if (String(input).endsWith('/engine-work-reports')) {
        reportCalls++;
        return response(
          200,
          wireStopped(request, {
            receipts,
            generation: 1,
            stopCondition: 'deadline_exceeded',
          }),
        );
      }
      return response(201, wireSnapshot(request, 'awaiting_core'));
    }) as typeof fetch);
    try {
      const emitted: MoveReviewSnapshot[] = [];
      await createMoveReviewRuntimeSource(async () => completedOutcome()).run(
        request,
        snapshot => emitted.push(snapshot),
        new AbortController().signal,
      );
      assert.deepEqual(emitted.map(snapshot => snapshot.kind), ['awaiting-core', 'abstained']);
    } finally {
      restoreFetch();
    }
    assert.equal(reportCalls, 1);
    assert.equal(deleteCalls, 0);
  }
});

test('runtime rejects non-authoritative or skipped generation invalidation', async t => {
  const request = runtimeRequest();
  for (const invalid of [
    { generation: 1, stopCondition: 'engine_execution_failed' as const },
    { generation: 2, stopCondition: 'deadline_exceeded' as const },
  ]) {
    let deleteCalls = 0;
    const restoreFetch = replaceGlobal('fetch', (async (
      input: RequestInfo | URL,
      init?: RequestInit,
    ): Promise<Response> => {
      if (init?.method === 'delete') {
        deleteCalls++;
        return response(204, {});
      }
      if (String(input).endsWith('/engine-work-reports'))
        return response(
          200,
          wireStopped(request, {
            receipts: [],
            generation: invalid.generation,
            stopCondition: invalid.stopCondition,
          }),
        );
      return response(201, wireSnapshot(request, 'awaiting_core'));
    }) as typeof fetch);
    try {
      await assert.rejects(
        createMoveReviewRuntimeSource(async () => completedOutcome()).run(
          request,
          () => {},
          new AbortController().signal,
        ),
        runtimeError('malformed-response', false),
      );
    } finally {
      restoreFetch();
    }
    assert.equal(deleteCalls, 1);
  }
});

test('runtime publishes a terminal position action without executing or cancelling work', async t => {
  const request = runtimeRequest();
  let executeCount = 0;
  let deleteCount = 0;
  const terminal = rawResponse({
    request_id: request.requestId,
    job_id: jobId,
    engine_profile: request.engineProfile,
    progress: rawProgress('completed', []),
    metrics: rawMetrics([]),
    work_receipts: [],
    result: { kind: 'automatic_terminal', terminal: { kind: 'stalemate' } },
  });
  const restoreFetch = replaceGlobal('fetch', (async (
    _input: RequestInfo | URL,
    init?: RequestInit,
  ): Promise<Response> => {
    if (init?.method === 'delete') {
      deleteCount++;
      return response(204, {});
    }
    return response(201, terminal);
  }) as typeof fetch);
  t.after(restoreFetch);

  const emitted: MoveReviewSnapshot[] = [];
  await createMoveReviewRuntimeSource(async () => {
    executeCount++;
    return completedOutcome();
  }).run(request, snapshot => emitted.push(snapshot), new AbortController().signal);

  assert.deepEqual(emitted.map(snapshot => snapshot.kind), ['position-action']);
  assert.equal(executeCount, 0);
  assert.equal(deleteCount, 0);
});

test('runtime does not retry an unrelated 503 response', async t => {
  let attempts = 0;
  const restoreFetch = replaceGlobal('fetch', (async (): Promise<Response> => {
    attempts++;
    return response(
      503,
      {
        schema_version: 'chesstory.position-commentary.job-error.v6',
        error: 'runtime_busy',
      },
      { 'Retry-After': '0' },
    );
  }) as typeof fetch);
  t.after(restoreFetch);

  await assert.rejects(
    createMoveReviewRuntimeSource(async () => failedOutcome()).run(
      runtimeRequest(),
      () => {},
      new AbortController().signal,
    ),
    runtimeError('runtime-unavailable', true),
  );
  assert.equal(attempts, 1);
});

test('runtime maps 429 without retrying it', async t => {
  let attempts = 0;
  const restoreFetch = replaceGlobal('fetch', (async (): Promise<Response> => {
    attempts++;
    return response(429, {});
  }) as typeof fetch);
  t.after(restoreFetch);

  await assert.rejects(
    createMoveReviewRuntimeSource(async () => failedOutcome()).run(
      runtimeRequest(),
      () => {},
      new AbortController().signal,
    ),
    runtimeError('rate-limit', true),
  );
  assert.equal(attempts, 1);
});

test('runtime rejects an exact-decoder malformed response without retrying it', async t => {
  let attempts = 0;
  const restoreFetch = replaceGlobal('fetch', (async (): Promise<Response> => {
    attempts++;
    return response(200, { unexpected: true });
  }) as typeof fetch);
  t.after(restoreFetch);

  await assert.rejects(
    createMoveReviewRuntimeSource(async () => failedOutcome()).run(
      runtimeRequest(),
      () => {},
      new AbortController().signal,
    ),
    runtimeError('malformed-response', false),
  );
  assert.equal(attempts, 1);
});

test('runtime rejects job drift across reports and cancels the server job', async () => {
  const request = runtimeRequest();
  let executeCount = 0;
  let reportCalls = 0;
  let deleteCalls = 0;
  const restoreFetch = replaceGlobal('fetch', (async (
    input: RequestInfo | URL,
    init?: RequestInit,
  ): Promise<Response> => {
    if (init?.method === 'delete') {
      deleteCalls++;
      return response(204, {});
    }
    if (String(input).endsWith('/engine-work-reports')) {
      reportCalls++;
      return response(200, wireSnapshot(request, 'awaiting_evidence', { jobId: 'b'.repeat(32) }));
    }
    return response(201, wireSnapshot(request, 'awaiting_core'));
  }) as typeof fetch);

  try {
    await assert.rejects(
      createMoveReviewRuntimeSource(async () => {
        executeCount++;
        return failedOutcome();
      }).run(request, () => {}, new AbortController().signal),
      runtimeError('malformed-response', false),
    );
  } finally {
    restoreFetch();
  }

  assert.equal(executeCount, 1);
  assert.equal(reportCalls, 1);
  assert.equal(deleteCalls, 1);
});

test('coordinator reports an honest fault and cancels a malformed runtime job', async t => {
  const environment = installCoordinatorEnvironment();
  t.after(environment.restore);

  let capturedRequest: MoveReviewSourceRequest | undefined;
  let reportCalls = 0;
  let deleteCalls = 0;
  const runtime = createMoveReviewRuntimeSource(async () => completedOutcome());
  const source: MoveReviewSource = {
    run: (request, emit, signal) => {
      capturedRequest = request;
      return runtime.run(request, emit, signal);
    },
  };
  const restoreFetch = replaceGlobal('fetch', (async (
    input: RequestInfo | URL,
    init?: RequestInit,
  ): Promise<Response> => {
    assert.ok(capturedRequest);
    if (init?.method === 'delete') {
      deleteCalls++;
      return response(204, {});
    }
    if (String(input).endsWith('/engine-work-reports')) {
      reportCalls++;
      return response(
        200,
        reportCalls === 1 ? wireSnapshot(capturedRequest, 'awaiting_evidence') : { malformed: true },
      );
    }
    return response(201, wireSnapshot(capturedRequest, 'awaiting_core'));
  }) as typeof fetch);
  t.after(restoreFetch);

  const tracked = createHost(async () => successfulPreparation(source));
  const coordinator = new MoveReviewCoordinator('en-US', tracked.host);
  t.after(() => coordinator.destroy());
  coordinator.activate();
  coordinator.settle(subjectE5);
  environment.timers.runAll();
  await eventually(() => lastState(tracked.states)?.kind === 'fault');

  const fault = lastState(tracked.states);
  assert.equal(fault?.kind, 'fault');
  if (fault?.kind === 'fault') assert.equal(fault.retryable, false);
  assert.deepEqual(
    tracked.states.map(state => state.kind),
    ['loading', 'fault'],
  );
  assert.equal(reportCalls, 2);
  assert.equal(deleteCalls, 1, 'an incomplete server job must be cancelled after failure');
});
