import AnalyseCtrl from '../src/ctrl';
import { CevalCtrl, moveReviewEngineProfile } from 'lib/ceval';
import { makeTree } from 'lib/tree';
import pgnImport from '../src/pgnImport';
import { treeReconstruct } from '../src/util';
import { MoveReviewCoordinator } from '../src/moveReviewCoordinator';
import { createMoveReviewRuntimeSource } from '../src/moveReviewRuntimeSource';
import { moveReviewCopy, moveReviewObservations, type MoveReviewObservations } from '../src/moveReview';
import { renderMoveReview } from '../src/view/moveReview';
import { patch } from '../src/view/util';
import { h, type VNode } from 'snabbdom';

// Bounded HTTP integration entrypoint. The page shell stands in for unavailable
// Lila hosting; controller dispatch, engine consumer, source, coordinator and view are real.
const el = <T extends HTMLElement>(id: string) => document.getElementById(id)! as T;
const check = (condition: unknown, message: string): void => {
  if (!condition) throw Error(message);
};
let owner: any;
let coordinator: MoveReviewCoordinator | undefined;
let vnode: VNode | HTMLElement = el('panel');
let report: any;
let initialJobCaptured = false;
let initialObservationFinished = false;
let initialRawHighlights: unknown;
let reportSealed = true;
let runAbort = new AbortController();
let activeEngineAccounting: any;
const outstandingExecutors = new Set<Promise<unknown>>();
const outstandingObservations = new Set<Promise<void>>();
const nativeFetch = globalThis.fetch.bind(globalThis);
const cancelButton = document.createElement('button');
cancelButton.textContent = 'Cancel measurement';
cancelButton.disabled = true;
el('run').after(cancelButton);
function stopRun(reason: 'budget_exhausted' | 'cancelled', trigger: string) {
  if (reportSealed || report.stop) return;
  report.stop = { reason, trigger, requestedAtMs: performance.now() - report.startedPerformanceMs };
  runAbort.abort();
  coordinator?.deactivate();
}
cancelButton.onclick = () => stopRun('cancelled', 'measurement_button');
window.addEventListener('pagehide', () => stopRun('cancelled', 'pagehide'));
const frame = () =>
  new Promise<void>((resolve, reject) => {
    const at = performance.now();
    let raf = 0;
    const abort = () => finish(Error('Measurement stopped while awaiting a frame'));
    const timer = setTimeout(() => finish(Error('No animation frame within 45 seconds')), 45000);
    const finish = (error?: Error) => {
      clearTimeout(timer);
      cancelAnimationFrame(raf);
      runAbort.signal.removeEventListener('abort', abort);
      if (error) {
        report.frameWaits.interrupted++;
        reject(error);
      } else resolve();
    };
    report.frameWaits.requested++;
    if (document.visibilityState === 'hidden') report.frameWaits.requestedWhileHidden++;
    if (runAbort.signal.aborted) return abort();
    runAbort.signal.addEventListener('abort', abort, { once: true });
    raf = requestAnimationFrame(() => {
      const elapsed = performance.now() - at;
      report.frameWaits.completed++;
      report.frameWaits.totalMs += elapsed;
      report.frameWaits.maxMs = Math.max(report.frameWaits.maxMs, elapsed);
      finish();
    });
  });
// Compare ordered fields, independent of JSON object key order and decoder filtering.
const highlightContent = (highlights: NonNullable<MoveReviewObservations['highlights']>) =>
  JSON.stringify(
    highlights.map(n => [
      n.occurrenceId,
      n.text,
      n.squares,
      n.premiseIds,
      n.frames?.map(f => [f.phase, f.positionId, f.squares, f.sourceIds]),
    ]),
  );
const sha256 = async (content: string) =>
  Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256', new TextEncoder().encode(content))), byte =>
    byte.toString(16).padStart(2, '0'),
  ).join('');
globalThis.fetch = (input, init) => {
  const at = performance.now();
  const request = nativeFetch(input, init);
  if (!reportSealed && report && String(input).startsWith('/api/chess-judgment/')) {
    const row: any = { method: init?.method ?? 'get', path: String(input), startedMs: at - report.startedPerformanceMs };
    report.http.push(row);
    const initial =
      !initialJobCaptured &&
      init?.method?.toLowerCase() === 'post' &&
      String(input) === '/api/chess-judgment/position-commentary-jobs';
    if (initial) initialJobCaptured = true;
    const observation = request.then(async response => {
      if (reportSealed) return;
      row.status = response.status;
      row.headersMs = performance.now() - at;
      const cloneStarted = performance.now();
      const raw = await response.clone().json();
      if (reportSealed) return;
      row.cloneBodyAndParseMs = performance.now() - cloneStarted;
      const extractionStarted = performance.now();
      if (initial) initialRawHighlights = raw.observations ? (raw.observations.highlights ?? []) : undefined;
      Object.assign(row, {
        durationMs: performance.now() - at,
        state: raw.state,
        stopCondition: raw.stop_condition,
        focus: raw.focus,
        workId: raw.issued_engine_work?.work_id,
      });
      row.metadataExtractionMs = performance.now() - extractionStarted;
    }).catch(error => {
      if (!reportSealed) row.observationError = String(error);
    }).finally(() => {
      if (!reportSealed) {
        row.observationSettledMs = performance.now() - report.startedPerformanceMs;
        if (initial) initialObservationFinished = true;
      }
      outstandingObservations.delete(observation);
    });
    outstandingObservations.add(observation);
  }
  // Observing a clone must not postpone the production source's receipt/lease timestamp.
  return request;
};
function redraw() {
  if (!owner) return;
  vnode = patch(vnode, h('main#panel', [renderMoveReview(owner.moveReviewPanelProps())]));
  el('selection').textContent = `${owner.node.ply}: ${owner.node.san ?? 'Initial position'}`;
}
async function until(predicate: () => boolean, message: string) {
  const deadline = performance.now() + 45000;
  while (!predicate()) {
    if (runAbort.signal.aborted) throw Error('Measurement stopped');
    if (performance.now() > deadline) throw Error(message);
    await new Promise<void>(resolve => setTimeout(resolve, 25));
  }
  if (runAbort.signal.aborted) throw Error('Measurement stopped');
}
el('run').onclick = async () => {
  el<HTMLButtonElement>('run').disabled = true;
  const started = performance.now();
  initialJobCaptured = false;
  initialObservationFinished = false;
  initialRawHighlights = undefined;
  reportSealed = false;
  runAbort = new AbortController();
  cancelButton.disabled = false;
  report = {
    id: `navigation-http-${Date.now()}`,
    scope:
      'Actual runtime HTTP via local proxy; real AnalyseCtrl methods, CevalCtrl, source, coordinator and renderer in a minimal page shell. Not full Lila.',
    startedAt: new Date().toISOString(),
    startedPerformanceMs: started,
    accountingRevision: 1,
    policy: 'Baseline accounting preparation only: unchanged 109-ply navigation, automatic first grade and explicit ply-41 grade. No new quiet-sample or whole-game grading policy.',
    observationScope: 'Clone body/parse wall time includes waiting and overlaps production work; metadata extraction is synchronous elapsed time. Neither is a CPU measurement or a value to subtract from total elapsed time.',
    stop: null,
    http: [],
    engine: [],
    frames: [],
    frameWaits: { requested: 0, completed: 0, interrupted: 0, requestedWhileHidden: 0, totalMs: 0, maxMs: 0 },
    environment: {
      userAgent: navigator.userAgent,
      hardwareConcurrency: navigator.hardwareConcurrency,
      deviceMemory: (navigator as any).deviceMemory,
      crossOriginIsolated,
      visibilityState: document.visibilityState,
    },
    engineProfile: moveReviewEngineProfile,
    targetMs: 120000,
    ceilingMs: 180000,
  };
  const timeout = setTimeout(() => stopRun('budget_exhausted', '180_second_measurement_deadline'), 180000);
  try {
    const games = await (await fetch('/pc-games.json', { signal: runAbort.signal })).json();
    const game = games.find((g: any) => g.id === 'ai-game-26');
    const data = pgnImport(game.san.join(' ') + ' *');
    check(data.game!.initialFen === game.initialFen, 'Actual PGN root differs');
    const tree = makeTree(treeReconstruct(data.treeParts!, data.sidelines));
    const paths: string[] = [];
    let node = tree.root,
      path = '';
    while (node.children[0]) {
      node = node.children[0]!;
      path += node.id;
      paths.push(path);
    }
    report.game = { id: game.id, plies: paths.length, initialFen: game.initialFen };
    owner = Object.assign(Object.create(AnalyseCtrl.prototype), {
      data,
      tree,
      opts: { study: { id: 'integration', chapterId: 'record' } },
      path: '',
      node: tree.root,
      nodeList: [tree.root],
      moveReviewJob: { kind: 'idle', reason: 'root' },
      moveReviewView: { evidenceExpanded: false },
      moveReviewAnnotations: new Map(),
      moveReviewCopyValue: moveReviewCopy('en-US'),
      moveReviewLocale: 'en-US',
      cevalEnabled: () => false,
      startCeval() {},
      redraw,
      getOrientation: () => 'white',
      canWriteStudy: () => false,
      setPath(path: string) {
        this.path = path;
        this.nodeList = tree.getNodeList(path);
        this.node = this.nodeList.at(-1)!;
      },
      treeView: { requestAutoScroll() {} },
      threatMode() {},
      explorer: { setNode() {} },
      syncHref() {},
      promotion: { cancel() {} },
      showGround() {},
      pluginUpdate() {},
      selectMoveReviewCandidate(uci: string) {
        owner.moveReviewView.selectedCandidateUci = uci;
        redraw();
      },
      toggleMoveReviewEvidence() {
        owner.moveReviewView.evidenceExpanded = !owner.moveReviewView.evidenceExpanded;
        redraw();
      },
      toggleMoveReviewProof(id: string) {
        owner.moveReviewView.expandedProofId = owner.moveReviewView.expandedProofId === id ? undefined : id;
        redraw();
      },
      previewMoveReviewFrame(frame: unknown) {
        owner.moveReviewView.hoveredFrame = frame;
        redraw();
      },
      clearMoveReviewPreview() {
        owner.moveReviewView.hoveredFrame = undefined;
        redraw();
      },
      pinMoveReviewFrame(frame: unknown) {
        owner.moveReviewView.pinnedFrame = frame;
        owner.moveReviewView.hoveredFrame = undefined;
        redraw();
      },
      clearMoveReviewPin() {
        owner.moveReviewView.pinnedFrame = undefined;
        redraw();
      },
      retryMoveReview() {
        coordinator!.retry();
      },
      addMoveReviewProof() {},
      viewAddedMoveReviewProof() {},
    });
    owner.ceval = new CevalCtrl({
      variant: data.game!.variant,
      initialFen: game.initialFen,
      emit() {},
      onUciHover() {},
      redraw,
    });
    const startMoveReview = owner.ceval.startMoveReview.bind(owner.ceval) as CevalCtrl['startMoveReview'];
    owner.ceval.startMoveReview = (...[profile, input]: Parameters<CevalCtrl['startMoveReview']>) => {
      const accounting = activeEngineAccounting;
      return startMoveReview(profile, {
        ...input,
        observe: (nodes, engineTimeMs) => {
          if (accounting && !reportSealed && accounting.state === 'pending') {
            accounting.observeCalls++;
            accounting.maxObservedNodes = Math.max(accounting.maxObservedNodes, nodes);
            accounting.maxObservedEngineTimeMs = Math.max(accounting.maxObservedEngineTimeMs, engineTimeMs);
            accounting.lastObservationAtMs = performance.now() - started;
          }
          input.observe?.(nodes, engineTimeMs);
        },
      });
    };
    coordinator = new MoveReviewCoordinator('en-US', {
      engineProfile: moveReviewEngineProfile,
      source: createMoveReviewRuntimeSource((work, received, signal) => {
        const dispatched = performance.now();
        const accounting: any = {
          workId: work.workId,
          executionKey: work.executionKeySha256,
          purpose: work.purpose,
          history: work.enginePositionMovesUci,
          limits: work.searchLimits,
          rootRestriction: work.rootRestriction,
          maxSearchElapsedMs: work.maxSearchElapsedMs,
          receivedAtMs: received - started,
          dispatchedAtMs: dispatched - started,
          state: 'pending',
          observeCalls: 0,
          maxObservedNodes: 0,
          maxObservedEngineTimeMs: 0,
        };
        report.engine.push(accounting);
        activeEngineAccounting = accounting;
        const aborted = () => {
          if (!reportSealed) {
            accounting.abortRequestedAtMs = performance.now() - started;
            accounting.abortContext = report.stop?.reason ?? 'coordinator_or_cleanup';
          }
        };
        signal.addEventListener('abort', aborted, { once: true });
        if (signal.aborted) aborted();
        const execution = (async () => {
          try {
            const outcome = await owner.executeMoveReviewEngineWork(work, received, signal);
            if (!reportSealed) Object.assign(accounting, {
              state: 'settled',
              resolvedAtMs: performance.now() - started,
              receivedToResolveMs: performance.now() - received,
              dispatchToResolveMs: performance.now() - dispatched,
              kind: outcome.kind,
              failureCode: outcome.failureCode,
              depth: outcome.completedDepth,
              nodes: outcome.nodes ?? outcome.observedNodes,
              engineTimeMs: outcome.engineTimeMs,
              elapsedMs: outcome.executorElapsedMs,
              diagnostic: outcome.diagnostic,
              usageIncomplete: outcome.diagnostic?.includes('incomplete') ?? false,
            });
            return outcome;
          } catch (error) {
            if (!reportSealed) Object.assign(accounting, {
              state: 'rejected', resolvedAtMs: performance.now() - started,
              receivedToResolveMs: performance.now() - received,
              dispatchToResolveMs: performance.now() - dispatched, error: String(error),
            });
            throw error;
          } finally {
            signal.removeEventListener('abort', aborted);
            if (activeEngineAccounting === accounting) activeEngineAccounting = undefined;
          }
        })();
        outstandingExecutors.add(execution);
        void execution.then(
          () => outstandingExecutors.delete(execution),
          () => outstandingExecutors.delete(execution),
        );
        return execution;
      }),
      suspendLiveEngine: redraw,
      resumeLiveEngine: owner.resumeLiveEngineAfterMoveReview.bind(owner),
      stateChanged(state) {
        owner.setMoveReviewState(state);
        if (moveReviewObservations(state)) report.firstObservationMs ??= performance.now() - started;
      },
    });
    owner.moveReviewCoordinator = coordinator;
    coordinator.activate();
    owner.scheduleMoveReviewForCurrentEdge(); // Root entry, no invented prefix.
    await until(
      () => !!moveReviewObservations(owner.moveReviewJob),
      'No HTTP observations within 45 seconds',
    );
    const original = moveReviewObservations(owner.moveReviewJob)!;
    await until(() => initialObservationFinished, 'Initial HTTP clone observation did not close');
    if (!initialJobCaptured || !Array.isArray(initialRawHighlights))
      throw Error('Initial job HTTP highlights were not captured as an array');
    // Our server's produced highlights must all survive. Do not reuse the decoder's
    // malformed-input filter here: that would let wholesale loss pass again.
    const rawContent = highlightContent(
      initialRawHighlights.map(n => ({
        occurrenceId: n.occurrence_id,
        text: n.text,
        squares: n.squares,
        premiseIds: n.premise_ids,
        frames: n.frames?.map((f: any) => ({
          phase: f.phase,
          positionId: f.position_id,
          squares: f.squares,
          sourceIds: f.source_ids,
        })),
      })),
    );
    const decodedContent = highlightContent(original.highlights ?? []);
    report.highlightAudit = {
      rawCount: initialRawHighlights.length,
      decodedCount: original.highlights?.length ?? 0,
      rawSha256: await sha256(rawContent),
      decodedSha256: await sha256(decodedContent),
      matched: rawContent === decodedContent,
    };
    check(report.highlightAudit.matched, 'Initial HTTP highlights differ from decoded ordered fields');
    const supplied = original.paths.find(p => p.moves.length === paths.length)!;
    check(supplied && supplied.occurrenceIds.every(Boolean), 'Incomplete supplied record');
    report.positions = original.positions?.length;
    report.occurrences = new Set(supplied.occurrenceIds).size;
    report.highlights = original.highlights?.length;
    for (const [i, path] of paths.entries()) {
      const before = performance.now();
      owner.jump(path);
      redraw();
      await frame();
      const job = owner.moveReviewJob;
      const obs = moveReviewObservations(job)!;
      const selected = job.observationFrame;
      check(selected, `Missing frame at ply ${i + 1}`);
      const observedPath = obs.paths.find(p => p.id === selected.proofId)!;
      check(observedPath.moves[selected.ply - 1].fenAfter === owner.node.fen, `Wrong board at ${i + 1}`);
      const occurrence = supplied.occurrenceIds[i];
      check(observedPath.occurrenceIds[selected.ply - 1] === occurrence, `Wrong occurrence at ${i + 1}`);
      const expected = original.highlights?.filter(n => n.occurrenceId === occurrence).map(n => n.text) ?? [];
      const rendered = Array.from(
        document.querySelectorAll('.move-review__work .move-review__observation > p'),
      ).map(n => n.textContent);
      check(JSON.stringify(rendered) === JSON.stringify(expected), `Wrong rendered notes at ${i + 1}`);
      if (i > 0) check(job.kind === 'unsearched', `Grade from another move at ${i + 1}`);
      report.frames.push({
        ply: i + 1,
        occurrence,
        notes: rendered.length,
        kind: job.kind,
        elapsedMs: performance.now() - before,
      });
    }
    report.ordinaryNavigationMs = performance.now() - started;
    report.createsAfterNavigation = report.http.filter(
      (r: any) => r.method === 'post' && !r.path.includes('engine-work-reports'),
    ).length;
    check(report.createsAfterNavigation === 1, 'Navigation recreated the full record');
    await until(() => !coordinator!.isPreemptingLiveEngine(), 'Initial grading did not close');
    report.initialLifecycleMs = performance.now() - started;
    check(owner.moveReviewJob.kind === 'unsearched', 'Late initial grade changed ply 109');
    owner.jump(paths[40]);
    redraw();
    const button = document.querySelector<HTMLButtonElement>('.move-review__request-grade');
    check(button, 'Explicit grade action missing');
    button!.click();
    await until(
      () =>
        report.http.filter((r: any) => r.method === 'post' && !r.path.includes('engine-work-reports'))
          .length === 2,
      'Explicit grade did not create a job',
    );
    await until(() => !coordinator!.isPreemptingLiveEngine(), 'Explicit grading did not close');
    report.explicitGradeState = owner.moveReviewJob.kind;
    report.explicitGradeStop = owner.moveReviewJob.stopCondition;
    owner.jump(paths[41]);
    redraw();
    check(owner.moveReviewJob.kind === 'unsearched', 'Explicit grade leaked onto another edge');
    check(
      moveReviewObservations(owner.moveReviewJob) === original,
      'Explicit packet replaced original record',
    );
    report.status = 'passed';
  } catch (error) {
    report.status = report.stop?.reason ?? 'failed';
    report.error = String(error);
  } finally {
    clearTimeout(timeout);
    cancelButton.disabled = true;
    report.executionFinishedMs = performance.now() - started;
    coordinator?.destroy();
    const drainStarted = performance.now();
    const drainLimitMs = 5000;
    while (outstandingExecutors.size || outstandingObservations.size) {
      const remaining = drainLimitMs - (performance.now() - drainStarted);
      if (remaining <= 0) break;
      let timer: ReturnType<typeof setTimeout> | undefined;
      await Promise.race([
        Promise.allSettled([...outstandingExecutors, ...outstandingObservations]),
        new Promise<void>(resolve => { timer = setTimeout(resolve, remaining); }),
      ]);
      clearTimeout(timer);
    }
    report.drain = {
      limitMs: drainLimitMs,
      elapsedMs: performance.now() - drainStarted,
      outstandingExecutors: outstandingExecutors.size,
      outstandingObservations: outstandingObservations.size,
      observationErrors: report.http.filter((row: any) => row.observationError).length,
      complete: outstandingExecutors.size === 0 && outstandingObservations.size === 0,
    };
    for (const entry of report.engine) if (entry.state === 'pending') entry.usageIncomplete = true;
    if ((!report.drain.complete || report.drain.observationErrors) && report.status === 'passed')
      report.status = 'accounting_incomplete';
    // Any callbacks after the bounded drain may still deliver to the product, but cannot alter the saved record.
    reportSealed = true;
    owner?.ceval.stopMoveReview();
    report.environment.finalVisibilityState = document.visibilityState;
    report.totalMs = performance.now() - started;
    el('result').textContent = JSON.stringify(report, null, 2);
    await nativeFetch('/pc-navigation-result', { method: 'POST', body: JSON.stringify(report) });
    el('status').textContent =
      `${report.status}: ${report.frames.length} frames, ${report.createsAfterNavigation ?? '?'} initial job, ${(report.totalMs / 1000).toFixed(2)} seconds`;
  }
};
