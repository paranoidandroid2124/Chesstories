import { Protocol } from './protocol';
import { makeMoveReviewWork, moveReviewEngineProfile, type Work } from './types';
import { moveReviewEngineOutcomeAtRequiredDepth, type IssuedMoveReviewEngineWork,
  type MoveReviewEngineOutcome, type MoveReviewEngineFailure } from '../review/moveReview';
import type { Tree } from '../review/types';

export class BrowserEngine {
  private protocol = new Protocol();
  private worker?: Worker;
  private ready?: Promise<void>;
  private failure?: (reason: Error) => void;
  private destroyed = false;

  private prepare(): Promise<void> {
    if (this.ready) return this.ready;
    this.ready = new Promise((resolve, reject) => {
      this.failure = reject;
      if (this.destroyed || !crossOriginIsolated || typeof SharedArrayBuffer === 'undefined' || navigator.hardwareConcurrency < 2)
        return reject(Error('이 브라우저에서 2스레드 엔진을 시작할 수 없습니다.'));
      this.worker = new Worker('/engine-worker.js', { type: 'module' });
      this.worker.onmessage = event => {
        if (typeof event.data === 'string') this.protocol.received(event.data);
        else this.failure?.(Error(event.data?.error ?? '엔진 초기화 실패'));
      };
      this.worker.onerror = event => this.failure?.(Error(event.message || '엔진 실행 실패'));
      this.protocol.connected(command => this.worker?.postMessage(command));
      this.protocol.ready.then(resolve);
    });
    return this.ready;
  }

  execute = (work: IssuedMoveReviewEngineWork, receivedAtMs: number, signal: AbortSignal,
    deadlineAtMs = Infinity): Promise<MoveReviewEngineOutcome> => new Promise(resolve => {
    let done = false, nodes = 0, millis = 0, previous: Tree.LocalEval | undefined;
    const elapsed = () => Math.max(0, Math.floor(performance.now() - receivedAtMs));
    const finish = (outcome: MoveReviewEngineOutcome) => {
      if (done) return;
      done = true;
      clearTimeout(timer);
      signal.removeEventListener('abort', stop);
      this.failure = undefined;
      resolve(outcome);
    };
    const fail = (failureCode: MoveReviewEngineFailure, diagnostic: string) => {
      this.protocol.compute(undefined);
      finish({ kind: 'executor_failed', failureCode, diagnostic,
        executorElapsedMs: elapsed(), observedNodes: nodes, engineTimeMs: millis });
    };
    const stop = () => fail(signal.aborted ? 'cancelled' : 'lease_expired', 'The browser search was stopped.');
    const remaining = Math.min(work.maxSearchElapsedMs - elapsed(), deadlineAtMs - performance.now());
    const timer = setTimeout(stop, Math.max(0, remaining));
    signal.addEventListener('abort', stop, { once: true });
    if (signal.aborted || remaining <= 0) return stop();
    if (work.searchLimits.depth > 18 || work.searchLimits.depth < 1)
      return fail('engine_failure', 'Search depth exceeds the product limit.');
    const onError = (error: Error) => fail('engine_unavailable', error.message);
    void this.prepare().then(() => {
      if (done) return;
      this.failure = onError;
      this.protocol.compute(makeMoveReviewWork(work.engineProfile, {
        variant: work.variant, initialFen: work.enginePositionInitialFen,
        currentFen: work.searchFen, moves: work.enginePositionMovesUci,
        path: work.workId, multiPv: work.searchLimits.multiPv, searchLimits: work.searchLimits,
        ply: fenPly(work.searchFen), rootMoves: work.rootRestriction.kind === 'restricted' ? work.rootRestriction.movesUci : [],
        observe: (n, t) => { nodes = Math.max(nodes, n); millis = Math.max(millis, t); },
        emit: ev => {
          if (done) return;
          nodes = Math.max(nodes, ev.nodes); millis = Math.max(millis, ev.millis);
          if (ev.depth === work.searchLimits.depth - 1 && ev.bestmove === undefined) previous = structuredClone(ev);
          const result = moveReviewEngineOutcomeAtRequiredDepth(work, { ...ev, nodes, millis }, previous, elapsed());
          if (result) finish(result);
          else if (ev.bestmove !== undefined) fail('search_budget_exhausted', `Completed depth ${ev.depth}.`);
        },
      }));
    }, onError);
  });

  async analyse(initialFen: string, moves: string[], fen: string, emit: (ev: Tree.LocalEval) => void,
    error: (message: string) => void): Promise<void> {
    const timeout = setTimeout(() => { error('엔진 준비 시간을 초과했습니다. 다시 시작하세요.'); this.destroy(); }, 10000);
    try {
      await this.prepare();
      if (this.destroyed) return;
      this.failure = reason => error(reason.message);
      const work: Work = makeMoveReviewWork(moveReviewEngineProfile, {
        variant: 'standard', initialFen, currentFen: fen, moves, path: 'analysis',
        multiPv: 2, ply: fenPly(fen), rootMoves: [],
        searchLimits: { depth: 16, nodes: 2_000_000, movetimeMs: 2_500 }, emit,
      });
      this.protocol.compute(work);
    } catch (reason) { if (!this.destroyed) error(reason instanceof Error ? reason.message : '엔진 오류'); }
    finally { clearTimeout(timeout); }
  }

  destroy(): void {
    this.destroyed = true;
    this.failure?.(Error('Engine closed'));
    this.failure = undefined;
    this.worker?.terminate();
    this.worker = undefined;
  }
}

const fenPly = (fen: string): number => (Number(fen.split(' ')[5]) - 1) * 2 + Number(fen.split(' ')[1] === 'b');
