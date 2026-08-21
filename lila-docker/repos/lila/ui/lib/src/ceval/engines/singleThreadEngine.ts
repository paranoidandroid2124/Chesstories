import { Protocol } from '../protocol';
import {
  CevalState,
  type BrowserEngineInfo,
  type EngineNotifier,
  type MoveReviewEngine,
  type Work,
} from '../types';

export class SingleThreadEngine implements MoveReviewEngine {
  readonly ready: Promise<boolean>;

  private readonly protocol = new Protocol();
  private worker: Worker | undefined;
  private failed: Error | undefined;
  private destroyed = false;
  private settleReady: ((ready: boolean) => void) | undefined;
  private readonly abortListener = (): void => this.destroy();

  constructor(
    readonly info: BrowserEngineInfo,
    readonly status: EngineNotifier | undefined,
    readonly signal: AbortSignal | undefined,
  ) {
    this.ready = new Promise(resolve => (this.settleReady = resolve));
    if (signal?.aborted) {
      this.destroy();
      return;
    }
    signal?.addEventListener('abort', this.abortListener, { once: true });
    this.boot();
  }

  getInfo(): BrowserEngineInfo {
    return this.info;
  }

  getState(): CevalState {
    return this.failed
      ? CevalState.Failed
      : !this.worker
        ? CevalState.Initial
        : this.protocol.isComputing()
          ? CevalState.Computing
          : this.protocol.engineName
            ? CevalState.Idle
            : CevalState.Loading;
  }

  start(work: Work): void {
    this.protocol.compute(work);
  }

  stop(): void {
    this.protocol.compute(undefined);
  }

  destroy(): void {
    this.destroyed = true;
    this.signal?.removeEventListener('abort', this.abortListener);
    this.worker?.terminate();
    this.worker = undefined;
    this.resolveReady(false);
  }

  private boot(): void {
    const { root, js, wasm } = this.info.assets;
    const scriptUrl = site.asset.url(`${root}/${js}`, { pathOnly: true });
    const wasmUrl = site.asset.url(`${root}/${wasm}`, { pathOnly: true });

    try {
      const worker = new Worker(`${scriptUrl}#${encodeURIComponent(wasmUrl)}`);
      this.worker = worker;
      worker.addEventListener('message', event => this.protocol.received(String(event.data)), true);
      worker.addEventListener('error', event => this.fail(new Error(event.message)), true);
      this.protocol.connected(command => worker.postMessage(command));
      void this.protocol.ready.then(() => this.resolveReady(!this.destroyed));
    } catch (error) {
      this.fail(error instanceof Error ? error : new Error(String(error)));
    }
  }

  private fail(error: Error): void {
    if (this.destroyed) return;
    this.failed = error;
    this.status?.({ error: error.message });
    this.resolveReady(false);
  }

  private resolveReady(ready: boolean): void {
    this.settleReady?.(ready);
    this.settleReady = undefined;
  }
}
