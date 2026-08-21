import {
  CevalState,
  type Work,
  type CevalEngine,
  type BrowserEngineInfo,
  type EngineNotifier,
} from '../types';
import { Protocol } from '../protocol';
import { sharedWasmMemory } from '../util';
import type StockfishWeb from '@lichess-org/stockfish-web';
import { bigFileStorage } from '@/bigFileStorage';

export class StockfishWebEngine implements CevalEngine {
  failed: Error | undefined;
  protocol: Protocol;
  module?: StockfishWeb;
  readonly ready: Promise<boolean>;
  private blobUrl?: string;
  private destroyed = false;
  private readonly abortController = new AbortController();
  private readonly abortListener = (): void => this.destroy();

  constructor(
    readonly info: BrowserEngineInfo,
    readonly status?: EngineNotifier | undefined,
    readonly signal?: AbortSignal | undefined,
  ) {
    this.protocol = new Protocol();
    if (signal?.aborted) this.destroyed = true;
    else signal?.addEventListener('abort', this.abortListener, { once: true });
    this.ready = (this.destroyed ? Promise.resolve(false) : this.boot()).then(
      ready => ready,
      error => {
        if (this.destroyed) return false;
        this.failed = error instanceof Error ? error : new Error(String(error));
        this.status?.({ error: String(error) });
        return false;
      },
    );
  }

  getInfo(): BrowserEngineInfo {
    return this.info;
  }

  private async boot(): Promise<boolean> {
    let blobUrl: string | undefined;
    let module: StockfishWeb | undefined;
    let booting = true;
    const [root, js] = [this.info.assets.root, this.info.assets.js];
    const scriptUrl = site.asset.url(`${root}/${js}`, { documentOrigin: true });

    try {
      // Fetch the worker script and create a Blob URL to avoid pthread sub-worker
      // URL resolution issues with hashed asset filenames in cloud environments.
      const response = await fetch(scriptUrl, { signal: this.abortController.signal });
      if (this.destroyed) return false;
      if (!response.ok) throw new Error(`Failed to fetch engine script: ${response.status}`);
      const scriptText = await response.text();
      if (this.destroyed) return false;
      const blob = new Blob([scriptText], { type: 'text/javascript' });
      blobUrl = URL.createObjectURL(blob);
      this.blobUrl = blobUrl;

      if (this.destroyed) return false;
      const makeModule = await import(blobUrl);
      if (this.destroyed) return false;
      const loadedModule: StockfishWeb = await makeModule.default({
        wasmMemory: sharedWasmMemory(this.info.minMem!),
        locateFile: (file: string) => {
          const path = file.includes('/') ? file : `${root}/${file}`;
          return site.asset.url(path);
        },
        mainScriptUrlOrBlob: blobUrl,
      });
      module = loadedModule;
      if (this.destroyed) return false;

      if (this.info.tech === 'NNUE') {
        loadedModule.onError = this.makeErrorHandler(loadedModule);
        const nnueFilenames: string[] = this.info.assets.nnue ?? [];
        if (!nnueFilenames.length)
          for (let i = 0; ; i++) {
            const nnueFilename = loadedModule.getRecommendedNnue(i);
            if (!nnueFilename || nnueFilenames.includes(nnueFilename)) break;
            nnueFilenames.push(nnueFilename);
          }
        await Promise.all(
          nnueFilenames.map(async (name, index) => {
            const buffer = await bigFileStorage().get(
              site.asset.url(`lifat/nnue/${name}`),
              (bytes, total) => {
                if (!this.destroyed && booting) this.status?.({ download: { bytes, total } });
              },
              this.abortController.signal,
            );
            if (!this.destroyed && booting) loadedModule.setNnueBuffer(buffer, index);
          }),
        );
        if (this.destroyed) return false;
      }
      loadedModule.listen = (data: string) => this.protocol.received(data);
      if (this.destroyed) return false;
      this.protocol.connected(cmd => loadedModule.uci(cmd));
      await this.protocol.ready;
      if (this.destroyed) return false;
      this.module = loadedModule;
      module = undefined;
      blobUrl = undefined;
      return true;
    } finally {
      booting = false;
      this.cleanup(module, blobUrl);
    }
  }

  private cleanup(module: StockfishWeb | undefined, blobUrl: string | undefined): void {
    const ownedBlobUrl = this.blobUrl === blobUrl ? blobUrl : undefined;
    if (module && this.module === module) this.module = undefined;
    if (ownedBlobUrl) this.blobUrl = undefined;
    if (module) {
      try {
        module.uci('quit');
      } catch {
        // Module shutdown is best effort.
      }
    }
    if (ownedBlobUrl) {
      try {
        URL.revokeObjectURL(ownedBlobUrl);
      } catch {
        // Blob cleanup is best effort.
      }
    }
  }

  makeErrorHandler(module: StockfishWeb) {
    return (msg: string): void => {
      if (this.destroyed) return;
      if (msg.startsWith('BAD_NNUE')) {
        const index = Math.max(0, Number(msg.slice(9)));
        const nnueFilename = this.info.assets.nnue ?? module.getRecommendedNnue(index);
        // if we got this from bigFileStorage, we should remove it. but wait for async ops to finish first
        setTimeout(() => {
          console.warn(`Corrupt NNUE file, removing ${nnueFilename} from OPFS/IDB`);
          bigFileStorage().delete(site.asset.url(`lifat/nnue/${nnueFilename}`));
        }, 2000);
      } else this.status?.({ error: msg });
    };
  }

  getState(): CevalState {
    return this.failed
      ? CevalState.Failed
      : !this.module
        ? CevalState.Loading
        : this.protocol.isComputing()
          ? CevalState.Computing
          : CevalState.Idle;
  }

  start = (work?: Work): void => this.protocol.compute(work);
  stop = (): void => this.protocol.compute(undefined);
  engineName = (): string | undefined => this.protocol.engineName;
  destroy = (): void => {
    this.destroyed = true;
    this.signal?.removeEventListener('abort', this.abortListener);
    this.abortController.abort();
    this.cleanup(this.module, this.blobUrl);
  };
}
