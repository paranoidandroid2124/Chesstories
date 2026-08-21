import { Protocol } from '../protocol';
import {
  CevalState,
  type Work,
  type CevalEngine,
  type BrowserEngineInfo,
  type EngineNotifier,
} from '../types';
import { sharedWasmMemory } from '../util';

interface WasmModule {
  (opts: {
    wasmBinary?: ArrayBuffer;
    locateFile(path: string): string;
    wasmMemory: WebAssembly.Memory;
    printErr(msg: string): void;
    onError(err: Error): void;
  }): Promise<Stockfish>;
}

interface Stockfish {
  addMessageListener(cb: (msg: string) => void): void;
  postMessage(msg: string): void;
}

declare global {
  interface Window {
    Stockfish?: WasmModule;
  }
}

export class ThreadedEngine implements CevalEngine {
  failed: Error;
  protocol: Protocol;
  module?: Stockfish;

  constructor(
    readonly info: BrowserEngineInfo,
    readonly status?: EngineNotifier | undefined,
  ) {}

  onError = (err: Error): void => {
    console.error(err);
    this.failed = err;
    this.status?.({ error: String(err) });
  };

  getInfo(): BrowserEngineInfo {
    return this.info;
  }

  getState(): CevalState {
    return !this.protocol
      ? CevalState.Initial
      : this.failed
        ? CevalState.Failed
        : !this.protocol.engineName
          ? CevalState.Loading
          : this.protocol.isComputing()
            ? CevalState.Computing
            : CevalState.Idle;
  }

  private async boot() {
    const [root, js, wasm, pathVersion] = [
        this.info.assets.root,
        this.info.assets.js,
        this.info.assets.wasm,
        this.info.assets.version,
      ],
      wasmPath = `${root}/${wasm}`;

    let wasmBinary: ArrayBuffer | undefined;
    if (this.info.id === '__sf14nnue') {
      wasmBinary = await new Promise((resolve, reject) => {
        const req = new XMLHttpRequest();
        req.open('GET', site.asset.url(wasmPath, { pathVersion }), true);
        req.responseType = 'arraybuffer';
        req.onerror = event => reject(event);
        req.onprogress = event => this.status?.({ download: { bytes: event.loaded, total: event.total } });
        req.onload = _ => {
          this.status?.();
          resolve(req.response);
        };
        req.send();
      });
    }

    // Load Emscripten module.
    await site.asset.loadIife(`${root}/${js}`, { pathVersion });
    const sf = await window.Stockfish!({
      wasmBinary,
      printErr: (msg: string) => this.onError(new Error(msg)),
      onError: this.onError,
      locateFile: (path: string) =>
        site.asset.url(`${root}/${path}`, { pathVersion, pathOnly: path.endsWith('.worker.js') }),
      wasmMemory: sharedWasmMemory(this.info.minMem!),
    });

    sf.addMessageListener(data => this.protocol.received(data));
    this.protocol.connected(msg => sf.postMessage(msg));
    this.module = sf;
  }

  async start(work: Work): Promise<void> {
    if (!this.protocol) {
      this.protocol = new Protocol();
      this.boot().catch(this.onError);
    }
    this.protocol.compute(work);
  }

  stop(): void {
    this.protocol.compute(undefined);
  }

  destroy(): void {
    this.module?.postMessage('quit');
    this.module = undefined;
  }
}
