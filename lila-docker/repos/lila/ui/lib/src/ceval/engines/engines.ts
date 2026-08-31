import {
  type BrowserEngineInfo,
  type MoveReviewEngine,
  type EngineNotifier,
  type EngineInfo,
  type CevalEngine,
} from '../types';
import type CevalCtrl from '../ctrl';
import { SimpleEngine } from './simpleEngine';
import { StockfishWebEngine } from './stockfishWebEngine';
import { ThreadedEngine } from './threadedEngine';
import { storedStringProp, type StoredProp } from '@/storage';
import { isAndroid, isIos, isIPad, features as browserSupport } from '@/device';
import {
  moveReviewEngineCapability,
  type MoveReviewEngineCapability,
  type MoveReviewEngineSupportedCapability,
} from './moveReviewEngineProfiles';

import { log } from '@/permalog';

export class Engines {
  private activeEngine: EngineInfo | undefined = undefined;
  localEngines: BrowserEngineInfo[];
  localEngineMap: Map<string, WithMake>;
  selectProp: StoredProp<string>;

  constructor(private ctrl: CevalCtrl) {
    this.localEngineMap = this.makeEngineMap();
    this.localEngines = [...this.localEngineMap.values()].map(e => e.info);
    this.selectProp = storedStringProp('ceval.engine', this.localEngines[0].id);
  }

  status = (status: { download?: { bytes: number; total: number }; error?: string } = {}): void => {
    if (this.ctrl.available()) this.ctrl.download = status.download;
    if (status.error) {
      log(status.error);
      this.ctrl.engineFailed(status.error);
    }
    this.ctrl.opts.redraw();
  };

  makeEngineMap(): Map<string, WithMake> {
    const browserEngines: WithMake[] = [
      {
        info: {
          id: '__sf18nnuesmall',
          name: 'Stockfish 18 NNUE · 7MB smallnet',
          short: 'SF 18 · 7MB',
          tech: 'NNUE',
          requires: ['sharedMem', 'simd', 'dynamicImportFromWorker'],
          minMem: 1536,
          assets: {
            root: 'npm/stockfish-web',
            nnue: ['nn-4ca89e4b3abf.nnue'],
            js: 'sf_18_smallnet.js',
          },
        },
        make: (e: BrowserEngineInfo) => new StockfishWebEngine(e, this.status),
      },
      {
        info: {
          id: '__sf18nnue79',
          name: 'Stockfish 18 NNUE · 79MB',
          short: 'SF 18 · 79MB',
          tech: 'NNUE',
          requires: ['sharedMem', 'simd', 'dynamicImportFromWorker'],
          minMem: 2560,
          assets: {
            root: 'npm/stockfish-web',
            nnue: ['nn-c288c895ea92.nnue', 'nn-37f18f62d772.nnue'],
            js: 'sf_18.js',
          },
        },
        make: (e: BrowserEngineInfo) => new StockfishWebEngine(e, this.status),
      },
      {
        info: {
          id: '__sf14nnue',
          name: 'Stockfish 14 NNUE',
          short: 'SF 14',
          tech: 'NNUE',
          obsoletedBy: 'dynamicImportFromWorker',
          requires: ['sharedMem', 'simd'],
          minMem: 2048,
          assets: {
            version: 'b6939d',
            root: 'npm/stockfish-nnue.wasm',
            js: 'stockfish.js',
            wasm: 'stockfish.wasm',
          },
        },
        make: (e: BrowserEngineInfo) => new ThreadedEngine(e, this.status),
      },
      {
        info: {
          id: '__sf11hce',
          name: 'Stockfish 11 HCE',
          short: 'SF 11',
          tech: 'HCE',
          requires: ['sharedMem'],
          minThreads: 1,
          assets: {
            version: 'a022fa',
            root: 'npm/stockfish.wasm',
            js: 'stockfish.js',
            wasm: 'stockfish.wasm',
          },
        },
        make: (e: BrowserEngineInfo) => new ThreadedEngine(e),
      },
      {
        info: {
          id: '__sfwasm',
          name: 'Stockfish WASM',
          short: 'Stockfish',
          tech: 'HCE',
          minThreads: 1,
          maxThreads: 1,
          requires: ['wasm'],
          obsoletedBy: 'sharedMem',
          assets: {
            version: 'a022fa',
            root: 'npm/stockfish.js',
            js: 'stockfish.wasm.js',
          },
        },
        make: (e: BrowserEngineInfo) => new SimpleEngine(e),
      },
      {
        info: {
          id: '__sfjs',
          name: 'Stockfish JS',
          short: 'Stockfish',
          tech: 'HCE',
          minThreads: 1,
          maxThreads: 1,
          requires: [],
          obsoletedBy: 'wasm',
          assets: {
            version: 'a022fa',
            root: 'npm/stockfish.js',
            js: 'stockfish.js',
          },
        },
        make: (e: BrowserEngineInfo) => new SimpleEngine(e),
      },
    ];
    return new Map<string, WithMake>(
      browserEngines
        .filter(
          e =>
            e.info.requires.every(req => browserSupport().includes(req)) &&
            !(e.info.obsoletedBy && browserSupport().includes(e.info.obsoletedBy)),
        )
        .map(e => [e.info.id, { info: withDefaults(e.info), make: e.make }]),
    );
  }

  get active(): EngineInfo | undefined {
    return this.activeEngine ?? this.activate();
  }

  activate(): EngineInfo | undefined {
    this.activeEngine = this.getEngine({ id: this.selectProp(), variant: this.ctrl.opts.variant.key });
    return this.activeEngine;
  }

  select(id: string): void {
    this.selectProp(id);
    this.activate();
  }

  updateCevalCtrl(ctrl: CevalCtrl): void {
    this.ctrl = ctrl;
  }

  supporting(variant: VariantKey): EngineInfo[] {
    return this.localEngines.filter(e => e.variants?.includes(variant));
  }

  moveReviewCapability(): MoveReviewEngineCapability {
    return moveReviewEngineCapability(this.moveReviewEnvironment());
  }

  makeMoveReview(
    capability: MoveReviewEngineSupportedCapability,
    status: EngineNotifier,
    signal: AbortSignal,
  ): MoveReviewEngine {
    return new StockfishWebEngine(capability.info, status, signal);
  }

  getEngine(selector?: { id?: string; variant?: VariantKey }): EngineInfo | undefined {
    const id = selector?.id || this.selectProp();
    const variant = selector?.variant || 'standard';
    return (
      this.localEngines.find(e => e.id === id && e.variants?.includes(variant)) ??
      this.localEngines.find(e => e.variants?.includes(variant))
    );
  }

  make(selector?: { id?: string; variant?: VariantKey }): CevalEngine {
    const e = (this.activeEngine = this.getEngine(selector));
    if (!e) throw Error(`Engine not found ${selector?.id ?? selector?.variant ?? this.selectProp()}}`);

    return this.localEngineMap.get(e.id)!.make(e);
  }

  private moveReviewEnvironment() {
    return {
      features: browserSupport(),
      hardwareConcurrency: Math.max(0, navigator.hardwareConcurrency || 0),
    };
  }
}

function maxHashMB() {
  if (isAndroid())
    return 64; // budget androids are easy to crash @ 128
  else if (isIPad())
    return 64; // iPadOS safari pretends to be desktop but acts more like iphone
  else if (isIos()) return 32;
  return 512; // allocating 1024 often fails and offers little benefit over 512, or 16 for that matter
}
const maxHash = maxHashMB();

const withDefaults = (engine: BrowserEngineInfo): BrowserEngineInfo => ({
  variants: ['standard', 'chess960', 'fromPosition'],
  minMem: 1024,
  maxHash,
  minThreads: 2,
  maxThreads: 32,
  ...engine,
});

type WithMake = {
  info: BrowserEngineInfo;
  make: (e: BrowserEngineInfo) => CevalEngine;
};
