import type { Feature } from '@/device';
import type { BrowserEngineInfo } from '../types';

export const moveReviewEngineProfile = 'sf18-smallnet-t2-h16-v1' as const;
export type MoveReviewEngineProfile = typeof moveReviewEngineProfile;
export type MoveReviewVariant = 'standard';

type MoveReviewEngineManifest = {
  readonly profile: MoveReviewEngineProfile;
  readonly variants: readonly MoveReviewVariant[];
  readonly threads: 2;
  readonly hashSize: 16;
  readonly requiredDepth: 16;
  readonly info: Omit<BrowserEngineInfo, 'assets' | 'variants' | 'minThreads' | 'maxThreads' | 'maxHash'>;
  readonly assets: BrowserEngineInfo['assets'];
};

const nnue = 'nn-4ca89e4b3abf.nnue';

const moveReviewEngineManifest: MoveReviewEngineManifest = {
  profile: moveReviewEngineProfile,
  variants: ['standard'],
  threads: 2,
  hashSize: 16,
  requiredDepth: 16,
  info: {
    id: '__sf18nnuesmall-move-review-t2',
    name: 'Stockfish 18 NNUE · 7MB smallnet · 2 threads',
    short: 'SF 18 · 7MB · 2t',
    tech: 'NNUE',
    minMem: 1536,
    requires: ['sharedMem', 'simd', 'dynamicImportFromWorker'],
  },
  assets: {
    version: '0.4.2',
    root: 'npm/stockfish-web',
    js: 'sf_18_smallnet.js',
    wasm: 'sf_18_smallnet.wasm',
    nnue: [nnue],
  },
};

export function isMoveReviewEngineProfile(value: unknown): value is MoveReviewEngineProfile {
  return value === moveReviewEngineProfile;
}

export function moveReviewEngineProfileSpec(): MoveReviewEngineManifest {
  return moveReviewEngineManifest;
}

export type MoveReviewEngineCapabilityReason =
  | 'insufficient-hardware-concurrency'
  | 'missing-browser-feature';

export type MoveReviewEngineSupportedCapability = {
  readonly supported: true;
  readonly profile: MoveReviewEngineProfile;
  readonly manifest: MoveReviewEngineManifest;
  readonly info: BrowserEngineInfo;
};

export type MoveReviewEngineCapability =
  | MoveReviewEngineSupportedCapability
  | {
      readonly supported: false;
      readonly profile: MoveReviewEngineProfile;
      readonly reason: MoveReviewEngineCapabilityReason;
    };

type MoveReviewEngineEnvironment = {
  readonly features: readonly Feature[];
  readonly hardwareConcurrency: number;
};

export function moveReviewEngineCapability(
  environment: MoveReviewEngineEnvironment,
): MoveReviewEngineCapability {
  const profile = moveReviewEngineProfile;
  const manifest = moveReviewEngineManifest;
  if (environment.hardwareConcurrency < manifest.threads)
    return { supported: false, profile, reason: 'insufficient-hardware-concurrency' };
  if (manifest.info.requires.some(feature => !environment.features.includes(feature)))
    return { supported: false, profile, reason: 'missing-browser-feature' };
  return {
    supported: true,
    profile,
    manifest,
    info: {
      ...manifest.info,
      variants: [...manifest.variants],
      minThreads: manifest.threads,
      maxThreads: manifest.threads,
      maxHash: manifest.hashSize,
      assets: manifest.assets,
    },
  };
}
