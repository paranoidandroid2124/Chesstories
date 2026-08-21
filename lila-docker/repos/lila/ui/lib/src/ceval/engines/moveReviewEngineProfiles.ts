import type { Feature } from '@/device';
import type { BrowserEngineInfo } from '../types';

export const moveReviewEngineProfile = 'sf18-smallnet-t2-h16-v1' as const;
const moveReviewFallbackEngineProfile = 'sf18-smallnet-single-t1-h16-v1' as const;
export type MoveReviewEngineProfile = typeof moveReviewEngineProfile | typeof moveReviewFallbackEngineProfile;
const moveReviewEngineProfiles: readonly MoveReviewEngineProfile[] = [
  moveReviewEngineProfile,
  moveReviewFallbackEngineProfile,
];
export type MoveReviewVariant = 'standard' | 'chess960';

type MoveReviewEngineLoader = 'stockfish-web-pthread' | 'stockfish-web-single-thread-worker';

type MoveReviewEngineManifest = {
  readonly profile: MoveReviewEngineProfile;
  readonly variants: readonly MoveReviewVariant[];
  readonly threads: 1 | 2;
  readonly hashSize: 16;
  readonly requiredDepth: 16;
  readonly loader: MoveReviewEngineLoader;
  readonly info: Omit<BrowserEngineInfo, 'assets' | 'variants' | 'minThreads' | 'maxThreads' | 'maxHash'>;
  readonly assets: BrowserEngineInfo['assets'];
};

const nnue = 'nn-4ca89e4b3abf.nnue';

const moveReviewEngineManifests: Readonly<Record<MoveReviewEngineProfile, MoveReviewEngineManifest>> = {
  [moveReviewEngineProfile]: {
    profile: moveReviewEngineProfile,
    variants: ['standard', 'chess960'],
    threads: 2,
    hashSize: 16,
    requiredDepth: 16,
    loader: 'stockfish-web-pthread',
    info: {
      id: '__sf18nnuesmall-move-review-t2',
      name: 'Stockfish 18 NNUE · 7MB smallnet · 2 threads',
      short: 'SF 18 · 7MB · 2t',
      tech: 'NNUE',
      minMem: 1536,
      requires: ['sharedMem', 'simd', 'dynamicImportFromWorker'],
    },
    assets: {
      version: '0.2.3',
      root: 'npm/stockfish-web',
      js: 'sf_18_smallnet.js',
      wasm: 'sf_18_smallnet.wasm',
      nnue: [nnue],
    },
  },
  [moveReviewFallbackEngineProfile]: {
    profile: moveReviewFallbackEngineProfile,
    variants: ['standard', 'chess960'],
    threads: 1,
    hashSize: 16,
    requiredDepth: 16,
    loader: 'stockfish-web-single-thread-worker',
    info: {
      id: '__sf18nnuesmall-move-review-t1',
      name: 'Stockfish 18 NNUE · 7MB smallnet · 1 thread',
      short: 'SF 18 · 7MB · 1t',
      tech: 'NNUE',
      requires: ['wasm', 'simd'],
    },
    assets: {
      root: 'npm/stockfish-web-move-review',
      js: 'sf_18_smallnet_single.js',
      wasm: 'sf_18_smallnet_single.wasm',
    },
  },
};

export function isMoveReviewEngineProfile(value: unknown): value is MoveReviewEngineProfile {
  return value === moveReviewEngineProfile || value === moveReviewFallbackEngineProfile;
}

export function moveReviewEngineProfileSpec(profile: MoveReviewEngineProfile): MoveReviewEngineManifest {
  return moveReviewEngineManifests[profile];
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

export function moveReviewEngineCapabilities(
  environment: MoveReviewEngineEnvironment,
): readonly MoveReviewEngineCapability[] {
  return moveReviewEngineProfiles.map(profile => capability(profile, environment));
}

function capability(
  profile: MoveReviewEngineProfile,
  environment: MoveReviewEngineEnvironment,
): MoveReviewEngineCapability {
  const manifest = moveReviewEngineManifests[profile];
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
