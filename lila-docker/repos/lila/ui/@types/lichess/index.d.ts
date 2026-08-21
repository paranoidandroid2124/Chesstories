/// <reference path="./tree.d.ts" />
/// <reference path="./chessground.d.ts" />
/// <reference path="./cash.d.ts" />

// file://./../../site/src/site.ts
interface Site {
  debug?: boolean | string;
  info?: {
    commit: string;
  };
  mousetrap: {
    // file://./../../site/src/mousetrap.ts
    bind(
      keys: string | string[],
      callback: (e: KeyboardEvent) => void,
      action?: 'keypress' | 'keydown' | 'keyup',
      multiple?: boolean,
    ): Site['mousetrap'];
  };
  sri: string;
  asset: {
    // file://./../../site/src/asset.ts
    baseUrl(): string;
    url(
      url: string,
      opts?: { documentOrigin?: boolean; pathOnly?: boolean; pathVersion?: true | string },
    ): string;
    loadCss(href: string, key?: string): Promise<void>;
    loadCssPath(key: string): Promise<void>;
    removeCss(href: string): void;
    removeCssPath(key: string): void;
    loadIife(
      path: string,
      opts?: { documentOrigin?: boolean; pathOnly?: boolean; pathVersion?: true | string },
    ): Promise<void>;
    loadEsm<T>(
      key: string,
      opts?: {
        documentOrigin?: boolean;
        pathOnly?: boolean;
        pathVersion?: true | string;
        init?: any;
        npm?: boolean;
      },
    ): Promise<T>;
    loadPieces: Promise<void>;
  };
  unload: { expected: boolean };
  redirect(o: RedirectTo): void;
  reload(err?: any): void;
  sound: {
    // file://./../../site/src/sound.ts
    move(opts?: {
      name?: string; // either provide this or valid san
      san?: string;
      volume?: number;
    }): void;
    load(name: string, path?: string): Promise<any>;
    play(name: string, volume?: number): Promise<void>;
    saySan(san?: San, cut?: boolean, force?: boolean): void;
    preloadBoardSounds(): void;
    url(name: string): string;
  };
  blindMode: boolean;
  load: Promise<void>; // DOMContentLoaded promise
  analysis?: any; // expose the analysis ctrl
  // file://./../../.build/src/manifest.ts
  manifest: {
    css: Record<string, string>;
    js: Record<string, string>;
    hashed: Record<string, string>;
  };
}

type Redraw = () => void;
type RedirectTo = string | { id: string; url: string };

type Timeout = ReturnType<typeof setTimeout>;

interface Window {
  site: Site;
}

type VariantKey = 'standard' | 'chess960' | 'fromPosition';

type Uci = string;
type San = string;
type Ply = number;

interface Variant {
  key: VariantKey;
  name: string;
  short: string;
  title?: string;
}

interface EvalScore {
  cp?: number;
  mate?: number;
}

declare const site: Site;
declare const $html: (s: TemplateStringsArray, ...k: any[]) => string; // file://./../../.build/src/esbuild.ts
declare const $trim: (s: TemplateStringsArray, ...k: any[]) => string; // file://./../../.build/src/esbuild.ts
