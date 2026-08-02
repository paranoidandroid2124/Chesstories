/* eslint no-restricted-syntax:"error" */ // no side effects allowed due to re-export by index.ts

import { defined, notNull, type Prop, withEffect } from './common';
import { preferenceLocalStorage, preferenceSessionStorage } from './cookieConsent';

export const storage: ChesstoryStorageHelper = makeStorage(() => preferenceLocalStorage());
export const tempStorage: ChesstoryStorageHelper = makeStorage(() => preferenceSessionStorage());

export interface StoredProp<V> extends Prop<V> {
  (replacement?: V): V;
}

export function storedProp<V>(
  key: string,
  defaultValue: V,
  fromStr: (str: string) => V,
  toStr: (v: V) => string,
): StoredProp<V> {
  const compatKey = key.startsWith('analyse.') ? null : `analyse.${key}`;
  let cached: V;
  return function (replacement?: V) {
    if (defined(replacement) && replacement != cached) {
      cached = replacement;
      storage.set(key, toStr(replacement));
    } else if (!defined(cached)) {
      if (compatKey) {
        const compatValue = storage.get(compatKey);
        if (notNull(compatValue)) {
          storage.set(key, compatValue);
          storage.remove(compatKey);
        }
      }
      const str = storage.get(key);
      cached = str === null ? defaultValue : fromStr(str);
    }
    return cached;
  };
}

export const storedStringProp = (k: string, defaultValue: string): StoredProp<string> =>
  storedProp<string>(
    k,
    defaultValue,
    str => str,
    v => v,
  );

export const storedBooleanProp = (k: string, defaultValue: boolean): StoredProp<boolean> =>
  storedProp<boolean>(
    k,
    defaultValue,
    str => str === 'true',
    v => v.toString(),
  );

export const storedBooleanPropWithEffect = (
  k: string,
  defaultValue: boolean,
  effect: (v: boolean) => void,
): Prop<boolean> => withEffect(storedBooleanProp(k, defaultValue), effect);

export const storedIntProp = (k: string, defaultValue: number): StoredProp<number> =>
  storedProp<number>(
    k,
    defaultValue,
    str => Number(str),
    v => v + '',
  );

export function once(key: string, every?: { seconds?: number; hours?: number; days?: number }): boolean {
  const now = Date.now();
  const last = Number(storage.get(key)) || 0;
  const seconds = (every?.seconds ?? 0) + (every?.hours ?? 0) * 3600 + (every?.days ?? 0) * 24 * 3600;

  if (last && (!every || now - last < seconds * 1000)) return false;
  storage.set(key, now.toString());
  return true;
}

export interface ChesstoryStorage {
  get(): string | null;
  set(v: any): void;
  remove(): void;
  listen(f: (e: ChesstoryStorageEvent) => void): void;
  fire(v?: string): void;
}

export interface ChesstoryBooleanStorage {
  get(): boolean;
  getOrDefault(defaultValue: boolean): boolean;
  set(v: boolean): void;
  toggle(): void;
}

interface ChesstoryStorageHelper {
  make(k: string, ttl?: number): ChesstoryStorage;
  boolean(k: string): ChesstoryBooleanStorage;
  get(k: string): string | null;
  set(k: string, v: string): void;
  fire(k: string, v?: string): void;
  remove(k: string): void;
}

interface ChesstoryStorageEvent {
  sri: string;
  nonce: number;
  value?: string;
}

function makeStorage(storageGetter: () => Storage | null): ChesstoryStorageHelper {
  const api = {
    get: (k: string): string | null => storageGetter()?.getItem(k) ?? null,
    set: (k: string, v: string): void => storageGetter()?.setItem(k, v),
    fire: (k: string, v?: string) =>
      storageGetter()?.setItem(
        k,
        JSON.stringify({
          sri: site.sri,
          nonce: Math.random(), // ensure item changes
          value: v,
        }),
      ),
    remove: (k: string) => storageGetter()?.removeItem(k),
    make: (k: string, ttl?: number) => {
      const bdKey = ttl && `${k}--bd`;
      const remove = () => {
        api.remove(k);
        if (bdKey) api.remove(bdKey);
      };
      return {
        get: () => {
          if (!bdKey) return api.get(k);
          const birthday = Number(api.get(bdKey));
          if (!birthday) api.set(bdKey, String(Date.now()));
          else if (Date.now() - birthday > ttl) remove();
          return api.get(k);
        },
        set: (v: any) => {
          api.set(k, v);
          if (bdKey) api.set(bdKey, String(Date.now()));
        },
        fire: (v?: string) => api.fire(k, v),
        remove,
        listen: (f: (e: ChesstoryStorageEvent) => void) =>
          window.addEventListener('storage', e => {
            const storage = storageGetter();
            if (!storage || e.key !== k || e.storageArea !== storage || e.newValue === null) return;
            let parsed: ChesstoryStorageEvent | null;
            try {
              parsed = JSON.parse(e.newValue);
            } catch (_) {
              return;
            }
            // check sri, because Safari fires events also in the original
            // document when there are multiple tabs
            if (parsed?.sri && parsed.sri !== site.sri) f(parsed);
          }),
      };
    },
    boolean: (k: string) => ({
      get: () => api.get(k) === '1',
      getOrDefault: (defaultValue: boolean) => {
        const stored = api.get(k);
        return stored === null ? defaultValue : stored === '1';
      },
      set: (v: boolean): void => api.set(k, v ? '1' : '0'),
      toggle: () => api.set(k, api.get(k) === '1' ? '0' : '1'),
    }),
  };
  return api;
}
