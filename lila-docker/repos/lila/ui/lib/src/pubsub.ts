
export type PubsubEventKey = keyof PubsubEvents;

export interface PubsubEvents {
  'analysis.closeAll': () => void;
  'analysis.change': (fen: FEN, path: string) => void;
  'content-loaded': (el?: HTMLElement) => void;
  jump: (ply: string) => void;

  ply: (ply: number, isMainline?: boolean) => void;
  'ply.trigger': () => void;

  'socket.in.crowd': (data: { nb: number }) => void;

  'socket.in.redirect': (d: RedirectTo) => void;
  'socket.send': (event: string, d?: any, o?: any) => void;
}

export interface OneTimeEvents {
  'polyfill.dialog': ((dialog: HTMLElement) => void) | undefined;
  'socket.hasConnected': void;
}

export class Pubsub {
  private allSubs: Map<keyof PubsubEvents, Set<PubsubEvents[keyof PubsubEvents]>> = new Map();
  private oneTimeEvents: Map<OneTimeKey, OneTimeHandler<OneTimeEvents[OneTimeKey]>> = new Map();

  on<K extends keyof PubsubEvents>(name: K, cb: PubsubEvents[K]): void {
    const subs = this.allSubs.get(name);
    if (subs) subs.add(cb);
    else this.allSubs.set(name, new Set([cb]));
  }

  off<K extends keyof PubsubEvents>(name: K, cb: PubsubEvents[K]): void {
    this.allSubs.get(name)?.delete(cb);
  }

  emit<K extends keyof PubsubEvents>(name: K, ...args: Parameters<PubsubEvents[K]>): void {
    const callbacks = this.allSubs.get(name);
    if (callbacks) {
      for (const cb of callbacks) {
        (cb as (...args: Parameters<PubsubEvents[K]>) => void)(...args);
      }
    }
  }

  after<K extends OneTimeKey>(event: K): Promise<OneTimeEvents[K]> {
    const found = this.oneTimeEvents.get(event);
    if (found) return found.promise as Promise<OneTimeEvents[K]>;

    const handler = {} as OneTimeHandler<OneTimeEvents[K]>;
    handler.promise = new Promise<OneTimeEvents[K]>(resolve => (handler!.resolve = resolve));
    this.oneTimeEvents.set(event, handler);

    return handler.promise;
  }

  complete<K extends OneTimeKey>(event: K, value?: OneTimeEvents[K]): void {
    const found = this.oneTimeEvents.get(event);
    if (found) {
      found.resolve?.(value);
      found.resolve = undefined;
    } else this.oneTimeEvents.set(event, { promise: Promise.resolve(value) });
  }

  past(event: OneTimeKey): boolean {
    return this.oneTimeEvents.has(event) && !this.oneTimeEvents.get(event)?.resolve;
  }
}

export const pubsub: Pubsub = new Pubsub();

type OneTimeKey = keyof OneTimeEvents;
interface OneTimeHandler<T = any> {
  promise: Promise<T>;
  resolve?: (value: T) => void;
}
