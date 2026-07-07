export type PubsubEventKey = keyof PubsubEvents;
export interface PubsubEvents {
    'analysis.closeAll': () => void;
    'analysis.change': (fen: FEN, path: string) => void;
    'content-loaded': (el?: HTMLElement) => void;
    jump: (ply: string) => void;
    ply: (ply: number, isMainline?: boolean) => void;
    'ply.trigger': () => void;
    'socket.in.crowd': (data: {
        nb: number;
    }) => void;
    'socket.in.redirect': (d: RedirectTo) => void;
    'socket.send': (event: string, d?: any, o?: any) => void;
}
export interface OneTimeEvents {
    'polyfill.dialog': ((dialog: HTMLElement) => void) | undefined;
    'socket.hasConnected': void;
}
export declare class Pubsub {
    private allSubs;
    private oneTimeEvents;
    on<K extends keyof PubsubEvents>(name: K, cb: PubsubEvents[K]): void;
    off<K extends keyof PubsubEvents>(name: K, cb: PubsubEvents[K]): void;
    emit<K extends keyof PubsubEvents>(name: K, ...args: Parameters<PubsubEvents[K]>): void;
    after<K extends OneTimeKey>(event: K): Promise<OneTimeEvents[K]>;
    complete<K extends OneTimeKey>(event: K, value?: OneTimeEvents[K]): void;
    past(event: OneTimeKey): boolean;
}
export declare const pubsub: Pubsub;
type OneTimeKey = keyof OneTimeEvents;
export {};
