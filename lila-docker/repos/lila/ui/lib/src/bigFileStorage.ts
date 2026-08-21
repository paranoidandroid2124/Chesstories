import { memoize } from './index';

// url keyed storage for very large assets

export const bigFileStorage: () => BigFileStorage = memoize(() => new BigFileStorage());

type U8 = Uint8Array<ArrayBuffer>;

class BigFileStorage {
  private files = new Map<string, Promise<U8>>();

  async get(
    assetUrl: string,
    onProgress?: (loaded: number, total: number) => void,
    signal?: AbortSignal,
  ): Promise<U8> {
    if (signal?.aborted) throw signal.reason ?? new DOMException('The operation was aborted.', 'AbortError');
    const existing = this.files.get(assetUrl);
    if (existing) return signal ? waitFor(existing, signal) : existing;
    const fetched = new Promise<U8>((resolve, reject) => {
      const xhr = new XMLHttpRequest();
      const cleanup = () => signal?.removeEventListener('abort', abort);
      const abort = () => xhr.abort();
      xhr.open('GET', assetUrl, true);
      xhr.responseType = 'arraybuffer';
      if (onProgress) xhr.onprogress = e => onProgress(e.loaded, e.total);
      xhr.onabort = () => {
        cleanup();
        reject(signal?.reason ?? new DOMException('The operation was aborted.', 'AbortError'));
      };
      xhr.onerror = () => {
        cleanup();
        reject(new Error(`fetch '${assetUrl}' failed: ${xhr.status}`));
      };
      xhr.onload = () => {
        cleanup();
        if (xhr.status / 100 === 2) resolve(new Uint8Array(xhr.response));
        else reject(new Error(`fetch '${assetUrl}' failed: ${xhr.status}`));
      };
      signal?.addEventListener('abort', abort, { once: true });
      xhr.send();
    });
    const cached = fetched.catch(error => {
      if (this.files.get(assetUrl) === cached) this.files.delete(assetUrl);
      throw error;
    });
    this.files.set(assetUrl, cached);
    return cached;
  }

  async delete(assetUrl: string): Promise<void> {
    this.files.delete(assetUrl);
  }
}

function waitFor<T>(promise: Promise<T>, signal: AbortSignal): Promise<T> {
  return new Promise((resolve, reject) => {
    const abort = () => reject(signal.reason ?? new DOMException('The operation was aborted.', 'AbortError'));
    signal.addEventListener('abort', abort, { once: true });
    promise.then(
      value => {
        signal.removeEventListener('abort', abort);
        resolve(value);
      },
      error => {
        signal.removeEventListener('abort', abort);
        reject(error);
      },
    );
  });
}
