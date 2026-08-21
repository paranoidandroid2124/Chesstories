const legacyIndexedDbNames = ['analyse-collapse', 'big-file', 'ceval-wasm-cache--db', 'lichess', 'log--db'];

export async function clearLegacyClientStorage(): Promise<void> {
  if (typeof window === 'undefined') return;

  expireVisibleCookiesExceptSid();
  clearWebStorage(() => window.localStorage);
  clearWebStorage(() => window.sessionStorage);
  deleteLegacyIndexedDbs();
  await clearOriginPrivateFileSystem();
}

function expireVisibleCookiesExceptSid(): void {
  try {
    const secure = window.location.protocol === 'https:' ? '; Secure' : '';
    for (const cookie of window.document.cookie.split(';')) {
      const rawName = cookie.slice(0, Math.max(0, cookie.indexOf('='))).trim();
      if (!rawName) continue;
      const name = decodeCookieName(rawName);
      if (name === 'sid') continue;

      const encodedName = encodeURIComponent(name);
      const expired = `${encodedName}=; Path=/; Max-Age=0; SameSite=Lax${secure}`;
      window.document.cookie = expired;
      window.document.cookie = `${expired}; Domain=${window.location.hostname}`;
    }
  } catch {
    // Cookie access can be disabled independently of the rest of browser storage.
  }
}

function decodeCookieName(name: string): string {
  try {
    return decodeURIComponent(name);
  } catch {
    return name;
  }
}

function clearWebStorage(getStorage: () => Storage): void {
  try {
    getStorage().clear();
  } catch {
    // Storage can be unavailable in hardened or opaque-origin browser contexts.
  }
}

function deleteLegacyIndexedDbs(): void {
  try {
    if (!window.indexedDB?.deleteDatabase) return;
    for (const name of legacyIndexedDbNames) {
      try {
        const request = window.indexedDB.deleteDatabase(name);
        request.onblocked = () =>
          console.info(`Legacy browser database deletion is waiting for another tab: ${name}`);
      } catch {
        // Keep clearing the remaining known databases.
      }
    }
  } catch {
    // Best-effort migration cleanup. A future boot retries without a marker.
  }
}

async function clearOriginPrivateFileSystem(): Promise<void> {
  try {
    const root = await window.navigator.storage?.getDirectory?.();
    const entries = (root as any)?.entries?.();
    if (!root || !entries) return;
    for await (const [name, handle] of entries as AsyncIterable<[string, { kind?: string }]>) {
      await root.removeEntry(name, { recursive: handle?.kind === 'directory' }).catch(() => {});
    }
  } catch {
    // OPFS is optional and may be unavailable or busy in another tab.
  }
}
