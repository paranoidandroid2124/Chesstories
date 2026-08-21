interface PermaLog {
  (...args: any[]): Promise<number | void>;
  clear(): Promise<void>;
  get(): Promise<string>;
}

interface LogEntry {
  key: number;
  message: string;
}

const memoryLogWindow = 100;

export const log: PermaLog = makeLog(memoryLogWindow);

function makeLog(windowSize: number): PermaLog {
  const entries: LogEntry[] = [];
  let lastKey = 0;
  let drift = 0.001;

  (Error.prototype as any).toJSON ??= function () {
    return { [this.name]: this.message, stack: this.stack };
  };

  const stringify = (value: any): string =>
    !value || typeof value === 'string' ? String(value) : JSON.stringify(value);

  const log: PermaLog = async (...args: any[]): Promise<number> => {
    console.log(...args);
    const message =
      (site.info ? `#${site.info.commit.substring(0, 7)} - ` : '') + args.map(stringify).join(' ');
    let nextKey = Date.now();
    if (nextKey === lastKey) {
      nextKey += drift;
      drift += 0.001;
    } else drift = 0.001;
    lastKey = nextKey;
    entries.push({ key: nextKey, message });
    if (windowSize >= 0 && entries.length > windowSize) entries.splice(0, entries.length - windowSize);
    return nextKey;
  };

  log.clear = async () => {
    entries.length = 0;
    lastKey = 0;
  };

  log.get = async (): Promise<string> =>
    entries
      .map(entry => `${new Date(entry.key).toISOString().replace(/[TZ]/g, ' ')}${entry.message}`)
      .join('\n');

  return log;
}
