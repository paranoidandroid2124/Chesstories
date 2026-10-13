import { createServer, request as httpRequest } from 'node:http';
import { createServer as createTcpServer } from 'node:net';
import { readFile, stat, mkdir } from 'node:fs/promises';
import { createReadStream } from 'node:fs';
import { spawn } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { resolve, sep, extname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { setTimeout as delay } from 'node:timers/promises';

const root = fileURLToPath(new URL('../', import.meta.url));
const dist = resolve(root, 'dist');
const port = Number(process.env.CHESSTORIES_PORT ?? 8787);
if (!Number.isInteger(port) || port < 1 || port > 65535) throw Error('Invalid CHESSTORIES_PORT');
let classpath;
try {
  classpath = await readFile(resolve(root, '../chesstory-runtime/target/runtime-classpath.txt'), 'utf8');
  await stat(resolve(dist, 'index.html'));
} catch {
  console.error('Build the app first: npm run build:runtime && npm run build');
  process.exit(1);
}
const freePort = createTcpServer();
await new Promise((resolve, reject) => freePort.once('error', reject).listen(0, '127.0.0.1', resolve));
const runtimePort = freePort.address().port;
await new Promise(resolve => freePort.close(resolve));
const token = randomUUID();
// JDK 21 on Windows cannot create its selector pipe under a non-ASCII TEMP path.
const javaOptions = [];
if (process.platform === 'win32') {
  const socketTemp = process.env.CHESSTORIES_SOCKET_TEMP ?? resolve(process.env.PUBLIC ?? 'C:/Users/Public', 'Chesstories/socket-temp');
  if (!/^[\x20-\x7e]+$/.test(socketTemp)) throw Error('CHESSTORIES_SOCKET_TEMP must be an ASCII path');
  await mkdir(socketTemp, { recursive: true });
  javaOptions.push(`-Djdk.net.unixdomain.tmpdir=${socketTemp}`);
}
const runtime = spawn(process.env.CHESSTORIES_JAVA ?? 'java', [
  ...javaOptions, '-Xms64m', '-Xmx768m', '-XX:ActiveProcessorCount=4', '-cp', classpath.trim(), 'io.chesstory.runtime.ChesstoryRuntime',
], { cwd: root, windowsHide: true, stdio: ['ignore', 'inherit', 'inherit'],
  env: { ...process.env, CHESSTORY_HOST: '127.0.0.1', CHESSTORY_PORT: String(runtimePort), CHESSTORY_TOKEN: token, CHESSTORY_WORKERS: '2' } });
let runtimeFailure;
runtime.once('error', error => { runtimeFailure = error; });
runtime.once('exit', code => { runtimeFailure = Error(`Runtime exited (${code})`); });
process.once('exit', () => runtime.kill());
const allowedHosts = new Set([`127.0.0.1:${port}`, `localhost:${port}`]);
const headers = { 'Cross-Origin-Opener-Policy': 'same-origin', 'Cross-Origin-Embedder-Policy': 'require-corp',
  'X-Content-Type-Options': 'nosniff', 'Referrer-Policy': 'no-referrer',
  'Content-Security-Policy': "default-src 'self'; script-src 'self' 'wasm-unsafe-eval'; worker-src 'self' blob:; style-src 'self' 'unsafe-inline'; img-src 'self' data:; connect-src 'self'; object-src 'none'; frame-ancestors 'none'; base-uri 'none'" };
const mime = { '.html': 'text/html; charset=utf-8', '.js': 'text/javascript; charset=utf-8', '.css': 'text/css; charset=utf-8',
  '.wasm': 'application/wasm', '.json': 'application/json', '.map': 'application/json' };
const server = createServer(async (req, res) => {
  const fail = (status, message) => { res.writeHead(status, { ...headers, 'Content-Type': 'text/plain; charset=utf-8' }); res.end(message); };
  if (!allowedHosts.has(req.headers.host)) return fail(403, 'Local access only');
  const origin = req.headers.origin;
  if (origin && ![...allowedHosts].some(host => origin === `http://${host}`)) return fail(403, 'Origin rejected');
  let pathname;
  try { pathname = decodeURIComponent(new URL(req.url, `http://127.0.0.1:${port}`).pathname); }
  catch { return fail(400, 'Invalid path'); }
  const api = '/api/chess-judgment/position-commentary-jobs';
  if (pathname === api || new RegExp(`^${api}/[A-Za-z0-9_-]{32}(/engine-work-reports)?$`).test(pathname)) {
    if (!['GET', 'POST', 'DELETE'].includes(req.method)) return fail(405, 'Method not allowed');
    if (runtimeFailure) return fail(503, 'Commentary runtime unavailable');
    if (req.method === 'POST' && !req.headers['content-type']?.startsWith('application/json')) return fail(415, 'JSON required');
    const chunks = []; let size = 0;
    try {
      for await (const chunk of req) {
        size += chunk.length;
        if (size > 1024 * 1024) return fail(413, 'Request too large');
        chunks.push(chunk);
      }
    } catch { return; }
    const upstream = httpRequest({ hostname: '127.0.0.1', port: runtimePort,
      path: pathname.replace('/api/chess-judgment/', '/v1/'), method: req.method,
      headers: { Authorization: `Bearer ${token}`, 'Content-Type': 'application/json',
        'Content-Length': size, ...(req.headers.prefer ? { Prefer: req.headers.prefer } : {}) } }, response => {
      res.writeHead(response.statusCode, { ...headers, 'Content-Type': 'application/json', 'Cache-Control': 'no-store' });
      response.pipe(res);
    });
    upstream.setTimeout(15000, () => upstream.destroy(Error('Runtime timeout')));
    upstream.once('error', () => { if (!res.headersSent) fail(502, 'Commentary runtime unavailable'); else res.destroy(); });
    res.once('close', () => upstream.destroy());
    upstream.end(Buffer.concat(chunks));
    return;
  }
  if (!['GET', 'HEAD'].includes(req.method)) return fail(405, 'Method not allowed');
  const file = resolve(dist, `.${pathname === '/' ? '/index.html' : pathname}`);
  if (!file.startsWith(dist + sep)) return fail(403, 'Invalid path');
  try {
    const info = await stat(file);
    if (!info.isFile()) return fail(404, 'Not found');
    res.writeHead(200, { ...headers, 'Content-Type': mime[extname(file)] ?? 'application/octet-stream',
      'Content-Length': info.size, 'Cache-Control': pathname.startsWith('/engine/') ? 'public, max-age=86400' : 'no-cache' });
    if (req.method === 'HEAD') res.end();
    else createReadStream(file).on('error', () => res.destroy()).pipe(res);
  } catch { fail(404, 'Not found'); }
});
const stop = () => { runtime.kill(); server.close(); server.closeAllConnections(); };
process.once('SIGINT', stop);
process.once('SIGTERM', stop);
server.once('error', error => { console.error(error.message); runtime.kill(); process.exitCode = 1; });
try {
  let healthy = false;
  for (let attempt = 0; attempt < 100; attempt++) {
    if (runtimeFailure) throw runtimeFailure;
    try { healthy = (await fetch(`http://127.0.0.1:${runtimePort}/health`, { signal: AbortSignal.timeout(500) })).ok; } catch {}
    if (healthy) break;
    await delay(200);
  }
  if (!healthy) throw Error('Commentary runtime did not start');
  server.listen(port, '127.0.0.1', () => console.log(`Chesstories: http://127.0.0.1:${port} (Ctrl+C to stop)`));
} catch (error) { console.error(error.message); runtime.kill(); process.exitCode = 1; }
