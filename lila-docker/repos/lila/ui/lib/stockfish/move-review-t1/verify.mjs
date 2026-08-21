import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { spawn } from 'node:child_process';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const enginePath = resolve(process.argv[2] ?? join(here, 'assets', 'sf_18_smallnet_single.js'));
const wasmPath = enginePath.replace(/\.js$/, '.wasm');
const expectedHashes = {
  js: '57b1934a8207622278ba075b0e06d3c14f8cd8073880dcd2a2626b87e3e3e2fa',
  wasm: 'd81cf446e5f978ec4d1045c4ef9900a95f8d286780cf64926262e488b6281982',
};

const [javascript, wasm] = await Promise.all([readFile(enginePath), readFile(wasmPath)]);
assert.equal(sha256(javascript), expectedHashes.js, 'unexpected JavaScript artifact');
assert.equal(sha256(wasm), expectedHashes.wasm, 'unexpected WASM artifact');
assert.equal(wasmMemoryFlags(wasm) & 0x02, 0, 'WASM memory must not be shared');
await verifyUci(enginePath);
console.log('verified non-pthread UCI, readyok, Standard depth 16, and Chess960 depth 16');

function sha256(bytes) {
  return createHash('sha256').update(bytes).digest('hex');
}

function wasmMemoryFlags(bytes) {
  assert.equal(bytes.subarray(0, 4).toString('hex'), '0061736d', 'invalid WASM header');
  const cursor = { value: 8 };
  while (cursor.value < bytes.length) {
    const section = bytes[cursor.value++];
    const sectionSize = readUleb(bytes, cursor);
    const sectionEnd = cursor.value + sectionSize;
    if (section === 5) {
      assert.equal(readUleb(bytes, cursor), 1, 'expected one internal WASM memory');
      return readUleb(bytes, cursor);
    }
    cursor.value = sectionEnd;
  }
  throw new Error('WASM memory section not found');
}

function readUleb(bytes, cursor) {
  let result = 0;
  let shift = 0;
  while (true) {
    const byte = bytes[cursor.value++];
    result |= (byte & 0x7f) << shift;
    if ((byte & 0x80) === 0) return result >>> 0;
    shift += 7;
    if (shift > 28) throw new Error('invalid ULEB128 value');
  }
}

function verifyUci(path) {
  return new Promise((resolveVerification, rejectVerification) => {
    const engine = spawn(process.execPath, [path], { stdio: ['pipe', 'pipe', 'pipe'] });
    let output = '';
    let stderr = '';
    let pending = '';
    let phase = 'uci';
    let standardReachedDepth = false;
    let chess960ReachedDepth = false;
    let settled = false;

    const timeout = setTimeout(
      () => fail(new Error(`engine verification timed out in ${phase}\n${output}\n${stderr}`)),
      30_000,
    );

    const fail = error => {
      if (settled) return;
      settled = true;
      clearTimeout(timeout);
      engine.kill();
      rejectVerification(error);
    };

    engine.on('error', fail);
    engine.stderr.on('data', chunk => (stderr += chunk));
    engine.stdout.on('data', chunk => {
      pending += chunk;
      const lines = pending.split(/\r?\n/);
      pending = lines.pop() ?? '';
      for (const line of lines) {
        output += `${line}\n`;
        if (phase === 'uci' && line === 'uciok') {
          try {
            assert.match(output, /option name Threads type spin default 1 min 1 max 1/);
            assert.match(output, /option name Hash type spin default 16/);
            assert.match(output, /option name EvalFile type string default nn-4ca89e4b3abf\.nnue/);
          } catch (error) {
            fail(error);
            return;
          }
          phase = 'ready';
          engine.stdin.write('isready\n');
        } else if (phase === 'ready' && line === 'readyok') {
          phase = 'standard';
          engine.stdin.write('setoption name Hash value 16\nposition startpos\ngo depth 16\n');
        } else if (phase === 'standard' && /^info depth 16\b/.test(line)) {
          standardReachedDepth = true;
        } else if (phase === 'standard' && line.startsWith('bestmove ')) {
          if (!standardReachedDepth) {
            fail(new Error('Standard search stopped before depth 16'));
            return;
          }
          phase = 'chess960';
          engine.stdin.write(
            'setoption name UCI_Chess960 value true\n' +
              'position fen rkrbbnnq/pppppppp/8/8/8/8/PPPPPPPP/RKRBBNNQ w ACac - 0 1\n' +
              'go depth 16\n',
          );
        } else if (phase === 'chess960' && /^info depth 16\b/.test(line)) {
          chess960ReachedDepth = true;
        } else if (phase === 'chess960' && line.startsWith('bestmove ')) {
          if (!chess960ReachedDepth) {
            fail(new Error('Chess960 search stopped before depth 16'));
            return;
          }
          phase = 'done';
          engine.stdin.write('quit\n');
        }
      }
    });
    engine.on('exit', code => {
      if (settled) return;
      if (phase !== 'done' || code !== 0) {
        fail(new Error(`engine exited with code ${code} in ${phase}\n${output}\n${stderr}`));
        return;
      }
      settled = true;
      clearTimeout(timeout);
      resolveVerification();
    });
    engine.stdin.write('uci\n');
  });
}
