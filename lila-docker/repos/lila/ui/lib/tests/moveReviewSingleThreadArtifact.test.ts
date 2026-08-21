import assert from 'node:assert/strict';
import { execFile } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { promisify } from 'node:util';
import { test } from 'node:test';

const execFileAsync = promisify(execFile);
const verifier = fileURLToPath(new URL('../stockfish/move-review-t1/verify.mjs', import.meta.url));
const engine = fileURLToPath(
  new URL('../stockfish/move-review-t1/assets/sf_18_smallnet_single.js', import.meta.url),
);

test('the bundled t1 artifact is non-pthread and reaches depth 16', { timeout: 30_000 }, async () => {
  const { stdout, stderr } = await execFileAsync(process.execPath, [verifier, engine], {
    timeout: 30_000,
  });
  assert.equal(stderr, '');
  assert.match(stdout, /verified non-pthread UCI, readyok, Standard depth 16, and Chess960 depth 16/);
});
