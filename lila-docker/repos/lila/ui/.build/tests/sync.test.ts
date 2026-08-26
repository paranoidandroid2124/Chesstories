import assert from 'node:assert/strict';
import { mkdir, mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { test } from 'node:test';
import { resolveSyncSrc } from '../src/sync.ts';

test('dependency glob prefers its package and falls back only when absent', async t => {
  const root = await mkdtemp(join(tmpdir(), 'chesstory-sync-'));
  t.after(() => rm(root, { recursive: true, force: true }));

  const src = 'ui/lib/node_modules/@scope/engine/*.{js,wasm}';
  const fallback = 'node_modules/@scope/engine/*.{js,wasm}';
  const localDir = join(root, 'ui/lib/node_modules/@scope/engine');
  const fallbackDir = join(root, 'node_modules/@scope/engine');

  await Promise.all([mkdir(localDir, { recursive: true }), mkdir(fallbackDir, { recursive: true })]);
  await Promise.all([
    writeFile(join(localDir, 'engine.js'), ''),
    writeFile(join(fallbackDir, 'engine.js'), ''),
  ]);

  assert.equal(await resolveSyncSrc(root, src), src);
  await rm(localDir, { recursive: true });
  assert.equal(await resolveSyncSrc(root, src), fallback);
});
