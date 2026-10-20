import assert from 'node:assert/strict';
import { test } from 'node:test';
import { existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, utimesSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { prepare } from './prepare.mjs';

test('preparation detects edited, added and removed inputs, missing outputs and manual rebuilds', t => {
  const root = mkdtempSync(join(tmpdir(), 'chesstories-prepare-'));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  mkdirSync(join(root, 'src'));
  writeFileSync(join(root, 'src/main'), 'first');
  let builds = 0;
  const options = { root, label: 'Check', inputs: ['src'], outputs: () => ['dist'], directory: 'dist',
    build() { builds++; mkdirSync(join(root, 'dist'), { recursive: true }); writeFileSync(join(root, 'dist/app'), `build ${builds}`); } };
  assert.equal(prepare(options), true);
  assert.equal(prepare(options), false);
  utimesSync(join(root, 'src/main'), new Date(0), new Date(0));
  assert.equal(prepare(options), false, 'timestamps alone do not rebuild unchanged source');
  writeFileSync(join(root, 'src/main'), 'edited');
  assert.equal(prepare(options), true);
  writeFileSync(join(root, 'src/extra'), 'added');
  assert.equal(prepare(options), true);
  rmSync(join(root, 'src/extra'));
  assert.equal(prepare(options), true);
  rmSync(join(root, 'dist/app'));
  assert.equal(prepare(options), true);
  writeFileSync(join(root, 'dist/app'), 'a manually built different revision');
  assert.equal(prepare(options), true);
  assert.equal(prepare({ ...options, version: 'new toolchain' }), true);
  assert.equal(prepare({ ...options, version: 'new toolchain' }), false);
});

test('failed updates invalidate old readiness, including when source is reverted afterward', t => {
  const root = mkdtempSync(join(tmpdir(), 'chesstories-failed-update-'));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  writeFileSync(join(root, 'source'), 'old');
  const options = { root, label: 'Check', inputs: ['source'], directory: 'dist', outputs: () => ['dist'],
    build() { mkdirSync(join(root, 'dist'), { recursive: true }); writeFileSync(join(root, 'dist/app'), readFileSync(join(root, 'source'))); } };
  prepare(options);
  writeFileSync(join(root, 'source'), 'new');
  assert.throws(() => prepare({ ...options, build() {
    writeFileSync(join(root, 'dist/app'), 'partial');
    throw Error('build failed');
  } }), /build failed/);
  assert.equal(existsSync(join(root, 'dist/.chesstories-prepared.json')), false);
  writeFileSync(join(root, 'source'), 'old');
  assert.equal(prepare(options), true);
  assert.equal(readFileSync(join(root, 'dist/app'), 'utf8'), 'old');
  assert.equal(prepare(options), false);
});
