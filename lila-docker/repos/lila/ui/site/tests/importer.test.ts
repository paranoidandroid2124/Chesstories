import { test } from 'node:test';
import assert from 'node:assert/strict';
import { JSDOM } from 'jsdom';
import { initImporter } from '../src/site.importer';

const dom = new JSDOM('<!doctype html>');
Object.assign(globalThis, { window: dom.window, document: dom.window.document });

test('rejects submission while reading, preserves manual edits, and pre-rejects oversized files', async () => {
  document.body.innerHTML = `
    <form id="import-workspace"><textarea id="import-pgn" data-import-byte-limit="200000" data-import-character-limit="200000"></textarea>
      <input id="import-file" type="file"><p id="import-workspace-status"></p>
      <button id="import-submit" type="submit">Open</button></form>`;
  initImporter();

  const form = document.getElementById('import-workspace') as HTMLFormElement;
  const pgn = document.getElementById('import-pgn') as HTMLTextAreaElement;
  const file = document.getElementById('import-file') as HTMLInputElement;
  const submit = document.getElementById('import-submit') as HTMLButtonElement;
  let resolveBytes!: (value: ArrayBuffer) => void;
  const pending = {
    name: 'game.pgn',
    size: 12,
    arrayBuffer: () => new Promise<ArrayBuffer>(resolve => (resolveBytes = resolve)),
  } as File;
  Object.defineProperty(file, 'files', { configurable: true, value: [pending] });

  file.dispatchEvent(new window.Event('change'));
  assert.equal(submit.disabled, true);
  const waiting = new window.Event('submit', { cancelable: true });
  form.dispatchEvent(waiting);
  assert.equal(waiting.defaultPrevented, true);

  pgn.value = 'manual game';
  pgn.dispatchEvent(new window.Event('input'));
  resolveBytes(new TextEncoder().encode('file game').buffer as ArrayBuffer);
  await Promise.resolve();
  await Promise.resolve();
  assert.equal(pgn.value, 'manual game');
  assert.equal(submit.disabled, false);

  let triedToRead = false;
  const oversized = {
    name: 'large.pgn',
    size: 200001,
    arrayBuffer: () => {
      triedToRead = true;
      return Promise.resolve(new ArrayBuffer(0));
    },
  } as File;
  Object.defineProperty(file, 'files', { configurable: true, value: [oversized] });
  file.dispatchEvent(new window.Event('change'));
  assert.equal(triedToRead, false);
});
