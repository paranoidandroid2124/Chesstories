import assert from 'node:assert/strict';
import { afterEach, test } from 'node:test';
import { createStudyFromAnalysis } from '../src/studyApi';

const originalFetch = globalThis.fetch;

afterEach(() => {
  globalThis.fetch = originalFetch;
});

test('createStudyFromAnalysis submits review study setup fields', async () => {
  let capturedUrl = '';
  let capturedBody = '';

  globalThis.fetch = (async (url, init) => {
    capturedUrl = String(url);
    capturedBody = String(init?.body);
    return new Response('{}', { headers: { 'content-type': 'application/json' } });
  }) as typeof fetch;

  await createStudyFromAnalysis({
    pgn: '1. e4 e5 *',
    orientation: 'white',
    name: 'ych24 vs RojoCapo review',
    chapterName: 'Opening to middlegame',
    visibility: 'private',
  });

  const form = new URLSearchParams(capturedBody);
  assert.equal(capturedUrl, '/study');
  for (const [field, value] of [
    ['pgn', '1. e4 e5 *'],
    ['as', 'study'],
    ['orientation', 'white'],
    ['name', 'ych24 vs RojoCapo review'],
    ['chapterName', 'Opening to middlegame'],
    ['visibility', 'private'],
  ])
    assert.equal(form.get(field), value);
});
