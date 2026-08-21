import assert from 'node:assert/strict';
import { test } from 'node:test';
import type { TreeWrapper } from 'lib/tree';
import type { MoveReviewProof } from '../src/moveReview';
import { mergeMoveReviewProofIntoStudy } from '../src/moveReviewStudy';
import { afterFen, beforeFen, continuationFen, subject } from './moveReviewTestSupport';

test('merges a literal backend proof from P and reuses Q without replaying it', async t => {
  const proof: MoveReviewProof = {
    id: 'proof.study',
    startFen: beforeFen,
    moves: [
      { uci: 'e7e5', label: 'e7e5', fenAfter: afterFen },
      { uci: 'g1f3', label: 'g1f3', fenAfter: continuationFen },
    ],
    annotations: [],
  };

  const after = {
    id: 'bb',
    ply: 2,
    uci: 'e7e5',
    san: 'e5',
    fen: afterFen,
    children: [],
  } as Tree.Node;
  const before = {
    id: 'aa',
    ply: 1,
    uci: 'e2e4',
    san: 'e4',
    fen: beforeFen,
    children: [after],
  } as Tree.Node;
  const nodes = new Map<Tree.Path, Tree.Node>([
    [subject.before.path, before],
    [subject.after.path, after],
  ]);
  const addedParents: Tree.Path[] = [];
  const tree = {
    nodeAtPath: (path: Tree.Path) => nodes.get(path),
    addNode: (node: Tree.Node, path: Tree.Path) => {
      const parent = nodes.get(path);
      if (!parent) return;
      parent.children.push(node);
      const nextPath = (path + node.id) as Tree.Path;
      nodes.set(nextPath, node);
      addedParents.push(path);
      return nextPath;
    },
  } as unknown as TreeWrapper;

  const calls: Array<{ url: string; body: Record<string, unknown> }> = [];
  const originalFetch = globalThis.fetch;
  t.after(() => {
    globalThis.fetch = originalFetch;
  });
  globalThis.fetch = (async (input: RequestInfo | URL, init?: RequestInit): Promise<Response> => {
    calls.push({ url: String(input), body: JSON.parse(String(init?.body)) as Record<string, unknown> });
    return new Response(
      JSON.stringify({
        ch: 'chapter1',
        path: 'aabbcc',
        node: {
          id: 'cc',
          ply: 3,
          uci: 'g1f3',
          san: 'backend-Nf3',
          fen: continuationFen,
          children: [],
        },
      }),
      { status: 200, headers: { 'content-type': 'application/json' } },
    );
  }) as typeof fetch;

  const currentPath = subject.after.path;
  const addedPath = await mergeMoveReviewProofIntoStudy(
    tree,
    { id: 'study123', chapterId: 'chapter1' },
    subject,
    proof,
  );

  assert.equal(calls.length, 1, 'the existing played-move node must not be posted again');
  assert.equal(calls[0]?.url, '/api/study/study123/chapter1/ana-move');
  assert.deepEqual(calls[0]?.body.d, {
    orig: 'g1',
    dest: 'f3',
    fen: afterFen,
    path: subject.after.path,
    variant: 'standard',
    ch: 'chapter1',
  });
  assert.deepEqual(addedParents, [subject.after.path]);
  assert.equal(currentPath, subject.after.path, 'the merge helper has no main-board cursor mutation');
  assert.equal(addedPath, 'aabbcc');
});
