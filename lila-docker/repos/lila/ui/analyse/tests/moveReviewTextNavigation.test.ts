import assert from 'node:assert/strict';
import { test } from 'node:test';
import { commentaryMoveTokens } from '../src/moveReviewText.ts';
import type { MoveReviewProof } from '../src/moveReview';

test('commentary SAN keeps exact owners, repeated positions and complete notation', () => {
  const line = (id: string, labels: string[], fens = labels): MoveReviewProof => ({
    id, startFen: 'root', moves: labels.map((label, i) => ({ label, uci: `a${i + 1}a${i + 2}`, fenAfter: fens[i] })),
  } as MoveReviewProof);
  const left = line('left', ['Nf3', 'Nf6', 'Ng1', 'Ng8', 'Nf3']);
  const right = line('right', ['Nf3', 'd5']);
  const tokens = commentaryMoveTokens('Nf3 returns. Nf3+ and xNf3 are not this move.', [left, right]);
  assert.deepEqual(tokens.filter(token => token.frames.length), [{ text: 'Nf3', frames: [
    { proofId: 'left', ply: 1, phase: 'after' }, { proofId: 'left', ply: 5, phase: 'after' },
  ] }]);
  assert.deepEqual(commentaryMoveTokens('Nf3', [right])[0].frames.map(frame => frame.proofId), ['right']);
  assert.equal(commentaryMoveTokens('In the provided Nf3 line, the knight returns.', [left, right])
    .find(token => token.frames.length)!.frames.length, 1);
  assert.equal(commentaryMoveTokens('Nf3', [left, line('different-root', ['Nf3'], ['other-position'])])[0].frames.length, 3);
  const special = line('special', ['O-O', 'O-O-O+', 'b8=Q+', 'exd6', 'e4']);
  const text = 'O-O and O-O-O+; b8=Q+ follows exd6. After e4, the pawn on e4 controls d5.';
  const specialTokens = commentaryMoveTokens(text, [special]);
  assert.equal(specialTokens.map(token => token.text).join(''), text);
  assert.deepEqual(specialTokens.filter(token => token.frames.length).map(token => token.text),
    ['O-O', 'O-O-O+', 'b8=Q+', 'exd6', 'e4']);
  assert.deepEqual(commentaryMoveTokens('Qb1', [left]), [{ text: 'Qb1', frames: [] }]);
});
