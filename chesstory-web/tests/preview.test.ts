import assert from 'node:assert/strict';
import { test } from 'node:test';
import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, moveReviewCopy } from '../src/review/moveReview';
import { renderMoveReview } from '../src/review/view/moveReview';

function nodes(root: any): any[] {
  return root && typeof root === 'object' ? [root, ...(root.children ?? []).flatMap(nodes)] : [];
}

test('a typed reply outside the root pair opens its exact position, including before/after', () => {
  const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/lila-exit-browser.json.gz', import.meta.url,
  ))).toString());
  const live = fixture.cases.find((row: any) => row.id === 'new');
  const request = live.replies[0].request;
  const raw = live.replies.at(-1).body;
  const snapshot = decodeMoveReviewSnapshot(raw, { requestId: request.request_id,
    engineProfile: request.engine_profile, subject: {
      variant: 'standard', initialFen: request.initial_fen, movePrefixUci: request.move_prefix_uci,
      inputLines: request.input_lines,
      before: { path: '', fen: request.current_fen }, after: { path: 'aa', fen: request.focus.resulting_fen },
      played: { uci: request.focus.played_move_uci, san: 'e4' },
    } });
  assert.equal(snapshot?.kind, 'completed');
  const props: any = { job: { kind: 'completed', snapshot }, view: { evidenceExpanded: false },
    copy: moveReviewCopy('en-US'), locale: 'en-US', orientation: 'white', liveEnginePaused: false,
    actions: { pinFrame(frame: any) { props.view.pinnedFrame = frame; } } };
  const link = nodes(renderMoveReview(props)).find(node => node.data?.attrs?.['aria-label'] === 'Nxb4 · Explore position');
  assert.ok(link);
  link.data.on.click({ detail: 1 });
  const frame = props.view.pinnedFrame;
  const selected = snapshot!.observations!.selection!.candidates.find(c => c.selection === 'selected')!;
  assert.ok(!selected.lineIds.includes(frame.proofId), 'this must exercise a conditional reply, not the root pair');
  const previews = () => nodes(renderMoveReview(props)).filter(node => node.sel?.startsWith('section.move-review__comparison-preview'));
  assert.equal(previews().length, 1);
  props.view.pinnedFrame = { ...frame, phase: 'before' };
  assert.equal(previews().length, 1);
  props.view.pinnedFrame = { ...frame, ply: frame.ply - 1 };
  assert.equal(previews().length, 0, 'the text link must not license another occurrence on the same line');
  props.view.pinnedFrame = { ...frame, proofId: selected.lineIds[0] };
  assert.equal(previews().length, 0, 'a root line must not replace the linked reply');
});
