import { h, type VNode } from 'snabbdom';
import { moveReviewCopy, type MoveReviewViewState } from '../src/moveReview';
import { renderMoveReview, type MoveReviewPanelProps } from '../src/view/moveReview';
import { patch } from '../src/view/util';
import { observationFixtureJob, type CertifiedObservationFixture } from './moveReviewTestSupport';

// Research integration: actual public decoder, renderer, board and interactions.
// The saved response was produced by the real request/reducer/public protocol.
let disposePreview: (() => void) | undefined;
export async function loadWholeGameDelivery(gameId: string, plies: number) {
  disposePreview?.();
  const started = performance.now();
  const response = await fetch('whole-game-public/fixtures.json');
  if (!response.ok) throw Error(`Whole-game public response unavailable: ${response.status}`);
  const encoded = await response.text();
  const fetchedMs = performance.now() - started;
  const decodeStart = performance.now();
  const fixture = (JSON.parse(encoded) as CertifiedObservationFixture[]).find(f => f.name === gameId);
  if (!fixture) throw Error(`No production whole-game response for ${gameId}`);
  const job = observationFixtureJob(fixture);
  if (job.kind !== 'abstained' || !job.observations)
    throw Error('Expected real stopped independent observations');
  const observations = job.observations;
  const proof = observations.paths.find(p => p.moves.length === plies);
  if (!proof || proof.occurrenceIds.length !== plies) throw Error('Mainline lost in public decoding');
  const decodeMs = performance.now() - decodeStart;
  const view: MoveReviewViewState = { evidenceExpanded: true, expandedProofId: proof.id };
  const root = document.getElementById('role-preview');
  if (!root) throw Error('Missing role preview');
  let vnode: VNode | HTMLElement = root;
  disposePreview = () => { vnode = patch(vnode, h('div#role-preview')); };
  const actions: MoveReviewPanelProps['actions'] = {
    selectCandidate() {},
    toggleEvidence() {
      view.evidenceExpanded = !view.evidenceExpanded;
      redraw();
    },
    toggleProof(id) {
      view.expandedProofId = view.expandedProofId === id ? undefined : id;
      redraw();
    },
    previewFrame(frame) {
      view.hoveredFrame = frame;
      redraw();
    },
    clearPreview() {
      view.hoveredFrame = undefined;
      redraw();
    },
    pinFrame(frame) {
      view.pinnedFrame = frame;
      view.hoveredFrame = undefined;
      redraw();
    },
    clearPin() {
      view.pinnedFrame = undefined;
      redraw();
    },
    retry() {},
    addProof() {},
    viewAddedLine() {},
  };
  function redraw() {
    vnode = patch(
      vnode,
      h('div#role-preview', [
        renderMoveReview({
          job,
          view,
          copy: moveReviewCopy('en-US'),
          locale: 'en-US',
          orientation: 'white',
          actions,
          canWrite: false,
          liveEnginePaused: false,
        }),
      ]),
    );
  }
  function show(ply: number) {
    view.pinnedFrame = { proofId: proof!.id, ply, phase: 'after' };
    view.hoveredFrame = undefined;
    redraw();
  }
  const frameTimes: number[] = [];
  let renderedNotes = 0;
  let firstVisibleMs: number | undefined;
  for (let ply = 1; ply <= plies; ply++) {
    const frameStart = performance.now();
    show(ply);
    const expected =
      observations.highlights?.filter(n => n.occurrenceId === proof.occurrenceIds[ply - 1]) ?? [];
    const visible = document.querySelector('#role-preview .move-review__work')?.textContent ?? '';
    if (!expected.every(n => visible.includes(n.text))) throw Error(`Highlight delivery lost at ply ${ply}`);
    if (!expected.length && visible) throw Error(`Wrong occurrence's role text at ply ${ply}`);
    renderedNotes += expected.length;
    frameTimes.push(performance.now() - frameStart);
    // Include actual browser painting and keep the page responsive during the audit.
    await new Promise<void>(resolve => requestAnimationFrame(() => resolve()));
    if (expected.length) firstVisibleMs ??= performance.now() - started;
  }
  show(1);
  const report = {
    source: 'WholeGameRoleExport: actual stopped public response, no accepted engine evaluations',
    encodedBytes: new TextEncoder().encode(encoded).length,
    fetchedMs,
    decodeMs,
    totalDeliveryAnd109FrameAuditMs: performance.now() - started,
    firstVisibleMs,
    decodedPositions: observations.positions?.length,
    historicalOccurrences: proof.occurrenceIds.length,
    decodedHighlights: observations.highlights?.length ?? 0,
    renderedNotes,
    allFramesVerified: true,
    renderMs: frameTimes.reduce((a, b) => a + b, 0),
    maxFrameRenderMs: Math.max(...frameTimes),
    frameTimes,
  };
  return { report, show };
}
