import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, moveReviewSan, type MoveReviewSubject } from '../src/moveReview';

const load = (name: string, digest: string) => {
  const bytes = readFileSync(new URL(
    `../../../../../../judgment-evaluation/fixtures/public-commentary-v6/${name}`, import.meta.url));
  assert.equal(createHash('sha256').update(bytes).digest('hex'), digest);
  return JSON.parse(gunzipSync(bytes).toString('utf8'));
};
// Unmodified native H05 work17 frame. Its earlier completed work14/15 really
// reached19. The pending descriptor controls below do not create engine results.
const pending = load('h05-depth18-pending17-historical-produced.json.gz',
  '3911248de36544c8fd1afdf39153a68c88275e28f8900e999de92e9fe3f97f56');
const pendingRequest = pending.request.request;
const completed = load('reserve-family-completed43-controlled10i6.json.gz',
  '07e9524a336aa97442a464611934307b789335b3688621379b362391ae6333ac');
const context = (request: any) => ({
  requestId: request.request_id,
  engineProfile: request.engine_profile,
  subject: {
    variant: 'standard', initialFen: request.initial_fen, movePrefixUci: request.move_prefix_uci,
    before: { path: '', fen: request.current_fen },
    played: { uci: request.focus.played_move_uci,
      san: moveReviewSan(request.current_fen, request.focus.played_move_uci)! },
    after: { path: 'aa', fen: request.focus.resulting_fen },
  } as MoveReviewSubject,
});
// The native execution key covers work identity, full history, anchor, roots,
// depth and all time/node limits. Never retain a depth20 key on an18/19 control.
const executionKey = (work: any) => {
  const anchor = work.search_anchor;
  const prefix = anchor?.prefix_moves_uci ?? (anchor ? [anchor.root_move] : []);
  const fields = [work.engine_profile, work.work_id, work.purpose, work.engine_position_initial_fen,
    work.engine_position_moves_uci.join(','), work.search_fen,
    work.root_restriction.kind === 'unrestricted' ? 'unrestricted' :
      `restricted:${work.root_restriction.moves_uci.join(',')}`,
    String(work.search_limits.depth), String(work.search_limits.nodes),
    String(work.search_limits.movetime_ms), String(work.search_limits.multi_pv),
    String(work.max_search_elapsed_ms),
    ...(anchor ? [anchor.root_move, anchor.root_occurrence_id, anchor.position_id,
      ...(prefix.length > 1 ? [prefix.join(',')] : [])] : [])];
  return createHash('sha256').update(fields.join('\0')).digest('hex');
};
const standardPending = (depth: number) => {
  const raw = structuredClone(pending.snapshot), work = raw.issued_engine_work;
  assert.equal(work.execution_key_sha256, executionKey(work));
  Object.assign(work.search_limits, { depth, nodes: 20_000_000, movetime_ms: 5_000 });
  work.max_search_elapsed_ms = 6_000;
  work.execution_key_sha256 = executionKey(work);
  return raw;
};

test('current owned pending depth18 preserves genuine earlier depth19 receipts and the independent grade', () => {
  const raw = standardPending(18), before = JSON.stringify(raw);
  const searches = raw.observations.selection.response_searches;
  assert.ok(searches.some((row: any) => row.required_depth === 19 &&
    row.candidates.every((candidate: any) => candidate.evaluation.depth === 19)));
  const owners = raw.observations.selection.proposals.flatMap((proposal: any) =>
    proposal.requirements.filter((reading: any) => reading.kind === 'compare_moves' &&
      reading.state === 'awaiting_engine' && reading.work_id === 'work:17'));
  assert.ok(owners.length > 0, 'the test uses the genuine currently pending family, not an anonymous search');
  const decoded = decodeMoveReviewSnapshot(raw, context(pendingRequest));
  assert.equal(decoded?.kind, 'awaiting-evidence');
  if (decoded?.kind !== 'awaiting-evidence') assert.fail('depth18 should remain executable');
  assert.equal(decoded.issuedEngineWork.searchLimits.depth, 18);
  assert.equal(decoded.issuedEngineWork.executionKeySha256, executionKey(raw.issued_engine_work));
  assert.ok(JSON.stringify(decoded.observations?.selection?.response_searches) === JSON.stringify(searches),
    'historical19 receipts are retained without rebinding depth, score, owner or key');
  assert.ok(decoded.assessment, 'the independently completed root judgment remains present');
  assert.deepEqual(raw.assessment, pending.snapshot.assessment);
  assert.equal(JSON.stringify(raw), before);
});

test('the same pending exact owner at depth19 is rejected even with a correct new execution key', () => {
  const at18 = standardPending(18), at19 = standardPending(19), before = JSON.stringify(at19);
  assert.notEqual(at18.issued_engine_work.execution_key_sha256, at19.issued_engine_work.execution_key_sha256);
  assert.equal(at19.issued_engine_work.execution_key_sha256, executionKey(at19.issued_engine_work));
  assert.deepEqual(at19.observations, at18.observations);
  assert.deepEqual(at19.assessment, at18.assessment);
  assert.ok(decodeMoveReviewSnapshot(at19, context(pendingRequest)) === undefined,
    'owned and legal historical continuations cannot authorize a new query above18');
  assert.equal(JSON.stringify(at19), before);
});

test('already completed higher-depth receipts remain readable with their original prices and numerical judgment', () => {
  const raw = completed.snapshot, before = JSON.stringify(raw);
  assert.ok(raw.observations.selection.response_searches.some((row: any) => row.required_depth === 19));
  const decoded = decodeMoveReviewSnapshot(raw, context(completed.request));
  assert.equal(decoded?.kind, 'completed');
  assert.ok(JSON.stringify(decoded?.observations?.selection?.response_searches) ===
    JSON.stringify(raw.observations.selection.response_searches));
  assert.equal(JSON.stringify(raw), before,
    'new-work18 admission does not rewrite completed results, proof, price or grade');
});
