import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, moveReviewSan, type MoveReviewSubject } from '../../src/review/moveReview';

// Exact native event after completed work:15; work:16 is still genuinely pending here.
const bytes = readFileSync(new URL(
  '../../../judgment-evaluation/fixtures/public-commentary-v6/functional-choice-v8b-prefix3-work16-produced.json.gz',
  import.meta.url,
));
assert.equal(createHash('sha256').update(bytes).digest('hex'),
  '61f55298b94d6a83a5232259e00030cd31ac6b8a1a808d381c47bf8b72ac32ca');
const fixture = JSON.parse(gunzipSync(bytes).toString('utf8'));
const rawSnapshot = fixture.report_response.snapshot;
const requestBytes = readFileSync(new URL(
  '../../../judgment-evaluation/fixtures/public-commentary-v6/functional-choice-v8b-original-request-produced.json',
  import.meta.url,
));
assert.equal(createHash('sha256').update(requestBytes).digest('hex'),
  '3ce6ca9b2b1319439a3dcbdb8d1852565a5c458c388bc2b4191f308f0f4b2ab4');
const request = JSON.parse(requestBytes.toString('utf8'))[0].request;
const context = {
  requestId: request.request_id,
  subject: {
    variant: 'standard', initialFen: request.initial_fen, movePrefixUci: request.move_prefix_uci,
    before: { path: '', fen: request.current_fen },
    played: { uci: request.focus.played_move_uci,
      san: moveReviewSan(request.current_fen, request.focus.played_move_uci)! },
    after: { path: 'aa', fen: request.focus.resulting_fen },
  } as MoveReviewSubject,
};
const taskId = 'discovery:3992d3d2ed16eb3083c102f5a0789f84a9620de3747ab3b32b22532ed32c8615';
const presentId = 'function_value_present:371dd3d72efcb89775f24158d86c1d92c271fbc0905103198defa26ac4683cac';
const pendingId = 'function_value_response:e354c63e090d83ea2dd84a85a0ff7af69474f2c0bb1cb0cab4be21ed76f888ff';
const issuedSeedId = 'function_value_response:402a7cafd0a7c3ef31b66b7370a98e7596610009e37b29e86f61f616b2c8e503';
const selectedId = 'function_value_response:572afcc39ef695d08c1f7919e786c1129197c262a3915bab8025fe8afac10a5d';

function task(raw: any) {
  const found = raw.observations.selection.proposals.find((row: any) => row.id === taskId);
  assert.ok(found, 'retain the genuine original task');
  return found;
}
function reading(raw: any, id: string) {
  const found = task(raw).requirements.find((row: any) => row.id === id);
  assert.ok(found, `retain the actual ${id} requirement`);
  return found;
}
function search(raw: any, workId: string) {
  const found = raw.observations.selection.response_searches.find((row: any) => row.work_id === workId);
  assert.ok(found, `retain the actual ${workId} receipt`);
  return found;
}

test('actual capture arrival retains original resource identity and genuine prefix3 response ownership', () => {
  const raw = rawSnapshot, work = raw.issued_engine_work;
  const assessment = JSON.stringify(raw.assessment);
  assert.equal(work.work_id, 'work:16');
  assert.deepEqual(work.search_anchor.prefix_moves_uci, ['d2b1', 'e8d6', 'd1d6']);
  const beforeResource = raw.observations.positions.find((row: any) =>
    row.id === search(raw, 'work:9').child_position_id);
  assert.ok(beforeResource);
  assert.equal(moveReviewSan(beforeResource.fen, 'd1d6'), 'Rxd6',
    'the actual resource takes the knight; this is not a fabricated quiet arrival');
  assert.deepEqual(reading(raw, presentId).predicate,
    { kind: 'presence', subject: { side: 'white', role: 'rook', square: 'd1' } });
  assert.deepEqual(reading(raw, presentId).path.source.moves_uci, ['d2b1', 'e8d6', 'd1d6']);
  assert.equal(reading(raw, presentId).state, 'supported');
  const completed = search(raw, 'work:15');
  assert.deepEqual(completed.prefix_moves_uci, ['d2b1', 'e8c7', 'd1d6']);
  assert.deepEqual(completed.proposal_ownership.question_refs,
    [{ proposal_id: taskId, requirement_id: issuedSeedId }]);
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'awaiting-evidence', 'admit the genuine prefix3 work after resource capture');
  assert.equal(decoded.issuedEngineWork.executionKeySha256, work.execution_key_sha256);
  assert.deepEqual(decoded.issuedEngineWork.enginePositionMovesUci, work.engine_position_moves_uci);
  assert.deepEqual(decoded.issuedEngineWork.rootRestriction, { kind: 'unrestricted' });
  const kept = decoded.observations?.selection?.proposals?.find(row => row.id === taskId);
  assert.ok(kept);
  assert.ok(JSON.stringify(decoded.observations?.selection?.response_searches) ===
    JSON.stringify(raw.observations.selection.response_searches), 'retain every actual completed receipt unchanged');
  const seed = kept.requirements.find(row => row.id === issuedSeedId);
  assert.equal(seed?.state, 'stopped');
  assert.equal(seed?.answer, null, 'the original named refusal is not silently answered by another competitor');
  const selected = kept.requirements.find(row => row.id === selectedId);
  assert.equal(selected?.state, 'answered');
  assert.equal(selected?.work_id, 'work:15');
  assert.ok(JSON.stringify(selected?.answer) === JSON.stringify(reading(raw, selectedId).answer),
    'the actual completed resistance comparison keeps its own exact numeric owner');
  const pending = kept.requirements.find(row => row.id === pendingId);
  assert.equal(pending?.state, 'awaiting_engine');
  assert.equal(pending?.work_id, 'work:16');
  assert.equal(pending?.answer, null, 'this historical frame contains no successful work16 report yet');
  assert.ok(JSON.stringify(raw.assessment) === assessment, 'no borrowed child score changes the root assessment');
});

const mutations: [string, (raw: any) => void][] = [
  ['another original rook cannot borrow the actual capture-arrival presence', raw => {
    reading(raw, presentId).predicate.subject.square = 'a1';
  }],
  ['presence from another actual reply cannot authorize the current prefix3 work', raw => {
    reading(raw, presentId).path.source.moves_uci = ['d2b1', 'e8c7', 'd1d6'];
  }],
  ['missing original-resource presence cannot authorize a Black response survey', raw => {
    reading(raw, presentId).state = 'deferred';
  }],
  ['another real response work cannot borrow this awaiting prefix3 owner', raw => {
    reading(raw, pendingId).work_id = 'work:15';
  }],
  ['a genuine different reply owner cannot certify the retained completed prefix3 pool', raw => {
    search(raw, 'work:15').proposal_ownership.question_refs[0].requirement_id = pendingId;
  }],
];

for (const [name, mutate] of mutations) test(name, () => {
  const raw = structuredClone(rawSnapshot), assessment = JSON.stringify(raw.assessment);
  mutate(raw);
  assert.ok(decodeMoveReviewSnapshot(raw, context) === undefined, name);
  assert.ok(JSON.stringify(raw.assessment) === assessment, 'reject the scope mutation without changing root judgment');
});
