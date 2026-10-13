import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, moveReviewSan, type MoveReviewSubject } from '../../src/review/moveReview';

// Byte-exact choicev7b output: six actual completed receipts and an unqueried work:10.
// Reselecting a competitor does not transfer the completed survey's issued owner.
const fixtureBytes = readFileSync(new URL(
  '../../../judgment-evaluation/fixtures/public-commentary-v6/functional-choice-v7b-work10-six-receipts-produced.json.gz',
  import.meta.url,
));
assert.equal(createHash('sha256').update(fixtureBytes).digest('hex'),
  'f054f0368083c4b1bbcd98902cfbbbb349baf82cfa2379c2014f165384545c94');
const fixture = JSON.parse(gunzipSync(fixtureBytes).toString('utf8'));
const request = fixture.request;
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
const proposalId = 'discovery:3992d3d2ed16eb3083c102f5a0789f84a9620de3747ab3b32b22532ed32c8615';
const seedId = 'function_value_use:616d852db4de93552ebe5d938974ea9f50b3fdbec357e1a1388a7d60cb29cb89';
const selectedId = 'function_value_use:9c6f57285a1059aca63fb0552ebe53972f706e32646443dde609cd99486b1cfd';
const otherReplyId = 'function_value_use:2e177e95aba40327f17154215829276f6d3081d89b0c8e197b5eba83c275915e';
const legalId = 'function_value_legal:5651b1818720488a121ab3a71b58d4974b6b93b49dbad36f1c9cabc9e0360f62';
const pendingId = 'function_value_use:3f2e1d60883f5d895f5d06f14e5e6caf0684f1e47ae4166eeb08c0a6878db38c';

function proposal(raw: any) {
  const found = raw.observations.selection.proposals.find((value: any) => value.id === proposalId);
  assert.ok(found, 'keep the actual generated task');
  return found;
}
function requirement(raw: any, id: string) {
  const found = proposal(raw).requirements.find((value: any) => value.id === id);
  assert.ok(found, `keep the actual ${id} reading`);
  return found;
}
function search(raw: any, workId: string) {
  const found = raw.observations.selection.response_searches.find((value: any) => value.work_id === workId);
  assert.ok(found, `keep the actual ${workId} receipt`);
  return found;
}

test('six actual response receipts survive competitor reselection without answering the old survey seed', () => {
  const raw = fixture.snapshot, work = raw.issued_engine_work;
  const assessment = JSON.stringify(raw.assessment), issued = JSON.stringify(work);
  assert.equal(fixture.result.pending, true);
  assert.equal(fixture.result.works, 10, 'work:10 has never received an engine report');
  assert.equal(work.work_id, 'work:10');
  assert.deepEqual(work.search_anchor.prefix_moves_uci, ['d2c4', 'c6c7']);
  assert.deepEqual(raw.observations.selection.response_searches.map((row: any) => row.work_id),
    ['work:4', 'work:5', 'work:6', 'work:7', 'work:8', 'work:9']);
  const full = search(raw, 'work:6'), exact = search(raw, 'work:7');
  assert.deepEqual(full.prefix_moves_uci, ['d2b1', 'd8b8']);
  assert.deepEqual(full.root_restriction, { kind: 'unrestricted' });
  assert.deepEqual(full.proposal_ownership.question_refs,
    [{ proposal_id: proposalId, requirement_id: seedId }]);
  assert.deepEqual(exact.root_restriction, { kind: 'restricted', moves_uci: ['d1d6', 'e2d3'] });
  assert.deepEqual(exact.proposal_ownership.question_refs,
    [{ proposal_id: proposalId, requirement_id: selectedId }]);
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'awaiting-evidence', 'admit the genuine collection and pending next task');
  assert.equal(decoded.issuedEngineWork.workId, work.work_id);
  assert.equal(decoded.issuedEngineWork.executionKeySha256, work.execution_key_sha256);
  assert.deepEqual(decoded.issuedEngineWork.enginePositionMovesUci, work.engine_position_moves_uci);
  const keptSelection = decoded.observations?.selection;
  assert.ok(JSON.stringify(keptSelection?.response_searches) ===
    JSON.stringify(raw.observations.selection.response_searches), 'retain all exact completed receipt objects');
  const kept = keptSelection?.proposals?.find(row => row.id === proposalId);
  assert.ok(kept);
  const seed = kept.requirements.find(row => row.id === seedId);
  assert.equal(seed?.state, 'stopped');
  assert.equal(seed?.stop_reason, 'not_required_for_decision');
  assert.equal(seed?.work_id, null);
  assert.equal(seed?.answer, null, 'the survey omitted the resource; it did not answer the seed pair');
  const selected = kept.requirements.find(row => row.id === selectedId);
  assert.equal(selected?.state, 'answered');
  assert.equal(selected?.work_id, 'work:7');
  assert.ok(JSON.stringify(selected?.answer) === JSON.stringify(requirement(raw, selectedId).answer),
    'retain only the genuinely completed selected pair answer and its exact numeric owner');
  const pending = kept.requirements.find(row => row.id === pendingId);
  assert.equal(pending?.state, 'awaiting_engine');
  assert.equal(pending?.work_id, 'work:10');
  assert.equal(pending?.answer, null);
  assert.ok(decoded.assessment, 'keep the independent original-root grade');
  assert.ok(JSON.stringify(raw.assessment) === assessment && JSON.stringify(work) === issued,
    'admission rewrites neither the root assessment nor the pending work');
});

const mutations: [string, (raw: any) => void][] = [
  ['a real foreign proposal owner cannot borrow the retained prefix2 survey', raw => {
    search(raw, 'work:6').proposal_ownership.question_refs =
      structuredClone(search(raw, 'work:4').proposal_ownership.question_refs);
  }],
  ['an any-group cannot authorize the exact all-seven functional task', raw => {
    requirement(raw, 'function_task_context').mode = 'any';
  }],
  ['another original rook cannot borrow a retained functional survey', raw => {
    requirement(raw, legalId).predicate.subject.square = 'a1';
  }],
  ['the genuine different-reply reading cannot own this completed pool', raw => {
    search(raw, 'work:6').proposal_ownership.question_refs[0].requirement_id = otherReplyId;
  }],
  ['the other original root occurrence cannot authorize this task branch', raw => {
    const answer = requirement(raw, 'function_root').answer;
    answer.reference_occurrence_id = answer.alternative_occurrence_id;
  }],
  ['another real work ID cannot certify the completed survey', raw => {
    search(raw, 'work:6').proposal_ownership.work_id = 'work:5';
  }],
  ['another real execution key cannot certify the completed survey', raw => {
    search(raw, 'work:6').proposal_ownership.execution_key_sha256 =
      search(raw, 'work:8').execution_key_sha256;
  }],
  ['six ordinary unowned receipts cannot borrow the functional allowance', raw => {
    for (const row of raw.observations.selection.response_searches) delete row.proposal_ownership;
  }],
];

for (const [name, mutate] of mutations) test(name, () => {
  const raw = structuredClone(fixture.snapshot), assessment = JSON.stringify(raw.assessment);
  mutate(raw);
  assert.ok(decodeMoveReviewSnapshot(raw, context) === undefined, name);
  assert.ok(JSON.stringify(raw.assessment) === assessment,
    'adversarial ownership input leaves the separately recorded root assessment unchanged');
});
