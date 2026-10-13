import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, moveReviewSan, type MoveReviewSubject } from '../../src/review/moveReview';

// This is the byte-exact actual choicev6 rejection before work:6 started.
// Admission keeps an investigation pending; it supplies no engine answer.
const fixtureBytes = readFileSync(new URL(
  '../../../judgment-evaluation/fixtures/public-commentary-v6/functional-choice-v6-work6-rejected-produced.json.gz',
  import.meta.url,
));
assert.equal(createHash('sha256').update(fixtureBytes).digest('hex'),
  'b32d1652a497d2fbd6a4678e79787c8820e15b99b7010717c7f8a493023da292');
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
const useId = 'function_value_use:616d852db4de93552ebe5d938974ea9f50b3fdbec357e1a1388a7d60cb29cb89';
const legalId = 'function_value_legal:5651b1818720488a121ab3a71b58d4974b6b93b49dbad36f1c9cabc9e0360f62';
const members = ['function_before', 'function_root_reference', 'function_root_alternative',
  'function_owned', 'function_changed', 'function_live', 'function_resource'];

function proposal(raw: any) {
  const found = raw.observations.selection.proposals.find((value: any) => value.id === proposalId);
  assert.ok(found, 'retain the exact actual task owner');
  return found;
}
function requirement(raw: any, id: string) {
  const found = proposal(raw).requirements.find((value: any) => value.id === id);
  assert.ok(found, `retain the actual ${id} requirement`);
  return found;
}

test('the unchanged owned functional own-choice survey is admitted with its original pending work and assessment', () => {
  const raw = fixture.snapshot, work = raw.issued_engine_work;
  const originalWork = JSON.stringify(work), originalAssessment = JSON.stringify(raw.assessment);
  assert.equal(fixture.result.pending, true);
  assert.equal(fixture.result.works, 6, 'the rejected seventh work has no submitted engine report');
  assert.equal(work.work_id, 'work:6');
  assert.deepEqual(work.search_anchor.prefix_moves_uci, ['d2b1', 'd8b8']);
  assert.deepEqual(requirement(raw, 'function_task_context').members, members);
  assert.deepEqual(requirement(raw, 'function_resource').predicate,
    { kind: 'legal_move', subject: { side: 'white', role: 'rook', square: 'd1' }, square: 'd6' });
  const position = raw.observations.positions.find((value: any) => value.id === work.search_anchor.position_id);
  assert.ok(position?.legal_moves.complete && position.legal_moves.moves.length === 50);
  assert.ok(['d1d6', 'a1a2'].every(move => position.legal_moves.moves.some((row: any) => row.uci === move)));
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'awaiting-evidence', 'admit the exact actual unrestricted prefix2 survey');
  assert.equal(decoded.issuedEngineWork.workId, work.work_id);
  assert.equal(decoded.issuedEngineWork.executionKeySha256, work.execution_key_sha256);
  assert.equal(decoded.issuedEngineWork.engineProfile, work.engine_profile);
  assert.deepEqual(decoded.issuedEngineWork.enginePositionMovesUci, work.engine_position_moves_uci);
  assert.equal(decoded.issuedEngineWork.searchFen, work.search_fen);
  assert.deepEqual(decoded.issuedEngineWork.rootRestriction, { kind: 'unrestricted' });
  assert.deepEqual(decoded.issuedEngineWork.searchLimits,
    { depth: 16, nodes: 20_000_000, movetimeMs: 5_000, multiPv: 3 });
  assert.equal(decoded.issuedEngineWork.maxSearchElapsedMs, 6_000);
  assert.ok(decoded.assessment, 'keep the current independent root numerical assessment');
  const kept = decoded.observations?.selection?.proposals?.find(value => value.id === proposalId);
  assert.ok(kept, 'keep the same supported task owner beside the issued work');
  const pending = kept.requirements.find(value => value.id === useId);
  assert.equal(pending?.state, 'awaiting_engine');
  assert.equal(pending?.work_id, 'work:6');
  assert.equal(pending?.answer, null, 'admission must not fabricate the use comparison answer');
  assert.ok(JSON.stringify(raw.assessment) === originalAssessment && JSON.stringify(work) === originalWork,
    'neither the root verdict nor the exact issued descriptor is rewritten');
});

const mutations: [string, (raw: any) => void][] = [
  ['another generator cannot authorize this resource task', raw => {
    proposal(raw).origin.question.producer = 'C-plan-counter';
  }],
  ['a context omitting the resource member cannot authorize a survey', raw => {
    requirement(raw, 'function_task_context').members = members.filter(id => id !== 'function_resource');
  }],
  ['an unanswered root comparison cannot authorize a descendant value', raw => {
    const root = requirement(raw, 'function_root');
    root.state = 'deferred'; root.answer = null; root.work_id = null;
  }],
  ['a different original rook cannot borrow the current resource fact', raw => {
    requirement(raw, legalId).predicate.subject.square = 'a1';
  }],
  ['a legal fact from another reply cannot authorize the current prefix', raw => {
    requirement(raw, legalId).path.source.moves_uci = ['d2b1', 'e8c7'];
  }],
  ['another work ID cannot borrow this awaiting comparison', raw => {
    requirement(raw, useId).work_id = 'work:5';
  }],
  ['an incomplete legal inventory cannot stand for a whole-position survey', raw => {
    const work = raw.issued_engine_work;
    raw.observations.positions.find((value: any) => value.id === work.search_anchor.position_id)
      .legal_moves.complete = false;
  }],
  ['a partial MultiPV cannot stand for the existing bounded whole-position survey', raw => {
    raw.issued_engine_work.search_limits.multi_pv = 2;
  }],
];

for (const [name, mutate] of mutations) test(name, () => {
  const raw = structuredClone(fixture.snapshot), originalAssessment = JSON.stringify(raw.assessment);
  mutate(raw);
  assert.ok(decodeMoveReviewSnapshot(raw, context) === undefined, name);
  assert.ok(JSON.stringify(raw.assessment) === originalAssessment,
    'the adversarial admission input does not change the separately recorded root judgment');
});
