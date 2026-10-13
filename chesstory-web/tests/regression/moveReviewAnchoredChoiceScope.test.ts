import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, moveReviewSan, type MoveReviewSubject } from '../../src/review/moveReview';

// Exact actual H05 frame: the fourth completed survey owns two question names,
// while the next Black-response survey has not received an engine report.
const bytes = readFileSync(new URL(
  '../../../judgment-evaluation/fixtures/public-commentary-v6/anchored-choice-v10a-pending-shared-owner-produced.json.gz',
  import.meta.url,
));
assert.equal(createHash('sha256').update(bytes).digest('hex'),
  '5d3e27bffdfe51769be93222893c6d4d830cebe1db055b5cd9ac73c5317f59c7');
const fixture = JSON.parse(gunzipSync(bytes).toString('utf8'));
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
const proposalId = 'discovery:b071ecee7906a3969fe2b3f89a18375b33e71beaf6c982bd63bf8fdee01da20d';
const ownId = 'anchored_value_own:a86576574a889670d0588a0712fb37acae07d58572f3de634adaa424888c09b8';
const responseId = 'anchored_value_response:ce51e19cbd4106119c98a27761c89122a6b94162d3fb0162dd80c76c2c2d8ed2';

function proposal(raw: any) {
  const found = raw.observations.selection.proposals.find((value: any) => value.id === proposalId);
  assert.ok(found);
  return found;
}
function requirement(raw: any, id: string) {
  const found = proposal(raw).requirements.find((value: any) => value.id === id);
  assert.ok(found);
  return found;
}
function ownPool(raw: any) {
  const found = raw.observations.selection.response_searches.find((value: any) => value.work_id === 'work:3');
  assert.ok(found);
  return found;
}

test('one exact completed pool retains both timing names and admits the unanswered response survey', () => {
  const raw = fixture.snapshot, originalWork = JSON.stringify(raw.issued_engine_work);
  const originalAssessment = JSON.stringify(raw.assessment), pool = ownPool(raw);
  assert.equal(fixture.result.works, 4);
  assert.equal(fixture.result.pending, true);
  assert.equal(raw.issued_engine_work.work_id, 'work:4');
  assert.deepEqual(raw.issued_engine_work.search_anchor.prefix_moves_uci, ['f3e3', 'f7f6', 'e3f3']);
  assert.deepEqual(pool.proposal_ownership.question_refs.map((ref: any) => ref.requirement_id).sort(),
    [ownId, 'reserve_timing'].sort());
  assert.deepEqual(requirement(raw, ownId).answer, requirement(raw, 'reserve_timing').answer);
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'awaiting-evidence');
  assert.equal(decoded.issuedEngineWork.workId, 'work:4');
  assert.equal(decoded.issuedEngineWork.executionKeySha256, raw.issued_engine_work.execution_key_sha256);
  assert.deepEqual(decoded.issuedEngineWork.rootRestriction, { kind: 'unrestricted' });
  assert.deepEqual(decoded.issuedEngineWork.searchLimits,
    { depth: 16, nodes: 20_000_000, movetimeMs: 5_000, multiPv: 3 });
  const retained = decoded.observations?.selection?.proposals?.find(value => value.id === proposalId);
  assert.ok(retained);
  const pending = retained.requirements.find(value => value.id === responseId);
  assert.equal(pending?.state, 'awaiting_engine');
  assert.equal(pending?.answer, null, 'admission supplies no Black response value');
  assert.equal(pending?.work_id, 'work:4');
  const refs = decoded.observations!.selection!.response_searches!
    .find(value => value.work_id === 'work:3')!.proposal_ownership!.question_refs;
  assert.equal(refs.length, 2, 'retain both issued owners, including the original timing name');
  assert.ok(decoded.assessment, 'retain the separately verified root judgment');
  assert.equal(JSON.stringify(raw.issued_engine_work), originalWork);
  assert.equal(JSON.stringify(raw.assessment), originalAssessment);
});

const mutations: [string, (raw: any) => void][] = [
  ['the timing alias cannot borrow a pool from another prefix', raw => {
    requirement(raw, 'reserve_timing').prefix_moves_uci = ['f3e3', 'e5e6'];
  }],
  ['every alias must name a legal choice at this exact position', raw => {
    requirement(raw, 'reserve_timing').alternative_move = 'b2b4';
  }],
  ['the old timing name cannot borrow another receipt', raw => {
    requirement(raw, 'reserve_timing').work_id = 'work:2';
  }],
  ['duplicate owner names cannot substitute for two genuine issued refs', raw => {
    const refs = ownPool(raw).proposal_ownership.question_refs;
    refs[1] = structuredClone(refs[0]);
  }],
  ['the shared pool must retain its original execution key', raw => {
    ownPool(raw).proposal_ownership.execution_key_sha256 = '0'.repeat(64);
  }],
  ['another generator cannot grant the deeper response scope', raw => {
    proposal(raw).origin.question.producer = 'C-plan-counter';
  }],
  ['the response context must retain all current state facts', raw => {
    const id = responseId.slice('anchored_value_response:'.length);
    // Context keys follow the current position rather than the comparison key.
    const group = proposal(raw).requirements.find((row: any) => row.kind === 'fact_group' &&
      row.id.startsWith('anchored_value_context:') && row.members.some((member: string) =>
        requirement(raw, member).path?.source?.moves_uci?.length === 3));
    assert.ok(group, id);
    group.members.pop();
  }],
  ['an incomplete legal inventory cannot authorize the whole response survey', raw => {
    raw.observations.positions.find((row: any) =>
      row.id === raw.issued_engine_work.search_anchor.position_id).legal_moves.complete = false;
  }],
];
for (const [name, mutate] of mutations) test(name, () => {
  const raw = structuredClone(fixture.snapshot), assessment = JSON.stringify(raw.assessment);
  mutate(raw);
  assert.ok(decodeMoveReviewSnapshot(raw, context) === undefined, name);
  assert.equal(JSON.stringify(raw.assessment), assessment,
    'invalid investigation ownership never rewrites the separately recorded root judgment');
});
