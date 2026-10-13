import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, moveReviewSan, type MoveReviewSubject } from '../../src/review/moveReview';

// Unmodified actual work9 frame: three original-defense surveys were mistaken
// for ordinary investigations. This fixture contains no answer to work9.
const bytes = readFileSync(new URL(
  '../../../judgment-evaluation/fixtures/public-commentary-v6/anchored-choice-v10c-pending-root-family-produced.json.gz',
  import.meta.url,
));
assert.equal(createHash('sha256').update(bytes).digest('hex'),
  'ac71ccda70f3186e2c3a252b05b7c69912670eee24483b3530fdcc5e990e8317');
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
const familyId = 'discovery:821fd2accb10bd6c6aeebd7759dec5cb41ee12f8bc5bd6457f42ae2312229ccb';
const family = (raw: any) => raw.observations.selection.proposals.find((row: any) => row.id === familyId);
const root = (raw: any) => family(raw).requirements.find((row: any) => row.id === 'reserve_root');
const rootFacts = (raw: any, role: string) => family(raw).requirements.filter((row: any) =>
  row.kind === 'fact' && row.id.startsWith(`anchored_value_${role}:`) &&
  row.path.source.kind === 'declared' && row.path.source.moves_uci.length === 1);

test('both original pawn arms retain exact Black pools and the next unanswered survey', () => {
  const raw = fixture.snapshot;
  const work = JSON.stringify(raw.issued_engine_work), assessment = JSON.stringify(raw.assessment);
  assert.equal(fixture.result.works, 9);
  assert.equal(fixture.result.pending, true);
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'awaiting-evidence');
  assert.equal(decoded.issuedEngineWork.workId, 'work:9');
  assert.deepEqual(decoded.issuedEngineWork.searchAnchor?.prefixMovesUci, ['f3e3']);
  assert.deepEqual(decoded.issuedEngineWork.searchLimits,
    { depth: 17, nodes: 20_000_000, movetimeMs: 5_000, multiPv: 3 });
  const selection = decoded.observations!.selection!;
  assert.equal(selection.response_searches!.length, 6, 'retain all six exact completed surveys');
  const original = selection.response_searches!.filter(search => ['work:3', 'work:4', 'work:8'].includes(search.work_id));
  assert.equal(original.length, 3);
  assert.deepEqual(new Set(original.map(search => search.root_move)), new Set(['g3g4', 'f3e3']));
  assert.ok(original.every(search => search.proposal_ownership!.question_refs.some(ref => ref.proposal_id === familyId)));
  const pending = selection.proposals!.find(row => row.id === familyId)!.requirements.find(row =>
    row.kind === 'compare_moves' && row.work_id === 'work:9');
  assert.equal(pending?.state, 'awaiting_engine');
  assert.equal(pending?.answer, null, 'scope admission supplies no missing Black comparison value');
  assert.equal(root(raw).answer.required_depth, 17);
  assert.ok(original.some(search => search.candidates.every(candidate => candidate.evaluation.depth === 16)),
    'a retained lower-depth receipt is not a current answer');
  assert.equal(JSON.stringify(raw.issued_engine_work), work);
  assert.equal(JSON.stringify(raw.assessment), assessment);
});

const mutations: [string, (raw: any) => void][] = [
  ['another generator cannot grant original reserve-family scope', raw => {
    family(raw).origin.question.producer = 'C-plan-counter';
  }],
  ['a different original pawn cannot certify these reply pools', raw => {
    family(raw).requirements.find((row: any) => row.id === 'reserve_before').predicate.subject.square = 'e4';
  }],
  ['a root family must keep its complete three-fact context', raw => {
    family(raw).requirements.find((row: any) => row.id === 'reserve_context').members.pop();
  }],
  ['spending and retaining the original pawn have different current locations', raw => {
    for (const row of rootFacts(raw, 'location')) row.expected = 'g2';
  }],
  ['the original response actor belongs to the opponent', raw => {
    for (const row of rootFacts(raw, 'legal')) row.predicate.subject.side = 'white';
  }],
  ['a retained physical pool cannot borrow another execution key', raw => {
    raw.observations.selection.response_searches.find((row: any) => row.work_id === 'work:3')
      .proposal_ownership.execution_key_sha256 = '0'.repeat(64);
  }],
  ['an incomplete current legal inventory cannot authorize work9', raw => {
    raw.observations.positions.find((row: any) =>
      row.id === raw.issued_engine_work.search_anchor.position_id).legal_moves.complete = false;
  }],
];
for (const [name, mutate] of mutations) test(name, () => {
  const raw = structuredClone(fixture.snapshot), assessment = JSON.stringify(raw.assessment);
  mutate(raw);
  assert.ok(decodeMoveReviewSnapshot(raw, context) === undefined, name);
  assert.equal(JSON.stringify(raw.assessment), assessment);
});
