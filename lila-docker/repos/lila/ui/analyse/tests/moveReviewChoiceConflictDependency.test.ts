import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeCommentarySelection } from '../src/moveReviewSelection';
import type { MoveReviewObservations } from '../src/moveReview';

const load = (name: string, digest: string) => {
  const bytes = readFileSync(new URL(
    `../../../../../../judgment-evaluation/fixtures/public-commentary-v6/${name}`, import.meta.url));
  assert.equal(createHash('sha256').update(bytes).digest('hex'), digest);
  return JSON.parse(gunzipSync(bytes).toString('utf8'));
};
// Native work21 is unanswered and has a research-only lease. Test the public
// selection seam against its genuinely accepted production graph projection.
const fixture = load('anchored-choice-v10e-work21-conflict-dependencies-produced.json.gz',
  '63f34201085160954147ffeb6db6eb69e860b36d3d2fce6eea911608cbe94f95');
const observations = load('anchored-choice-v10e-work21-projected-observations.json.gz',
  '444ed9fe7dd716c7ddd685b900447ffe13c1dd1444517e4033a99d105a2117d3') as MoveReviewObservations;
const rawSelection = fixture.snapshot.observations.selection;
const context = { rootFen: fixture.request.current_fen,
  playedMove: fixture.request.focus.played_move_uci, engineProfile: fixture.request.engine_profile };
const familyId = 'discovery:821fd2accb10bd6c6aeebd7759dec5cb41ee12f8bc5bd6457f42ae2312229ccb';
const family = (selection: any) => selection.proposals.find((row: any) => row.id === familyId);
const conflicts = (selection: any) => selection.proposals.filter((row: any) =>
  row.origin.question.producer === 'C-comparison-conflict');
const sources = (selection: any, conflict: any) => conflict.origin.question.source_refs.map((id: string) =>
  selection.response_searches.find((row: any) => row.work_id === id));
const decode = (selection: any) => decodeCommentarySelection(selection, observations, context);

test('five exact conflict children remain dependencies without answering native work21', () => {
  const selectionBefore = JSON.stringify(rawSelection);
  const assessmentBefore = JSON.stringify(fixture.snapshot.assessment);
  const decoded = decode(rawSelection);
  assert.ok(!!decoded, 'the existing 19 physical receipts must survive their exact dependency classification');
  assert.equal(decoded!.response_searches!.length, 19);
  const children = decoded!.response_searches!.filter(row =>
    ['work:8', 'work:11', 'work:14', 'work:17', 'work:20'].includes(row.work_id));
  assert.equal(children.length, 5);
  assert.ok(JSON.stringify(decoded!.response_searches) === JSON.stringify(rawSelection.response_searches),
    'classification preserves every receipt, value, line, occurrence, work and execution key');
  const pending = decoded!.proposals!.find(row => row.id === familyId)!.requirements.find(row =>
    row.kind === 'compare_moves' && row.work_id === 'work:21');
  assert.equal(pending?.state, 'awaiting_engine');
  assert.equal(pending?.answer, null, 'a ledger slot supplies no missing comparison value');
  const root = family(decoded).requirements.find((row: any) => row.id === 'reserve_root');
  assert.equal(root.answer.work_id, 'work:1');
  assert.equal(root.answer.reference_evaluation.white_score.value, 0);
  assert.equal(root.answer.alternative_evaluation.white_score.value, 275);
  assert.ok(JSON.stringify(rawSelection) === selectionBefore);
  assert.ok(JSON.stringify(fixture.snapshot.assessment) === assessmentBefore);
});

// One unclassified child can still use an ordinary slot. Each control removes
// eligibility from all five, so six ordinary receipts must fail the same cap.
const mutations: [string, (selection: any) => void][] = [
  ['another producer does not inherit typed conflict dependency slots', selection => {
    for (const row of conflicts(selection)) row.origin.question.producer = 'C-plan-counter';
  }],
  ['both claimed sources must be distinct decoded physical response receipts', selection => {
    for (const row of conflicts(selection)) for (const kind of ['question', 'candidates', 'evidence'])
      row.origin[kind].source_refs[0] = 'work:0';
  }],
  ['source order must match the actual ordered first divergence', selection => {
    for (const row of conflicts(selection)) for (const kind of ['question', 'candidates', 'evidence'])
      row.origin[kind].source_refs.reverse();
  }],
  ['a different source prefix cannot own these divergent children', selection => {
    for (const row of conflicts(selection)) sources(selection, row)[0].prefix_moves_uci[1] = 'f7f6';
  }],
  ['a foreign source profile cannot certify a current typed conflict', selection => {
    for (const row of conflicts(selection)) sources(selection, row)[0].engine_profile = 'sf18-smallnet-t2-h128-v1';
  }],
  ['achieved source depths must be homogeneous and equal', selection => {
    for (const row of conflicts(selection)) sources(selection, row)[0].candidates[0].evaluation.depth = 17;
  }],
  ['a physical parent cannot borrow another execution key', selection => {
    for (const row of conflicts(selection)) sources(selection, row)[0].proposal_ownership.execution_key_sha256 =
      '0'.repeat(64);
  }],
  ['a different original pawn cannot grant current choice dependency ownership', selection => {
    family(selection).requirements.find((row: any) => row.id === 'reserve_before').predicate.subject.square = 'e4';
  }],
  ['the current original-pawn context cannot be removed', selection => {
    family(selection).requirements.find((row: any) => row.id === 'reserve_context').members.pop();
  }],
  ['a different child prefix cannot stand in for the first divergent decision', selection => {
    for (const row of conflicts(selection)) row.requirements[0].prefix_moves_uci[1] = 'f7f6';
  }],
  ['a deferred child grants no completed dependency receipt', selection => {
    for (const row of conflicts(selection)) Object.assign(row.requirements[0],
      { state: 'deferred', answer: null, work_id: null });
  }],
  ['a foreign numerical root owner cannot authorize otherwise legal children', selection => {
    family(selection).requirements.find((row: any) => row.id === 'reserve_root').answer.work_id = 'work:0';
  }],
];
for (const [name, mutate] of mutations) test(name, () => {
  const selection = structuredClone(rawSelection);
  const assessmentBefore = JSON.stringify(fixture.snapshot.assessment);
  mutate(selection);
  assert.ok(decode(selection) === undefined, name);
  assert.ok(JSON.stringify(fixture.snapshot.assessment) === assessmentBefore);
});
