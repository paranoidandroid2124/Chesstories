import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, moveReviewSan, type MoveReviewSubject } from '../src/moveReview';

// Genuine second public projection over all26 unchanged native reports. The
// first invalid research envelope remains separately sealed in its own target.
const fixtureRoot = new URL('../../../../../../judgment-evaluation/fixtures/public-commentary-v6/', import.meta.url);
const bytes = readFileSync(new URL('fact-qualified-nb1-produced10h-corrected.json.gz', fixtureRoot));
assert.equal(createHash('sha256').update(bytes).digest('hex'),
  'bf3846e0ce06ec2e3ab7cd7076d1c9ecbbef8d84fceeaa4b0c74126c26291984');
const raw = JSON.parse(gunzipSync(bytes).toString('utf8')).snapshot;
const native = readFileSync(new URL('functional-choice-native26-produced.json.gz', fixtureRoot));
assert.equal(createHash('sha256').update(native).digest('hex'),
  '5d0595d782c0f47499e17d8d3193b8e0598d34f94c122fb0b77d2a55ad6c8b2d');
const request = JSON.parse(gunzipSync(native).toString('utf8')).request;
const context = { requestId: request.request_id, subject: {
  variant: request.variant, initialFen: request.initial_fen, movePrefixUci: request.move_prefix_uci,
  before: { path: '', fen: request.current_fen },
  played: { uci: request.focus.played_move_uci, san: moveReviewSan(request.current_fen, request.focus.played_move_uci)! },
  after: { path: 'probe', fen: request.focus.resulting_fen },
} as MoveReviewSubject };
const root = (observations: any) => observations?.selection?.candidates.find((candidate: any) =>
  candidate.detail?.type === 'root_comparison');
const baseline = decodeMoveReviewSnapshot(raw, context);
assert.ok(baseline?.kind === 'completed' && baseline.observations?.selection);

test('actual qualified-source answer retains its body, protected prices and the independent root assessment', () => {
  assert.equal(raw.job_id, '70d354f57c565e6a9f4eaa3166854432');
  assert.deepEqual(root(baseline.observations).detail, root(raw.observations).detail);
  const question = root(raw.observations).detail.summary_question;
  const conditional = raw.observations.paths.find((path: any) =>
    path.moves.slice(0, 5).join(' ') === 'd2c4 e8f6 d1d6 c6d6 c4d6' &&
      question.units.some((unit: any) => unit.covered_moves.some((ref: any) =>
        ref.line_id === path.id && ref.occurrence_id === path.occurrence_ids[4])));
  assert.ok(conditional, 'the exact currently owned Qxd6/Nxd6 conditional transaction is in basic');
  const primary = raw.result.selected_move_reviews.find((review: any) => review.commentary)?.commentary.primary;
  assert.equal(baseline.evidence.candidates.find(candidate => candidate.roles.includes('played'))?.winPercent,
    primary.played_endpoint.win_percent_for_mover);
  assert.equal(baseline.evidence.candidates.find(candidate => candidate.roles.includes('best'))?.winPercent,
    primary.reference_endpoint.win_percent_for_mover);
});

function chosen(laterPoint = false): any {
  const observations = raw.observations;
  for (const [proposalIndex, proposal] of observations.selection.proposals.entries()) {
    for (const [readingIndex, reading] of proposal.requirements.entries()) {
      if (reading.kind !== 'fact' || reading.predicate.kind !== 'legal_move') continue;
      for (const [sampleIndex, sample] of (reading.result?.samples ?? []).entries()) {
        if (laterPoint && (sample.from_ply !== sample.through_ply || sample.from_ply < 1)) continue;
        const path = observations.paths.find((path: any) => path.id === sample.line_id);
        for (const [refIndex, ref] of sample.source_ids.entries()) {
          const match = ref.match(/^(.*):evidence:relation:position:([a-f0-9]{64}):legal_move:([a-f0-9]{64})$/);
          if (!match) continue;
          const earlier = path.position_ids.slice(0, sample.from_ply).find((id: string) =>
            observations.positions.find((position: any) => position.id === id)?.legal_moves.moves.some(
              (move: any) => move.source_id === match[3]));
          if (laterPoint && !earlier) continue;
          return { proposalIndex, readingIndex, sampleIndex, refIndex, namespace: match[1],
            positionId: match[2], semanticId: match[3], earlier, point: sample.from_ply, ref };
        }
      }
    }
  }
  assert.fail('the actual frame has this qualified legal reading');
}
function changeSample(change: (sample: any, reading: any, target: any) => void, target = chosen()): any {
  const original = raw.observations.selection.proposals[target.proposalIndex];
  const proposal = structuredClone(original);
  const reading = proposal.requirements[target.readingIndex];
  change(reading.result.samples[target.sampleIndex], reading, target);
  return { ...raw, observations: { ...raw.observations, selection: { ...raw.observations.selection,
    proposals: raw.observations.selection.proposals.map((entry: any, index: number) =>
      index === target.proposalIndex ? proposal : entry) } } };
}
function rejected(copy: any, name: string) {
  const result = decodeMoveReviewSnapshot(copy, context);
  assert.ok(!result?.observations?.selection, name);
  assert.ok(result?.kind === 'completed', 'invalid commentary does not erase the root assessment');
  assert.deepEqual(result.evidence, baseline.evidence);
}
const mutations: [string, (sample: any, reading: any, target: any) => void][] = [
  ['a foreign packet namespace cannot license the same semantic ID', (sample, _reading, t) => {
    sample.source_ids[t.refIndex] = `foreign:${t.ref}`;
  }],
  ['a source from another typed resource kind is rejected', (sample, _reading, t) => {
    sample.source_ids[t.refIndex] = t.ref.replace(':legal_move:', ':ray_barrier:');
  }],
  ['an unknown semantic ID is rejected', (sample, _reading, t) => {
    sample.source_ids[t.refIndex] = t.ref.slice(0, -64) + 'f'.repeat(64);
  }],
  ['mixed pawn-state IDs cannot fabricate a passage certificate', (sample, _reading, t) => {
    sample.source_ids[t.refIndex] = t.ref.replace(':legal_move:', ':pawn_passage:');
  }],
  ['a legal result cannot be relabeled as a control predicate', (_sample, reading) => {
    reading.predicate.kind = 'control';
  }],
];
for (const [name, change] of mutations) test(name, () => rejected(changeSample(change), name));

test('a point2 reading cannot borrow point0 in the same used history even when the raw legal ID recurs', () => {
  const target = chosen(true);
  assert.equal(target.point, 2);
  assert.ok(target.earlier !== target.positionId);
  rejected(changeSample((sample, _reading, t) => {
    sample.source_ids[t.refIndex] = `${t.namespace}:evidence:relation:position:${t.earlier}:legal_move:${t.semanticId}`;
  }, target), 'wrong earlier exact queried position');
});

test('another occurrence cannot substitute for a deleted current legal row', () => {
  const target = chosen();
  const repeated = raw.observations.positions.filter((position: any) => position.id !== target.positionId &&
    position.legal_moves.moves.some((move: any) => move.source_id === target.semanticId));
  assert.ok(repeated.length >= 18, 'the actual semantic ID recurs in other positions');
  const copy = changeSample(() => {}, target);
  copy.observations.positions = raw.observations.positions.map((position: any) => position.id === target.positionId
    ? { ...position, legal_moves: { ...position.legal_moves,
      moves: position.legal_moves.moves.filter((move: any) => move.source_id !== target.semanticId) } } : position);
  rejected(copy, 'removed actual position resource cannot be repaired by another occurrence');
});
