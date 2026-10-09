import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, moveReviewSan, type MoveReviewSubject } from '../src/moveReview';

const fixtureRoot = new URL('../../../../../../judgment-evaluation/fixtures/public-commentary-v6/', import.meta.url);
const load = (name: string, digest: string) => {
  const bytes = readFileSync(new URL(name, fixtureRoot));
  assert.equal(createHash('sha256').update(bytes).digest('hex'), digest);
  return JSON.parse(gunzipSync(bytes).toString('utf8'));
};
// Genuine controlled producer output: all43 whole native reports matched.
// The two held Scala consumer changes are separately recorded; this is not
// relabeled as a fresh all10i job or as a new engine investigation.
const actual = load('reserve-family-completed43-controlled10i6.json.gz',
  '07e9524a336aa97442a464611934307b789335b3688621379b362391ae6333ac');
const raw = actual.snapshot, request = actual.request;
const makeContext = (request: any) => ({ requestId: request.request_id, subject: {
  variant: request.variant, initialFen: request.initial_fen, movePrefixUci: request.move_prefix_uci,
  before: { path: '', fen: request.current_fen },
  played: { uci: request.focus.played_move_uci, san: moveReviewSan(request.current_fen, request.focus.played_move_uci)! },
  after: { path: 'probe', fen: request.focus.resulting_fen },
} as MoveReviewSubject });
const context = makeContext(request);
const root = (observations: any) => observations?.selection?.candidates.find((candidate: any) =>
  candidate.detail?.type === 'root_comparison');
const originalSelection = raw.observations.selection;
const familyId = 'discovery:821fd2accb10bd6c6aeebd7759dec5cb41ee12f8bc5bd6457f42ae2312229ccb';
const family = (selection: any) => selection.proposals.find((proposal: any) => proposal.id === familyId);
const requirement = (selection: any, id: string) => family(selection).requirements.find((row: any) => row.id === id);
const search = (selection: any, id: string) => selection.response_searches.find((row: any) => row.work_id === id);
const baseline = decodeMoveReviewSnapshot(raw, context);
assert.ok(baseline?.kind === 'completed' && baseline.observations?.selection,
  'the whole actual41-row ledger is accepted after exact family allocation');

test('all41 genuine receipts, six historical parents and protected body/prices survive unchanged', () => {
  assert.equal(originalSelection.response_searches.length, 41);
  assert.equal(raw.progress.physical_reports_accepted, 43);
  assert.deepEqual(baseline.observations!.selection!.response_searches, originalSelection.response_searches);
  assert.deepEqual(root(baseline.observations).detail, root(raw.observations).detail,
    'basic, detail, exact covered price references and proof metadata are not trimmed');
  for (const [child, parents] of [['work:8', ['work:6', 'work:7']],
    ['work:11', ['work:9', 'work:10']], ['work:14', ['work:12', 'work:13']]] as const) {
    const proposal = originalSelection.proposals.find((row: any) =>
      row.origin.question.producer === 'C-comparison-conflict' && row.requirements[0].work_id === child);
    assert.deepEqual(proposal.origin.question.source_refs, parents);
    for (const id of [child, ...parents]) assert.deepEqual(
      search(baseline.observations!.selection, id), search(originalSelection, id));
  }
  const primary = raw.result.selected_move_reviews.find((review: any) => review.commentary)?.commentary.primary;
  assert.equal(baseline.evidence.candidates.find(candidate => candidate.roles.includes('played'))?.winPercent,
    primary.played_endpoint.win_percent_for_mover);
  assert.equal(baseline.evidence.candidates.find(candidate => candidate.roles.includes('best'))?.winPercent,
    primary.reference_endpoint.win_percent_for_mover);
});

function changed(mutate: (selection: any) => void) {
  const selection = structuredClone(originalSelection);
  mutate(selection);
  return { ...raw, observations: { ...raw.observations, selection } };
}
function rejected(copy: any, name: string) {
  const numerical = JSON.stringify(copy.result);
  const result = decodeMoveReviewSnapshot(copy, context);
  assert.ok(!result?.observations?.selection, name);
  assert.equal(JSON.stringify(copy.result), numerical, 'rejection never changes the numerical input');
  if (result?.kind === 'completed') assert.deepEqual(result.evidence, baseline.evidence);
}
function duplicate(selection: any, sourceId: string, number: number, keepOwner = true) {
  const row = structuredClone(search(selection, sourceId));
  row.work_id = `work:${number}`;
  row.execution_key_sha256 = createHash('sha256').update(`negative duplicate ${number}`).digest('hex');
  if (keepOwner) {
    row.proposal_ownership.work_id = row.work_id;
    row.proposal_ownership.execution_key_sha256 = row.execution_key_sha256;
  } else delete row.proposal_ownership;
  selection.response_searches.push(row);
}

test('a new valid work/key or requirement alias cannot buy a second full or pair slot', () => {
  for (const [id, alias] of [['work:3', false], ['work:40', false], ['work:3', true]] as const)
    rejected(changed(selection => {
      duplicate(selection, id, 100);
      if (alias) {
        const row = search(selection, 'work:100');
        const ref = row.proposal_ownership.question_refs[0];
        const original = family(selection).requirements.find((reading: any) => reading.id === ref.requirement_id);
        // Reuse an unissued alias compare's existing unique ID, so the
        // unchanged600 declaration bound is not the reason for rejection.
        const owned = new Set(selection.response_searches.flatMap((search: any) =>
          search.proposal_ownership?.question_refs.filter((ref: any) => ref.proposal_id === familyId)
            .map((ref: any) => ref.requirement_id) ?? []));
        const index = family(selection).requirements.findIndex((reading: any) => reading.kind === 'compare_moves' &&
          reading.id.startsWith('anchored_value_response:') && !owned.has(reading.id) &&
          reading.prefix_moves_uci.length === original.prefix_moves_uci.length &&
          reading.prefix_moves_uci.every((move: string, index: number) => move === original.prefix_moves_uci[index]));
        assert.ok(index >= 0);
        const aliasId = family(selection).requirements[index].id;
        family(selection).requirements[index] = { ...structuredClone(original), id: aliasId };
        ref.requirement_id = aliasId;
      }
    }), `${id} duplicates the same canonical physical slot`);
});

const mutations: [string, (selection: any) => void][] = [
  ['a different original pawn grants no family capacity', selection => {
    requirement(selection, 'reserve_before').predicate.subject.square = 'e4';
  }],
  ['a foreign root receipt cannot license the family', selection => {
    requirement(selection, 'reserve_root').answer.work_id = 'work:0';
  }],
  ['root execution ownership must match, not just the same FEN and score', selection => {
    requirement(selection, 'reserve_root').answer.execution_key_sha256 = 'a'.repeat(64);
  }],
  ['another family at this board does not own the current branch', selection => {
    const foreign = selection.proposals.find((proposal: any) => proposal.origin.question.producer ===
      'C-reserve-use' && proposal.id !== familyId);
    assert.ok(foreign);
    search(selection, 'work:3').proposal_ownership.question_refs[0].proposal_id = foreign.id;
  }],
  ['a deferred current context grants no receipt slots', selection => {
    Object.assign(requirement(selection, 'reserve_context'), { state: 'deferred', result: null });
  }],
  ['a historical child cannot substitute a foreign parent', selection => {
    const conflict = selection.proposals.find((proposal: any) =>
      proposal.origin.question.producer === 'C-comparison-conflict' && proposal.requirements[0].work_id === 'work:8');
    for (const kind of ['question', 'candidates', 'evidence']) conflict.origin[kind].source_refs[0] = 'work:3';
  }],
  ['a historical parent key remains exact', selection => {
    search(selection, 'work:6').proposal_ownership.execution_key_sha256 = 'b'.repeat(64);
  }],
  ['a foreign response profile cannot borrow a family proof', selection => {
    search(selection, 'work:6').engine_profile = 'sf18-smallnet-t2-h128-v1';
  }],
  ['an incomplete parent depth cannot retain its completed child', selection => {
    search(selection, 'work:6').candidates[0].evaluation.depth = 8;
  }],
  ['current comparison ownership cannot be replaced by another legal question', selection => {
    search(selection, 'work:40').proposal_ownership.question_refs[0].requirement_id =
      search(selection, 'work:3').proposal_ownership.question_refs[0].requirement_id;
  }],
];
for (const [name, mutate] of mutations) test(name, () => rejected(changed(mutate), name));

test('a known equal FEN through a different exact history grants no receipt', () => {
  const paths = raw.observations.paths;
  const first = paths.find((path: any) => path.moves.slice(0, 4).join(' ') === 'g3g4 e5e6 g4g5 e6e5');
  const other = paths.find((path: any) => path.moves.slice(0, 4).join(' ') === 'g3g4 e5d6 g4g5 d6e5');
  assert.ok(first && other);
  const positions = raw.observations.positions;
  assert.equal(positions.find((position: any) => position.id === first.position_ids[4]).fen,
    positions.find((position: any) => position.id === other.position_ids[4]).fen);
  assert.notEqual(first.position_ids[4], other.position_ids[4]);
  rejected(changed(selection => {
    const ref = search(selection, 'work:3').proposal_ownership.question_refs[0];
    family(selection).requirements.find((row: any) => row.id === ref.requirement_id).prefix_moves_uci = other.moves.slice(0, 4);
  }), 'canonical history, rather than board equality, owns the declared question');
});

test('six ordinary receipts and declarations above600 remain rejected', () => {
  rejected(changed(selection => {
    for (let index = 0; index < 5; index++) duplicate(selection, 'work:2', 100 + index, false);
  }), 'the finite family does not enlarge the ordinary five slots');
  rejected(changed(selection => {
    assert.equal(selection.proposals.reduce((sum: number, proposal: any) => sum + proposal.requirements.length, 0), 600);
    family(selection).requirements.push({ id: 'capacity-extra-declaration', kind: 'uninterpreted', text: 'No value',
      state: 'deferred', stop_reason: null, answer: null, line_id: null, occurrence_id: null, position_id: null, work_id: null });
  }), 'the envelope cannot grant declarations above the unchanged600 grammar limit');
});

test('a generic task still has24 slots; a family-free ledger gains no allowance from the envelope', () => {
  const produced = load('fact-qualified-nb1-produced10h-corrected.json.gz',
    'bf3846e0ce06ec2e3ab7cd7076d1c9ecbbef8d84fceeaa4b0c74126c26291984');
  const native = load('functional-choice-native26-produced.json.gz',
    '5d0595d782c0f47499e17d8d3193b8e0598d34f94c122fb0b77d2a55ad6c8b2d');
  const copy = structuredClone(produced.snapshot), selection = copy.observations.selection;
  assert.equal(selection.response_searches.length, 22);
  for (let index = 0; index < 5; index++) duplicate(selection, 'work:6', 100 + index);
  const decoded = decodeMoveReviewSnapshot(copy, makeContext(native.request));
  assert.ok(decoded?.kind === 'completed' && !decoded.observations?.selection,
    '25 otherwise task-owned generic receipts stay above the unchanged24 limit');
});
