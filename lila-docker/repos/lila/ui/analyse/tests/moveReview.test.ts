import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { moveReviewEngineProfile } from 'lib/ceval/types';
import pgnImport from '../src/pgnImport';
import { standardUci, treeReconstruct } from '../src/util';
import {
  buildMoveReviewEngineWorkReport,
  buildMoveReviewJobRequest,
  decodeMoveReviewSnapshot,
  formatMoveReviewPercent,
  formatMoveReviewPercentagePointChange,
  moveReviewEngineOutcomeAtRequiredDepth,
  moveReviewProofById,
  moveReviewSubjectFromNodeList,
  moveReviewRecordFromTree,
  moveReviewFrameForSubject,
  type MoveReviewObservations,
  normalizeMoveReviewLocale,
  selectedMoveReviewCandidate,
  moveReviewObservations,
  type IssuedMoveReviewEngineWork,
  type MoveReviewSearchScope,
  type MoveReviewSubject,
} from '../src/moveReview';
import {
  afterFen,
  beforeFen,
  decodeContext,
  initialFen,
  rawResponse,
  rawSnapshot,
  rawInvestigation,
  requestId,
  subject,
  observationFixtureJob,
  linkedHighlightFixture,
  type CertifiedObservationFixture,
} from './moveReviewTestSupport';

type JsonObject = Record<string, unknown>;

test('investigation work validates exact history and bounded samples of legal root pools while retaining the assessment', () => {
  const raw = rawInvestigation();
  const snapshot = decodeMoveReviewSnapshot(raw, decodeContext());
  assert.equal(snapshot?.kind, 'awaiting-evidence');
  assert.ok(snapshot?.kind === 'awaiting-evidence');
  assert.equal(snapshot.issuedEngineWork.purpose, 'investigation_comparison');
  assert.equal(snapshot.assessment?.kind, 'completed');
  const baseline = decodeMoveReviewSnapshot(rawResponse(), decodeContext());
  assert.ok(baseline?.kind === 'completed');
  assert.deepEqual(
    snapshot.assessment?.kind === 'completed' && snapshot.assessment.evidence,
    baseline.evidence,
  );
  for (const mutate of [
    (v: JsonObject) => {
      delete v.assessment;
    },
    (v: JsonObject) => {
      v.assessment_state = 'best_confirmed';
    },
    (v: JsonObject) => {
      (v.issued_engine_work as JsonObject).root_restriction = {
        kind: 'restricted',
        moves_uci: ['c7c5', 'e7e4'],
      };
    },
    (v: JsonObject) => {
      (v.issued_engine_work as JsonObject).engine_position_moves_uci = [];
    },
    (v: JsonObject) => {
      (v.issued_engine_work as JsonObject).search_limits = {
        depth: 16,
        nodes: 2_000_000,
        movetime_ms: 2_500,
        multi_pv: 3,
      };
    },
  ]) {
    const malformed = structuredClone(raw);
    mutate(malformed);
    assert.equal(decodeMoveReviewSnapshot(malformed, decodeContext()), undefined);
  }
  const pool = structuredClone(raw);
  const poolWork = pool.issued_engine_work as JsonObject;
  poolWork.root_restriction = { kind: 'restricted', moves_uci: ['c7c5', 'g8f6', 'e7e5', 'd7d5'] };
  (poolWork.search_limits as JsonObject).multi_pv = 3;
  assert.equal(decodeMoveReviewSnapshot(pool, decodeContext())?.kind, 'awaiting-evidence');
  (poolWork.search_limits as JsonObject).multi_pv = 2;
  assert.equal(
    decodeMoveReviewSnapshot(pool, decodeContext()),
    undefined,
    'the pool does not pretend to return every root',
  );
  const pair = rawResponse({ assessment_state: 'current', assessment_scope: 'investigation_pair' });
  const reviews = (pair.result as JsonObject).selected_move_reviews as JsonObject[];
  delete (reviews[0].selection as JsonObject).root_rank;
  assert.equal(decodeMoveReviewSnapshot(pair, decodeContext())?.assessmentScope, 'investigation_pair');
  pair.assessment_scope = 'root_search';
  assert.equal(decodeMoveReviewSnapshot(pair, decodeContext()), undefined);
});

test('record origin remains the imported root; revisions and transposed occurrences are distinct', () => {
  const data = pgnImport('1. Nf3 (1. Nc3 Nc6 2. Nf3 Nf6) Nf6 2. Nc3 Nc6 *');
  const root = treeReconstruct(data.treeParts!, data.sidelines);
  const record = moveReviewRecordFromTree('standard', root, data.recordedMoves, 'one')!;
  assert.equal(record.subject.initialFen, root.fen);
  assert.deepEqual(record.subject.movePrefixUci, []);
  const original = record.subject.played.uci;
  root.children.reverse();
  const promoted = moveReviewRecordFromTree('standard', root, data.recordedMoves, 'one')!;
  assert.equal(promoted.subject.played.uci, original);
  assert.notEqual(promoted.key, record.key);
  assert.notEqual(promoted.key, moveReviewRecordFromTree('standard', root, data.recordedMoves, 'two')!.key);
  const lines = root.children.map(first => {
    const nodes = [root, first];
    while (nodes.at(-1)!.children[0]) nodes.push(nodes.at(-1)!.children[0]!);
    return {
      subject: moveReviewSubjectFromNodeList(
        'standard',
        nodes
          .slice(1)
          .map(n => n.id)
          .join(''),
        nodes,
      )!,
      path: {
        id: first.id,
        kind: 'analysis' as const,
        startFen: root.fen,
        moves: nodes.slice(1).map(n => ({ uci: n.uci!, label: n.san!, fenAfter: n.fen })),
        occurrenceIds: nodes.slice(1).map((_, i) => `${first.id}:${i}`),
      },
    };
  });
  assert.equal(lines[0].subject.after.fen, lines[1].subject.after.fen);
  const observations: MoveReviewObservations = { paths: [lines[0].path], statements: [] };
  assert.equal(moveReviewFrameForSubject(observations, record.subject, lines[1].subject), undefined);
  observations.paths.push(lines[1].path);
  assert.equal(
    moveReviewFrameForSubject(observations, record.subject, lines[1].subject)?.proofId,
    lines[1].path.id,
  );
  const wrongBefore = { ...lines[1].subject, before: { ...lines[1].subject.before, fen: root.fen } };
  assert.equal(moveReviewFrameForSubject(observations, record.subject, wrongBefore), undefined);
  observations.paths[1].occurrenceIds[3] = null;
  assert.equal(moveReviewFrameForSubject(observations, record.subject, lines[1].subject), undefined);
});
const object = (value: unknown): JsonObject => value as JsonObject;

const certifiedFixtures = JSON.parse(
  readFileSync(
    new URL(
      '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/certified-observations-produced.json',
      import.meta.url,
    ),
    'utf8',
  ),
) as CertifiedObservationFixture[];

test('role highlights are occurrence-bound and malformed presentation does not erase facts', () => {
  const item = structuredClone(certifiedFixtures.find(fixture => fixture.name === 'Bf3')!);
  const raw = object(item.response.observations);
  const statements = raw.statements as JsonObject[];
  const occurrence = statements[0]!.occurrence_id;
  const valid = { occurrence_id: occurrence, text: 'A local role of the bishop.', squares: ['f3'] };
  raw.highlights = [valid, { ...valid, occurrence_id: 'unrelated-history' }, { ...valid, squares: ['z9'] }];
  const decoded = moveReviewObservations(observationFixtureJob(item))!;
  assert.equal(decoded.statements.length, statements.length);
  assert.deepEqual(decoded.highlights, [{ occurrenceId: occurrence, text: valid.text, squares: ['f3'] }]);
  raw.highlights = { damaged: true };
  const damaged = moveReviewObservations(observationFixtureJob(item))!;
  assert.equal(damaged.statements.length, statements.length);
  assert.equal(damaged.highlights, undefined);
});

test('linked highlights retain full premises, point witnesses and every position-local resource kind', () => {
  const { fixture, highlight } = linkedHighlightFixture(certifiedFixtures.find(f => f.name === 'Bf3')!);
  const wire = object(fixture.response.observations);
  const statement = (wire.statements as JsonObject[])[0];
  const pointRefs = (object(statement.before).premise_ids as string[]).concat(
    object(statement.after).premise_ids as string[],
  );
  // A witness may be delivered only on its point, not repeated on the statement.
  statement.premise_ids = [highlight.occurrence_id];
  const positions = moveReviewObservations(observationFixtureJob(fixture))!.positions!;
  highlight.premise_ids = [
    ...new Set([highlight.occurrence_id, ...pointRefs, ...positions.map(p => p.premise_id)]),
  ];
  for (const frame of highlight.frames) {
    const p = positions.find(p => p.id === frame.position_id)!;
    frame.source_ids = [
      ...new Set([
        ...p.controls.pieces.flatMap(p => p.source_ids),
        ...p.rays.lines.map(r => r.source_id),
        ...p.legal_moves.moves.map(m => m.source_id),
        ...p.pawn_states.pawns.flatMap(p => p.source_ids),
        ...p.pawn_files.files.flatMap(f => f.source_ids),
        ...p.pawn_advances.paths.map(p => p.source_id),
      ]),
    ];
  }
  const decoded = moveReviewObservations(observationFixtureJob(fixture))!;
  assert.deepEqual(decoded.highlights, [
    {
      occurrenceId: highlight.occurrence_id,
      text: highlight.text,
      squares: highlight.squares,
      premiseIds: highlight.premise_ids,
      frames: highlight.frames.map(f => ({
        phase: f.phase,
        positionId: f.position_id,
        squares: f.squares,
        sourceIds: f.source_ids,
      })),
    },
  ]);
  highlight.frames.forEach(f => (f.source_ids = []));
  highlight.premise_ids = [highlight.occurrence_id];
  assert.equal(moveReviewObservations(observationFixtureJob(fixture))!.highlights?.length, 1);
  highlight.premise_ids = [];
  assert.deepEqual(moveReviewObservations(observationFixtureJob(fixture))!.highlights?.[0].premiseIds, []);
  // All references resolve; only the delivery count limit makes 65 invalid.
  const refs = Array.from({ length: 65 }, (_, i) => `bounded-full-reference:${i}`);
  statement.premise_ids = refs;
  highlight.premise_ids = refs.slice(0, 64);
  assert.equal(moveReviewObservations(observationFixtureJob(fixture))!.highlights?.length, 1);
  highlight.premise_ids = refs;
  const bounded = moveReviewObservations(observationFixtureJob(fixture))!;
  assert.equal(bounded.highlights, undefined);
  assert.deepEqual(bounded.statements[0].premiseIds, refs);
});

test('malformed highlight links discard only that highlight and preserve all independent lower results', () => {
  const { fixture, highlight } = linkedHighlightFixture(certifiedFixtures.find(f => f.name === 'Bf3')!);
  const wire = object(fixture.response.observations);
  const sibling = { ...structuredClone(highlight), text: 'Independent linked role.' };
  wire.highlights = [sibling];
  const expected = moveReviewObservations(observationFixtureJob(fixture))!;
  const mutations: ((h: JsonObject) => void)[] = [
    h => {
      delete h.premise_ids;
    },
    h => {
      delete h.frames;
    },
    h => {
      h.premise_ids = null;
    },
    h => {
      h.premise_ids = ['unknown-full-reference'];
    },
    h => {
      h.premise_ids = [highlight.premise_ids[0].split(':').at(-1)];
    },
    h => {
      h.premise_ids = [highlight.premise_ids[0], highlight.premise_ids[0]];
    },
    h => {
      h.frames = [];
    },
    h => {
      h.frames = null;
    },
    h => {
      (h.frames as JsonObject[])[0].phase = 'transition';
    },
    h => {
      (h.frames as JsonObject[])[0].position_id = highlight.frames[1].position_id;
    },
    h => {
      (h.frames as JsonObject[])[0].position_id = 'missing-position';
    },
    h => {
      (h.frames as JsonObject[])[0].squares = ['z9'];
    },
    h => {
      (h.frames as JsonObject[])[0].squares = ['a1'];
    },
    h => {
      (h.frames as JsonObject[])[0].source_ids = ['unknown'];
    },
    h => {
      (h.frames as JsonObject[])[0].source_ids = ['f'.repeat(64)];
    },
    h => {
      delete (h.frames as JsonObject[])[0].source_ids;
    },
    h => {
      (h.frames as JsonObject[])[0].unexpected = true;
    },
    h => {
      h.frames = [highlight.frames[0], { ...highlight.frames[0], squares: ['h2'] }];
    },
  ];
  for (const mutate of mutations) {
    const malformed: JsonObject = structuredClone(highlight);
    mutate(malformed);
    wire.highlights = [malformed, sibling];
    assert.deepEqual(moveReviewObservations(observationFixtureJob(fixture)), expected, mutate.toString());
  }
  wire.highlights = [
    ...Array.from({ length: 10 }, () => ({ ...highlight, premise_ids: ['missing'] })),
    sibling,
  ];
  assert.deepEqual(moveReviewObservations(observationFixtureJob(fixture)), expected);
  delete wire.highlights;
  const lower = moveReviewObservations(observationFixtureJob(fixture))!;
  wire.highlights = [{ ...highlight, premise_ids: ['missing'] }];
  assert.deepEqual(moveReviewObservations(observationFixtureJob(fixture)), lower);
});

test('linked highlights cannot borrow a frame, premise or resource from another equal-FEN history', () => {
  const { fixture, highlight } = linkedHighlightFixture(certifiedFixtures.find(f => f.name === 'Bf3')!);
  const wire = object(fixture.response.observations);
  const positions = wire.positions as JsonObject[];
  const paths = wire.paths as JsonObject[];
  const fork = structuredClone(positions.find(p => p.id === highlight.frames[1].position_id)!);
  fork.id = 'separate-history-same-fen';
  fork.premise_id = 'separate-history:full-position-premise';
  const ray = (object(fork.rays).lines as JsonObject[])[0];
  ray.source_id = 'f'.repeat(64);
  positions.push(fork);
  const forkPath = {
    ...structuredClone(paths[0]),
    id: 'separate-history-path',
    occurrence_ids: ['separate-history-edge'],
    position_ids: [highlight.frames[0].position_id, fork.id],
  };
  delete (forkPath as JsonObject).line_summary;
  paths.push(forkPath);
  const statements = wire.statements as JsonObject[];
  statements.push({
    ...structuredClone(statements[0]),
    occurrence_id: 'separate-history-edge',
    premise_ids: ['separate-history:full-statement-premise'],
  });
  wire.highlights = [];
  const expected = moveReviewObservations(observationFixtureJob(fixture))!;
  assert.equal(expected.paths[0].moves[0].fenAfter, expected.paths[1].moves[0].fenAfter);
  const invalid = [
    { ...highlight, frames: [highlight.frames[0], { ...highlight.frames[1], position_id: fork.id }] },
    { ...highlight, premise_ids: [fork.premise_id] },
    { ...highlight, premise_ids: ['separate-history-edge'] },
    { ...highlight, premise_ids: ['separate-history:full-statement-premise'] },
    { ...highlight, frames: [{ ...highlight.frames[1], source_ids: [ray.source_id] }] },
  ];
  for (const item of invalid) {
    wire.highlights = [item];
    assert.deepEqual(moveReviewObservations(observationFixtureJob(fixture)), expected);
  }
  wire.highlights = [highlight];
  assert.equal(moveReviewObservations(observationFixtureJob(fixture))!.highlights?.length, 1);
});

test('all runtime-certified observation payloads preserve every nested witness in the UI decoder', () => {
  assert.equal(certifiedFixtures.length, 12);
  for (const fixture of certifiedFixtures) {
    const observations = moveReviewObservations(observationFixtureJob(fixture));
    assert.ok(observations, fixture.name);
    const raw = object(fixture.response.observations).statements as JsonObject[];
    assert.equal(observations.statements.length, raw.length, fixture.name);
    raw.forEach((statement, index) => {
      const expected = structuredClone(statement);
      expected.occurrenceId = expected.occurrence_id;
      expected.premiseIds = expected.premise_ids;
      delete expected.occurrence_id;
      delete expected.premise_ids;
      if (expected.kind === 'capture') {
        expected.capturedRole = expected.captured_role;
        expected.capturedSquare = expected.captured_square;
        delete expected.captured_role;
        delete expected.captured_square;
      }
      if (expected.kind === 'development') {
        expected.centralSquares = expected.central_squares;
        delete expected.central_squares;
      }
      if (expected.kind === 'piece_control')
        for (const phase of ['before', 'after']) {
          const point = object(expected[phase]);
          point.premiseIds = point.premise_ids;
          delete point.premise_ids;
        }
      assert.deepEqual(observations.statements[index], expected, `${fixture.name}: ${statement.kind}`);
    });
  }
});

test('closed empty structural results remain distinct from an omitted result', () => {
  const fixture = certifiedFixtures.find(fixture => fixture.name === 'moving-target')!;
  const observations = moveReviewObservations(observationFixtureJob(fixture))!;
  const structural = observations.statements.find(statement => statement.kind === 'structural_change');
  assert.ok(structural?.kind === 'structural_change');
  assert.deepEqual(structural.changes, []);
});

test('position catalogues retain exact paths, completeness and all transmitted resources', () => {
  for (const fixture of certifiedFixtures) {
    const wire = object(fixture.response.observations);
    const observations = moveReviewObservations(observationFixtureJob(fixture))!;
    assert.deepEqual(observations.positions, wire.positions);
    observations.paths.forEach((path, i) => {
      assert.deepEqual(path.positionIds, (wire.paths as JsonObject[])[i].position_ids);
      assert.equal(path.positionIds!.length, path.moves.length + 1);
    });
  }
  const raw = structuredClone(certifiedFixtures[0]);
  const wire = object(raw.response.observations);
  const paths = wire.paths as JsonObject[];
  const positions = wire.positions as JsonObject[];
  const copy = structuredClone(positions[1]);
  copy.id = 'separate-history-same-board';
  positions.push(copy);
  const fork = structuredClone(paths[0]);
  fork.id = 'another-path';
  fork.occurrence_ids = ['another-occurrence'];
  delete fork.line_summary;
  fork.position_ids = [(fork.position_ids as string[])[0], copy.id];
  paths.push(fork);
  const observed = moveReviewObservations(observationFixtureJob(raw))!;
  assert.equal(observed.positions!.length, positions.length);
  assert.equal(observed.paths[1].positionIds![1], copy.id);
  (fork.position_ids as string[])[1] = (paths[0].position_ids as string[])[1];
  assert.throws(() => observationFixtureJob(raw), /Rejected runtime observation fixture/);
  (fork.position_ids as string[])[1] = copy.id as string;
  object(copy.controls).complete = false;
  object(copy.controls).pieces = [];
  assert.equal(moveReviewObservations(observationFixtureJob(raw))!.positions![2].controls.complete, false);
  (fork.position_ids as (string | null)[])[1] = null;
  assert.equal(moveReviewObservations(observationFixtureJob(raw))!.paths[1].positionIds![1], null);
});

test('position catalogue boundary rejects malformed payloads and incorrect path ownership', () => {
  const base = certifiedFixtures.find(fixture => fixture.name === 'Bf3')!;
  for (const mutate of [
    (w: JsonObject) => ((w.paths as JsonObject[])[0].position_ids = []),
    (w: JsonObject) => (((w.paths as JsonObject[])[0].position_ids as string[])[1] = 'unlisted'),
    (w: JsonObject) => ((w.positions as JsonObject[])[1].fen = (w.positions as JsonObject[])[0].fen),
    (w: JsonObject) => (w.positions as JsonObject[]).push(structuredClone((w.positions as JsonObject[])[0])),
    (w: JsonObject) => (object((w.positions as JsonObject[])[0].controls).complete = 'yes'),
    (w: JsonObject) =>
      ((object((w.positions as JsonObject[])[0].controls).pieces as JsonObject[])[0].source_ids = []),
    (w: JsonObject) =>
      (object((object((w.positions as JsonObject[])[0].controls).pieces as JsonObject[])[0].piece).square =
        'a9'),
    (w: JsonObject) => {
      const ray = (object((w.positions as JsonObject[])[0].rays).lines as JsonObject[]).find(
        r => (r.occupants as unknown[]).length > 1,
      )!;
      (ray.occupants as unknown[]).reverse();
    },
    (w: JsonObject) =>
      ((object((w.positions as JsonObject[])[0].pawn_advances).paths as JsonObject[])[0].source_id =
        'unscoped'),
    (w: JsonObject) => (object((w.positions as JsonObject[])[0].legal_moves).complete = 'yes'),
    (w: JsonObject) => {
      const moves = object((w.positions as JsonObject[])[0].legal_moves).moves as JsonObject[];
      moves.push(structuredClone(moves[0]));
    },
    (w: JsonObject) =>
      ((object((w.positions as JsonObject[])[0].legal_moves).moves as JsonObject[])[0].uci = 'h4h2'),
    (w: JsonObject) =>
      ((object((w.positions as JsonObject[])[0].legal_moves).moves as JsonObject[])[0].destination = 'a9'),
    (w: JsonObject) =>
      ((object((w.positions as JsonObject[])[0].legal_moves).moves as JsonObject[])[0].destination = 'a8'),
  ]) {
    const raw = structuredClone(base);
    mutate(object(raw.response.observations));
    assert.throws(() => observationFixtureJob(raw), /Rejected runtime observation fixture/);
  }
});

test('certified legal move check metadata preserves unknown separately from either boolean', () => {
  const base = certifiedFixtures.find(fixture => fixture.name === 'Bf3')!;
  const original = moveReviewObservations(observationFixtureJob(base))!.positions![0].legal_moves.moves[0];
  assert.equal(original.gives_check, undefined, 'the unchanged resource fixture has no check metadata');
  for (const value of [false, true, null, 0, 'true', []]) {
    const raw = structuredClone(base);
    const position = (object(raw.response.observations).positions as JsonObject[])[0];
    const move = (object(position.legal_moves).moves as JsonObject[])[0];
    move.gives_check = value;
    if (typeof value === 'boolean') {
      const decoded = moveReviewObservations(observationFixtureJob(raw))!.positions![0].legal_moves.moves[0];
      assert.equal(decoded.gives_check, value);
      assert.equal(decoded.source_id, original.source_id, 'check metadata stays on the same legal witness');
    } else assert.throws(() => observationFixtureJob(raw), /Rejected runtime observation fixture/);
  }
});

test('current pawn facets preserve independent known facts and reject false completeness or capture binding', () => {
  const fixtures = JSON.parse(
    readFileSync(
      new URL(
        '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/resources-produced.json',
        import.meta.url,
      ),
      'utf8',
    ),
  ) as CertifiedObservationFixture[];
  const base = fixtures.find(f => f.name === 'unchanged-pawn-states')!;
  const partial = structuredClone(base);
  const state = object((object(partial.response.observations).positions as JsonObject[])[0].pawn_states);
  const pawn = (state.pawns as JsonObject[])[0];
  pawn.passed = null;
  state.complete = false;
  const decoded = moveReviewObservations(observationFixtureJob(partial))!.positions![0].pawn_states.pawns[0];
  assert.equal(decoded.passed, null);
  assert.equal(decoded.isolated, pawn.isolated);
  assert.equal(decoded.doubled, pawn.doubled);
  state.complete = true;
  assert.throws(() => observationFixtureJob(partial), /Rejected runtime observation fixture/);
  state.complete = false;
  pawn.source_ids = [];
  assert.throws(() => observationFixtureJob(partial), /Rejected runtime observation fixture/);
  for (const mutation of [
    (p: JsonObject) => (object(p.pawn_states).pawns = []),
    (p: JsonObject) => (object(p.pawn_files).files as unknown[]).pop(),
    (p: JsonObject) => ((object(p.legal_moves).moves as JsonObject[])[0].captured_square = 'a1'),
  ]) {
    const raw = structuredClone(base);
    mutation((object(raw.response.observations).positions as JsonObject[])[0]);
    assert.throws(() => observationFixtureJob(raw), /Rejected runtime observation fixture/);
  }
});

test('actual observed line ledgers preserve all fields and reject events from another occurrence', () => {
  const fixtures = JSON.parse(
    readFileSync(
      new URL(
        '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/resources-produced.json',
        import.meta.url,
      ),
      'utf8',
    ),
  ) as CertifiedObservationFixture[];
  for (const fixture of fixtures) {
    const observations = moveReviewObservations(observationFixtureJob(fixture))!;
    const wire = object(fixture.response.observations).paths as JsonObject[];
    for (const [i, path] of observations.paths.entries())
      assert.deepEqual(path.lineSummary, wire[i].line_summary);
  }
  const base = fixtures.find(fixture => fixture.name === 'finite-material-branches')!;
  for (const mutate of [
    (s: JsonObject) => (s.scope = 'forced_gain'),
    (s: JsonObject) => (s.perspective = 'black'),
    (s: JsonObject) => (object(s.ending).ply_offset = 0),
    (s: JsonObject) => ((s.events as JsonObject[])[0].occurrence_id = 'another-branch'),
    (s: JsonObject) => ((s.events as JsonObject[])[0].move_uci = 'a1a8'),
    (s: JsonObject) => ((object(s.material).events as JsonObject[])[0].occurrence_id = 'another-branch'),
    (s: JsonObject) => ((object(s.material).events as JsonObject[])[0].balance_cp = '0'),
    (s: JsonObject) => ((object(s.material).events as JsonObject[])[0].balance_cp = 9876),
    (s: JsonObject) => ((object(s.material).events as JsonObject[])[0].delta_cp = 9876),
    (s: JsonObject) => (object(s.material).net_cp = 9876),
    (s: JsonObject) => (object(s.material).max_loss_cp = -9876),
    (s: JsonObject) => (object(s.material).recovered_after_loss = !object(s.material).recovered_after_loss),
    (s: JsonObject) => (object(s.material).recovered_after_loss = 'yes'),
  ]) {
    const raw = structuredClone(base);
    const path = (object(raw.response.observations).paths as JsonObject[]).find(
      path => (path.moves as unknown[]).length > 1,
    )!;
    mutate(object(path.line_summary));
    assert.throws(() => observationFixtureJob(raw), /Rejected runtime observation fixture/);
  }
});

test('unknown control points retain their independent positive sibling, and malformed nested wire fails closed', () => {
  const fixture = certifiedFixtures.find(fixture => fixture.name === 'moving-target')!;
  const raw = structuredClone(fixture);
  const statements = object(raw.response.observations).statements as JsonObject[];
  const control = statements.find(statement => statement.kind === 'piece_control')!;
  object(control.before).state = 'unknown';
  object(control.before).premise_ids = [];
  const decoded = moveReviewObservations(observationFixtureJob(raw))!.statements.find(
    statement => statement.kind === 'piece_control',
  );
  assert.ok(decoded?.kind === 'piece_control');
  assert.equal(decoded.before.state, 'unknown');
  assert.equal(decoded.after.state, 'present');
  for (const mutate of [
    (statement: JsonObject) => (statement.kind = 'maintained_control'),
    (statement: JsonObject) => (object(statement.after).controller = null),
    (statement: JsonObject) => (object(object(statement.after).controller).square = 'i9'),
    (statement: JsonObject) => (object(statement.after).state = 'legal'),
    (statement: JsonObject) => (statement.relation = 'legal_capture'),
    (statement: JsonObject) => (statement.unannounced = true),
  ]) {
    const invalid = structuredClone(fixture);
    const statement = (object(invalid.response.observations).statements as JsonObject[]).find(
      item => item.kind === 'piece_control',
    )!;
    mutate(statement);
    assert.throws(() => observationFixtureJob(invalid), /Rejected runtime observation fixture/);
  }
  const restricted = structuredClone(certifiedFixtures.find(fixture => fixture.name === 'pinned-recapture')!);
  const inventory = (object(restricted.response.observations).statements as JsonObject[]).find(
    item => item.kind === 'recapture_resources',
  )!;
  const restriction = (inventory.restricted_recaptures as JsonObject[])[0];
  delete restriction.post_move_controllers;
  assert.throws(() => observationFixtureJob(restricted), /Rejected runtime observation fixture/);
});

function typedSubject(before: FEN, played: Uci, after: FEN): MoveReviewSubject {
  return {
    variant: 'standard',
    initialFen: before,
    movePrefixUci: [],
    before: { path: '' as Tree.Path, fen: before },
    played: { uci: played, san: played as San },
    after: { path: 'aa' as Tree.Path, fen: after },
  };
}

test('emits the sole v6 standard played-focus request', () => {
  assert.deepEqual(buildMoveReviewJobRequest(requestId, subject, moveReviewEngineProfile), {
    schema_version: 'chesstory.position-commentary.job-request.v6',
    request_id: requestId,
    variant: 'standard',
    initial_fen: initialFen,
    move_prefix_uci: ['e2e4'],
    current_fen: beforeFen,
    focus: { kind: 'played_move', played_move_uci: 'e7e5', resulting_fen: afterFen },
    engine_profile: moveReviewEngineProfile,
  });
});

test('PGN record survives tree promotion while nested input alternatives and Study provenance stay distinct', () => {
  const data = pgnImport('1. e4 (1. d4 d5) d5 2. exd5 Qxd5 (2... Nf6) *');
  const root = treeReconstruct(data.treeParts!, data.sidelines);
  root.children.reverse(); // Promoting the alternative cannot rewrite the imported record.
  const e4 = root.children.find(node => node.uci === 'e2e4')!;
  const result = moveReviewSubjectFromNodeList('standard', e4.id, [root, e4], data.recordedMoves)!;
  assert.deepEqual(result.inputLines, [
    { source: 'pgn_record', moves: ['e2e4', 'd7d5', 'e4d5', 'd8d5'] },
    { source: 'analysis', moves: ['d2d4', 'd7d5'] },
    { source: 'analysis', moves: ['e2e4', 'd7d5', 'e4d5', 'g8f6'] },
  ]);
  assert.deepEqual(
    buildMoveReviewJobRequest('paths', result, moveReviewEngineProfile).input_lines,
    result.inputLines,
  );
  const study = moveReviewSubjectFromNodeList('standard', e4.id, [root, e4], undefined, 'study')!;
  assert(study.inputLines?.every(line => line.source === 'study'));
  const d4 = root.children[0]!;
  const d5 = d4.children[0]!;
  const alternative = moveReviewSubjectFromNodeList(
    'standard',
    d4.id + d5.id,
    [root, d4, d5],
    data.recordedMoves,
  )!;
  assert(alternative.inputLines?.every(line => line.source === 'analysis'));
});

test('standard review converts legal castling in focus, history and later record without changing rook moves', () => {
  const data = pgnImport('1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. O-O Nf6 5. d3 O-O *');
  const root = treeReconstruct(data.treeParts!, data.sidelines);
  const nodes = [root];
  while (nodes.at(-1)!.children.length) nodes.push(nodes.at(-1)!.children[0]!);
  const focus = moveReviewSubjectFromNodeList(
    'standard',
    nodes
      .slice(0, 8)
      .map(n => n.id)
      .join(''),
    nodes.slice(0, 8),
    data.recordedMoves,
  )!;
  assert.equal(focus.played.uci, 'e1g1');
  assert.deepEqual(focus.inputLines?.[0].moves, ['e1g1', 'g8f6', 'd2d3', 'e8g8']);
  const later = moveReviewSubjectFromNodeList(
    'standard',
    nodes.map(n => n.id).join(''),
    nodes,
    data.recordedMoves,
  )!;
  assert(later.movePrefixUci.includes('e1g1'));
  assert.equal(later.played.uci, 'e8g8');
  assert.equal(standardUci('3k4/8/8/8/8/8/8/K3R3 w - - 0 1', 'e1h1'), 'e1h1');
  assert.equal(standardUci('r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1', 'e1a1'), 'e1c1');
  assert.equal(standardUci('r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1', 'e8a8'), 'e8c8');
});

test('decodes the root and focused comparison work', () => {
  const root = decodeMoveReviewSnapshot(rawSnapshot('awaiting_core'), decodeContext());
  assert.equal(root?.kind, 'awaiting-core');
  if (root?.kind !== 'awaiting-core') return;
  assert.equal(root.issuedEngineWork.purpose, 'root_search');
  assert.equal(root.issuedEngineWork.searchLimits.multiPv, 3);
  assert.deepEqual(root.issuedEngineWork.rootRestriction, { kind: 'unrestricted' });

  const focus = decodeMoveReviewSnapshot(rawSnapshot('awaiting_evidence'), decodeContext());
  assert.equal(focus?.kind, 'awaiting-evidence');
  if (focus?.kind !== 'awaiting-evidence') return;
  assert.equal(focus.issuedEngineWork.purpose, 'focus_comparison');
  assert.deepEqual(focus.issuedEngineWork.rootRestriction, {
    kind: 'restricted',
    movesUci: ['c7c5', 'e7e5'],
  });

  const stopped = rawSnapshot('stopped');
  stopped.stop_condition = 'search_budget_exhausted';
  object(stopped.progress).legal_move_count = 0;
  const stoppedSnapshot = decodeMoveReviewSnapshot(stopped, decodeContext());
  assert.equal(stoppedSnapshot?.kind, 'abstained');
  assert.equal(
    stoppedSnapshot?.kind === 'abstained' && stoppedSnapshot.stopCondition,
    'search_budget_exhausted',
  );

  const causalWaves = structuredClone(stopped);
  object(causalWaves.progress).causal_waves = 0;
  assert.equal(decodeMoveReviewSnapshot(causalWaves, decodeContext()), undefined);

  const inventedStopCondition = structuredClone(stopped);
  inventedStopCondition.stop_condition = 'no_legal_moves';
  assert.equal(decodeMoveReviewSnapshot(inventedStopCondition, decodeContext()), undefined);
});

test('fails closed on unknown schemas, Chess960, execution drift, and illegal engine history', () => {
  const unsupported = rawSnapshot('awaiting_core');
  unsupported.schema_version = 'unsupported.position-commentary.job-status';
  assert.equal(decodeMoveReviewSnapshot(unsupported, decodeContext()), undefined);

  const chess960 = rawSnapshot('awaiting_core');
  chess960.variant = 'chess960';
  assert.equal(decodeMoveReviewSnapshot(chess960, decodeContext()), undefined);

  const drift = rawSnapshot('awaiting_core');
  object(drift.issued_engine_work).execution_key_sha256 = 'not-a-key';
  assert.equal(decodeMoveReviewSnapshot(drift, decodeContext()), undefined);

  const illegal = rawSnapshot('awaiting_evidence');
  object(illegal.issued_engine_work).engine_position_moves_uci = ['e2e5'];
  assert.equal(decodeMoveReviewSnapshot(illegal, decodeContext()), undefined);
});

test('projects an exact primary and its comparison proof', () => {
  const decoded = decodeMoveReviewSnapshot(rawResponse(), decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.uci, 'e7e5');
  assert.equal(played?.review.kind, 'move-verdict');
  if (!played || played.review.kind !== 'move-verdict') return;
  assert.equal(played.review.core.bestUci, 'c7c5');
  assert.equal(played.review.core.verdictSymbol, '?!');
  assert.equal(played.review.comparisonProofs[0]?.moves[0]?.uci, 'e7e5');
  assert.equal(
    moveReviewProofById(played.review, played.review.comparisonProofs[0]!.id),
    played.review.comparisonProofs[0],
  );
});

test('rejects the retired occurrence-relations commentary field', () => {
  const raw = rawResponse();
  const selected = object(raw.result).selected_move_reviews as JsonObject[];
  const commentary = object(selected.find(review => object(review).commentary)?.commentary);
  commentary.occurrence_relations = [];
  assert.equal(decodeMoveReviewSnapshot(raw, decodeContext()), undefined);
});

test('an occurrence shared by observation paths must retain its UCI and both replayed FENs', () => {
  const path = (id: string, moves: Uci[], occurrenceIds: string[]) => ({
    id,
    kind: 'analysis',
    start_fen: beforeFen,
    moves,
    occurrence_ids: occurrenceIds,
  });
  const raw = rawSnapshot('stopped');
  raw.observations = {
    statements: [],
    paths: [
      path('line:a', ['e7e5', 'g1f3'], ['occ:e5', 'occ:nf3']),
      path('line:b', ['e7e5', 'g1f3', 'b8c6'], ['occ:e5', 'occ:nf3', 'occ:nc6']),
    ],
  };
  assert.ok(decodeMoveReviewSnapshot(raw, decodeContext())?.observations);

  // Same UCI, different boards: Nf3 follows ...c5 instead of ...e5.
  object(raw.observations).paths = [
    path('line:a', ['e7e5', 'g1f3'], ['occ:e5', 'occ:nf3']),
    path('line:b', ['c7c5', 'g1f3'], ['occ:c5', 'occ:nf3']),
  ];
  assert.equal(decodeMoveReviewSnapshot(raw, decodeContext()), undefined);
});

test('occurrence validation compares the before FEN even when capture paths converge afterwards', () => {
  const before = 'r6k/1P6/8/8/8/8/8/7K w - - 0 1' as FEN;
  const after = 'rN5k/8/8/8/8/8/8/7K b - - 0 1' as FEN;
  const current = typedSubject(before, 'b7b8n', after);
  const raw = rawSnapshot('stopped');
  raw.focus = { kind: 'played_move', played_move_uci: 'b7b8n', resulting_fen: after };
  const paths = [
    {
      id: 'line:knight',
      kind: 'analysis',
      start_fen: before,
      moves: ['b7b8n', 'a8b8'],
      occurrence_ids: ['occ:promote-knight', 'occ:capture-knight'],
    },
    {
      id: 'line:rook',
      kind: 'analysis',
      start_fen: before,
      moves: ['b7b8r', 'a8b8'],
      occurrence_ids: ['occ:promote-rook', 'occ:capture-rook'],
    },
  ];
  raw.observations = {
    statements: [],
    paths,
  };
  const decoded = decodeMoveReviewSnapshot(raw, { requestId, subject: current });
  assert.ok(decoded?.observations);
  assert.equal(
    decoded.observations.paths[0].moves[1].fenAfter,
    decoded.observations.paths[1].moves[1].fenAfter,
  );
  paths[1].occurrence_ids[1] = paths[0].occurrence_ids[1];
  assert.equal(decodeMoveReviewSnapshot(raw, { requestId, subject: current }), undefined);
});

test('preserves BestChoice and its transmitted runner-up comparison without synthesizing improves', () => {
  const raw = JSON.parse(
    readFileSync(
      new URL(
        '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/best-choice-produced.json',
        import.meta.url,
      ),
      'utf8',
    ),
  ) as JsonObject;
  const producedAfter = object(raw.focus).resulting_fen as FEN;
  const decoded = decodeMoveReviewSnapshot(raw, {
    requestId: 'request-best-choice',
    subject: typedSubject(initialFen, 'e2e4', producedAfter),
    engineProfile: moveReviewEngineProfile,
  });
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (played?.review.kind !== 'move-verdict') return;
  const core = played.review.core;
  assert.equal(core.kind, 'best-choice');
  if (core.kind !== 'best-choice') return;
  assert.equal(core.verdictSymbol, 'none');
  assert.equal('verdictCode' in core, false);
  assert.deepEqual(core.bestChoice, {
    runnerUpVerdictCode: 'playable_loss',
    runnerUpUci: 'd2d4',
  });
  assert.equal(core.bestUci, 'e2e4');
  assert.ok(core.winChance!.changePercentagePoints > 0);
  assert.equal(played.review.comparisonProofs.length, 2);
  const runnerUpProof = played.review.comparisonProofs[1]!;
  assert.equal(runnerUpProof.moves[0]?.uci, 'd2d4');
  assert.equal(moveReviewProofById(played.review, runnerUpProof.id), runnerUpProof);
});

test('preserves forced single moves and explicit position actions', () => {
  const forced = rawResponse({
    progress: {
      phase: 'completed',
      legal_move_count: 1,
      root_candidate_lines_admitted: 1,
      selected_commentaries_completed: 0,
      physical_works_issued: 1,
      physical_reports_accepted: 1,
    },
    result: {
      kind: 'forced_single_move',
      move_uci: 'e7e5',
      supporting_endpoint: {
        kind: 'engine_search',
        moves: ['e7e5', 'g1f3'],
        win_percent_for_mover: 50,
        depth: 16,
      },
    },
  });
  const forcedDecoded = decodeMoveReviewSnapshot(forced, decodeContext());
  assert.equal(forcedDecoded?.kind, 'completed');
  if (forcedDecoded?.kind === 'completed')
    assert.equal(forcedDecoded.evidence.candidates[0]?.review.kind, 'forced-single-move');

  const terminal = rawResponse({
    progress: {
      phase: 'completed',
      legal_move_count: 1,
      root_candidate_lines_admitted: 0,
      selected_commentaries_completed: 0,
      physical_works_issued: 0,
      physical_reports_accepted: 0,
    },
    result: { kind: 'automatic_terminal', terminal: { kind: 'stalemate' } },
  });
  assert.deepEqual(
    (decodeMoveReviewSnapshot(terminal, decodeContext()) as { action?: unknown } | undefined)?.action,
    { kind: 'automatic-terminal', terminal: { kind: 'stalemate' } },
  );

  const draw = rawResponse({
    result: {
      kind: 'draw_claim_action',
      claims: [{ rule: 'threefold_repetition', availability: 'available_now' }],
    },
  });
  assert.equal(decodeMoveReviewSnapshot(draw, decodeContext())?.kind, 'position-action');
});

test('emits only the v6 work identity and admitted line suffixes in reports', () => {
  const decoded = decodeMoveReviewSnapshot(rawSnapshot('awaiting_core'), decodeContext());
  assert.equal(decoded?.kind, 'awaiting-core');
  if (decoded?.kind !== 'awaiting-core') return;
  const work = decoded.issuedEngineWork;
  const report = buildMoveReviewEngineWorkReport(work, {
    kind: 'completed',
    completedDepth: 16,
    selectiveDepth: 20,
    nodes: 100_000,
    engineTimeMs: 500,
    executorElapsedMs: 550,
    bestmoveUci: 'c7c5',
    lineSuffixes: [
      { moves: ['c7c5'], depth: 16, whiteScore: { kind: 'cp', value: 20 } },
      { moves: ['e7e5'], depth: 16, whiteScore: { kind: 'cp', value: 10 } },
      { moves: ['g8f6'], depth: 16, whiteScore: { kind: 'cp', value: 5 } },
    ],
  });
  assert.deepEqual(Object.keys(report), [
    'schema_version',
    'engine_profile',
    'work_id',
    'execution_key_sha256',
    'outcome',
  ]);
  assert.deepEqual(Object.keys(object(report.outcome)), ['kind', 'line_suffixes']);
});

test('requires exact D15 and D16 browser bundles before reporting D16', () => {
  const decoded = decodeMoveReviewSnapshot(rawSnapshot('awaiting_core'), decodeContext());
  assert.equal(decoded?.kind, 'awaiting-core');
  if (decoded?.kind !== 'awaiting-core') return;
  const work = decoded.issuedEngineWork;
  const pvs = [
    { moves: ['c7c5'], cp: 20 },
    { moves: ['e7e5'], cp: 10 },
    { moves: ['g8f6'], cp: 5 },
  ];
  const previous = {
    fen: beforeFen,
    depth: 15,
    nodes: 50_000,
    millis: 250,
    pvs,
  } as Tree.LocalEval;
  const current = {
    fen: beforeFen,
    depth: 16,
    nodes: 100_000,
    millis: 500,
    bestmove: 'c7c5',
    pvs,
  } as Tree.LocalEval;
  assert.equal(moveReviewEngineOutcomeAtRequiredDepth(work, current, undefined), undefined);
  const searchScope: MoveReviewSearchScope = {
    enginePositionInitialFen: work.enginePositionInitialFen,
    enginePositionMovesUci: work.enginePositionMovesUci,
    searchFen: work.searchFen,
    rootRestriction: work.rootRestriction,
    searchLimits: work.searchLimits,
    maxSearchElapsedMs: work.maxSearchElapsedMs,
  };
  assert.equal(moveReviewEngineOutcomeAtRequiredDepth(searchScope, current, previous)?.kind, 'completed');
});

test('supports standard subjects only and keeps locale in frontend formatting', () => {
  const nodes = [
    { fen: initialFen },
    { id: 'aa', fen: beforeFen, uci: 'e2e4', san: 'e4' },
  ] as Tree.NodeBase[];
  assert.ok(moveReviewSubjectFromNodeList('standard', 'aa', nodes));
  assert.equal(moveReviewSubjectFromNodeList('chess960', 'aa', nodes), undefined);
  assert.equal(normalizeMoveReviewLocale('ko-KR'), 'ko-KR');
  assert.equal(formatMoveReviewPercent(54.25, 'en-US'), '54.3%');
  assert.equal(formatMoveReviewPercentagePointChange(-8, 'en-US'), '-8.0%p');
});

test('executor failure report does not echo metrics or diagnostics to the server', () => {
  const work = (
    decodeMoveReviewSnapshot(rawSnapshot('awaiting_core'), decodeContext()) as {
      issuedEngineWork: IssuedMoveReviewEngineWork;
    }
  ).issuedEngineWork;
  const report = buildMoveReviewEngineWorkReport(work, {
    kind: 'executor_failed',
    executorElapsedMs: 10,
    observedNodes: 1,
    engineTimeMs: 5,
    failureCode: 'search_budget_exhausted',
    diagnostic: 'local detail',
  });
  assert.deepEqual(report.outcome, { kind: 'executor_failed', failure_code: 'search_budget_exhausted' });
});
