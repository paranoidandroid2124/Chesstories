import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { Protocol } from '../../src/engine/protocol';
import { makeMoveReviewWork, moveReviewEngineProfile, type MoveReviewWorkInput } from '../../src/engine/types';
import { moveReviewEngineOutcomeAtRequiredDepth, type IssuedMoveReviewEngineWork } from '../../src/review/moveReview';

const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../../judgment-evaluation/fixtures/public-commentary-v6/h05-work17-same-depth-pv-replacement.json.gz', import.meta.url,
))).toString('utf8'));
const native = fixture.issued_work;
const issued = {
  engineProfile: native.engine_profile,
  workId: native.work_id,
  purpose: native.purpose,
  executionKeySha256: native.execution_key_sha256,
  variant: native.variant,
  enginePositionInitialFen: native.engine_position_initial_fen,
  enginePositionMovesUci: native.engine_position_moves_uci,
  searchFen: native.search_fen,
  searchAnchor: {
    rootMove: native.search_anchor.root_move,
    rootOccurrenceId: native.search_anchor.root_occurrence_id,
    positionId: native.search_anchor.position_id,
    prefixMovesUci: native.search_anchor.prefix_moves_uci,
  },
  rootRestriction: native.root_restriction,
  searchLimits: {
    depth: native.search_limits.depth, nodes: native.search_limits.nodes,
    movetimeMs: native.search_limits.movetime_ms, multiPv: native.search_limits.multi_pv,
  },
  maxSearchElapsedMs: native.max_search_elapsed_ms,
} as IssuedMoveReviewEngineWork;

function review(multiPv = 3) {
  const evaluations: Tree.LocalEval[] = [], commands: string[] = [], usage: Array<[number, number]> = [];
  const protocol = new Protocol();
  protocol.connected(command => commands.push(command));
  protocol.received('uciok');
  protocol.received('readyok');
  const work = makeMoveReviewWork(moveReviewEngineProfile, {
    variant: 'standard', path: 'owned-work17-transcript', ply: 70,
    initialFen: issued.enginePositionInitialFen, currentFen: issued.searchFen,
    moves: [...issued.enginePositionMovesUci], multiPv, rootMoves: [],
    // This immutable fixture is the separately declared research lease. It is
    // replayed as protocol input, never issued as an ordinary product request.
    searchLimits: issued.searchLimits as MoveReviewWorkInput['searchLimits'],
    observe: (nodes, millis) => usage.push([nodes, millis]),
    emit: evaluation => evaluations.push(structuredClone(evaluation)),
  });
  protocol.compute(work);
  protocol.received('readyok');
  return { protocol, work, evaluations, commands, usage };
}
const info = (depth: number, index: number, move: string, cp: number, extra = '') =>
  `info depth ${depth} seldepth 30 multipv ${index} score cp ${cp} ${extra} nodes 100 time 1 pv ${move}`;
const batch = (protocol: Protocol, depth: number, moves: string[], scores = [30, 20, 10]) =>
  moves.forEach((move, index) => protocol.received(info(depth, index + 1, move, scores[index]!)));

test('actual work17 final same-depth batch keeps its own rank, PV and final bestmove', () => {
  const { protocol, evaluations, usage } = review();
  for (const row of fixture.execution.execution.transcript) if (row.received) protocol.received(row.received);
  const final = evaluations.at(-1)!;
  assert.equal(fixture.execution.report.outcome.kind, 'executor_failed', 'the historical failed report is untouched');
  assert.equal(fixture.execution.report.outcome.failure_code, 'search_budget_exhausted');
  assert.equal(final.depth, 20);
  assert.equal(final.bestmove, 'e3f3');
  assert.deepEqual(final.pvs.map(pv => [pv.moves[0], pv.cp, pv.depth, pv.bound]), [
    ['e3f3', 571, 20, undefined], ['e3f4', 553, 20, undefined], ['e3d2', 524, 20, undefined],
  ]);
  assert.equal(final.nodes, 45_106_226);
  assert.equal(final.millis, 17_282);
  assert.deepEqual(usage.at(-1), [45_106_226, 17_282]);
  const previous = evaluations.filter(evaluation => evaluation.depth === 19).at(-1);
  const result = moveReviewEngineOutcomeAtRequiredDepth(issued, final, previous, 17_985);
  assert.equal(result?.kind, 'completed', 'the result checker receives a whole genuine final batch');
  if (result?.kind === 'completed') {
    assert.equal(result.completedDepth, 20);
    assert.equal(result.bestmoveUci, 'e3f3');
    assert.deepEqual(result.lineSuffixes.map(line => line.moves), final.pvs.map(pv => pv.moves));
  }
});

test('a changing same-depth leader cannot borrow secondary PVs from the previous batch', () => {
  const { protocol, evaluations } = review();
  batch(protocol, 19, ['e3f3', 'e3f4', 'e3d2']);
  batch(protocol, 20, ['e3f3', 'e3f4', 'e3d2']);
  const completedCount = evaluations.length;
  protocol.received(info(20, 1, 'e3f4', 40));
  protocol.received(info(20, 2, 'e3d2', 35));
  assert.equal(evaluations.length, completedCount, 'two fresh lines do not complete an MPV3 result');
  protocol.received(info(20, 3, 'e3f3', 30));
  assert.equal(evaluations.length, completedCount + 1);
  protocol.received('bestmove e3f4');
  assert.deepEqual(evaluations.at(-1)?.pvs.map(pv => [pv.moves[0], pv.cp]), [
    ['e3f4', 40], ['e3d2', 35], ['e3f3', 30],
  ]);
  assert.equal(moveReviewEngineOutcomeAtRequiredDepth(issued, evaluations.at(-1)!, evaluations[0], 1)?.kind, 'completed');
});

test('an incomplete newer batch remains incomplete even when bestmove arrives', () => {
  const { protocol, evaluations } = review();
  batch(protocol, 19, ['e3f3', 'e3f4', 'e3d2']);
  batch(protocol, 20, ['e3f3', 'e3f4', 'e3d2']);
  protocol.received(info(20, 1, 'e3f4', 40));
  protocol.received(info(20, 2, 'e3d2', 35));
  protocol.received('bestmove e3f4');
  const final = evaluations.at(-1)!;
  assert.equal(final.pvs[0]?.moves[0], 'e3f3', 'only the older complete batch can be retained');
  assert.equal(moveReviewEngineOutcomeAtRequiredDepth(issued, final, evaluations[0], 1), undefined,
    'the final bestmove mismatch cannot be repaired with a partial or fabricated PV');
});

test('lower-depth and orphan secondary lines do not complete a fresh current batch', () => {
  const { protocol, evaluations } = review();
  batch(protocol, 19, ['e3f3', 'e3f4', 'e3d2']);
  protocol.received(info(20, 2, 'e3d2', 40));
  protocol.received(info(20, 3, 'e3f3', 30));
  assert.equal(evaluations.length, 1, 'secondary lines without their new primary are not a batch');
  protocol.received(info(20, 1, 'e3f4', 50));
  protocol.received(info(19, 2, 'e3f3', 45));
  protocol.received(info(20, 3, 'e3d2', 30));
  assert.equal(evaluations.length, 1, 'a lower-depth secondary cannot fill the missing current index');
  protocol.received(info(20, 2, 'e3f3', 45));
  assert.deepEqual(evaluations.at(-1)?.pvs.map(pv => [pv.moves[0], pv.depth]), [
    ['e3f4', 20], ['e3f3', 20], ['e3d2', 20],
  ]);
});

test('a bound primary cannot borrow an earlier partial primary and exact secondary bounds remain visible', () => {
  const { protocol, evaluations } = review();
  batch(protocol, 19, ['e3f3', 'e3f4', 'e3d2']);
  protocol.received(info(20, 1, 'e3f3', 30));
  protocol.received(info(20, 1, 'e3f4', 50, 'lowerbound'));
  protocol.received(info(20, 2, 'e3d2', 40));
  protocol.received(info(20, 3, 'e3f3', 30));
  assert.equal(evaluations.length, 1, 'no exact batch is emitted after a primary bound');
  protocol.received(info(20, 1, 'e3f4', 50));
  protocol.received(info(20, 2, 'e3d2', 40, 'upperbound'));
  protocol.received(info(20, 3, 'e3f3', 30));
  protocol.received('bestmove e3f4');
  assert.equal(evaluations.at(-1)?.pvs[1]?.bound, 'upperbound');
  assert.equal(moveReviewEngineOutcomeAtRequiredDepth(issued, evaluations.at(-1)!, evaluations[0], 1), undefined,
    'a completed protocol batch still cannot certify bounded numerical values');
});

test('cancellation observes final usage while refusing another same-depth batch', () => {
  const { protocol, evaluations, usage, commands } = review();
  batch(protocol, 19, ['e3f3', 'e3f4', 'e3d2']);
  protocol.compute(undefined);
  assert.equal(commands.at(-1), 'stop');
  batch(protocol, 20, ['e3f4', 'e3d2', 'e3f3']);
  assert.equal(evaluations.length, 1);
  assert.deepEqual(usage.at(-1), [100, 1]);
  protocol.received('bestmove e3f4');
  assert.equal(evaluations.at(-1)?.depth, 19);
  assert.equal(moveReviewEngineOutcomeAtRequiredDepth(issued, evaluations.at(-1)!, undefined, 1), undefined);
});
