import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, moveReviewSan, type MoveReviewSubject } from '../src/moveReview';
import { reviewedMoverResponsePoolMatches, responsePoolMatchesClasses } from '../src/moveReviewSelection';

// These are byte-exact actual meaningv37 public snapshots. Positive tests change
// neither a request nor a receipt. Negative mutations test client admission;
// they are not new engine reports, scheduling successes or chess judgments.
const fixtures = {
  'candidates2018-09': ['pending', 'c297f10652cf38b67eaa113fc50969f6e53294d9d423bd4ca43af31c34f36225'],
  'wch2024-07': ['pending', 'b0d06dec59bca9c6718b605b6f1c426e8688496564a9b06d164f27c615a8dbb1'],
  'TAT25-10': ['pending', '017d472037a2f7e1c797206dafe04bb297ac9c7cfc15c22fcbf906779412f67a'],
  'wch2016-08': ['completed', '87e0bcb45ea739f7e70a7a520a2d2ed0510a7cea9e1db0892232a4d370cd4e4d'],
} as const;
type ActualCase = keyof typeof fixtures;
function actualJob(name: ActualCase) {
  const [status, sha] = fixtures[name];
  const bytes = readFileSync(new URL(
    `../../../../../../judgment-evaluation/fixtures/public-commentary-v6/response-scope-${name}-${status}-produced.json.gz`,
    import.meta.url,
  ));
  assert.equal(createHash('sha256').update(bytes).digest('hex'), sha, 'use the exact sealed actual fixture');
  const fixture = JSON.parse(gunzipSync(bytes).toString('utf8'));
  const request = fixture.request;
  return { raw: fixture.snapshot, context: {
    requestId: request.request_id,
    subject: {
      variant: 'standard', initialFen: request.initial_fen, movePrefixUci: request.move_prefix_uci,
      before: { path: '', fen: request.current_fen },
      played: { uci: request.focus.played_move_uci,
        san: moveReviewSan(request.current_fen, request.focus.played_move_uci)! },
      after: { path: 'aa', fen: request.focus.resulting_fen },
    } as MoveReviewSubject,
  } };
}
function mover(raw: any) {
  const played = raw.focus.played_move_uci;
  const inquiry = raw.observations.selection.response_inquiries.find((value: any) =>
    value.root_move === played && value.original_target.square === played.slice(0, 2) &&
    value.target.square === played.slice(2, 4));
  assert.ok(inquiry, 'the actual reviewed mover must own a target inquiry');
  return inquiry;
}
function sameChild(search: any, inquiry: any) {
  const prefix = search.prefix_moves_uci ?? [search.root_move];
  return prefix.length === 1 && prefix[0] === inquiry.root_move &&
    search.root_move === inquiry.root_move && search.root_move_occurrence_id === inquiry.root_move_occurrence_id &&
    search.child_position_id === inquiry.child_position_id;
}
function wholePool(raw: any, inquiry: any) {
  const found = raw.observations.selection.response_searches.filter((value: any) =>
    sameChild(value, inquiry) && value.root_restriction.kind === 'unrestricted')
    .sort((left: any, right: any) => Number(right.work_id.slice(5)) - Number(left.work_id.slice(5)))[0];
  assert.ok(found, 'use the actual latest completed unrestricted receipt at this child');
  return found;
}
function rejectedPending(raw: any, context: ReturnType<typeof actualJob>['context'], label: string) {
  assert.ok(decodeMoveReviewSnapshot(raw, context) === undefined, label);
}
function rejectedSelection(raw: any, context: ReturnType<typeof actualJob>['context'], label: string) {
  const decoded = decodeMoveReviewSnapshot(raw, context);
  // Invalid optional commentary must not erase a valid completed numeric result.
  // Small boolean assertions avoid rendering multi-megabyte object diffs.
  assert.ok(decoded?.kind === 'completed', `keep the valid numerical result: ${label}`);
  assert.ok(decoded.observations?.selection === undefined, label);
}
function* objects(value: any): Generator<any> {
  if (Array.isArray(value)) for (const child of value) yield* objects(child);
  else if (value && typeof value === 'object') {
    yield value;
    for (const child of Object.values(value)) yield* objects(child);
  }
}
function usedPawnSupport(raw: any) {
  const effect = [...objects(raw.observations.selection)].find(value => value.scope === 'support_use' &&
    value.actor?.square === 'e6' && value.target?.square === 'f7' && value.square === 'f5' &&
    value.through_plies?.length === 1 && value.through_plies[0] === 10);
  assert.ok(effect, 'use the actual Be6 support of the same original f7 pawn, later lost on f5');
  return effect;
}
function retainedInquiry(raw: any) {
  const inquiry = raw.observations.selection.response_inquiries.find((value: any) =>
    value.id === 'target-response:18227fe1e9ee42fc9feeaf856f22dca8e0176cff805647b0f502e213b7b6afd8');
  assert.ok(inquiry, 'use the exact completed Rxd8 versus Rd2 inquiry');
  return inquiry;
}

test('three unchanged actual pending first-response descriptors are admitted without completing their work', () => {
  for (const name of ['candidates2018-09', 'wch2024-07', 'TAT25-10'] as const) {
    const { raw, context } = actualJob(name), work = raw.issued_engine_work;
    const originalWork = JSON.stringify(work), originalResult = JSON.stringify(raw.result);
    const decoded = decodeMoveReviewSnapshot(raw, context);
    assert.ok(decoded?.kind === 'awaiting-evidence', `admit the actual pending ${name} descriptor`);
    assert.equal(decoded.issuedEngineWork.workId, work.work_id);
    assert.equal(decoded.issuedEngineWork.executionKeySha256, work.execution_key_sha256);
    assert.deepEqual(decoded.issuedEngineWork.rootRestriction,
      { kind: 'restricted', movesUci: work.root_restriction.moves_uci });
    assert.deepEqual(decoded.issuedEngineWork.searchLimits,
      { depth: work.search_limits.depth, nodes: 40_000_000, movetimeMs: 8_000,
        multiPv: work.search_limits.multi_pv });
    assert.equal(decoded.issuedEngineWork.maxSearchElapsedMs, 9_000);
    assert.ok(decoded.observations?.selection, 'preserve the validated selection beside pending work');
    assert.ok(JSON.stringify(raw.result) === originalResult && JSON.stringify(work) === originalWork,
      'neither a numerical judgment nor a pending work is changed');
    if (name !== 'candidates2018-09') {
      const inquiry = mover(raw);
      assert.equal(work.search_limits.multi_pv, 3);
      assert.equal(work.search_limits.depth, name === 'wch2024-07' ? 17 : 16);
      assert.equal(inquiry.capture_moves.length, 2);
      assert.ok(inquiry.capture_moves.every((move: string) => work.root_restriction.moves_uci.includes(move)));
      const kept = decoded.observations.selection.response_inquiries?.find(value => value.id === inquiry.id);
      assert.equal(kept?.state, 'awaiting_engine');
      assert.equal(kept?.answer, null, 'admission is not a comparison answer');
    } else {
      assert.notEqual(work.search_anchor.root_move, context.subject.played.uci,
        'the generated alternative-root proposal retains its existing separate allowance');
    }
  }
});

test('the common class matcher uses only the reviewed mover missing class and exact current whole-pool leader', () => {
  for (const name of ['wch2024-07', 'TAT25-10'] as const) {
    const { raw, context } = actualJob(name), inquiry = mover(raw), full = wholePool(raw, inquiry);
    const searches = raw.observations.selection.response_searches;
    const pool = raw.issued_engine_work.root_restriction.moves_uci;
    assert.equal(full.candidates[0].move_uci, name === 'wch2024-07' ? 'b6b7' : 'f7f6');
    assert.ok(reviewedMoverResponsePoolMatches([inquiry], pool, searches, context.subject.played.uci));
    assert.ok(responsePoolMatchesClasses([inquiry], pool, searches, context.subject.played.uci));
    assert.ok(!reviewedMoverResponsePoolMatches([inquiry], [inquiry.capture_moves[0], full.candidates[0].move_uci],
      searches, context.subject.played.uci), 'one sampled capture cannot stand for the whole missing class');
    const foreign = searches.map((search: any) => search === full ?
      { ...search, child_position_id: inquiry.original_target.position_id } : search);
    assert.ok(!reviewedMoverResponsePoolMatches([inquiry], pool, foreign, context.subject.played.uci),
      'a full receipt from another child cannot supply a leader');
    const otherTarget = raw.observations.selection.response_inquiries.find((value: any) =>
      sameChild(full, value) && value.id !== inquiry.id);
    assert.ok(otherTarget, 'a real different target is co-observed at this child');
    assert.ok(!reviewedMoverResponsePoolMatches([otherTarget], pool, searches, context.subject.played.uci),
      'that other physical target cannot authorize the reviewed mover class');
  }
});

test('the actual three-root allowance rejects a partial class and another legal decline instead of the leader', () => {
  for (const name of ['wch2024-07', 'TAT25-10'] as const) {
    const { raw, context } = actualJob(name), inquiry = mover(raw), full = wholePool(raw, inquiry);
    const leader = full.candidates[0].move_uci;
    const otherDecline = inquiry.decline_moves.find((move: string) => move !== leader);
    assert.ok(otherDecline, 'choose an existing legal different decline, without inventing an engine result');
    for (const pool of [[inquiry.capture_moves[0], leader], [...inquiry.capture_moves, otherDecline]]) {
      const changed = structuredClone(raw);
      changed.issued_engine_work.root_restriction.moves_uci = pool;
      changed.issued_engine_work.search_limits.multi_pv = pool.length;
      rejectedPending(changed, context, `${name}: exact whole class plus its actual strongest leader required`);
    }
  }
});

test('the class allowance preserves exact prefix occurrence child depth and existing limit tuples', () => {
  const { raw, context } = actualJob('wch2024-07');
  for (const [label, mutate] of [
    ['two-ply prefix', (work: any) => { work.search_anchor.prefix_moves_uci = ['f4e3', 'b6b7']; }],
    ['foreign occurrence', (work: any) => { work.search_anchor.root_occurrence_id = 'foreign-root-occurrence'; }],
    ['root position instead of child', (work: any) => { work.search_anchor.position_id = mover(raw).original_target.position_id; }],
    ['shallower profile', (work: any) => { work.search_limits.depth = 15; }],
    ['mixed node/time tuple', (work: any) => { work.search_limits.nodes = 20_000_000; }],
    ['foreign lease duration', (work: any) => { work.max_search_elapsed_ms = 9_001; }],
  ] as [string, (work: any) => void][]) {
    const changed = structuredClone(raw); mutate(changed.issued_engine_work);
    rejectedPending(changed, context, label);
  }
});

test('an absent deferred or foreign physical mover cannot authorize the existing longer class allowance', () => {
  const { raw, context } = actualJob('TAT25-10');
  for (const [label, mutate] of [
    ['deferred mover', (changed: any) => { const value = mover(changed); value.state = 'deferred'; value.stop_reason = 'investigation_limit'; }],
    ['foreign original target', (changed: any) => { mover(changed).original_target.square = 'c4'; }],
    ['mover question absent', (changed: any) => {
      const id = mover(changed).id;
      changed.observations.selection.response_inquiries = changed.observations.selection.response_inquiries
        .filter((value: any) => value.id !== id);
      for (const value of changed.observations.selection.response_inquiries)
        value.related_inquiry_ids = value.related_inquiry_ids.filter((related: string) => related !== id);
    }],
  ] as [string, (changed: any) => void][]) {
    const changed = structuredClone(raw); mutate(changed);
    rejectedPending(changed, context, label);
  }
});

test('newly gained same-pawn support keeps its continuous evidence and exact immediate recapture requirements', () => {
  const { raw, context } = actualJob('candidates2018-09'), effect = usedPawnSupport(raw);
  const path = raw.observations.paths.find((value: any) => value.id === effect.line_ids[0]);
  assert.deepEqual(path.moves.slice(7, 10), ['f7f5', 'e4f5', 'e6f5']);
  assert.equal(effect.target.square, 'f7'); assert.equal(effect.square, 'f5');
  const middle = raw.observations.positions.find((value: any) => value.id === path.position_ids[4]);
  const control = middle.controls.pieces.find((value: any) => value.piece.square === 'e6');
  const supportSource = control.source_ids[control.squares.indexOf('f7')];
  assert.ok(effect.evidence_ids.includes(supportSource), 'the same original pawn support is certified through the route');
  const immediate = raw.observations.positions.find((value: any) => value.id === path.position_ids[9]);
  const recapture = immediate.legal_moves.moves.find((value: any) => value.uci === 'e6f5');
  assert.ok(recapture && effect.evidence_ids.includes(recapture.source_id));
  for (const [label, mutate] of [
    ['continuous support evidence absent', (changed: any) => {
      const value = usedPawnSupport(changed); value.evidence_ids = value.evidence_ids.filter((id: string) => id !== supportSource);
    }],
    ['immediate legal recapture evidence absent', (changed: any) => {
      const value = usedPawnSupport(changed); value.evidence_ids = value.evidence_ids.filter((id: string) => id !== recapture.source_id);
    }],
    ['foreign provider identity', (changed: any) => { usedPawnSupport(changed).actor.square = 'd7'; }],
  ] as [string, (changed: any) => void][]) {
    const changed = structuredClone(raw); mutate(changed); rejectedPending(changed, context, label);
  }
});

test('a completed contested response retains its exact earlier choice origin and its distinct current answer receipt', () => {
  const { raw, context } = actualJob('wch2016-08'), inquiry = retainedInquiry(raw);
  const originalAnswer = JSON.stringify(inquiry.answer), originalResult = JSON.stringify(raw.result);
  assert.equal(inquiry.reference_selection.work_id, 'work:2');
  assert.equal(inquiry.answer.work_id, 'work:4');
  assert.notEqual(inquiry.reference_selection.line_id, inquiry.answer.reference_line_id);
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'completed' && decoded.observations?.selection,
    'the optional actual contested_pair_response must survive beside the completed numeric result');
  const kept = decoded.observations.selection.response_inquiries?.find(value => value.id === inquiry.id);
  assert.equal(kept?.reference_move, 'd4d8');
  assert.equal(kept?.reference_selection?.work_id, 'work:2');
  assert.equal(kept?.answer?.work_id, 'work:4');
  assert.equal(kept?.answer?.required_depth, 17);
  assert.ok(JSON.stringify(kept?.answer) === originalAnswer, 'never borrow earlier source scores');
  assert.ok([...objects(decoded.observations.selection)].some(value => value.question === 'contested_pair_response'));
  assert.ok(JSON.stringify(raw.result) === originalResult, 'root numerical judgment is unchanged');
});

test('a retained choice cannot borrow a wrong work line prefix child or engine profile', () => {
  const { raw, context } = actualJob('wch2016-08');
  for (const [label, mutate] of [
    ['wrong claimed work', (changed: any) => { retainedInquiry(changed).reference_selection.work_id = 'work:3'; }],
    ['wrong claimed line', (changed: any) => {
      const value = retainedInquiry(changed); value.reference_selection.line_id = value.alternative_selection.line_id;
    }],
    ['foreign retained prefix', (changed: any) => {
      const source = changed.observations.selection.response_searches.find((value: any) => value.work_id === 'work:2');
      source.prefix_moves_uci = ['g4f6'];
    }],
    ['foreign retained child', (changed: any) => {
      const source = changed.observations.selection.response_searches.find((value: any) => value.work_id === 'work:2');
      source.child_position_id = retainedInquiry(changed).original_target.position_id;
    }],
    ['foreign retained engine profile', (changed: any) => {
      changed.observations.selection.response_searches.find((value: any) => value.work_id === 'work:2').engine_profile = 'foreign-profile';
    }],
  ] as [string, (changed: any) => void][]) {
    const changed = structuredClone(raw); mutate(changed); rejectedSelection(changed, context, label);
  }
});

test('an exact earlier legal choice is still rejected when it is a different current class representative', () => {
  const { raw, context } = actualJob('wch2016-08'), inquiry = retainedInquiry(raw);
  const full = raw.observations.selection.response_searches.find((value: any) => value.work_id === 'work:2');
  const weaker = full.candidates.find((value: any) => value.move_uci === 'c4c5');
  assert.ok(weaker && inquiry.decline_moves.includes(weaker.move_uci));
  assert.equal(full.candidates.find((value: any) => inquiry.decline_moves.includes(value.move_uci)).move_uci, 'e1d2');
  inquiry.alternative_move = weaker.move_uci;
  inquiry.alternative_selection = { source: 'child_search', work_id: full.work_id, line_id: weaker.line_id };
  // Remove the now-inapplicable answer rather than synthesizing a replacement.
  inquiry.state = 'observed'; inquiry.answer = null;
  rejectedSelection(raw, context, 'an exact owned legal c5 line cannot substitute for the current stronger Rd2 representative');
});

test('preserving a source choice never validates a mismatched current answer', () => {
  const { raw, context } = actualJob('wch2016-08');
  for (const [label, mutate] of [
    ['old source line is not the current answer line', (changed: any) => {
      const value = retainedInquiry(changed); value.answer.reference_line_id = value.reference_selection.line_id;
    }],
    ['current answer cannot borrow old source work', (changed: any) => {
      retainedInquiry(changed).answer.work_id = 'work:2';
    }],
  ] as [string, (changed: any) => void][]) {
    const changed = structuredClone(raw); mutate(changed); rejectedSelection(changed, context, label);
  }
});
