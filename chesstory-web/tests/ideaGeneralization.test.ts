import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { test } from 'node:test';
import { decodeMoveReviewSnapshot, type MoveReviewSubject } from '../src/review/moveReview';

// First real Ng8 screen failed before submitting this correctly owned task.
const row = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../judgment-evaluation/fixtures/public-commentary-v6/idea-generalization-pending.json.gz', import.meta.url,
))).toString('utf8'));
function contextFor(r: any) { return { requestId: r.request_id, engineProfile: r.engine_profile, subject: {
  variant: 'standard', initialFen: r.initial_fen, movePrefixUci: r.move_prefix_uci,
  inputLines: r.input_lines, before: { path: '', fen: r.current_fen },
  after: { path: 'aa', fen: r.focus.resulting_fen }, played: { uci: r.focus.played_move_uci, san: 'Ng8' },
} as MoveReviewSubject }; }
const context = contextFor(row.request);

test('quiet preparation and moving support retain their actual replies and exchange costs', () => {
  const sources = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/quiet-resources-reading.json.gz', import.meta.url,
  ))).toString('utf8'));
  for (const name of ['h4', 'qg2']) {
    const source = sources[name];
    const selected = source.snapshot.observations.selection.candidates.find((c: any) => c.selection === 'selected');
    const decoded = decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request));
    const shown = decoded?.observations?.selection?.candidates.find(c => c.id === selected.id)?.detail.summary_question;
    assert.deepEqual(shown, selected.detail.summary_question, name);
    assert.ok(shown?.units.every(u => !u.english_only));
    assert.match(JSON.stringify(shown?.units), name === 'h4' ? /캐슬링 경로/ : /진출한 뒤에도 그 지원/);
    const raw = structuredClone(source.snapshot);
    raw.observations.selection.candidates.find((c: any) => c.id === selected.id).detail.summary_question
      .units.find((u: any) => u.covered_moves?.length).covered_moves = [];
    assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request))?.observations?.selection, undefined,
      'preparation cannot hide the actual exchange cost');
  }
  const source = sources.qg2, raw = structuredClone(source.snapshot);
  const q = raw.observations.selection.purpose_question;
  const after = raw.observations.positions.find((p: any) => p.id === q.child_position_id);
  const queen = after.controls.pieces.find((r: any) => r.piece.square === 'g2');
  assert.ok(queen.squares.includes('g3'));
  queen.squares = queen.squares.filter((square: string) => square !== 'g3');
  assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request))?.observations?.selection, undefined,
    'the later rook arrival cannot invent the missing initial queen support');
});

test('moving beneficiaries and conditional resources retain the basic account and their prices', () => {
  const sources = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/resources-reading.json.gz', import.meta.url,
  ))).toString('utf8'));
  for (const name of ['kh1', 'qg2']) {
    const source = sources[name];
    const selected = source.snapshot.observations.selection.candidates.find((c: any) => c.selection === 'selected');
    const decoded = decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request));
    const shown = decoded?.observations?.selection?.candidates.find(c => c.id === selected.id)?.detail.summary_question;
    assert.deepEqual(shown, selected.detail.summary_question);
    assert.ok(shown?.units.every(u => !u.english_only));
    const text = JSON.stringify(shown?.units);
    assert.match(text, name === 'kh1' ? /기물을 잡지 않고 체크만/ : /g4에 진출하는 룩/);
    if (name === 'qg2') {
      assert.match(text, /h3 통제를 더하여/);
      assert.match(text, /응수에 따라 달라집니다/);
    }
    const raw = structuredClone(source.snapshot);
    const q = raw.observations.selection.candidates.find((c: any) => c.id === selected.id).detail.summary_question;
    q.units.find((u: any) => u.covered_moves?.length).covered_moves = [];
    assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request))?.observations?.selection, undefined,
      'adding a purpose cannot erase the connected exchange cost');
  }
});

test('a prevented checking escape retains the actual pair and its conditional queen capture cost', () => {
  const source = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/prevention-reading.json.gz', import.meta.url,
  ))).toString('utf8')).kh1;
  const decoded = decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request));
  assert.equal(decoded?.kind, 'completed');
  const selected = source.snapshot.observations.selection.candidates.find((c: any) => c.selection === 'selected');
  const shown = decoded?.observations?.selection?.candidates.find(c => c.id === selected.id)?.detail.summary_question;
  assert.deepEqual(shown, selected.detail.summary_question);
  assert.equal(shown?.question, 'functional_response');
  assert.ok(shown?.units.every(u => !u.english_only));
  assert.match(JSON.stringify(shown?.units), /퀸을 잡는 반격/);
  const raw = structuredClone(source.snapshot);
  const q = raw.observations.selection.candidates.find((c: any) => c.id === selected.id).detail.summary_question;
  const conditional = q.units.find((u: any) => JSON.stringify(u.ko).includes('퀸을 잡는 반격'));
  assert.equal(conditional.covered_moves.length, 2);
  conditional.covered_moves.pop();
  assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request))?.observations?.selection, undefined,
    'a legal countercapture cannot conceal the preceding knight loss or the captured queen');
});

test('a later reply witness preserves both the original ranking and its own returned path', () => {
  const source = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/prevention-reading.json.gz', import.meta.url,
  ))).toString('utf8')).d6;
  const selected = source.snapshot.observations.selection.candidates.find((c: any) => c.selection === 'selected');
  const decoded = decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request));
  assert.deepEqual(decoded?.observations?.selection?.candidates.find(c => c.id === selected.id)?.detail.summary_question,
    selected.detail.summary_question);
  const raw = structuredClone(source.snapshot);
  const searches = raw.observations.selection.response_searches;
  const later = searches.find((s: any) => s.root_restriction.kind === 'restricted');
  assert.ok(later);
  later.child_position_id = 'unowned-child';
  assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request))?.observations?.selection, undefined,
    'the later branch is not authorized by merely sharing the displayed root move');
});

test('linked preparations and defensive recapture reach the same basic account', () => {
  const source = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/preparation-reading.json.gz', import.meta.url,
  ))).toString('utf8'));
  const decoded = decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request));
  assert.equal(decoded?.kind, 'completed');
  const chosen = source.snapshot.observations.selection.candidates.find((c: any) => c.selection === 'selected');
  const shown = decoded?.observations?.selection?.candidates.find(c => c.id === chosen.id)?.detail.summary_question;
  assert.deepEqual(shown, chosen.detail.summary_question);
  assert.equal(shown?.question, 'selected_comparison');
  assert.ok(shown?.units.every(u => !u.english_only));
  assert.match(JSON.stringify(shown?.units), /진입로를 차례로 비웁니다/);
  assert.match(JSON.stringify(shown?.units), /되잡아 나이트가 e5에 남습니다/);
  for (const strip of ['covered_moves', 'proof_moves']) {
    const raw = structuredClone(source.snapshot);
    const q = raw.observations.selection.candidates.find((c: any) => c.id === chosen.id).detail.summary_question;
    q.units.find((u: any) => u[strip]?.length)[strip] = [];
    assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request))?.observations?.selection, undefined,
      'a preparation cannot hide its steps or the exchange price');
  }
});

test('check evasion and a stronger opposing reply retain the exact basic account', () => {
  const sources = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/obligation-reading.json.gz', import.meta.url,
  ))).toString('utf8'));
  for (const name of ['kf8', 'bd7']) {
    const source = sources[name];
    const decoded = decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request));
    assert.equal(decoded?.kind, 'completed');
    const selected = source.snapshot.observations.selection.candidates.find((c: any) => c.selection === 'selected');
    const shown = decoded?.observations?.selection?.candidates.find(c => c.id === selected.id)?.detail.summary_question;
    assert.deepEqual(shown, selected.detail.summary_question);
    assert.ok(shown?.units.every(u => !u.english_only));
    assert.match(JSON.stringify(shown?.units), name === 'kf8' ? /체크를 해소합니다/ : /f5 칸 통제 기능을 없앱니다/);
    assert.ok(shown?.units.some(u => u.covered_moves.length > 0));
  }
});

test('opposing activity cannot substitute another controller or conceal the price', () => {
  const source = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/obligation-reading.json.gz', import.meta.url,
  ))).toString('utf8')).bd7;
  assert.ok(decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request))?.observations?.selection);
  for (const change of [
    (q: any) => { q.reply_activities[1].control.controller.square = 'd4'; },
    (q: any) => { q.reply_activities[1].control.change = 2; },
    (q: any) => { q.reply_activities[1].user.side = 'black'; },
  ]) {
    const raw = structuredClone(source.snapshot);
    change(raw.observations.selection.purpose_question);
    for (const c of raw.observations.selection.candidates) {
      if (c.detail.summary_question?.move_purpose) change(c.detail.summary_question.move_purpose);
    }
    assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request))?.observations?.selection,
      undefined, 'the selected opposing plan must own the actual original defender and its removal');
  }
  const raw = structuredClone(source.snapshot);
  raw.observations.selection.candidates.find((c: any) => c.selection === 'selected')
    .detail.summary_question.units.find((u: any) => u.covered_moves.length).covered_moves = [];
  assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request))?.observations?.selection, undefined);
});

test('opponent exchange activity and the other choice retaining its defender reach basic commentary', () => {
  const source = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/counterplay-reading.json.gz', import.meta.url,
  ))).toString('utf8'));
  const decoded = decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request));
  assert.equal(decoded?.kind, 'completed');
  const selected = source.snapshot.observations.selection.candidates.find((c: any) => c.selection === 'selected');
  const shown = decoded?.observations?.selection?.candidates.find(c => c.id === selected.id)?.detail.summary_question;
  assert.deepEqual(shown, selected.detail.summary_question);
  assert.match(JSON.stringify(shown?.units), /f5 칸 통제 기능을 없앱니다/);
  assert.match(JSON.stringify(shown?.units), /흑 e7 나이트가 남아/);
  const raw = structuredClone(source.snapshot);
  const q = raw.observations.selection.candidates.find((c: any) => c.id === selected.id).detail.summary_question;
  q.units.find((u: any) => u.covered_moves.length).covered_moves = [];
  assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request))?.observations?.selection,
    undefined, 'the added continuation cannot hide its complete exchange price');
});

test('the best reply keeps its own beneficiary and the other reply keeps its complete exchange', () => {
  const source = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/reply-functions-reading.json.gz', import.meta.url,
  ))).toString('utf8'));
  const decoded = decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request));
  assert.equal(decoded?.kind, 'completed');
  const activities = decoded?.observations?.selection?.purpose_question?.reply_activities;
  assert.deepEqual(activities?.map(a => a.user.square), ['h7', 'g7']);
  const selected = source.snapshot.observations.selection.candidates.find((c: any) => c.selection === 'selected');
  const shown = decoded?.observations?.selection?.candidates.find(c => c.id === selected.id)?.detail.summary_question;
  assert.deepEqual(shown, selected.detail.summary_question);
  assert.match(JSON.stringify(shown?.units), /g5 칸을 새로 지원/);
  assert.match(JSON.stringify(shown?.units), /양쪽이 비숍 한 개씩 교환/);
  for (const change of [
    (q: any) => { q.reply_activities[0].user.square = 'a6'; },
    (q: any) => { q.reply_activities[1].route = [6]; },
    (q: any) => { q.reply_activities.reverse(); },
  ]) {
    const raw = structuredClone(source.snapshot);
    change(raw.observations.selection.purpose_question);
    for (const c of raw.observations.selection.candidates) {
      if (c.detail.summary_question?.move_purpose) change(c.detail.summary_question.move_purpose);
    }
    assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request))?.observations?.selection?.candidates
      .find(c => c.id === selected.id)?.detail.summary_question, undefined,
    'the same label cannot replace the original beneficiary, full route or strongest reply');
  }
  const restricted = structuredClone(source.snapshot);
  const pool = restricted.observations.selection.response_searches[0];
  pool.root_restriction = { kind: 'restricted', moves_uci: pool.candidates.map((c: any) => c.move_uci) };
  assert.equal(decodeMoveReviewSnapshot(restricted, contextFor(source.request))?.observations?.selection?.candidates
    .find(c => c.id === selected.id)?.detail.summary_question, undefined);
});

test('a root-cleared route keeps the late beneficiary and both owned reply accounts', () => {
  const source = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/priority-reading.json.gz', import.meta.url,
  ))).toString('utf8'));
  const decoded = decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request));
  assert.equal(decoded?.kind, 'completed');
  assert.deepEqual(decoded?.observations?.selection?.purpose_question?.activity_plan?.route, [4, 6, 8]);
  const selected = source.snapshot.observations.selection.candidates.find((c: any) => c.selection === 'selected');
  const shown = decoded?.observations?.selection?.candidates.find(c => c.id === selected.id)?.detail.summary_question;
  assert.deepEqual(shown, selected.detail.summary_question);
  assert.match(JSON.stringify(shown?.units), /f5에서의 활동까지는 이 수순에 나타나지 않습니다/);
  for (const change of [
    (raw: any) => { raw.observations.selection.purpose_question.activity_plan.user.square = 'c8'; },
    (raw: any) => { raw.observations.selection.purpose_question.activity_plan.route = [4, 8]; },
    (raw: any) => { raw.observations.selection.response_searches[0].root_restriction = {
      kind: 'restricted', moves_uci: ['h6f4', 'h3e3', 'a1e1'],
    }; },
  ]) {
    const raw = structuredClone(source.snapshot);
    change(raw);
    assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request))?.observations?.selection?.candidates
      .find(c => c.id === selected.id)?.detail.summary_question, undefined,
    'piece identity, the whole route and independent search scope are all required');
  }
});

test('an actual supported entry and the best other reply survive geometric challenge labels', () => {
  const source = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/hypothesis-reading.json.gz', import.meta.url,
  ))).toString('utf8'));
  const decoded = decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request));
  assert.equal(decoded?.kind, 'completed');
  const selected = source.snapshot.observations.selection.candidates.find((c: any) => c.selection === 'selected');
  const shown = decoded?.observations?.selection?.candidates.find(c => c.id === selected.id)?.detail.summary_question;
  assert.ok(shown);
  assert.deepEqual(shown, selected.detail.summary_question);
  assert.match(JSON.stringify(shown.units), /g4 칸을 새로 지원/);
  for (const change of [
    (pool: any) => { pool.candidates = pool.candidates.filter((c: any) => c.move_uci !== 'h8g8'); },
    (pool: any) => { pool.child_position_id = 'foreign-child'; },
  ]) {
    const raw = structuredClone(source.snapshot);
    change(raw.observations.selection.response_searches[0]);
    assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request))?.observations?.selection?.candidates
      .find(c => c.id === selected.id)?.detail.summary_question, undefined);
  }
});

test('an unseen idea is compared with the actual own move at its exact child', () => {
  const source = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/hypothesis-pending.json.gz', import.meta.url,
  ))).toString('utf8'));
  const decoded = decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request));
  assert.equal(decoded?.kind, 'awaiting-evidence');
  assert.equal(decoded?.observations?.selection?.preparation_question?.use_move, 'e2g4');
  for (const change of [
    (work: any) => { work.root_restriction.moves_uci = ['e2g4']; },
    (work: any) => { work.search_anchor.prefix_moves_uci[1] = 'h7h6'; },
  ]) {
    const raw = structuredClone(source.snapshot);
    change(raw.issued_engine_work);
    assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request)), undefined,
      'the proposed action cannot be tested alone or under a substituted reply');
  }
});

test('supported piece identity and both exchange prices survive independent screen validation', () => {
  const saved = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/supported-activity-reading.json.gz', import.meta.url,
  ))).toString('utf8'));
  for (const source of [saved, saved.survey]) {
  const decoded = decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request));
  assert.equal(decoded?.kind, 'completed');
  assert.equal(decoded?.observations?.selection?.purpose_question?.use_move, 'e2g4');
  const selected = source.snapshot.observations.selection.candidates.find((c: any) => c.selection === 'selected');
  const shown = decoded?.observations?.selection?.candidates.find(c => c.id === selected.id)?.detail.summary_question;
  assert.ok(shown, 'the supported bishop and both owned response prices must be delivered');
  assert.deepEqual(shown, selected.detail.summary_question);
  const raw = structuredClone(source.snapshot);
  const q = raw.observations.selection.purpose_question;
  const path = raw.observations.paths.find((p: any) => p.id === q.source_line_id);
  const end = raw.observations.positions.find((p: any) => p.id === path.position_ids[3]);
  end.controls.pieces.find((p: any) => p.piece.square === 'h3').squares = [];
  assert.equal(decodeMoveReviewSnapshot(raw, contextFor(source.request))?.observations?.selection,
    undefined, 'a missing continuing provider cannot be replaced by the support label');
  }
});

test('an owned full survey can explain two captures through their common activity and different prices', () => {
  const source = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/activity-reading.json.gz', import.meta.url,
  ))).toString('utf8'));
  const decoded = decodeMoveReviewSnapshot(source.snapshot, contextFor(source.request));
  assert.equal(decoded?.kind, 'completed');
  const selected = source.snapshot.observations.selection.candidates.find((c: any) => c.selection === 'selected');
  const shown = decoded?.observations?.selection?.candidates.find(c => c.id === selected.id)?.detail.summary_question;
  assert.ok(shown, 'the activity comparison must survive independent receipt validation');
  assert.deepEqual(shown, selected.detail.summary_question,
    'the new peer, delayed recovery, Korean wording and all price receipts must reach the screen together');
  assert.match(JSON.stringify(shown?.units), /같은 방식으로 나이트/);
  for (const change of [
    (pool: any) => { pool.candidates = pool.candidates.filter((c: any) => c.move_uci !== 'd7e5'); },
    (pool: any) => { pool.root_restriction = { kind: 'restricted', moves_uci: pool.candidates.map((c: any) => c.move_uci) }; },
    (pool: any) => { pool.child_position_id = 'foreign-position'; },
  ]) {
    const raw = structuredClone(source.snapshot);
    change(raw.observations.selection.response_searches[0]);
    const rejected = decodeMoveReviewSnapshot(raw, contextFor(source.request));
    assert.equal(rejected?.observations?.selection?.candidates.find(c => c.id === selected.id)?.detail.summary_question,
      undefined, 'a missing peer, forced subset or foreign child cannot supply the independent witness');
  }
});

test('a witnessed defensive route reaches its owned reply comparison inside the twenty-second job', () => {
  const decoded = decodeMoveReviewSnapshot(row.snapshot, context);
  assert.equal(decoded?.kind, 'awaiting-evidence');
  const q = decoded!.observations!.selection!.purpose_question!;
  assert.equal(q.plan?.targets[0].side, 'black');
  assert.equal(q.use_move, 'g8f6');
  const survey = row.full_survey_pending;
  const full = decodeMoveReviewSnapshot(survey.snapshot, contextFor(survey.request));
  assert.equal(full?.kind, 'awaiting-evidence');
  assert.equal(full!.observations!.selection!.purpose_question!.use_move, 'b3c5');
});

test('a functional reply in the actual comparison carries its search owner and complete price', () => {
  const row2 = row.completed_replay;
  const decoded = decodeMoveReviewSnapshot(row2.snapshot, contextFor(row2.request));
  const source = row2.snapshot.observations.selection.candidates[0].detail.summary_question;
  assert.equal(source.question, 'selected_comparison');
  assert.ok(source.move_purpose.work_id);
  assert.ok(decoded?.observations?.selection, 'the owned realization must reach the selected comparison');
  assert.deepEqual(decoded?.observations?.selection?.candidates[0].detail.summary_question, source);
  for (const change of [
    (q: any) => { delete q.move_purpose; },
    (q: any) => { q.move_purpose.execution_key_sha256 = '0'.repeat(64); },
    (q: any) => { q.units.find((u: any) => u.covered_moves.length).covered_moves = []; },
  ]) {
    const raw = structuredClone(row2.snapshot);
    change(raw.observations.selection.candidates[0].detail.summary_question);
    assert.equal(decodeMoveReviewSnapshot(raw, contextFor(row2.request))?.observations?.selection, undefined);
  }
});

test('support identity, legal scope and the depth ceiling remain mandatory', () => {
  for (const change of [
    (v: any) => { v.observations.selection.purpose_question.plan.targets[0].side = 'white'; },
    (v: any) => { v.observations.selection.purpose_question.plan.targets[0].square = 'g7'; },
    (v: any) => { v.issued_engine_work.root_restriction.moves_uci = ['b1f1', 'd3d2']; },
    (v: any) => { v.issued_engine_work.search_limits.depth = 19; },
    (v: any) => { v.issued_engine_work.max_search_elapsed_ms = 21000; },
  ]) {
    const raw = structuredClone(row.snapshot);
    change(raw);
    assert.equal(decodeMoveReviewSnapshot(raw, context), undefined);
  }
});

test('an owned exchange reply survey has the same short-job limits as a quiet purpose survey', () => {
  const exchange = row.exchange_pending;
  assert.equal(decodeMoveReviewSnapshot(exchange.snapshot, contextFor(exchange.request))?.kind, 'awaiting-evidence');
  for (const change of [
    (v: any) => { v.issued_engine_work.search_anchor.root_move = 'c5a4'; },
    (v: any) => { v.issued_engine_work.search_limits.depth = 19; },
    (v: any) => { v.issued_engine_work.max_search_elapsed_ms = 21000; },
    (v: any) => { v.observations.selection.response_inquiries = []; },
  ]) {
    const raw = structuredClone(exchange.snapshot);
    change(raw);
    assert.equal(decodeMoveReviewSnapshot(raw, contextFor(exchange.request)), undefined);
  }
});

test('exchanged control and later access reach the bilingual comparison with both reply prices', () => {
  const exchange = row.exchange_completed;
  const decoded = decodeMoveReviewSnapshot(exchange.snapshot, contextFor(exchange.request));
  const question = decoded?.observations?.selection?.candidates[0].detail.summary_question;
  assert.ok(question);
  assert.equal(question.question, 'selected_comparison');
  assert.ok(question.units.every(u => !u.english_only));
  assert.ok(question.units.some(u => JSON.stringify(u.ko).includes('통제를 없앱니다')));
  assert.ok(question.units.some(u => u.covered_moves.length >= 2));
});

test('declining the recapture preserves the root loss and a shared escape reaches Korean', () => {
  const exchange = row.exchange_declined;
  const decoded = decodeMoveReviewSnapshot(exchange.snapshot, contextFor(exchange.request));
  const question = decoded?.observations?.selection?.candidates[0].detail.summary_question;
  assert.ok(question);
  assert.ok(question.units.every(u => !u.english_only));
  const text = JSON.stringify(question.units.map(u => u.ko));
  assert.ok(text.includes('통제를 없앱니다'));
  assert.ok(text.includes('공격을 피합니다'));
  assert.ok(question.units.some(u => u.covered_moves.length > 0 && JSON.stringify(u.ko).includes('백은')),
    'the quiet Bf1/Nc6 reply must keep the white knight lost at the root');
});

test('an owned response recheck remains executable after its earlier survey was answered', () => {
  const row = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../judgment-evaluation/fixtures/public-commentary-v6/causal-function-pending.json.gz', import.meta.url,
  ))).toString('utf8'));
  const context = contextFor(row.request);
  assert.equal(decodeMoveReviewSnapshot(row.original_pending, context), undefined,
    'the first real failure had answered metadata attached to pending work');
  const decoded = decodeMoveReviewSnapshot(row.snapshot, context);
  assert.equal(decoded?.kind, 'awaiting-evidence');
  assert.deepEqual(decoded?.issuedEngineWork?.rootRestriction, { kind: 'restricted', movesUci: ['d6e5', 'f6f8'] });
  assert.equal(decoded?.observations?.selection?.response_searches?.length, 1);
  const completed = decodeMoveReviewSnapshot(row.completed.snapshot, contextFor(row.completed.request));
  assert.equal(completed?.kind, 'completed');
  const source = row.completed.snapshot.observations.selection.candidates.find((c: any) => c.selection === 'selected');
  const shown = completed?.observations?.selection?.candidates.find(c => c.id === source.id)?.detail.summary_question;
  assert.deepEqual(shown, source.detail.summary_question);
  assert.ok(shown!.units.length >= 4 && shown!.units.every(u => !u.english_only));
  assert.ok(shown!.units.some(u => JSON.stringify(u.ko).includes('백은 폰 한 개를 내줍니다')),
    'taking another action must preserve the sacrificed pawn in that reply');
  for (const change of [
    (v: any) => { v.issued_engine_work.search_anchor.root_move = 'c3e4'; },
    (v: any) => { v.issued_engine_work.search_limits.depth = 19; },
    (v: any) => { v.observations.selection.response_inquiries = []; },
  ]) {
    const raw = structuredClone(row.snapshot);
    change(raw);
    assert.equal(decodeMoveReviewSnapshot(raw, context), undefined);
  }
});
