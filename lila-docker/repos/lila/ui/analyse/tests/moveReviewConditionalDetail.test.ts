import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, type MoveReviewSubject } from '../src/moveReview';

/** Admission-only transformations of the actual meaningv16 SG14 output.
 * The recorded condition, declarations, prices, canonical history and engine
 * receipts stay intact. We move one complete conditional Unit to detail and
 * derive the primary text from the remaining actual Units; this is not a newly
 * generated product answer or new engine/numerical evidence.
 */
function sourceJob() {
  const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/functional-response-sg14-offpair-produced.json.gz',
    import.meta.url,
  ))).toString('utf8'));
  const request = fixture.request;
  return { raw: fixture.snapshot, context: {
    requestId: request.request_id,
    subject: {
      variant: 'standard', initialFen: request.initial_fen, movePrefixUci: request.move_prefix_uci,
      before: { path: '', fen: request.current_fen },
      played: { uci: request.focus.played_move_uci, san: 'Qb3' },
      after: { path: 'aa', fen: request.focus.resulting_fen },
    } as MoveReviewSubject,
  } };
}

function rootQuestion(raw: any) {
  const root = raw.observations.selection.candidates.find((candidate: any) =>
    candidate.selection === 'selected' && candidate.detail.type === 'root_comparison');
  assert.ok(root, 'the actual selected root comparison exists');
  return root;
}

function owner(raw: any) {
  const root = rootQuestion(raw), primary = [root.detail.reference_line_id, root.detail.candidate_line_id];
  const witness = raw.observations.paths.find((path: any) =>
    root.detail.summary_question.witness_line_ids.includes(path.id) && !primary.includes(path.id) &&
    !primary.map((id: string) => raw.observations.paths.find((p: any) => p.id === id).moves[0]).includes(path.moves[0]));
  assert.ok(witness, 'the actual O-O example is distinct from the primary Qb3/Ne5 pair');
  const owners = raw.observations.selection.proposals.filter((proposal: any) =>
    proposal.origin.question.producer === 'C-reply-function' && proposal.requirements.some((reading: any) =>
      reading.id === 'function_effect_alternative' && reading.path.source.kind === 'declared' &&
      JSON.stringify(reading.path.source.moves_uci) === JSON.stringify(witness.moves)));
  assert.equal(owners.length, 1, 'one actual generated conditional proof owns the foreign-root example');
  return owners[0];
}

function reading(proposal: any, id: string) {
  const result = proposal.requirements.find((value: any) => value.id === id);
  assert.ok(result, id);
  return result;
}

function detailJob() {
  const { raw, context } = sourceJob();
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'completed', 'the unmodified actual functional answer is the control');
  const root = rootQuestion(raw), question = root.detail.summary_question;
  assert.equal(question.question, 'functional_response');
  const primary = [root.detail.reference_line_id, root.detail.candidate_line_id];
  const conditional = question.units.filter((unit: any) =>
    unit.proof_moves?.some((ref: any) => !primary.includes(ref.line_id)));
  assert.equal(conditional.length, 1, 'the actual conditional explanation is one complete Unit');
  assert.equal(conditional[0].action_moves.length, 0);
  assert.equal(conditional[0].covered_moves.length, 0,
    'this legal example is a proof, never an actual material price receipt');
  question.units = question.units.filter((unit: any) => !conditional.includes(unit));
  question.detail_units = [...question.detail_units, ...conditional];
  question.question = 'selected_comparison';
  // Resolve existing actual move labels. Do not invent prose or notation.
  const paths = decoded.observations!.paths;
  root.detail.summary = question.units.map((unit: any) => unit.en.map((part: any) => {
    if (typeof part === 'string') return part;
    const path = paths.find(path => path.id === part.line_id);
    assert.ok(path);
    const at = path.occurrenceIds.indexOf(part.occurrence_id);
    assert.ok(at >= 0);
    return path.moves[at].label;
  }).join('')).join(' ');
  return { raw, context, root, question, conditional: conditional[0] };
}

function rejectSelection(raw: any, context: ReturnType<typeof sourceJob>['context'], label: string) {
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'completed', label + ': the original numerical assessment survives');
  assert.ok(decoded.observations?.selection === undefined, label + ': reject only the optional invalid selection');
}

test('a supported declared conditional example may stay in detail of a typed selected comparison', () => {
  const { raw, context, root, question, conditional } = detailJob();
  const originalResult = structuredClone(raw.result), proposal = owner(raw);
  assert.equal(reading(proposal, 'function_context').state, 'supported');
  assert.equal(reading(proposal, 'function_live').state, 'supported');
  assert.ok(question.witness_line_ids.every((id: string) =>
    question.detail_units.some((unit: any) => unit.proof_moves?.some((ref: any) => ref.line_id === id))));
  assert.ok(question.witness_line_ids.every((id: string) =>
    !question.units.some((unit: any) => unit.proof_moves?.some((ref: any) => ref.line_id === id))),
    'all three named legal witnesses are detail-only, while primary root duties remain core');
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'completed');
  const answer = decoded.observations?.selection?.candidates.find(candidate => candidate.id === root.id)?.detail;
  assert.ok(answer, 'the completed conditional detail has a currently supported exact board owner');
  assert.equal(answer.summary_question?.question, 'selected_comparison');
  assert.equal(answer.reference_line_id, root.detail.reference_line_id);
  assert.equal(answer.candidate_line_id, root.detail.candidate_line_id);
  assert.equal(answer.reference_net_cp, root.detail.reference_net_cp);
  assert.equal(answer.candidate_net_cp, root.detail.candidate_net_cp);
  assert.equal(answer.summary, root.detail.summary);
  assert.deepEqual(answer.summary_question?.detail_units?.at(-1), conditional);
  assert.deepEqual(answer.summary_question?.witness_line_ids, question.witness_line_ids);
  assert.deepEqual(raw.result, originalResult, 'moving a legal example does not create or revise a numerical receipt');
});

test('a supported detail-only board proof does not require an answered numerical function comparison', () => {
  const { raw, context, root } = detailJob(), proposal = owner(raw);
  const numeric = reading(proposal, 'function_root');
  numeric.state = 'deferred';
  numeric.stop_reason = 'comparison_not_current';
  numeric.answer = null;
  numeric.work_id = null;
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'completed');
  const selection = decoded.observations?.selection;
  assert.ok(selection?.candidates.some(candidate => candidate.id === root.id));
  const retained = selection.proposals?.find(value => value.id === proposal.id)?.requirements
    .find(value => value.id === 'function_root');
  assert.ok(retained);
  assert.equal(retained.state, 'deferred');
  assert.equal(retained.answer, null);
});

test('a typed comparison label does not admit unsupported or mismatched conditional detail ownership', () => {
  const { raw, context } = detailJob();
  for (const [label, mutate] of [
    ['another producer', (proposal: any) => { proposal.origin.question.producer = 'C-support'; }],
    ['missing all-context', (proposal: any) => {
      proposal.requirements = proposal.requirements.filter((value: any) => value.id !== 'function_context');
    }],
    ['any-context', (proposal: any) => { reading(proposal, 'function_context').mode = 'any'; }],
    ['unsupported live condition', (proposal: any) => {
      const live = reading(proposal, 'function_live');
      live.state = 'partial';
      live.result = { state: 'partial', samples: [], needs: [], proofs: [], reason: null };
    }],
    ['foreign physical condition', (proposal: any) => {
      reading(proposal, 'function_live').predicate.subject = { side: 'white', role: 'bishop', square: 'b5' };
    }],
    ['primary peer has another condition value', (proposal: any) => {
      reading(proposal, 'function_root_alternative').expected = 'b3';
    }],
    ['live declaration belongs to another legal root', (proposal: any) => {
      reading(proposal, 'function_live').path.source.moves_uci = ['f3e5'];
    }],
    ['declared peer belongs to another legal root', (proposal: any) => {
      reading(proposal, 'function_effect_alternative').path.source.moves_uci[0] = 'a4c2';
    }],
  ] as [string, (proposal: any) => void][]) {
    const changed = structuredClone(raw);
    mutate(owner(changed));
    rejectSelection(changed, context, label);
  }
});

test('a named detail witness still cannot borrow a known foreign path without its own supported declaration', () => {
  const { raw, context, root, conditional } = detailJob();
  const foreign = raw.observations.paths.find((path: any) => path.moves[0] === 'a4c2');
  assert.ok(foreign, 'the real snapshot already contains a legal Qc2 path');
  root.evidence_ids = [...new Set([...root.evidence_ids, ...foreign.line_summary.premise_ids, ...foreign.occurrence_ids])];
  root.detail.summary_question.witness_line_ids.push(foreign.id);
  conditional.proof_moves.push({ line_id: foreign.id, occurrence_id: foreign.occurrence_ids[0] });
  rejectSelection(raw, context, 'a known legal Qc2 root has no supported conditional declaration');
});

test('a compatible packed delivery Unit retains its actual proof, action account, prices and exact focus frame', () => {
  // This checks client admission of a final delivery shape. The two completed
  // actual Units remain separate in the saved source; it does not claim that
  // this fixture exercises the server's >8-Unit packing trigger.
  const { raw, context } = sourceJob(), root = rootQuestion(raw);
  const question = root.detail.summary_question, originals = structuredClone(question.units);
  const [left, right] = originals;
  assert.equal(left.role, 'reply');
  assert.equal(right.role, left.role);
  assert.equal(left.english_only, true);
  assert.equal(right.english_only, true);
  assert.equal(left.focus?.length ?? 0, 0);
  assert.equal(right.focus.length, 1, 'use the actual after-Nxe5 focus, without inventing a new frame');
  const unique = (values: any[]) => [...new Map(values.map(value =>
    [value.line_id + '/' + value.occurrence_id, value])).values()];
  const packed = {
    ...left,
    en: [...left.en, ' ', ...right.en],
    ko: [...left.ko, ' ', ...right.ko],
    proof_moves: unique([...left.proof_moves, ...right.proof_moves]),
    action_moves: unique([...left.action_moves, ...right.action_moves]),
    covered_moves: unique([...left.covered_moves, ...right.covered_moves]),
    required_moves: unique([...left.required_moves, ...right.required_moves]),
    focus: structuredClone(right.focus),
  };
  assert.equal(packed.en.length, 32);
  assert.equal(packed.ko.length, 32);
  const prices = structuredClone(originals.flatMap((unit: any) => unit.covered_moves));
  const details = structuredClone(question.detail_units), originalSummary = root.detail.summary;
  question.units = [packed, ...originals.slice(2)];
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'completed');
  const answer = decoded.observations?.selection?.candidates.find(candidate => candidate.id === root.id)?.detail;
  assert.ok(answer?.summary_question);
  const kept = answer.summary_question.units[0];
  assert.deepEqual(kept.proof_moves, packed.proof_moves);
  assert.deepEqual(kept.action_moves, packed.action_moves);
  assert.deepEqual(kept.required_moves, packed.required_moves);
  assert.deepEqual(kept.focus, right.focus);
  assert.deepEqual(answer.summary_question.units.flatMap(unit => unit.covered_moves), prices);
  assert.deepEqual(answer.summary_question.detail_units, details);
  assert.equal(answer.summary, originalSummary, 'whole-Unit packing preserves every original spoken part');
  assert.equal(answer.reference_line_id, root.detail.reference_line_id);
  assert.equal(answer.candidate_line_id, root.detail.candidate_line_id);

  const wrongFrame = structuredClone(raw), focus = rootQuestion(wrongFrame).detail.summary_question.units[0].focus[0];
  const path = wrongFrame.observations.paths.find((path: any) => path.id === focus.line_id);
  const at = path.occurrence_ids.indexOf(focus.occurrence_id);
  assert.ok(at >= 0);
  focus.position_id = path.position_ids[at];
  rejectSelection(wrongFrame, context, 'an actual before-position cannot replace the packed after-focus frame');
});
