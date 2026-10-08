import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { moveReviewEngineProfile } from 'lib/ceval/types';
import pgnImport from '../src/pgnImport';
import { standardUci, treeReconstruct } from '../src/util';
import { reserveResponsePoolMatches, type CommentaryProposal, type CommentaryProposalReading } from '../src/moveReviewSelection';
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

function shortFixtureJob(name: string, playedSan: string) {
  const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/' + name,
    import.meta.url,
  ))).toString('utf8'));
  const request = fixture.request;
  return { raw: fixture.snapshot, context: {
    requestId: request.request_id,
    subject: {
      variant: 'standard', initialFen: request.initial_fen, movePrefixUci: request.move_prefix_uci,
      before: { path: '', fen: request.current_fen },
      played: { uci: request.focus.played_move_uci, san: playedSan },
      after: { path: 'aa', fen: request.focus.resulting_fen },
    } as MoveReviewSubject,
  } };
}

function childChallengeRootFixtureJob(extended = false) {
  // Actual meaningv10 report after child work 2 and before root work 3.
  // The recorded 16+7 root-depth obligation is still pending. Changing the
  // work allowance below tests admission only, not a new engine receipt.
  const job = shortFixtureJob('child-challenge-root-sg32-pending-produced.json.gz', 'Kh2');
  const work = job.raw.issued_engine_work;
  assert.equal(work.work_id, 'work:3');
  assert.equal(work.purpose, 'root_search');
  assert.deepEqual(work.search_limits,
    { depth: 23, nodes: 5_000_000, movetime_ms: 5_000, multi_pv: 3 });
  assert.equal(work.max_search_elapsed_ms, 6_000);
  assert.deepEqual(job.raw.verification, {
    state: 'pending', target_depth: 23, reason: 'child_challenge',
    trigger_work_ids: ['work:2'], completed_depth: 16, stop_reason: null,
  });
  if (extended) {
    work.search_limits.nodes = 40_000_000;
    work.search_limits.movetime_ms = 8_000;
    work.max_search_elapsed_ms = 9_000;
  }
  return job;
}

test('a recorded pending child challenge admits the original root verification allowance', () => {
  const { raw, context } = childChallengeRootFixtureJob();
  const snapshot = decodeMoveReviewSnapshot(raw, context);
  assert.ok(snapshot?.kind === 'awaiting-evidence');
  assert.equal(snapshot.issuedEngineWork.purpose, 'root_search');
  assert.deepEqual(snapshot.issuedEngineWork.searchLimits,
    { depth: 23, nodes: 5_000_000, movetimeMs: 5_000, multiPv: 3 });
  assert.equal(snapshot.issuedEngineWork.maxSearchElapsedMs, 6_000);
  assert.deepEqual(snapshot.verification, {
    state: 'pending', targetDepth: 23, reason: 'child_challenge',
    triggerWorkIds: ['work:2'], completedDepth: 16,
  });
});

test('the exact pending child challenge admits a complete longer root verification allowance', () => {
  const { raw, context } = childChallengeRootFixtureJob(true);
  const snapshot = decodeMoveReviewSnapshot(raw, context);
  assert.ok(snapshot?.kind === 'awaiting-evidence');
  assert.equal(snapshot.issuedEngineWork.purpose, 'root_search');
  assert.deepEqual(snapshot.issuedEngineWork.rootRestriction, { kind: 'unrestricted' });
  assert.deepEqual(snapshot.issuedEngineWork.enginePositionMovesUci, context.subject.movePrefixUci);
  assert.equal(snapshot.issuedEngineWork.enginePositionInitialFen, context.subject.initialFen);
  assert.equal(snapshot.issuedEngineWork.searchFen, context.subject.before.fen);
  assert.deepEqual(snapshot.issuedEngineWork.searchLimits,
    { depth: 23, nodes: 40_000_000, movetimeMs: 8_000, multiPv: 3 });
  assert.equal(snapshot.issuedEngineWork.maxSearchElapsedMs, 9_000);
  assert.equal(snapshot.verification?.state, 'pending');
  assert.equal(snapshot.verification?.reason, 'child_challenge');
  assert.equal(snapshot.verification?.targetDepth, snapshot.issuedEngineWork.searchLimits.depth);
});

test('the longer root allowance requires its exact pending child verification reason and depth', () => {
  const { raw, context } = childChallengeRootFixtureJob(true);
  for (const [label, mutate] of [
    ['initial verification reason', (changed: any) => { changed.verification.reason = 'initial_verification'; }],
    ['other target depth', (changed: any) => { changed.verification.target_depth = 22; }],
    ['other work depth', (changed: any) => { changed.issued_engine_work.search_limits.depth = 24; }],
    ['matching shallow work and target', (changed: any) => {
      changed.verification.target_depth = 8; changed.issued_engine_work.search_limits.depth = 8;
    }],
    ['absent verification', (changed: any) => { delete changed.verification; }],
    ['stopped verification', (changed: any) => {
      changed.verification.state = 'stopped'; changed.verification.stop_reason = 'search_budget_exhausted';
    }],
    ['complete verification', (changed: any) => {
      changed.verification.state = 'complete'; changed.verification.completed_depth = 23;
    }],
    ['verification not required', (changed: any) => {
      changed.verification.state = 'not_required'; changed.verification.completed_depth = null;
    }],
  ] as [string, (changed: any) => void][]) {
    const changed = structuredClone(raw);
    mutate(changed);
    assert.ok(decodeMoveReviewSnapshot(changed, context) === undefined, label);
  }
});

test('the longer root verification lease rejects mixed and enlarged per-work limits', () => {
  const { raw, context } = childChallengeRootFixtureJob(true);
  for (const limits of [
    { nodes: 20_000_000 }, { nodes: 5_000_000 }, { nodes: 40_000_001 },
    { movetime_ms: 5_000 }, { movetime_ms: 2_500 }, { movetime_ms: 8_001 },
  ]) {
    const changed = structuredClone(raw);
    Object.assign(changed.issued_engine_work.search_limits, limits);
    assert.equal(decodeMoveReviewSnapshot(changed, context), undefined, JSON.stringify(limits));
  }
  for (const maxElapsed of [6_000, 9_001]) {
    const changed = structuredClone(raw);
    changed.issued_engine_work.max_search_elapsed_ms = maxElapsed;
    assert.equal(decodeMoveReviewSnapshot(changed, context), undefined, `elapsed ${maxElapsed}`);
  }
});

test('the child challenge allowance cannot authorize another legal position or restricted root work', () => {
  const { raw, context } = childChallengeRootFixtureJob(true);
  for (const [label, mutate] of [
    ['foreign legal history step', (work: any) => {
      work.engine_position_moves_uci = ['g1h2']; work.search_fen = context.subject.after.fen;
    }],
    ['foreign initial position', (work: any) => {
      work.engine_position_initial_fen = context.subject.after.fen; work.search_fen = context.subject.after.fen;
    }],
    ['foreign search board', (work: any) => { work.search_fen = context.subject.after.fen; }],
    ['restricted root work', (work: any) => {
      work.root_restriction = { kind: 'restricted', moves_uci: ['g1h2', 'd8g5'] };
      work.search_limits.multi_pv = 2;
    }],
  ] as [string, (work: any) => void][]) {
    const changed = structuredClone(raw);
    mutate(changed.issued_engine_work);
    assert.equal(decodeMoveReviewSnapshot(changed, context), undefined, label);
  }
});

function childChallengeRootCompletionFixtureJob() {
  const job = childChallengeRootFixtureJob();
  const work = job.raw.issued_engine_work;
  work.search_limits.nodes = 250_000_000;
  work.search_limits.movetime_ms = 120_000;
  work.max_search_elapsed_ms = 121_000;
  return job;
}

test('a pending exact child challenge admits the complete two-minute root verification profile', () => {
  const { raw, context } = childChallengeRootCompletionFixtureJob();
  const snapshot = decodeMoveReviewSnapshot(raw, context);
  assert.ok(snapshot?.kind === 'awaiting-evidence');
  assert.equal(snapshot.issuedEngineWork.purpose, 'root_search');
  assert.equal(snapshot.issuedEngineWork.searchLimits.depth, 23);
  assert.equal(snapshot.issuedEngineWork.searchLimits.nodes, 250_000_000);
  assert.equal(snapshot.issuedEngineWork.searchLimits.movetimeMs, 120_000);
  assert.equal(snapshot.issuedEngineWork.maxSearchElapsedMs, 121_000);
  assert.deepEqual(snapshot.issuedEngineWork.rootRestriction, { kind: 'unrestricted' });
  assert.deepEqual(snapshot.verification, {
    state: 'pending', targetDepth: 23, reason: 'child_challenge',
    triggerWorkIds: ['work:2'], completedDepth: 16,
  });
});

test('the two-minute root profile rejects other verification scopes, mixed limits and foreign positions', () => {
  const { raw, context } = childChallengeRootCompletionFixtureJob();
  for (const [label, mutate] of [
    ['other reason', (changed: any) => { changed.verification.reason = 'initial_verification'; }],
    ['other target depth', (changed: any) => { changed.verification.target_depth = 24; }],
    ['other work depth', (changed: any) => { changed.issued_engine_work.search_limits.depth = 22; }],
    ['absent verification', (changed: any) => { delete changed.verification; }],
    ['stopped verification', (changed: any) => {
      changed.verification.state = 'stopped'; changed.verification.stop_reason = 'search_budget_exhausted';
    }],
    ['complete verification', (changed: any) => {
      changed.verification.state = 'complete'; changed.verification.completed_depth = 23;
    }],
    ['mixed short node limit', (changed: any) => { changed.issued_engine_work.search_limits.nodes = 40_000_000; }],
    ['mixed short engine time', (changed: any) => { changed.issued_engine_work.search_limits.movetime_ms = 8_000; }],
    ['mixed short lease', (changed: any) => { changed.issued_engine_work.max_search_elapsed_ms = 9_000; }],
    ['enlarged nodes', (changed: any) => { changed.issued_engine_work.search_limits.nodes = 250_000_001; }],
    ['enlarged engine time', (changed: any) => { changed.issued_engine_work.search_limits.movetime_ms = 120_001; }],
    ['enlarged lease', (changed: any) => { changed.issued_engine_work.max_search_elapsed_ms = 121_001; }],
    ['foreign legal root step', (changed: any) => {
      changed.issued_engine_work.engine_position_moves_uci = ['g1h2'];
      changed.issued_engine_work.search_fen = context.subject.after.fen;
    }],
    ['foreign initial board', (changed: any) => {
      changed.issued_engine_work.engine_position_initial_fen = context.subject.after.fen;
      changed.issued_engine_work.search_fen = context.subject.after.fen;
    }],
  ] as [string, (changed: any) => void][]) {
    const changed = structuredClone(raw);
    mutate(changed);
    assert.equal(decodeMoveReviewSnapshot(changed, context), undefined, label);
  }
});

test('a selected inquiry accepts the complete bounded allowance while rejecting mixed or enlarged limits', () => {
  const raw = rawInvestigation();
  const work = raw.issued_engine_work as JsonObject;
  work.search_limits = { depth: 16, nodes: 5_000_000, movetime_ms: 5_000, multi_pv: 2 };
  work.max_search_elapsed_ms = 6_000;
  assert.equal(decodeMoveReviewSnapshot(raw, decodeContext())?.kind, 'awaiting-evidence');
  (work.search_limits as JsonObject).nodes = 20_000_000;
  assert.equal(decodeMoveReviewSnapshot(raw, decodeContext())?.kind, 'awaiting-evidence');
  for (const change of [
    { nodes: 20_000_001 }, { nodes: 5_000_001 }, { nodes: 2_000_000 }, { movetime_ms: 2_500 },
    { nodes: 40_000_000, movetime_ms: 8_000 }, { depth: 8 }, { multi_pv: 3 },
  ]) {
    const changed = structuredClone(raw);
    const changedWork = changed.issued_engine_work as JsonObject;
    changedWork.search_limits = { ...(changedWork.search_limits as JsonObject), ...change };
    assert.equal(decodeMoveReviewSnapshot(changed, decodeContext()), undefined);
  }
  const changed = structuredClone(raw);
  (changed.issued_engine_work as JsonObject).max_search_elapsed_ms = 6_001;
  assert.equal(decodeMoveReviewSnapshot(changed, decodeContext()), undefined);
});

test('a short critical comparison retains its recorded question owner and rejects an unowned allowance', () => {
  // Three actual saved reports, followed by an explicit remaining-clock boundary.
  // This fixture checks public work admission, not engine strength or latency.
  const { raw, context } = shortFixtureJob('short-critical-comparison-produced.json.gz', 'g4');
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.equal(decoded?.kind, 'awaiting-evidence');
  assert.ok(decoded?.kind === 'awaiting-evidence');
  assert.equal(decoded.issuedEngineWork.searchLimits.nodes, 20_000_000);
  assert.equal(decoded.issuedEngineWork.searchLimits.movetimeMs, 2_500);
  assert.equal(decoded.issuedEngineWork.maxSearchElapsedMs, 3_500);
  for (const change of [{ nodes: 20_000_001 }, { movetime_ms: 2_501 }, { depth: 8 }]) {
    const changed = structuredClone(raw);
    changed.issued_engine_work.search_limits = { ...changed.issued_engine_work.search_limits, ...change };
    assert.equal(decodeMoveReviewSnapshot(changed, context), undefined);
  }
  const wrongLease = structuredClone(raw);
  wrongLease.issued_engine_work.max_search_elapsed_ms = 3_501;
  assert.equal(decodeMoveReviewSnapshot(wrongLease, context), undefined);
  for (const change of [
    { work_id: 'foreign-work' }, { prefix_moves_uci: [] },
    { reference_move: 'e3d3', alternative_move: 'e3d4' },
  ]) {
    const changed = structuredClone(raw);
    const owner = changed.observations.selection.proposals.flatMap((p: any) => p.requirements)
      .find((r: any) => r.work_id === changed.issued_engine_work.work_id);
    assert.ok(owner);
    Object.assign(owner, change);
    assert.equal(decodeMoveReviewSnapshot(changed, context), undefined);
  }
  const bare = rawInvestigation();
  const bareWork = bare.issued_engine_work as JsonObject;
  bareWork.search_limits = { depth: 16, nodes: 20_000_000, movetime_ms: 2_500, multi_pv: 2 };
  bareWork.max_search_elapsed_ms = 3_500;
  assert.equal(decodeMoveReviewSnapshot(bare, decodeContext()), undefined);
});

function nonreserveFirstReplyAllowanceJob() {
  // Keep the recorded paths, ready facts, work and exact pair. Only the producer tag varies:
  // this tests the frontend contract, not a new chess claim or backend discovery.
  const job = shortFixtureJob('first-reply-allowance-produced.json.gz', 'g4');
  const work = job.raw.issued_engine_work;
  const owners = job.raw.observations.selection.proposals.filter((proposal: any) =>
    proposal.requirements.some((reading: any) => reading.kind === 'compare_moves' &&
      reading.state === 'awaiting_engine' && reading.work_id === work.work_id));
  assert.ok(owners.length);
  for (const owner of owners) {
    assert.equal(owner.admission, 'generated');
    assert.equal(owner.origin.question.producer, 'C-reserve-response');
    owner.origin.question.producer = 'C-plan-counter';
  }
  return { ...job, owners };
}

test('a prepared generated first-reply pair accepts its longer allowance without a reserve producer tag', () => {
  const { raw, context, owners } = nonreserveFirstReplyAllowanceJob();
  const snapshot = decodeMoveReviewSnapshot(raw, context);
  assert.ok(snapshot?.kind === 'awaiting-evidence');
  assert.equal(snapshot.issuedEngineWork.purpose, 'response_comparison');
  assert.deepEqual(snapshot.issuedEngineWork.searchAnchor?.prefixMovesUci, ['f3e3']);
  assert.deepEqual(snapshot.issuedEngineWork.rootRestriction,
    { kind: 'restricted', movesUci: ['e5f6', 'f7f6'] });
  assert.deepEqual(snapshot.issuedEngineWork.searchLimits,
    { depth: 16, nodes: 40_000_000, movetimeMs: 8_000, multiPv: 2 });
  assert.equal(snapshot.issuedEngineWork.maxSearchElapsedMs, 9_000);
  assert.equal(snapshot.assessment?.kind, 'completed');
  assert.ok(owners.every((owner: any) => owner.origin.question.producer === 'C-plan-counter'));
});

test('the longer first-reply allowance still rejects shallow, unowned, nonfirst and other-pair work', () => {
  const { raw, context } = nonreserveFirstReplyAllowanceJob();
  for (const [label, mutate] of [
    ['shallow work', (work: any, _owners: any[]) => { work.search_limits.depth = 8; }],
    ['foreign work owner', (_work: any, owners: any[]) => { for (const owner of owners)
      for (const reading of owner.requirements) if (reading.state === 'awaiting_engine')
        reading.work_id = 'work:999'; }],
    ['submitted-only owner', (_work: any, owners: any[]) => { for (const owner of owners)
      owner.admission = 'submitted'; }],
    ['owner no longer pending', (_work: any, owners: any[]) => { for (const owner of owners)
      for (const reading of owner.requirements) if (reading.state === 'awaiting_engine') {
        reading.state = 'deferred'; reading.work_id = null;
      } }],
    ['other prefix', (_work: any, owners: any[]) => { for (const owner of owners)
      for (const reading of owner.requirements) if (reading.state === 'awaiting_engine')
        reading.prefix_moves_uci = ['g3g4']; }],
    ['other legal pair', (_work: any, owners: any[]) => { for (const owner of owners)
      for (const reading of owner.requirements) if (reading.state === 'awaiting_engine')
        reading.alternative_move = 'f7f5'; }],
    ['foreign occurrence', (work: any, _owners: any[]) => {
      work.search_anchor.root_occurrence_id = 'foreign-root-occurrence'; }],
    ['mixed allowance', (work: any, _owners: any[]) => { work.search_limits.nodes = 20_000_000; }],
    ['enlarged elapsed allowance', (work: any, _owners: any[]) => { work.max_search_elapsed_ms = 9_001; }],
  ] as [string, (work: any, owners: any[]) => void][]) {
    const changed = structuredClone(raw);
    const work = changed.issued_engine_work;
    const owners = changed.observations.selection.proposals.filter((proposal: any) =>
      proposal.requirements.some((reading: any) => reading.kind === 'compare_moves' &&
        reading.state === 'awaiting_engine' && reading.work_id === work.work_id));
    mutate(work, owners);
    assert.equal(decodeMoveReviewSnapshot(changed, context), undefined, label);
  }
  // This other recorded snapshot owns a valid two-ply decision. A longer lease cannot turn it into a first reply.
  const later = shortFixtureJob('short-critical-comparison-produced.json.gz', 'g4');
  later.raw.issued_engine_work.search_limits.nodes = 40_000_000;
  later.raw.issued_engine_work.search_limits.movetime_ms = 8_000;
  later.raw.issued_engine_work.max_search_elapsed_ms = 9_000;
  assert.equal(decodeMoveReviewSnapshot(later.raw, later.context), undefined, 'nonfirst decision');
});

function functionalRootQuestion(raw: any) {
  const candidate = raw.observations.selection.candidates.find((candidate: any) =>
    candidate.selection === 'selected' && candidate.detail.type === 'root_comparison');
  assert.ok(candidate);
  assert.equal(candidate.detail.summary_question.question, 'functional_response');
  return candidate;
}

test('a recorded functional reply answer admits its owned legal witnesses while continuation work stays pending', () => {
  // Actual meaningv8 state before work 2: the legal purpose is already delivered,
  // but the seven-ply continuation and the job itself have not completed.
  const { raw, context } = shortFixtureJob('functional-response-sg32-pending-produced.json.gz', 'Kh2');
  const root = functionalRootQuestion(raw);
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'awaiting-evidence');
  assert.equal(decoded.assessment?.kind, 'completed');
  assert.equal(decoded.issuedEngineWork.purpose, 'response_comparison');
  assert.deepEqual(decoded.issuedEngineWork.searchAnchor?.prefixMovesUci,
    ['g1h2', 'b7c8', 'd8c8', 'c2c8', 'g4e5', 'c8c2', 'd1d6']);
  assert.deepEqual(decoded.issuedEngineWork.rootRestriction, { kind: 'unrestricted' });
  const answer = decoded.observations?.selection?.candidates.find(candidate => candidate.id === root.id)?.detail;
  assert.ok(answer);
  assert.equal(answer.summary_question?.question, 'functional_response');
  assert.deepEqual(answer.summary_question?.witness_line_ids, root.detail.summary_question.witness_line_ids);
  assert.match(answer.summary as string,
    /^Kh2 changes the king's position: against Qa7, White is not in check and can continue with Ne5\./);
  assert.match(answer.summary as string,
    /After Qg5 Qa7\+, the king is in check, so that same move cannot be played immediately\./);
});

test('a replayed functional capture answer retains its current pair and the owned check reply', () => {
  // Real saved engine reports replayed through meaningv7; no new engine search.
  // The short proof is a prefix of a longer prepared witness, not that witness's entire line.
  const { raw, context } = shortFixtureJob('functional-response-sg13-replayed-produced.json.gz', 'Bd5');
  const root = functionalRootQuestion(raw);
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'completed');
  const answer = decoded.observations?.selection?.candidates.find(candidate => candidate.id === root.id)?.detail;
  assert.ok(answer);
  assert.equal(answer.summary_question?.question, 'functional_response');
  assert.deepEqual(answer.summary_question?.witness_line_ids, root.detail.summary_question.witness_line_ids);
  assert.match(answer.summary as string,
    /^Qd7 adds queen control of g4\. In the legal variation Qd7 Bxh4 gxh4 Qg4\+, White can answer the check with Qxg4\./);
  assert.match(answer.summary as string,
    /After Bd5 Bxh4 gxh4 Qg4\+, that capture with the same queen is unavailable\./);
});

test('functional witnesses still reject foreign roots, nonexistent paths and the primary pair as a witness', () => {
  const { raw, context } = shortFixtureJob('functional-response-sg32-pending-produced.json.gz', 'Kh2');
  const root = functionalRootQuestion(raw), detail = root.detail;
  const paths = raw.observations.paths;
  const primary = paths.filter((path: any) => [detail.reference_line_id, detail.candidate_line_id].includes(path.id));
  assert.equal(primary.length, 2);
  const foreign = paths.find((path: any) => path.moves.length > 1 &&
    !primary.some((owned: any) => owned.occurrence_ids[0] === path.occurrence_ids[0]));
  assert.ok(foreign, 'the fixture contains an actual legal path owned by a different root');
  for (const [label, lineId] of [
    ['nonexistent witness', 'foreign-functional-witness'],
    ['different legal root', foreign.id],
    ['primary pair is not supplementary', detail.reference_line_id],
  ]) {
    const changed = structuredClone(raw);
    functionalRootQuestion(changed).detail.summary_question.witness_line_ids.push(lineId);
    assert.equal(decodeMoveReviewSnapshot(changed, context), undefined, label);
  }
  const wrongMove = structuredClone(raw);
  const question = functionalRootQuestion(wrongMove).detail.summary_question;
  const witnessRef = question.units.flatMap((unit: any) => unit.proof_moves ?? [])
    .find((ref: any) => question.witness_line_ids.includes(ref.line_id));
  assert.ok(witnessRef, 'the selected legal function owns a concrete proof move');
  const witnessPath = paths.find((path: any) => path.id === witnessRef.line_id);
  const otherRoot = primary.find((path: any) => !witnessPath.occurrence_ids.includes(path.occurrence_ids[0]));
  assert.ok(otherRoot);
  witnessRef.occurrence_id = otherRoot.occurrence_ids[0];
  assert.equal(decodeMoveReviewSnapshot(wrongMove, context), undefined, 'witness proof uses an occurrence from another path');
});

test('functional witness admission preserves the five-path bound and the explicit question contract', () => {
  const { raw, context } = shortFixtureJob('functional-response-sg32-pending-produced.json.gz', 'Kh2');
  const changed = structuredClone(raw), root = functionalRootQuestion(changed), detail = root.detail;
  const paths = changed.observations.paths;
  const primary = paths.filter((path: any) => [detail.reference_line_id, detail.candidate_line_id].includes(path.id));
  const witnesses = paths.filter((path: any) => path.moves.length > 1 && !primary.includes(path) &&
    primary.some((owned: any) => owned.occurrence_ids[0] === path.occurrence_ids[0]));
  assert.ok(witnesses.length >= 6, 'six distinct same-root paths exist without duplicate or foreign IDs');
  detail.summary_question.witness_line_ids = witnesses.slice(0, 6).map((path: any) => path.id);
  assert.equal(new Set(detail.summary_question.witness_line_ids).size, 6);
  assert.equal(decodeMoveReviewSnapshot(changed, context), undefined, 'six distinct supplementary paths exceed the bound');
  const untyped = structuredClone(raw);
  delete functionalRootQuestion(untyped).detail.summary_question.schema_version;
  assert.equal(decodeMoveReviewSnapshot(untyped, context), undefined, 'the new function answer requires typed ownership');
  const unknown = structuredClone(raw);
  functionalRootQuestion(unknown).detail.summary_question.question = 'unknown_functional_answer';
  assert.equal(decodeMoveReviewSnapshot(unknown, context), undefined, 'new names do not bypass the question allowlist');
});

function offPairFunctionalOwner(raw: any) {
  const root = functionalRootQuestion(raw);
  const paths = raw.observations.paths;
  const primary = [root.detail.reference_line_id, root.detail.candidate_line_id]
    .map(lineId => paths.find((path: any) => path.id === lineId));
  assert.ok(primary.every(Boolean));
  assert.deepEqual(primary.map((path: any) => path.moves[0]), ['a4b3', 'f3e5']);
  const foreignWitness = root.detail.summary_question.witness_line_ids
    .map((lineId: string) => paths.find((path: any) => path.id === lineId))
    .filter((path: any) => !primary.some((owned: any) => owned.moves[0] === path.moves[0]))
    .sort((a: any, b: any) => a.moves.length - b.moves.length)[0];
  assert.ok(foreignWitness);
  assert.deepEqual(foreignWitness.moves, ['e1g1', 'a7a6', 'b5c4', 'c6d4', 'f3d4']);
  const owners = raw.observations.selection.proposals.filter((proposal: any) =>
    proposal.origin.question.producer === 'C-reply-function' && proposal.requirements.some((reading: any) =>
      reading.id === 'function_effect_alternative' && reading.path.source.kind === 'declared' &&
      JSON.stringify(reading.path.source.moves_uci) === JSON.stringify(foreignWitness.moves)));
  assert.equal(owners.length, 1, 'the selected actual Nf3 recapture example keeps its own exact proposal');
  return { root, owner: owners[0], primary, foreignWitness };
}

function functionalReading(owner: any, id: string) {
  const reading = owner.requirements.find((reading: any) => reading.id === id);
  assert.ok(reading, id);
  return reading;
}

function rejectedFunctionalAnswer(raw: any, context: ReturnType<typeof shortFixtureJob>['context'], label: string) {
  const decoded = decodeMoveReviewSnapshot(raw, context);
  // Completed numerical assessments survive an invalid optional commentary
  // selection. Require the entire selection to be absent, so no rejected
  // conditional answer can remain visible alongside that assessment.
  assert.ok(decoded?.kind === 'completed', label + ': keep the valid numerical assessment');
  assert.ok(decoded.observations?.selection === undefined, label + ': reject the commentary selection');
}

test('the actual SG14 off-pair price witness preserves its numerical pair and unmodified conditional answer', () => {
  // Copied unchanged from meaningv16 mcBoardOwner16b final MC-SG14 row 0.
  // The compressed fixture records the source file and row SHA; no verdict,
  // body text or completed engine evidence was generated for this test.
  const { raw, context } = shortFixtureJob('functional-response-sg14-offpair-produced.json.gz', 'Qb3');
  const { root, owner, primary } = offPairFunctionalOwner(raw);
  assert.equal(functionalReading(owner, 'function_root').alternative_move, 'e1g1');
  assert.equal(primary[1].moves[0], 'f3e5');
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'completed');
  assert.equal(decoded.assessmentState, 'current');
  assert.equal(decoded.verification?.state, 'complete');
  const answer = decoded.observations?.selection?.candidates.find(candidate => candidate.id === root.id)?.detail;
  assert.ok(answer);
  assert.equal(answer.reference_line_id, root.detail.reference_line_id);
  assert.equal(answer.candidate_line_id, root.detail.candidate_line_id);
  assert.equal(answer.reference_net_cp, root.detail.reference_net_cp);
  assert.equal(answer.candidate_net_cp, root.detail.candidate_net_cp);
  assert.equal(answer.summary, root.detail.summary);
  assert.deepEqual(answer.summary_question, root.detail.summary_question);
  assert.equal(answer.summary_question?.question, 'functional_response');
  assert.equal(answer.summary_question?.witness_line_ids?.length, 3);
});

test('the actual SG14 board witness survives a deferred function comparison without manufacturing a numeric receipt', () => {
  const { raw, context } = shortFixtureJob('functional-response-sg14-offpair-produced.json.gz', 'Qb3');
  const { root, owner } = offPairFunctionalOwner(raw);
  const priorBody = structuredClone(root.detail.summary_question), priorResult = structuredClone(raw.result);
  const numeric = functionalReading(owner, 'function_root');
  numeric.state = 'deferred';
  numeric.stop_reason = 'comparison_not_current';
  numeric.answer = null;
  numeric.work_id = null;
  assert.equal(functionalReading(owner, 'function_context').state, 'supported');
  assert.equal(functionalReading(owner, 'function_live').state, 'supported');
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'completed');
  const answer = decoded.observations?.selection?.candidates.find(candidate => candidate.id === root.id)?.detail;
  assert.ok(answer);
  assert.deepEqual(answer.summary_question, priorBody);
  assert.equal(answer.summary, root.detail.summary);
  assert.equal(answer.reference_line_id, root.detail.reference_line_id);
  assert.equal(answer.candidate_line_id, root.detail.candidate_line_id);
  const preserved = decoded.observations?.selection?.proposals?.find(proposal => proposal.id === owner.id)
    ?.requirements.find(reading => reading.id === 'function_root');
  assert.ok(preserved);
  assert.equal(preserved.state, 'deferred');
  assert.equal(preserved.answer, null);
  assert.equal(preserved.work_id, null);
  assert.deepEqual(raw.result, priorResult, 'the actual primary numerical assessment stays unchanged');
});

test('the off-pair SG14 example needs its supported generated all-context and exact live declaration', () => {
  const { raw, context } = shortFixtureJob('functional-response-sg14-offpair-produced.json.gz', 'Qb3');
  for (const [label, mutate] of [
    ['another producer', (owner: any) => { owner.origin.question.producer = 'C-support'; }],
    ['missing function context', (owner: any) => {
      owner.requirements = owner.requirements.filter((reading: any) => reading.id !== 'function_context');
    }],
    ['unfinished function context', (owner: any) => {
      const reading = functionalReading(owner, 'function_context');
      reading.state = 'partial';
      reading.result = { state: 'partial', samples: [], needs: [], proofs: [], reason: null };
    }],
    ['any context does not prove all conditions', (owner: any) => {
      functionalReading(owner, 'function_context').mode = 'any';
    }],
    ['live condition is unfinished', (owner: any) => {
      const reading = functionalReading(owner, 'function_live');
      reading.state = 'partial';
      reading.result = { state: 'partial', samples: [], needs: [], proofs: [], reason: null };
    }],
    ['live condition borrows a comparison path', (owner: any) => {
      functionalReading(owner, 'function_live').path.source =
        { kind: 'comparison', requirement_id: 'function_root', arm: 'reference' };
    }],
    ['live declaration belongs to Ne5', (owner: any) => {
      functionalReading(owner, 'function_live').path.source.moves_uci = ['f3e5'];
    }],
    ['live condition has another physical original', (owner: any) => {
      functionalReading(owner, 'function_live').predicate.subject = { side: 'white', role: 'bishop', square: 'b5' };
    }],
    ['primary peer condition is different', (owner: any) => {
      functionalReading(owner, 'function_root_alternative').expected = 'b3';
    }],
    ['declared alternative is absent', (owner: any) => {
      functionalReading(owner, 'function_effect_alternative').path.source.moves_uci = [];
    }],
  ] as [string, (owner: any) => void][]) {
    const changed = structuredClone(raw);
    mutate(offPairFunctionalOwner(changed).owner);
    rejectedFunctionalAnswer(changed, context, label);
  }
});

test('the recorded off-pair proof cannot borrow another question label or a known foreign legal root', () => {
  const { raw, context } = shortFixtureJob('functional-response-sg14-offpair-produced.json.gz', 'Qb3');
  for (const question of ['selected_comparison', 'investigated_mechanism']) {
    const changed = structuredClone(raw);
    functionalRootQuestion(changed).detail.summary_question.question = question;
    rejectedFunctionalAnswer(changed, context, question);
  }
  const changed = structuredClone(raw), { root } = offPairFunctionalOwner(changed);
  const foreign = changed.observations.paths.find((path: any) => path.moves[0] === 'a4c2');
  assert.ok(foreign, 'the actual snapshot retains a legal Qc2 path from this same board');
  // All added IDs come from this stored path. Supplying its known premises
  // cannot give a different root ownership of the selected O-O price proof.
  root.evidence_ids = [...new Set([...root.evidence_ids, ...foreign.line_summary.premise_ids,
    ...foreign.occurrence_ids])];
  root.detail.summary_question.witness_line_ids.push(foreign.id);
  const unit = root.detail.summary_question.units[0];
  assert.ok(Array.isArray(unit.proof_moves));
  unit.proof_moves.push({ line_id: foreign.id, occurrence_id: foreign.occurrence_ids[0] });
  rejectedFunctionalAnswer(changed, context,
    'a known Qc2 original root still has no supported declaration for this conditional answer');
});

test('a stored third legal root cannot relabel the actual played focus of the off-pair answer', () => {
  const { raw, context } = shortFixtureJob('functional-response-sg14-offpair-produced.json.gz', 'Qb3');
  const { primary } = offPairFunctionalOwner(raw), peer = primary[1];
  assert.equal(peer.moves[0], 'f3e5');
  const after = raw.observations.positions.find((position: any) => position.id === peer.position_ids[1]);
  assert.ok(after, 'use the actual stored position after Ne5, not a fabricated board');
  const changed = structuredClone(raw), otherContext = structuredClone(context);
  changed.focus.played_move_uci = peer.moves[0];
  changed.focus.resulting_fen = after.fen;
  otherContext.subject.played = { uci: peer.moves[0], san: 'Ne5' };
  otherContext.subject.after.fen = after.fen;
  assert.equal(changed.focus.played_move_uci, otherContext.subject.played.uci);
  assert.equal(changed.focus.resulting_fen, otherContext.subject.after.fen);
  // Preserve every actual verdict, body and canonical row. This is rejection
  // of relabelling the saved Qb3 job, not a synthetic valid Ne5 job/receipt.
  assert.ok(decodeMoveReviewSnapshot(changed, otherContext) === undefined);
});

test('a recorded unrestricted first-reply survey retains its issued owner without answering an absent named branch', () => {
  const { raw, context } = shortFixtureJob('first-reply-survey-supplement-produced.json.gz', 'Bd5');
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'awaiting-evidence');
  assert.equal(decoded.issuedEngineWork.workId, 'work:5');
  assert.deepEqual(decoded.issuedEngineWork.searchAnchor?.prefixMovesUci, ['e8d7']);
  assert.deepEqual(decoded.issuedEngineWork.rootRestriction,
    { kind: 'restricted', movesUci: ['f6h4', 'f3h5'] });
  assert.equal(decoded.issuedEngineWork.searchLimits.movetimeMs, 8_000);
  const survey = decoded.observations?.selection?.response_searches?.find(search => search.work_id === 'work:4');
  assert.ok(survey);
  assert.deepEqual(survey.root_restriction, { kind: 'unrestricted' });
  assert.deepEqual(survey.prefix_moves_uci, ['e8d7']);
  assert.equal(survey.multi_pv, 3);
  assert.deepEqual(survey.candidates.map(candidate => candidate.move_uci), ['f3a3', 'g6g5', 'f6h4']);
  assert.ok(!survey.candidates.some(candidate => candidate.move_uci === 'f3h5'));
  assert.equal(survey.proposal_ownership?.work_id, survey.work_id);
  assert.equal(survey.proposal_ownership?.execution_key_sha256, survey.execution_key_sha256);
  const ref = survey.proposal_ownership!.question_refs[0];
  const proposal = decoded.observations!.selection!.proposals!.find(proposal => proposal.id === ref.proposal_id);
  assert.ok(proposal);
  assert.equal(proposal.admission, 'generated');
  const reading = proposal.requirements.find(reading => reading.id === ref.requirement_id);
  assert.ok(reading?.kind === 'compare_moves');
  assert.equal(reading.state, 'awaiting_engine');
  assert.equal(reading.work_id, decoded.issuedEngineWork.workId);
  assert.equal(reading.answer, null, 'the omitted named branch is awaiting its exact supplement');
  assert.equal(decoded.assessment?.kind, 'completed', 'the root assessment remains available during this work');
});

test('completed first-reply surveys still require exact issued ownership and a bounded complete legal reply scope', () => {
  const { raw, context } = shortFixtureJob('first-reply-survey-supplement-produced.json.gz', 'Bd5');
  for (const [label, mutate] of [
    ['foreign proposal', (survey: any, _proposal: any, _reading: any, _position: any) => {
      survey.proposal_ownership.question_refs[0].proposal_id = 'foreign-proposal'; }],
    ['foreign requirement', (survey: any, _proposal: any, _reading: any, _position: any) => {
      survey.proposal_ownership.question_refs[0].requirement_id = 'foreign-requirement'; }],
    ['other work', (survey: any, _proposal: any, _reading: any, _position: any) => {
      survey.proposal_ownership.work_id = 'work:5'; }],
    ['other execution key', (survey: any, _proposal: any, _reading: any, _position: any) => {
      survey.proposal_ownership.execution_key_sha256 = '0'.repeat(64); }],
    ['duplicate owner', (survey: any, _proposal: any, _reading: any, _position: any) => {
      survey.proposal_ownership.question_refs.push(structuredClone(survey.proposal_ownership.question_refs[0])); }],
    ['submitted owner', (_survey: any, proposal: any, _reading: any, _position: any) => {
      proposal.admission = 'submitted'; }],
    ['other prefix', (_survey: any, _proposal: any, reading: any, _position: any) => {
      reading.prefix_moves_uci = []; }],
    ['same named move twice', (_survey: any, _proposal: any, reading: any, _position: any) => {
      reading.alternative_move = reading.reference_move; }],
    ['illegal named move', (_survey: any, _proposal: any, reading: any, _position: any) => {
      reading.alternative_move = 'a1a2'; }],
    ['incomplete legal inventory', (_survey: any, _proposal: any, _reading: any, position: any) => {
      position.legal_moves.complete = false; }],
    ['reduced MultiPV', (survey: any, _proposal: any, _reading: any, _position: any) => {
      survey.multi_pv = 2; }],
  ] as [string, (survey: any, proposal: any, reading: any, position: any) => void][]) {
    const changed = structuredClone(raw);
    const selection = changed.observations.selection;
    const survey = selection.response_searches.find((search: any) => search.work_id === 'work:4');
    const ref = survey.proposal_ownership.question_refs[0];
    const proposal = selection.proposals.find((proposal: any) => proposal.id === ref.proposal_id);
    const reading = proposal.requirements.find((reading: any) => reading.id === ref.requirement_id);
    const position = changed.observations.positions.find((position: any) => position.id === survey.child_position_id);
    mutate(survey, proposal, reading, position);
    assert.equal(decodeMoveReviewSnapshot(changed, context), undefined, label);
  }
});

test('a short focus retry requires the exact stopped focus receipt and keeps verification pending', () => {
  const { raw, context } = shortFixtureJob('short-focus-retry-produced.json.gz', 'g5');
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.equal(decoded?.kind, 'awaiting-evidence');
  assert.ok(decoded?.kind === 'awaiting-evidence');
  assert.equal(decoded.issuedEngineWork.purpose, 'focus_comparison');
  assert.equal(decoded.issuedEngineWork.searchLimits.nodes, 20_000_000);
  assert.equal(decoded.issuedEngineWork.searchLimits.movetimeMs, 2_500);
  assert.equal(decoded.verification?.state, 'pending');
  for (const mutate of [
    (value: any) => { value.observations.selection.root_searches = []; },
    (value: any) => { for (const search of value.observations.selection.root_searches)
      if (search.stop_reason === 'search_budget_exhausted') search.stop_reason = 'cancelled'; },
    (value: any) => { for (const search of value.observations.selection.root_searches)
      if (search.stop_reason === 'search_budget_exhausted') search.root_restriction.moves_uci = ['e5e6', 'g6g5']; },
    (value: any) => { value.issued_engine_work.search_limits.depth = 8; },
    (value: any) => { value.issued_engine_work.max_search_elapsed_ms = 3_501; },
  ]) {
    const changed = structuredClone(raw);
    mutate(changed);
    assert.equal(decodeMoveReviewSnapshot(changed, context), undefined);
  }
});

test('a recorded rebound answer keeps its failed assessment refresh visible', () => {
  const { raw, context } = shortFixtureJob('failed-assessment-refresh-produced.json.gz', 'g4');
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded, 'the retained answer and unresolved assessment are a valid public state');
  assert.ok(decoded.observations?.selection);
  assert.equal(raw.assessment_state, 'needs_refresh');
  const inquiry = raw.observations.selection.inquiries.find((i: any) => i.stop_reason === 'search_budget_exhausted');
  assert.equal(inquiry.state, 'answered');
  assert.equal(inquiry.assessment_effect, 'needs_refresh');
  for (const effect of ['unchanged', 'refreshed']) {
    const changed = structuredClone(raw);
    changed.observations.selection.inquiries.find((i: any) => i.stop_reason === 'search_budget_exhausted')
      .assessment_effect = effect;
    assert.equal(decodeMoveReviewSnapshot(changed, context)?.observations?.selection, undefined);
  }
});

test('a reserve inquiry requires every legal king move in its pool and its original pawn', () => {
  // Synthetic resource subset at the legally checked endgame decision. The
  // full snapshot and its response ownership are covered by recorded UI runs.
  const proposal = { admission: 'generated', origin: { question: { producer: 'C-reserve-hold' } } } as CommentaryProposal;
  const reading = { id: 'reserve_hold', kind: 'compare_moves', reference_move: 'f6e6', alternative_move: 'a7a6' } as CommentaryProposalReading;
  const kings = ['f6e6', 'f6e7', 'f6f7', 'f6g6', 'f6g7'];
  const observations = { positions: [{ id: 'decision', fen: '8/p7/5k2/1p1p3p/1P1P1K1P/P7/8/8 b - - 0 49',
    controls: { pieces: [{ piece: { side: 'black', role: 'king', square: 'f6' } },
      { piece: { side: 'black', role: 'pawn', square: 'a7' } }] },
    legal_moves: { complete: true, moves: ['a7a5', 'a7a6', ...kings].map(uci => ({ uci })) },
  }] } as unknown as MoveReviewObservations;
  const pool = ['a7a6', ...kings];
  assert.equal(reserveResponsePoolMatches(proposal, reading, pool, observations, 'decision'), true);
  for (const wrong of [pool.slice(0, -1), [...pool, 'a7a5'], [...pool, 'f6e6'], ['a7a6', 'f6e6']])
    assert.equal(reserveResponsePoolMatches(proposal, reading, wrong, observations, 'decision'), false);
  assert.equal(reserveResponsePoolMatches(proposal, reading, pool, observations, 'foreign'), false);
  assert.equal(reserveResponsePoolMatches(proposal,
    { ...reading, alternative_move: 'f6g6' } as CommentaryProposalReading, pool, observations, 'decision'), false);
  assert.equal(reserveResponsePoolMatches({ ...proposal, admission: 'submitted' } as CommentaryProposal,
    reading, pool, observations, 'decision'), false);
});

test('a reserve counter pool includes pawn captures as well as quiet pawn waits', () => {
  const { raw, context } = shortFixtureJob('reserve-pawn-pool-produced.json.gz', 'g4');
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'awaiting-evidence');
  const work = raw.issued_engine_work;
  assert.deepEqual(work.root_restriction.moves_uci, ['g5g6', 'b4a3', 'b4b3', 'b4c3']);
  const owner = raw.observations.selection.proposals.find((p: any) => p.requirements.some((r: any) =>
    r.id === 'reserve_counter' && r.work_id === work.work_id));
  const reading = owner.requirements.find((r: any) => r.id === 'reserve_counter');
  const pool = work.root_restriction.moves_uci;
  assert.equal(reserveResponsePoolMatches(owner, reading, pool, raw.observations, work.search_anchor.position_id), true);
  for (const wrong of [pool.slice(0, -1), ['g5g6', 'b4b3'], [...pool, 'g5h5'], [...pool, 'b4b3']])
    assert.equal(reserveResponsePoolMatches(owner, reading, wrong, raw.observations, work.search_anchor.position_id), false);
  const forged = structuredClone(raw);
  forged.issued_engine_work.search_anchor.position_id = 'foreign-position';
  assert.equal(decodeMoveReviewSnapshot(forged, context), undefined);
});

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

test('the longer focus retry requires the same stopped comparison at the owned root', () => {
  const fixture = structuredClone(certifiedFixtures.find(f => f.name === 'Bf3')!);
  const raw = fixture.response;
  const observations = object(raw.observations);
  const path = (observations.paths as JsonObject[]).find(p => p.kind !== 'history')!;
  const roots = [fixture.move, 'a1b1'];
  const stopped = {
    work_id: 'work:1', execution_key_sha256: 'b'.repeat(64), engine_profile: raw.engine_profile,
    purpose: 'focus_comparison', required_depth: 16, multi_pv: 2,
    root_position_id: (path.position_ids as string[])[0],
    root_occurrence_id: (path.occurrence_ids as string[])[0],
    root_restriction: { kind: 'restricted', moves_uci: roots },
    state: 'stopped', stop_reason: 'search_budget_exhausted', candidates: [],
  };
  observations.selection = { version: 2, candidates: [], scenes: [], root_searches: [stopped] };
  raw.progress = { phase: 'focus_comparison', legal_move_count: 34, root_candidate_lines_admitted: 3,
    selected_commentaries_completed: 0, physical_works_issued: 3, physical_reports_accepted: 2 };
  const work = object(raw.issued_engine_work);
  Object.assign(work, { work_id: 'work:2', execution_key_sha256: 'c'.repeat(64), purpose: 'focus_comparison',
    root_restriction: { kind: 'restricted', moves_uci: roots },
    search_limits: { depth: 16, nodes: 40_000_000, movetime_ms: 8_000, multi_pv: 2 },
    max_search_elapsed_ms: 9_000 });
  const context = { requestId: raw.request_id as string, subject: {
    variant: 'standard' as const, initialFen: fixture.fen, movePrefixUci: [],
    before: { path: '', fen: fixture.fen }, played: { uci: fixture.move, san: fixture.move as San },
    after: { path: 'aa', fen: object(raw.focus).resulting_fen as FEN },
  } };
  assert.equal(decodeMoveReviewSnapshot(raw, context)?.kind, 'awaiting-evidence');
  for (const mutate of [
    (s: JsonObject) => { s.state = 'completed'; s.stop_reason = null; },
    (s: JsonObject) => { s.stop_reason = 'cancelled'; },
    (s: JsonObject) => { s.required_depth = 17; },
    (s: JsonObject) => { s.root_occurrence_id = 'foreign'; },
    (s: JsonObject) => { s.root_restriction = { kind: 'restricted', moves_uci: [fixture.move, 'a1a2'] }; },
  ]) {
    const changed = structuredClone(raw);
    mutate((object(object(changed.observations).selection).root_searches as JsonObject[])[0]);
    assert.equal(decodeMoveReviewSnapshot(changed, context), undefined);
  }
  const unowned = structuredClone(raw);
  object(object(unowned.observations).selection).root_searches = [];
  assert.equal(decodeMoveReviewSnapshot(unowned, context), undefined);
});

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
