import assert from 'node:assert/strict';
import { after, test } from 'node:test';
import { JSDOM } from 'jsdom';
import { Chess } from 'chessops/chess';
import { attacks } from 'chessops/attacks';
import { makeFen, parseFen } from 'chessops/fen';
import { makeSquare, parseSquare, parseUci } from 'chessops/util';
import type { VNode } from 'lib/view/snabbdom';
import type { MoveReviewFrameSelection, MoveReviewObservation, MoveReviewObservations, MoveReviewSubject, ObservedLineSummary, PositionResources } from '../src/moveReview';
import {
  decodeCommentarySelection,
  proposalContainsResponsePool,
  type CommentaryCandidate,
  type CommentaryEffect,
  type CommentaryInquiry,
  type CommentaryProposal,
  type CommentaryPvComparisonDetail,
  type CommentaryQuestionSummary,
  type CommentaryQuestionUnit,
  type CommentaryResponseSearch,
} from '../src/moveReviewSelection';
import type { MoveReviewPanelProps } from '../src/view/moveReview';

const dom = new JSDOM('<!doctype html><html><body></body></html>', { url: 'http://localhost/', pretendToBeVisual: true });
Object.assign(globalThis, { window: dom.window, document: dom.window.document,
  HTMLElement: dom.window.HTMLElement, Event: dom.window.Event,
  requestAnimationFrame: dom.window.requestAnimationFrame.bind(dom.window),
  cancelAnimationFrame: dom.window.cancelAnimationFrame.bind(dom.window) });
const { decodeMoveReviewSnapshot, moveReviewCopy, moveReviewSan, normalizeMoveReviewLocale, updateMoveReviewViewForJob } = await import('../src/moveReview');
const { renderMoveReview, renderMoveReviewBoard } = await import('../src/view/moveReview');
after(() => dom.window.close());

test('transport retains every owned highlight while rejecting invalid frame evidence', () => {
  const before = '4k3/8/8/8/8/8/8/4K3 w - - 0 1';
  const after = '4k3/8/8/8/8/8/8/5K2 b - - 1 1';
  const destinations = ['e1', 'e2', 'f2', 'g2'];
  const sourceIds = destinations.map((_, i) => String(i + 1).repeat(64));
  const position = (id: string, fen: string) => ({ id, fen, premise_id: `premise-${id}`,
    controls: { complete: false, pieces: id === 'after' ? [{
      piece: { side: 'white', role: 'king', square: 'f1' }, squares: destinations, source_ids: sourceIds,
    }] : [] },
    rays: { complete: false, lines: [] }, legal_moves: { complete: false, moves: [] },
    pawn_advances: { complete: false, paths: [] }, pawn_states: { complete: true, pawns: [] },
    pawn_files: { complete: false, files: [] },
  });
  const highlights = destinations.map((square, i) => ({ occurrence_id: 'king-step',
    text: `The king controls ${square}.`, squares: ['f1', square], premise_ids: [] as string[],
    frames: [{ phase: 'after', position_id: 'after', squares: ['f1', square], source_ids: [sourceIds[i]!] }],
  }));
  const raw = {
    schema_version: 'chesstory.position-commentary.job-status.v6', request_id: 'inline-highlights',
    job_id: 'a'.repeat(32), engine_profile: 'sf18-smallnet-t2-h16-v1', variant: 'standard', deadline_epoch_ms: 1,
    focus: { kind: 'played_move', played_move_uci: 'e1f1', resulting_fen: after },
    state: 'stopped', stop_condition: 'cancelled',
    progress: { phase: 'stopped', legal_move_count: 0, root_candidate_lines_admitted: 0,
      selected_commentaries_completed: 0, physical_works_issued: 0, physical_reports_accepted: 0 },
    observations: { statements: [], paths: [{ id: 'played', kind: 'played', start_fen: before,
      moves: ['e1f1'], occurrence_ids: ['king-step'], position_ids: ['before', 'after'] }],
    positions: [position('before', before), position('after', after)], highlights },
  };
  const context = { requestId: raw.request_id, subject: {
    variant: 'standard', initialFen: before, movePrefixUci: [], before: { path: '', fen: before },
    played: { uci: 'e1f1', san: 'Kf1' }, after: { path: 'ab', fen: after },
  } as MoveReviewSubject };
  const decoded = decodeMoveReviewSnapshot(raw, context)?.observations?.highlights;
  assert.equal(decoded?.length, 4, 'a delivered original frame cannot be lost to a display budget');
  assert.deepEqual(decoded![3]!.frames, [{ phase: 'after', positionId: 'after',
    squares: ['f1', 'g2'], sourceIds: [sourceIds[3]] }]);
  const mutations: [string, (highlight: typeof highlights[number]) => void][] = [
    ['foreign source', h => { h.frames[0]!.source_ids = ['f'.repeat(64)]; }],
    ['wrong position', h => { h.frames[0]!.position_id = 'before'; }],
    ['foreign occurrence', h => { h.occurrence_id = 'foreign'; }],
    ['foreign premise', h => { h.premise_ids = ['foreign']; }],
    ['duplicate phase', h => { h.frames.push(structuredClone(h.frames[0]!)); }],
  ];
  for (const [name, mutate] of mutations) {
    const changed = structuredClone(raw);
    mutate(changed.observations.highlights[3]!);
    assert.deepEqual(decodeMoveReviewSnapshot(changed, context)?.observations?.highlights, decoded!.slice(0, 3), name);
  }
});

// Inline renderer inputs: this suite does not load game or evaluation fixtures.
const startFen = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';
const playedFen = 'rnbqkbnr/pppppppp/8/8/8/3P4/PPP1PPPP/RNBQKBNR b KQkq - 0 1';
const replyFen = 'rnbqkbnr/ppppppp1/7p/8/8/3P4/PPP1PPPP/RNBQKBNR w KQkq - 0 2';
const alternativeFen = 'rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KQkq - 0 1';
const explanation = 'After d3, White bishop on c1 controls h6; after e3, it does not.';
const pvExplanation = 'In the provided d3 line, the bishop’s diagonal remains open.';
const subject: MoveReviewSubject = {
  variant: 'standard',
  initialFen: startFen,
  movePrefixUci: [],
  before: { path: '', fen: startFen },
  played: { uci: 'd2d3', san: 'd3' },
  after: { path: 'ab', fen: playedFen },
};

function primary(): CommentaryCandidate {
  return {
    id: 'primary',
    kind: 'comparison',
    selection: 'selected',
    rootOccurrenceId: 'root',
    occurrenceIds: ['root', 'alternative-root'],
    positionIds: ['before', 'after', 'alternative-after'],
    lineIds: ['played', 'alternative'],
    evidenceIds: [],
    sourceRefs: [],
    detail: {
      type: 'root_comparison',
      reference_line_id: 'played',
      candidate_line_id: 'alternative',
      material_relation: 'equal',
      text: explanation,
    },
  };
}

function pv(rootRelation: CommentaryPvComparisonDetail['root_observation_relation'] = 'same'): CommentaryCandidate {
  const evaluation = {
    source: 'root_search',
    kind: 'engine_search',
    depth: 8,
    white_score: { kind: 'cp', value: 0 },
  } as const;
  const detail: CommentaryPvComparisonDetail = {
    type: 'pv_comparison',
    scope: 'observed_lines',
    answer: {
      work_id: 'work',
      execution_key_sha256: 'a'.repeat(64),
      engine_profile: 'inline-render-test',
      required_depth: 8,
      reference_depth: 8,
      alternative_depth: 8,
      evaluation_relation: 'equal',
      reference_line_id: 'played',
      alternative_line_id: 'alternative',
      reference_occurrence_id: 'root',
      alternative_occurrence_id: 'alternative-root',
      reference_evaluation: evaluation,
      alternative_evaluation: evaluation,
    },
    material_relation: 'equal',
    event_sequence_relation: 'same',
    root_observation_relation: rootRelation,
    root_difference_occurrence_ids: [],
    root_participants: [],
    quality_band: 'close',
    text: pvExplanation,
  };
  return { ...primary(), id: 'pv', detail: { ...detail } };
}

function question(): CommentaryCandidate {
  return {
    ...primary(),
    id: 'question',
    detail: {
      type: 'question_comparison',
      scope: 'same_position_pair',
      prefix_moves_uci: [],
      reference_move: 'd2d3',
      alternative_move: 'e2e3',
      perspective: 'white',
      question_refs: [],
      evaluation_relation: 'better',
      stop_reason: null,
      text: 'The alternative evaluates higher; its separate continuation remains relevant.',
    },
  };
}

function reversedPv(reading = pv()): CommentaryCandidate {
  const result = structuredClone(reading);
  result.id = 'reversed-pv';
  result.lineIds.reverse();
  const answer = result.detail.answer as CommentaryPvComparisonDetail['answer'];
  [answer.reference_line_id, answer.alternative_line_id] = [answer.alternative_line_id, answer.reference_line_id];
  [answer.reference_occurrence_id, answer.alternative_occurrence_id] =
    [answer.alternative_occurrence_id, answer.reference_occurrence_id];
  [answer.reference_evaluation, answer.alternative_evaluation] =
    [answer.alternative_evaluation, answer.reference_evaluation];
  [answer.reference_depth, answer.alternative_depth] = [answer.alternative_depth, answer.reference_depth];
  answer.evaluation_relation = ({ better: 'worse', worse: 'better', equal: 'equal' } as const)[answer.evaluation_relation];
  return result;
}

function panelProps(
  candidates: CommentaryCandidate[],
  locale: MoveReviewPanelProps['locale'] = 'en-US',
  recordReview?: MoveReviewPanelProps['job']['recordReview'],
  actions: Partial<MoveReviewPanelProps['actions']> = {},
  extraPaths: MoveReviewObservations['paths'] = [],
  inquiries: CommentaryInquiry[] = [],
  viewState: MoveReviewPanelProps['view'] = { evidenceExpanded: false },
): MoveReviewPanelProps {
  const observations: MoveReviewObservations = {
    statements: [],
    paths: [
      {
        id: 'played',
        startFen,
        kind: 'played',
        occurrenceIds: ['root', 'reply'],
        positionIds: ['before', 'after', 'reply-position'],
        moves: [
          { uci: 'd2d3', label: 'd3', fenAfter: playedFen },
          { uci: 'h7h6', label: 'h6', fenAfter: replyFen },
        ],
      },
      {
        id: 'alternative',
        startFen,
        kind: 'analysis',
        occurrenceIds: ['alternative-root', 'alternative-reply'],
        positionIds: ['before', 'alternative-after', 'alternative-reply-position'],
        moves: [
          { uci: 'e2e3', label: 'e3', fenAfter: alternativeFen },
          { uci: 'h7h6', label: 'h6', fenAfter: 'rnbqkbnr/ppppppp1/7p/8/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 2' },
        ],
      },
      ...extraPaths,
    ],
    selection: {
      version: 2,
      candidates,
      inquiries,
      scenes: candidates
        .filter(candidate => candidate.id === 'primary' ||
          (candidate.detail.type === 'question_comparison' && candidate.selection === 'selected'))
        .map(candidate => ({
          id: `scene-${candidate.id}`,
          rootOccurrenceId: candidate.rootOccurrenceId,
          occurrenceIds: candidate.occurrenceIds,
          lineIds: candidate.lineIds,
          candidateIds: [candidate.id],
        })),
    },
  };
  const noop = () => {};
  return {
    job: {
      kind: 'loading',
      subject,
      observations,
      recordReview,
      observationFrame: { proofId: 'played', ply: 1, phase: 'after' },
    },
    view: viewState,
    copy: moveReviewCopy(locale),
    locale,
    orientation: 'white',
    canWrite: false,
    liveEnginePaused: false,
    actions: {
      selectCandidate: noop,
      toggleEvidence: noop,
      toggleProof: noop,
      previewFrame: noop,
      clearPreview: noop,
      pinFrame: noop,
      clearPin: noop,
      retry: noop,
      addProof: noop,
      viewAddedLine: noop,
      jumpToScene: noop,
      ...actions,
    },
  };
}

function render(...args: Parameters<typeof panelProps>): VNode {
  return renderMoveReview(panelProps(...args));
}

function nodes(node: VNode): VNode[] {
  return [
    node,
    ...(node.children ?? []).flatMap(child => (typeof child === 'object' && child ? nodes(child) : [])),
  ];
}

function terminalParagraphs(node: VNode): VNode[] {
  return nodes(node).filter(node => node.sel === 'p' && typeof node.data?.attrs?.['data-terminal'] === 'string');
}

function text(node: VNode): string {
  return [
    ...(node.text === undefined ? [] : [node.text]),
    ...(node.children ?? []).map(child => (typeof child === 'object' && child ? text(child) : (child ?? ''))),
  ].join('');
}

function linkedBoardProps() {
  const props = panelProps([primary(), pv()]);
  const observations = props.job.observations!;
  observations.positions = (['after', 'reply-position'] as const).map((id, index): PositionResources => ({
    id, fen: index ? replyFen : playedFen, premise_id: `premise-${id}`,
    controls: { complete: false, pieces: [
      { piece: { side: 'white', role: 'bishop', square: 'c1' }, squares: ['h6', 'e3'],
        source_ids: ['bishop-h6', 'bishop-e3'] },
      { piece: { side: 'black', role: 'pawn', square: 'g7' }, squares: ['h6'], source_ids: ['pawn-h6'] },
    ] },
    rays: { complete: false, lines: [] }, legal_moves: { complete: false, moves: [] },
    pawn_states: { complete: false, pawns: [] }, pawn_files: { complete: false, files: [] },
    pawn_advances: { complete: false, paths: [] },
  }));
  const highlight: NonNullable<MoveReviewObservations['highlights']>[number] = {
    occurrenceId: 'reply', text: 'The bishop controls h6.', squares: ['c1', 'h6'],
    premiseIds: ['premise-after', 'premise-reply-position'],
    frames: (['before', 'after'] as const).map((phase, index) => ({
      phase, positionId: index ? 'reply-position' : 'after', squares: ['c1', 'h6'], sourceIds: ['bishop-h6'],
    })),
  };
  observations.highlights = [highlight];
  const frame: MoveReviewFrameSelection = {
    proofId: 'played', ply: 2, phase: 'after', highlightKey: JSON.stringify(highlight),
  };
  props.view.pinnedFrame = frame;
  props.view.boardFrame = frame;
  return { props, frame, highlight };
}

function boardConfig(view: VNode) {
  const board = nodes(view).find(node => node.sel === 'div.cg-wrap.is2d')!;
  assert.ok(board, 'the selected position has a board');
  const elm = document.createElement('div') as HTMLDivElement & { _cgConfig?: {
    fen: string; drawable: { autoShapes: { orig: string; dest?: string; brush: string }[] };
  } };
  const observer = globalThis.ResizeObserver;
  globalThis.ResizeObserver = class { observe() {} unobserve() {} disconnect() {} };
  try {
    board.data!.hook!.insert!({ ...board, elm });
    assert.ok(elm._cgConfig);
    return elm._cgConfig;
  } finally {
    globalThis.ResizeObserver = observer;
  }
}

function trigger(node: VNode, name: string, event = new Event(name, { cancelable: true })) {
  const handler = node.data?.on?.[name];
  assert.equal(typeof handler, 'function');
  if (typeof handler === 'function') handler(event, node);
  return event;
}

function deliveryPath(id: string, fen: string, ucis: string[]): MoveReviewObservations['paths'][number] {
  const position = Chess.fromSetup(parseFen(fen).unwrap()).unwrap();
  const events: NonNullable<ObservedLineSummary['material']>['events'] = [];
  const value = { pawn: 100, knight: 300, bishop: 300, rook: 500, queen: 900, king: 0 };
  let balance = 0;
  const moves = ucis.map((uci, index) => {
    const move = parseUci(uci)!;
    assert.ok('from' in move && position.isLegal(move));
    const piece = position.board.get(move.from)!, captured = position.board.get(move.to);
    const label = moveReviewSan(makeFen(position.toSetup()), uci);
    position.play(move);
    const promotion = move.promotion ? value[move.promotion] - value.pawn : 0;
    const delta = (piece.color === 'white' ? 1 : -1) * ((captured ? value[captured.role] : 0) + promotion);
    balance += delta;
    events.push({ ply_offset: index, occurrence_id: `${id}-${index}`, move_uci: uci,
      side: piece.color, from: makeSquare(move.from), to: makeSquare(move.to), before_role: piece.role,
      after_role: move.promotion ?? piece.role, promotion_gain_cp: promotion, delta_cp: delta, balance_cp: balance,
      ...(captured ? { capture: { role: captured.role, square: makeSquare(move.to), value_cp: value[captured.role],
        recapture: index > 0 && ucis[index - 1].slice(2, 4) === makeSquare(move.to) ? 'proven' as const : 'excluded' as const } } : {}),
    });
    return { uci, label, fenAfter: makeFen(position.toSetup()) };
  });
  const kind = position.isCheckmate() ? 'checkmate' : position.isStalemate() ? 'stalemate'
    : position.isInsufficientMaterial() ? 'insufficient_material' : 'line_end';
  return { id, kind: id === 'played' ? 'played' : 'analysis', startFen: fen, moves,
    occurrenceIds: moves.map((_, index) => `${id}-${index}`), positionIds: ['before', ...moves.map((_, index) => `${id}-position-${index}`)],
    lineSummary: { scope: 'observed_line', perspective: 'white', premise_ids: [`material-${id}`], events: [],
      material: { events, net_cp: balance, max_gain_cp: Math.max(0, ...events.map(e => e.balance_cp)),
        max_loss_cp: Math.min(0, ...events.map(e => e.balance_cp)), recovered_after_loss: false },
      ending: { kind, ply_offset: moves.length - 1, occurrence_id: `${id}-${moves.length - 1}` } },
  };
}

function questionResponse(observations: MoveReviewObservations, question: CommentaryQuestionSummary['question'] = 'selected_comparison') {
  const paths = observations.paths;
  observations.positions ??= [...new Map(paths.flatMap(path => path.positionIds!.map((id, index) => [id!, {
    id: id!, fen: index ? path.moves[index - 1].fenAfter : path.startFen, premise_id: `position-${id}`,
    controls: { complete: false, pieces: [] }, rays: { complete: false, lines: [] },
    legal_moves: { complete: false, moves: [] }, pawn_states: { complete: false, pawns: [] },
    pawn_files: { complete: false, files: [] }, pawn_advances: { complete: false, paths: [] },
  }] as const))).values()];
  const units: CommentaryQuestionUnit[] = paths.map(path => {
    const moves = path.occurrenceIds.map(occurrence_id => ({ line_id: path.id, occurrence_id: occurrence_id! }));
    const covered_moves = path.lineSummary!.material!.events.filter(event => event.capture)
      .map(event => ({ line_id: path.id, occurrence_id: event.occurrence_id }));
    const en = ['The supplied line is ', ...moves.flatMap((move, index) => [index ? ' ' : '', move]).filter(part => part !== ''), '.'];
    return { role: covered_moves.length ? 'cost' : 'condition', en, ko: en, english_only: true,
      required_moves: moves, covered_moves };
  });
  const summary: CommentaryQuestionSummary = { question, units, detail_moves: [], detail_units: structuredClone(units) };
  const signature = (path: typeof paths[number]) => JSON.stringify(path.lineSummary!.material!.events.flatMap(event => [
    ...(event.capture ? [`loss:${event.side === 'white' ? 'black' : 'white'}:${event.capture.role}`] : []),
    ...(event.promotion_gain_cp ? [`promotion:${event.side}:${event.before_role}:${event.after_role}`] : []),
  ]).sort());
  const raw = { version: 2, scenes: [], candidates: [{ id: 'primary', kind: 'comparison', selection: 'selected',
    root_occurrence_id: paths[0].occurrenceIds[0], occurrence_ids: [...new Set(paths.flatMap(path => path.occurrenceIds))],
    position_ids: [...new Set(paths.flatMap(path => path.positionIds!))], line_ids: paths.map(path => path.id),
    evidence_ids: ['comparison-proof', ...paths.flatMap(path => [...path.occurrenceIds, ...path.lineSummary!.premise_ids])],
    source_refs: paths.map(path => ({ line_id: path.id, kind: 'admitted', source: 'legal_replay',
      accepted_ply_count: path.moves.length, stop: path.lineSummary!.ending.kind === 'line_end' ? null : 'automatic_terminal' })),
    detail: { type: 'root_comparison', scope: 'observed_lines', comparison_kind: 'played_vs_best',
      comparison_evidence_id: 'comparison-proof', reference_line_id: paths[0].id, candidate_line_id: paths[1].id,
      missing_line_ids: [], material_relation: signature(paths[0]) === signature(paths[1]) ? 'equal' : 'different',
      material_unavailable_reason: null, perspective: 'white', reference_net_cp: paths[0].lineSummary!.material!.net_cp,
      candidate_net_cp: paths[1].lineSummary!.material!.net_cp, summary_kind: 'question_answer',
      text: 'Legacy diagnostic prose.', summary: '', summary_question: summary },
  }] };
  const sync = () => {
    raw.candidates[0].detail.summary = summary.units.map(unit => unit.en.map(part => typeof part === 'string' ? part : (() => {
      const path = paths.find(path => path.id === part.line_id)!;
      return path.moves[path.occurrenceIds.indexOf(part.occurrence_id)].label;
    })()).join('')).join(' ');
  };
  sync();
  return { raw, summary, sync };
}

test('answer units admit exact focus and reject foreign, hidden, duplicate, and language-mismatched references', () => {
  const observations: MoveReviewObservations = { statements: [], paths: [
    deliveryPath('played', startFen, ['d2d3', 'h7h6']), deliveryPath('alternative', startFen, ['e2e3', 'h7h6']),
  ] };
  observations.highlights = [{ occurrenceId: 'played-1', text: 'The bishop controls h6.', squares: ['c1', 'h6'],
    premiseIds: ['material-played'], frames: [{ phase: 'after', positionId: 'played-position-1', squares: ['c1', 'h6'], sourceIds: ['bishop-h6'] }] }];
  const { raw, summary } = questionResponse(observations);
  summary.schema_version = 2;
  summary.units[0].focus = [{ line_id: 'played', occurrence_id: 'played-1', phase: 'after',
    position_id: 'played-position-1', squares: ['c1', 'h6'], source_ids: ['bishop-h6'] }];
  summary.detail_units![0].focus = structuredClone(summary.units[0].focus);
  assert.ok(decodeCommentarySelection(raw, observations));
  const changes: [string, (unit: CommentaryQuestionUnit) => void][] = [
    ['foreign occurrence', unit => { unit.focus![0].occurrence_id = 'alternative-1'; }],
    ['foreign position', unit => { unit.focus![0].position_id = 'alternative-position-1'; }],
    ['different phase', unit => { unit.focus![0].phase = 'before'; }],
    ['different squares', unit => { unit.focus![0].squares.push('e3'); }],
    ['different sources', unit => { unit.focus![0].source_ids.push('bishop-e3'); }],
    ['duplicate assignment', unit => { unit.focus!.push(structuredClone(unit.focus![0])); }],
    ['required hidden move', unit => { unit.required_moves!.push({ line_id: 'alternative', occurrence_id: 'alternative-1' }); }],
    ['duplicate required move', unit => { unit.required_moves!.push(unit.required_moves![0]); }],
    ['language reference mismatch', unit => { unit.ko = unit.ko.filter(part => typeof part === 'string'); }],
    ['English-only translation mismatch', unit => { unit.ko = ['다른 문장 ', ...unit.ko]; }],
  ];
  for (const [name, mutate] of changes) for (const section of ['units', 'detail_units'] as const) {
    const changed = structuredClone(raw);
    mutate(changed.candidates[0].detail.summary_question[section]![0]);
    assert.equal(decodeCommentarySelection(changed, observations), undefined, `${section}: ${name}`);
  }
});

test('version 2 admits a closed four-capture cost unit and rejects missing costs or physical check replies', () => {
  const fen = '3r2bk/p7/8/3p4/4P3/8/8/K2R4 w - - 0 1';
  const observations: MoveReviewObservations = { statements: [], paths: [
    deliveryPath('played', fen, ['e4d5', 'd8d5', 'd1d5', 'g8d5']),
    deliveryPath('alternative', fen, ['e4e5', 'h8h7']),
  ] };
  const { raw, summary } = questionResponse(observations, 'contested_pair_response');
  assert.equal(summary.units[0].covered_moves.length, 4);
  assert.equal(decodeCommentarySelection(raw, observations), undefined, 'unversioned input keeps its legacy grouping adapter');
  summary.schema_version = 2;
  assert.ok(decodeCommentarySelection(raw, observations), 'all four actual captures remain visible in one closed account');
  const unsupported = structuredClone(raw);
  Object.assign(unsupported.candidates[0].detail.summary_question, { schema_version: 3 });
  assert.equal(decodeCommentarySelection(unsupported, observations), undefined);
  for (const section of ['units', 'detail_units'] as const) {
    const missing = structuredClone(raw);
    missing.candidates[0].detail.summary_question[section]![0].covered_moves.pop();
    assert.equal(decodeCommentarySelection(missing, observations), undefined, `${section}: visible capture loses its cost`);
    const truncated = questionResponse(observations, 'contested_pair_response');
    truncated.summary.schema_version = 2;
    const unit = truncated.summary[section]![0], last = unit.required_moves!.at(-1)!;
    unit.en = unit.en.filter(part => typeof part === 'string' || part.occurrence_id !== last.occurrence_id);
    unit.ko = unit.en; unit.required_moves!.pop(); unit.covered_moves.pop(); truncated.sync();
    assert.equal(decodeCommentarySelection(truncated.raw, observations), undefined, `${section}: actual recapture is hidden`);
  }
  const checkingFen = '7k/8/8/8/8/8/8/R6K w - - 0 1';
  const checking: MoveReviewObservations = { statements: [], paths: [
    deliveryPath('played', checkingFen, ['a1a8', 'h8h7']),
    deliveryPath('alternative', checkingFen, ['a1a7', 'h8g8']),
  ] };
  // Delivery uses the admitted position to require a reply, not punctuation in display notation.
  checking.paths[0].moves[0].label = checking.paths[0].moves[0].label.replace(/[+#]$/, '');
  for (const section of ['units', 'detail_units'] as const) {
    const reply = questionResponse(checking, 'contested_pair_response');
    reply.summary.schema_version = 2;
    assert.ok(decodeCommentarySelection(reply.raw, checking));
    const unit = reply.summary[section]![0], last = unit.required_moves!.pop()!;
    unit.en = unit.en.filter(part => typeof part === 'string' || part.occurrence_id !== last.occurrence_id);
    unit.ko = unit.en; reply.sync();
    assert.equal(decodeCommentarySelection(reply.raw, checking), undefined, `${section}: the checked position still needs its actual reply`);
  }
});

test('recorded ends allow only a visible finite final check to have no recorded reply', () => {
  const fen = '7k/8/8/8/8/8/8/R6K w - - 0 1';
  const observations: MoveReviewObservations = { statements: [], paths: [
    deliveryPath('played', fen, ['a1a8', 'h8h7', 'a8h8']),
    deliveryPath('alternative', fen, ['a1a7', 'h8g8']),
  ] };
  const { raw, summary } = questionResponse(observations);
  summary.schema_version = 2;
  assert.equal(decodeCommentarySelection(raw, observations), undefined, 'an unmarked final check has no recorded reply');
  for (const section of ['units', 'detail_units'] as const) {
    summary[section]![0].recorded_end_moves = [{ line_id: 'played', occurrence_id: 'played-2' }];
  }
  assert.ok(decodeCommentarySelection(raw, observations), 'the finite recorded end explicitly limits both visible sections');
  const changes: [string, (unit: CommentaryQuestionUnit) => void][] = [
    ['missing limit', unit => { delete unit.recorded_end_moves; }],
    ['foreign path', unit => { unit.recorded_end_moves![0].line_id = 'unknown'; }],
    ['foreign occurrence', unit => { unit.recorded_end_moves![0].occurrence_id = 'alternative-1'; }],
    ['hidden end', unit => { unit.recorded_end_moves = [{ line_id: 'alternative', occurrence_id: 'alternative-1' }]; }],
    ['earlier check', unit => { unit.recorded_end_moves![0].occurrence_id = 'played-0'; }],
    ['duplicate limit', unit => { unit.recorded_end_moves!.push(structuredClone(unit.recorded_end_moves![0])); }],
  ];
  for (const section of ['units', 'detail_units'] as const) for (const [name, mutate] of changes) {
    const changed = structuredClone(raw);
    mutate(changed.candidates[0].detail.summary_question[section]![0]);
    assert.equal(decodeCommentarySelection(changed, observations), undefined, `${section}: ${name}`);
  }
  for (const mutate of [
    (ending: ObservedLineSummary['ending']) => { ending.ply_offset = 0; },
    (ending: ObservedLineSummary['ending']) => { ending.occurrence_id = 'played-0'; },
  ]) {
    const changed = structuredClone(observations);
    mutate(changed.paths[0].lineSummary!.ending);
    assert.equal(decodeCommentarySelection(raw, changed), undefined, 'the declared finite boundary must match the actual last occurrence');
  }
});

test('answer units retain capture costs and actual recaptures in each visible section', () => {
  const fen = '3r3k/8/8/3p4/4P3/8/8/7K w - - 0 1';
  const observations: MoveReviewObservations = { statements: [], paths: [
    deliveryPath('played', fen, ['e4d5', 'd8d5']), deliveryPath('alternative', fen, ['e4e5', 'h8g8']),
  ] };
  const { raw, summary: initial } = questionResponse(observations, 'observed_exchange');
  initial.schema_version = 2;
  assert.ok(decodeCommentarySelection(raw, observations));
  for (const section of ['units', 'detail_units'] as const) {
    const missing = structuredClone(raw), unit = missing.candidates[0].detail.summary_question[section]![0];
    unit.covered_moves.pop();
    assert.equal(decodeCommentarySelection(missing, observations), undefined, `${section}: visible capture cannot lose its cost`);
    const duplicate = structuredClone(raw), covered = duplicate.candidates[0].detail.summary_question[section]![0].covered_moves;
    covered.push(structuredClone(covered[0]));
    assert.equal(decodeCommentarySelection(duplicate, observations), undefined, `${section}: one Unit cannot declare a capture twice`);
    const repeated = questionResponse(observations, 'observed_exchange');
    repeated.summary.schema_version = 2;
    repeated.summary[section]!.push(structuredClone(repeated.summary[section]![0]));
    repeated.sync();
    assert.ok(decodeCommentarySelection(repeated.raw, observations), `${section}: distinct Units can reference the same closed capture cost`);
  }
  const { raw: truncated, summary, sync } = questionResponse(observations, 'observed_exchange');
  const unit = summary.units[0];
  unit.en = ['The pawn is taken with ', unit.required_moves![0], '.']; unit.ko = unit.en;
  unit.required_moves!.pop(); unit.covered_moves.pop(); sync();
  assert.equal(decodeCommentarySelection(truncated, observations), undefined, 'the immediate actual recapture must remain visible');
  const falseCost = structuredClone(raw), other = falseCost.candidates[0].detail.summary_question.units[1];
  other.role = 'cost'; other.covered_moves = [other.required_moves![0]];
  assert.equal(decodeCommentarySelection(falseCost, observations), undefined, 'a quiet move is not a covered capture');
  const priorFen = '7k/8/8/3p4/4P3/5P2/8/K7 w - - 0 1';
  const priorCapture: MoveReviewObservations = { statements: [], paths: [
    deliveryPath('played', priorFen, ['a1a2', 'd5e4', 'f3e4']),
    deliveryPath('alternative', priorFen, ['f3f4', 'h8h7']),
  ] };
  for (const section of ['units', 'detail_units'] as const) {
    const answer = questionResponse(priorCapture, 'observed_exchange');
    answer.summary.schema_version = 2;
    assert.ok(decodeCommentarySelection(answer.raw, priorCapture));
    const unit = answer.summary[section]![0], omitted = 'played-1';
    unit.en = unit.en.filter(part => typeof part === 'string' || part.occurrence_id !== omitted);
    unit.ko = unit.en;
    unit.required_moves = unit.required_moves!.filter(ref => ref.occurrence_id !== omitted);
    unit.covered_moves = unit.covered_moves.filter(ref => ref.occurrence_id !== omitted);
    answer.sync();
    assert.equal(decodeCommentarySelection(answer.raw, priorCapture), undefined,
      `${section}: a visible proven recapture must retain the capture immediately before it`);
  }
});

test('answer units allow certified terminal moves and promotions without weakening legacy motifs', () => {
  const mateFen = '7k/5K2/6Q1/8/8/8/8/8 w - - 0 1';
  const observations: MoveReviewObservations = { statements: [], paths: [
    deliveryPath('played', mateFen, ['g6g7']), deliveryPath('alternative', mateFen, ['g6g5']),
  ] };
  const { raw } = questionResponse(observations, 'observed_terminal_comparison');
  assert.equal(observations.paths[0].moves[0].label, 'Qg7#');
  assert.ok(decodeCommentarySelection(raw, observations));
  for (const section of ['units', 'detail_units'] as const) {
    const finite = structuredClone(raw);
    finite.candidates[0].detail.summary_question[section]![0].recorded_end_moves = [{ line_id: 'played', occurrence_id: 'played-0' }];
    assert.equal(decodeCommentarySelection(finite, observations), undefined, 'an automatic ending cannot be marked as merely unrecorded');
  }
  for (const mutate of [
    (path: typeof observations.paths[number]) => { path.lineSummary!.ending.kind = 'line_end'; },
    (path: typeof observations.paths[number]) => { path.lineSummary!.ending.occurrence_id = 'foreign'; },
    (path: typeof observations.paths[number]) => { path.lineSummary!.ending.ply_offset = 1; },
  ]) {
    const changed = structuredClone(observations); mutate(changed.paths[0]);
    assert.equal(decodeCommentarySelection(raw, changed), undefined);
  }
  const unowned = structuredClone(raw); unowned.candidates[0].source_refs[0].accepted_ply_count++;
  assert.equal(decodeCommentarySelection(unowned, observations), undefined);
  const legacy = structuredClone(raw); legacy.candidates[0].detail.summary_question.question = 'provider_transfer';
  assert.equal(decodeCommentarySelection(legacy, observations), undefined);
  legacy.candidates[0].detail.summary_question.schema_version = 2;
  assert.ok(decodeCommentarySelection(legacy, observations), 'a v2 question label does not suppress a certified ending');
  const promotionFen = '7k/P7/8/8/8/8/8/7K w - - 0 1';
  const promotion: MoveReviewObservations = { statements: [], paths: [
    deliveryPath('played', promotionFen, ['a7a8q', 'h8h7']), deliveryPath('alternative', promotionFen, ['a7a8n']),
  ] };
  const promoted = questionResponse(promotion);
  assert.ok(decodeCommentarySelection(promoted.raw, promotion));
  promoted.summary.question = 'provider_transfer';
  assert.equal(decodeCommentarySelection(promoted.raw, promotion), undefined);
  promoted.summary.schema_version = 2;
  assert.ok(decodeCommentarySelection(promoted.raw, promotion), 'v2 retains physical promotions for every selected question');
});

test('a structured terminal answer alone supplies its basic and expanded paragraphs', () => {
  const fen = '7k/5K2/6Q1/8/8/8/8/8 w - - 0 1';
  const observations: MoveReviewObservations = { statements: [], paths: [
    deliveryPath('played', fen, ['g6g7']), deliveryPath('alternative', fen, ['g6g5']),
  ] };
  const { raw, summary, sync } = questionResponse(observations, 'observed_terminal_comparison');
  const ending = summary.units[0].required_moves![0];
  summary.units[0].en = [ending, ' ends the line in checkmate.'];
  summary.units[0].ko = summary.units[0].en;
  sync();
  observations.selection = decodeCommentarySelection(raw, observations)!;
  assert.ok(observations.selection);
  observations.selection.scenes = [{ id: 'ending', rootOccurrenceId: ending.occurrence_id,
    occurrenceIds: observations.paths.flatMap(path => path.occurrenceIds),
    lineIds: observations.paths.map(path => path.id), candidateIds: ['primary'] }];
  const props = panelProps([]);
  props.job.observations = observations;
  props.actions.pinFrame = frame => { props.view.pinnedFrame = frame; };
  const card = nodes(renderMoveReview(props)).find(node => node.sel === 'article.move-review__scene')!;
  assert.ok(card);
  const basic = (card.children ?? []).filter((node): node is VNode => typeof node === 'object' && !!node &&
    (node.sel === 'p' || !!node.sel?.startsWith('p.')));
  assert.equal(basic.length, summary.units.length, 'the renderer adds no independently composed terminal paragraph');
  assert.equal(terminalParagraphs(card).length, 0);
  assert.equal(basic.map(text).join(' '), raw.candidates[0].detail.summary);
  const detail = nodes(card).find(node => node.sel === 'details.move-review__reading-details')!;
  assert.equal(nodes(detail).filter(node => node.sel === 'p.move-review__commentary').length, summary.detail_units!.length);
  const link = nodes(basic[0]).find(node => node.sel === 'button.move-review__inline-san')!;
  assert.ok(link, 'the terminal remains linked to its exact move');
  trigger(link, 'click');
  assert.equal(props.view.pinnedFrame?.proofId, ending.line_id);
  assert.equal(props.view.pinnedFrame?.ply, 1);
});

test('an undelivered selected answer stays explicit and preserves its raw records only in collapsed details', () => {
  const fen = '7k/5K2/6Q1/8/8/8/8/8 w - - 0 1';
  const observations: MoveReviewObservations = { statements: [], paths: [
    deliveryPath('played', fen, ['g6g7']), deliveryPath('alternative', fen, ['g6g5']),
  ] };
  const { raw } = questionResponse(observations, 'observed_terminal_comparison');
  const detail: Record<string, unknown> = raw.candidates[0].detail;
  for (const key of ['summary', 'summary_kind', 'summary_question']) delete detail[key];
  const unavailable = { question: 'observed_terminal_comparison', reason: 'answer_not_delivered' } as const;
  detail.summary_unavailable = unavailable;
  const records = [
    'Stored analysis: Qg7# is the last move recorded for the played line, and its original account remains available here.',
    'Stored alternative analysis: Qg5 is retained with the comparison record while the selected explanation has not been delivered.',
  ];
  detail.text = records.join('\n\n');
  const selection = decodeCommentarySelection(raw, observations)!;
  assert.ok(selection);
  assert.deepEqual(selection.candidates[0].detail.summary_unavailable, unavailable);
  assert.equal(selection.candidates[0].detail.summary_question, undefined);
  const changes: [string, (detail: Record<string, unknown>) => void][] = [
    ['delivered summary', detail => { detail.summary = 'A delivered answer.'; }],
    ['delivered kind', detail => { detail.summary_kind = 'observed_convergence'; }],
    ['delivered question', detail => { detail.summary_question = {}; }],
    ['delivered exchange', detail => { detail.summary_exchange = {}; }],
    ['wrong reason', detail => { detail.summary_unavailable = { ...unavailable, reason: 'unknown' }; }],
    ['empty question', detail => { detail.summary_unavailable = { ...unavailable, question: '' }; }],
    ['blank question', detail => { detail.summary_unavailable = { ...unavailable, question: '  ' }; }],
    ['extra field', detail => { detail.summary_unavailable = { ...unavailable, answer: 'complete' }; }],
    ['null metadata', detail => { detail.summary_unavailable = null; }],
  ];
  for (const [name, mutate] of changes) {
    const changed = structuredClone(raw);
    mutate(changed.candidates[0].detail);
    assert.equal(decodeCommentarySelection(changed, observations), undefined, name);
  }
  selection.scenes = [{ id: 'undelivered', rootOccurrenceId: 'played-0', occurrenceIds: ['played-0', 'alternative-0'],
    lineIds: ['played', 'alternative'], candidateIds: ['primary'] }];
  for (const merged of [false, true]) for (const locale of ['en-US', 'ko-KR'] as const) {
    const props = panelProps([], locale), current = structuredClone(observations);
    current.selection = structuredClone(selection);
    const otherRecord = 'This separate PV record retains Qg5.';
    if (merged) {
      const reading = pv();
      reading.rootOccurrenceId = 'played-0'; reading.occurrenceIds = ['played-0', 'alternative-0'];
      reading.detail.text = otherRecord;
      reading.detail.effects = [{ scope: 'line_history', line_ids: ['alternative'], prefix_moves_uci: [], through_plies: [1],
        actor: { position_id: 'before', side: 'white', role: 'queen', square: 'g6' }, target: null, square: 'g5',
        occurrence_ids: ['alternative-0'], evidence_ids: [] }];
      const answer = (reading.detail as CommentaryPvComparisonDetail).answer;
      answer.reference_occurrence_id = 'played-0'; answer.alternative_occurrence_id = 'alternative-0';
      current.selection.candidates.push(reading);
    }
    props.job.observations = current;
    props.actions.previewFrame = frame => { props.view.hoveredFrame = frame; };
    props.actions.pinFrame = frame => { props.view.pinnedFrame = frame; };
    const card = nodes(renderMoveReview(props)).find(node => node.sel ===
      (merged ? 'article.move-review__pv-reading' : 'article.move-review__scene'))!;
    assert.ok(card);
    const children = (card.children ?? []).filter((node): node is VNode => typeof node === 'object' && !!node);
    const status = children.find(node => node.data?.attrs?.['data-summary-state'] === 'unavailable')!;
    assert.ok(status && text(status).length > 0 && text(status).length < 100);
    assert.equal(status.data?.attrs?.['data-question'], unavailable.question);
    assert.equal(status.data?.attrs?.lang, locale);
    assert.equal(terminalParagraphs(card).length, 0, 'an independent terminal paragraph cannot hide answer delivery failure');
    const basic = children.filter(node => !node.sel?.startsWith('details.')).map(text).join(' ');
    const expanded = nodes(card).find(node => node.sel === 'details.move-review__reading-details')!;
    assert.ok(expanded && !expanded.data?.attrs?.open);
    for (const record of records) {
      assert.ok(!basic.includes(record), 'raw records do not return as the basic explanation');
      assert.ok(text(expanded).includes(record), 'the exact original record remains available');
    }
    if (merged) {
      const history = nodes(card).find(node => node.sel === 'details.move-review__additional-explanations')!;
      assert.ok(history && !history.data?.attrs?.open && text(history).includes(otherRecord));
      assert.ok(!text(expanded).includes(otherRecord));
    }
    const move = nodes(expanded).find(node => node.sel === 'button.move-review__inline-san' && text(node) === 'Qg7#')!;
    trigger(move, 'focus');
    assert.equal(props.view.hoveredFrame?.proofId, 'played');
    trigger(move, 'click');
    assert.equal(props.view.pinnedFrame?.proofId, 'played');
    assert.equal(props.view.pinnedFrame?.ply, 1);
  }
});

test('answer units drive basic and expanded prose with only their assigned SAN focus', () => {
  const observations: MoveReviewObservations = { statements: [], paths: [
    deliveryPath('played', startFen, ['d2d3', 'h7h6']), deliveryPath('alternative', startFen, ['e2e3', 'h7h6']),
  ] };
  const { raw, summary } = questionResponse(observations);
  const highlight: NonNullable<MoveReviewObservations['highlights']>[number] = {
    occurrenceId: 'played-1', text: 'The bishop controls h6.', squares: ['c1', 'h6'], premiseIds: ['material-played'],
    frames: [{ phase: 'after', positionId: 'played-position-1', squares: ['c1', 'h6'], sourceIds: ['bishop-h6'] }],
  };
  observations.highlights = [highlight];
  observations.positions!.find(position => position.id === 'played-position-1')!.controls.pieces = [
    { piece: { side: 'white', role: 'bishop', square: 'c1' }, squares: ['h6', 'e3'], source_ids: ['bishop-h6', 'bishop-e3'] },
  ];
  summary.units[0].focus = [{ line_id: 'played', occurrence_id: 'played-1', phase: 'after',
    position_id: 'played-position-1', squares: ['c1', 'h6'], source_ids: ['bishop-h6'] }];
  summary.detail_units![0].focus = structuredClone(summary.units[0].focus);
  summary.detail_units![0].en[0] = 'Expanded explanation: ';
  summary.detail_units![0].ko = summary.detail_units![0].en;
  observations.selection = decodeCommentarySelection(raw, observations)!;
  assert.ok(observations.selection);
  observations.selection.scenes = [{ id: 'answer', rootOccurrenceId: 'played-0', occurrenceIds: ['played-0', 'alternative-0'],
    lineIds: ['played', 'alternative'], candidateIds: ['primary'] }];
  const secondary = question(), reading = pv();
  secondary.rootOccurrenceId = reading.rootOccurrenceId = 'played-0';
  secondary.occurrenceIds = reading.occurrenceIds = ['played-0', 'alternative-0'];
  secondary.detail.text = 'In the provided d3 line, h6 belongs to a separate question.';
  reading.detail.text = 'In the provided e3 line, h6 belongs to a separate PV record.';
  reading.detail.effects = [{ scope: 'line_history', line_ids: ['alternative'], prefix_moves_uci: [], through_plies: [2],
    actor: { position_id: 'before', side: 'white', role: 'bishop', square: 'c1' }, target: null, square: 'h6',
    occurrence_ids: ['alternative-1'], evidence_ids: [] }];
  const answer = (reading.detail as CommentaryPvComparisonDetail).answer;
  answer.reference_occurrence_id = 'played-0'; answer.alternative_occurrence_id = 'alternative-0';
  observations.selection.candidates.push(secondary, reading);
  observations.selection.scenes.push({ id: 'secondary', rootOccurrenceId: 'played-0', occurrenceIds: ['played-0', 'alternative-0'],
    lineIds: ['played', 'alternative'], candidateIds: [secondary.id] });
  for (const locale of ['en-US', 'ko-KR'] as const) {
    const props = panelProps([], locale);
    props.job.observations = observations;
    props.actions.pinFrame = frame => { props.view.pinnedFrame = frame; delete props.view.hoveredFrame; };
    props.actions.inspectFrame = frame => { props.view.boardFrame = frame; };
    const view = renderMoveReview(props);
    const card = nodes(view).find(node => node.sel === 'article.move-review__pv-reading')!;
    const basic = (card.children ?? []).filter((node): node is VNode => typeof node === 'object' && !!node &&
      node.sel === 'p.move-review__commentary');
    assert.equal(basic.length, summary.units.length);
    assert.ok(basic.every(node => node.data?.attrs?.lang === 'en' && node.data?.attrs?.['data-no-translate'] === true));
    const detail = nodes(card).find(node => node.sel === 'details.move-review__reading-details')!;
    assert.ok(detail && !detail.data?.attrs?.open);
    assert.ok(text(detail).includes('Expanded explanation: d3 h6.'));
    assert.ok(!text(card).includes('Legacy diagnostic prose.'));
    assert.ok(!text(detail).includes(secondary.detail.text as string), 'another question does not become this answer’s expanded prose');
    assert.ok(!text(detail).includes(reading.detail.text as string));
    const records = nodes(card).find(node => node.sel === 'details.move-review__comparison-records')!;
    const history = nodes(records).find(node => node.sel === 'details.move-review__additional-explanations')!;
    assert.ok(history && !history.data?.attrs?.open, 'the merged questions remain available as separate collapsed records');
    assert.ok(text(history).includes(secondary.detail.text as string));
    assert.ok(text(history).includes(reading.detail.text as string));
    assert.equal(text(card).split(secondary.detail.text as string).length - 1, 1);
    assert.equal(text(card).split(reading.detail.text as string).length - 1, 1);
    const assigned = nodes(basic[0]).find(node => node.sel === 'button.move-review__inline-san' && text(node) === 'h6')!;
    trigger(assigned, 'click');
    assert.equal(props.view.pinnedFrame?.highlightKey, JSON.stringify(highlight));
    assert.equal(props.view.pinnedFrame?.phase, 'after');
    const preview = nodes(renderMoveReview(props)).find(node => node.sel === 'section.move-review__comparison-preview')!;
    trigger(nodes(preview).find(node => node.sel?.includes('move-review__inspect'))!, 'click');
    assert.deepEqual(boardConfig(renderMoveReviewBoard(props)!.board).drawable.autoShapes,
      [{ orig: 'c1', brush: 'blue' }, { orig: 'h6', brush: 'blue' }, { orig: 'c1', dest: 'h6', brush: 'blue' }]);
    const unassigned = nodes(basic[1]).find(node => node.sel === 'button.move-review__inline-san' && text(node) === 'h6')!;
    trigger(unassigned, 'click');
    assert.equal(props.view.pinnedFrame?.proofId, 'alternative');
    assert.equal(props.view.pinnedFrame?.highlightKey, undefined, 'identical SAN in the other line does not borrow focus');
    const detailLink = nodes(detail).find(node => node.sel === 'button.move-review__inline-san' && text(node) === 'h6')!;
    trigger(detailLink, 'click');
    assert.equal(props.view.pinnedFrame?.highlightKey, JSON.stringify(highlight));
    assert.notEqual(detailLink.data?.attrs?.id, assigned.data?.attrs?.id);
    const historyLinks = nodes(history).filter(node => node.sel === 'button.move-review__inline-san' && text(node) === 'h6');
    trigger(historyLinks[0], 'click');
    assert.equal(props.view.pinnedFrame?.proofId, 'played');
    assert.equal(props.view.pinnedFrame?.highlightKey, undefined);
    trigger(historyLinks[1], 'click');
    assert.equal(props.view.pinnedFrame?.proofId, 'alternative');
    assert.equal(props.view.pinnedFrame?.highlightKey, undefined, 'the separate record retains SAN ownership without borrowing the answer’s focus');
  }
});

test('review selection keeps exact source arrows in detailed and main boards', () => {
  const { props, frame } = linkedBoardProps();
  const originalSubject = structuredClone(props.job.subject);
  let inspected: MoveReviewFrameSelection | undefined;
  props.actions.inspectFrame = selection => { inspected = selection; };
  const detail = nodes(renderMoveReview(props)).find(node => node.sel === 'details.move-review__observations')!;
  const expected = [{ orig: 'c1', brush: 'blue' }, { orig: 'h6', brush: 'blue' },
    { orig: 'c1', dest: 'h6', brush: 'blue' }];
  assert.deepEqual(boardConfig(detail).drawable.autoShapes, expected);
  const inspect = nodes(detail).find(node => node.sel?.includes('move-review__inspect'))!;
  trigger(inspect, 'click');
  assert.deepEqual(inspected, frame);
  assert.deepEqual(boardConfig(renderMoveReviewBoard(props)!.board), boardConfig(detail));
  props.view.boardFrame = { proofId: 'played', ply: 2, phase: 'after' };
  assert.deepEqual(boardConfig(renderMoveReviewBoard(props)!.board).drawable.autoShapes,
    [{ orig: 'h7', dest: 'h6', brush: 'blue' }], 'plain SAN does not imply any relation highlight');
  assert.deepEqual(props.job.subject, originalSubject, 'inspection keeps the original game path and position');
});

test('review selection rejects stale owners and preserves statement phase', () => {
  const { props, frame, highlight } = linkedBoardProps();
  for (const invalid of [
    { ...frame, proofId: 'foreign' }, { ...frame, ply: 1 }, { ...frame, ply: 1.5 },
    { ...frame, highlightKey: JSON.stringify({ ...highlight, text: 'changed' }) },
    { ...frame, phase: undefined }, { ...frame, observationKey: 'foreign' },
  ]) {
    props.view.boardFrame = invalid;
    assert.equal(renderMoveReviewBoard(props), undefined);
  }
  const statement: MoveReviewObservation = {
    occurrenceId: 'reply', side: 'black', role: 'pawn', from: 'h7', to: 'h6', premiseIds: [],
    kind: 'piece_control', relation: 'attack',
    before: { controller: { side: 'white', role: 'bishop', square: 'c1' },
      target: { side: 'black', role: 'pawn', square: 'h7' }, state: 'absent', premiseIds: [] },
    after: { controller: { side: 'white', role: 'bishop', square: 'c1' },
      target: { side: 'black', role: 'pawn', square: 'h6' }, state: 'present', premiseIds: [] },
  };
  props.job.observations!.statements = [statement];
  const observationKey = JSON.stringify(['reply', 'piece_control', 'attack', statement.before.controller,
    statement.before.target, 0]);
  props.view.boardFrame = { proofId: 'played', ply: 2, phase: 'after', observationKey };
  assert.deepEqual(boardConfig(renderMoveReviewBoard(props)!.board).drawable.autoShapes,
    [{ orig: 'c1', dest: 'h6', brush: 'red' }, { orig: 'h6', brush: 'red' }]);
  props.view.boardFrame.phase = 'before';
  const before = boardConfig(renderMoveReviewBoard(props)!.board);
  assert.equal(before.fen, playedFen);
  assert.deepEqual(before.drawable.autoShapes, []);
});

test('a replacement runtime owner clears inspection even when the subject and frame IDs are reused', () => {
  const owner = { requestId: 'request-before', jobId: 'job-before' };
  for (const replacement of [
    { ...owner }, { ...owner, requestId: 'request-after' }, { ...owner, jobId: 'job-after' }, undefined,
  ]) {
    const { props, frame } = linkedBoardProps();
    props.job.observations!.owner = owner;
    props.view.hoveredFrame = props.view.pinnedFrame = props.view.boardFrame = frame;
    props.view.evidenceExpanded = true;
    const next = structuredClone(props.job);
    next.observations!.owner = replacement;
    const previous = props.job;
    assert.ok(renderMoveReviewBoard(props));
    updateMoveReviewViewForJob(props.view, previous, next);
    props.job = next;
    if (replacement?.requestId === owner.requestId && replacement?.jobId === owner.jobId) {
      assert.ok(renderMoveReviewBoard(props), 'progress within the same runtime job preserves inspection');
      assert.deepEqual(props.view.hoveredFrame, frame);
      assert.deepEqual(props.view.pinnedFrame, frame);
    } else {
      assert.equal(renderMoveReviewBoard(props), undefined, 'an old frame cannot alias the replacement job');
      assert.equal(props.view.hoveredFrame, undefined);
      assert.equal(props.view.pinnedFrame, undefined);
    }
    assert.equal(props.view.evidenceExpanded, true, 'the owner change only resets transient board selections');
  }
});

test('review selection opens SAN on focus and transfers its chosen preview unchanged', () => {
  const { props, frame } = linkedBoardProps();
  props.job.observations!.selection!.candidates[0].detail.text = 'The supplied d3 line continues with h6.';
  delete props.view.pinnedFrame;
  delete props.view.boardFrame;
  let preview: MoveReviewFrameSelection | undefined, inspected: MoveReviewFrameSelection | undefined;
  let clears = 0;
  props.actions.previewFrame = selection => { preview = selection; };
  props.actions.inspectFrame = selection => { inspected = selection; };
  props.actions.clearPreview = () => clears++;
  props.actions.clearPin = () => clears++;
  const link = nodes(renderMoveReview(props)).find(node => node.sel === 'button.move-review__inline-san' && text(node) === 'h6')!;
  trigger(link, 'focus');
  assert.equal(preview?.proofId, 'played');
  assert.equal(preview?.ply, 2);
  assert.equal(preview?.highlightKey, undefined, 'generic SAN only owns its occurrence');
  props.view.hoveredFrame = preview;
  const peek = nodes(renderMoveReview(props)).find(node => node.sel === 'section.move-review__comparison-preview')!;
  assert.ok(peek.data?.class?.['move-review__text-preview--peek']);
  const escape = new dom.window.KeyboardEvent('keydown', { key: 'Escape', cancelable: true });
  trigger(link, 'keydown', escape);
  assert.equal(escape.defaultPrevented, true);
  assert.equal(clears, 2);
  delete props.view.hoveredFrame;
  props.view.pinnedFrame = { ...frame, textAnchor: preview!.textAnchor };
  const view = nodes(renderMoveReview(props)).find(node => node.sel === 'section.move-review__comparison-preview')!;
  const expected = boardConfig(view);
  let pinned: MoveReviewFrameSelection | undefined;
  props.actions.pinFrame = selection => { pinned = selection; };
  const before = nodes(view).find(node => node.key === 'before')!;
  trigger(before, 'click');
  assert.deepEqual(pinned, { ...props.view.pinnedFrame, phase: 'before' });
  const previous = nodes(view).find(node => node.key === -1)!;
  const move = new Event('click');
  Object.defineProperty(move, 'currentTarget', { value: document.createElement('button') });
  trigger(previous, 'click', move);
  assert.deepEqual(pinned, { proofId: 'played', ply: 1, phase: 'after', textAnchor: preview!.textAnchor });
  trigger(nodes(view).find(node => node.sel?.includes('move-review__inspect'))!, 'click');
  assert.deepEqual(inspected, props.view.pinnedFrame);
  assert.equal(clears, 4);
  props.view.boardFrame = inspected;
  assert.deepEqual(boardConfig(renderMoveReviewBoard(props)!.board), expected);
});

test('review selection clears relation and popup state on movement and Escape', () => {
  const { props, frame } = linkedBoardProps();
  const originalSubject = structuredClone(props.job.subject);
  props.view.boardFrame = { ...frame, textAnchor: 'review-origin' };
  const origin = document.createElement('button');
  origin.id = 'review-origin';
  document.body.append(origin);
  const cleared: string[] = [];
  let inspected: MoveReviewFrameSelection | undefined;
  props.actions.clearPreview = () => cleared.push('hover');
  props.actions.clearPin = () => cleared.push('pin');
  props.actions.clearInspection = () => cleared.push('board');
  props.actions.inspectFrame = selection => { inspected = selection; };
  try {
    const main = renderMoveReviewBoard(props)!;
    const previous = nodes(main.notice).find(node => node.data?.attrs?.['aria-label'] === 'Previous move')!;
    trigger(previous, 'click');
    assert.deepEqual(inspected, { proofId: 'played', ply: 1, phase: 'after', textAnchor: 'review-origin' });
    assert.deepEqual(cleared, ['hover', 'pin']);
    cleared.length = 0;
    props.view.boardFrame = inspected;
    const next = nodes(renderMoveReviewBoard(props)!.notice).find(node => node.data?.attrs?.['aria-label'] === 'Next move')!;
    trigger(next, 'click');
    assert.deepEqual(inspected, { proofId: 'played', ply: 2, phase: 'after', textAnchor: 'review-origin' });
    assert.deepEqual(cleared, ['hover', 'pin']);
    cleared.length = 0;
    const escape = new dom.window.KeyboardEvent('keydown', { key: 'Escape', cancelable: true });
    trigger(main.notice, 'keydown', escape);
    assert.equal(escape.defaultPrevented, true);
    assert.deepEqual(cleared, ['hover', 'pin', 'board']);
    assert.equal(document.activeElement, origin);
    cleared.length = 0;
    trigger(nodes(main.notice).find(node => text(node) === 'Return to game')!, 'click');
    assert.deepEqual(cleared, ['hover', 'pin', 'board']);
    assert.deepEqual(props.job.subject, originalSubject);
  } finally {
    origin.remove();
  }
});

test('equal material counts retain the explanation and available scene navigation', () => {
  const candidate = primary();
  const recordReview = {
    status: 'finished',
    total: 2,
    assessed: 2,
    completed: 2,
    stopped: 0,
    scenes: [{ path: 'ab', label: 'd3' }, { path: 'abcd', label: 'd5' }],
  } as const;
  let jumped: string | undefined;
  const view = render([candidate], 'en-US', { ...recordReview, scenes: [...recordReview.scenes] }, {
    jumpToScene: path => { jumped = path; },
  });
  const scene = nodes(view).find(node => node.sel === 'article.move-review__scene')!;
  assert.ok(text(scene).includes(explanation));
  assert.ok(text(scene).includes('Comparison of the two choices'));
  assert.ok(!text(scene).includes('needs explanation'));
  assert.ok(!text(scene).includes('incomplete'));
  const next = nodes(view).find(node => node.sel?.startsWith('button') && text(node).trim() === 'Next moment')!;
  assert.ok(next);
  assert.equal(next.data?.attrs?.disabled, false);
  const jump = next.data?.on?.click;
  assert.equal(typeof jump, 'function');
  if (typeof jump === 'function') jump(new Event('click'), next);
  assert.equal(jumped, 'abcd');
  assert.ok(text(view).includes('2 of 2 moves assessed · 2 analyses completed'));
  assert.ok(!text(view).includes('Some comparisons still need explanatory evidence.'));
  const stopped = render([candidate], 'en-US', {
    ...recordReview,
    scenes: [...recordReview.scenes],
    status: 'blocked',
    completed: 0,
    stopped: 1,
  });
  assert.ok(text(stopped).includes('Game review stopped because analysis could not continue.'));
});

for (const relation of ['same', 'unavailable', 'different'] as const)
  test(`a merged PV card with ${relation} direct-mover observations shows the primary explanation once`, () => {
    const candidates = [primary(), pv(relation)];
    const view = render(candidates);
    const all = nodes(view);
    assert.equal(all.filter(node => node.sel === 'article.move-review__pv-reading').length, 1);
    const card = all.find(node => node.sel === 'article.move-review__pv-reading')!;
    const children = (card.children ?? []).filter((node): node is VNode => typeof node === 'object' && !!node);
    assert.equal(text(children.find(node => node.sel === 'p' || node.sel?.startsWith('p.'))!), explanation);
    assert.ok(children.findIndex(node => node.sel === 'p') < children.findIndex(node => node.sel === 'p.move-review__pv-quality'));
    assert.ok(children.findIndex(node => node.sel === 'p') <
      children.findIndex(node => node.sel === 'details.move-review__comparison-records'));
    assert.equal(all.filter(node => node.sel === 'article.move-review__scene').length, 0);
    assert.equal(text(view).split(explanation).length - 1, 1);
    assert.ok(!text(view).includes(pvExplanation));
    if (relation === 'same')
      assert.ok(text(view).includes('Effects on other pieces and later play can still differ.'));
    assert.ok(!text(view).includes('The observed facts do not yet fully explain'));
    assert.ok(!text(view).includes('No difference has been established'));
  });

test('a PV card uses its own explanation when the other comparison belongs to another occurrence', () => {
  const unrelated = { ...primary(), rootOccurrenceId: 'another-root' };
  const view = render([unrelated, pv()], 'ko-KR');
  assert.ok(text(view).includes(pvExplanation));
  assert.ok(!text(view).includes(explanation));
  assert.ok(!text(view).includes('아직 충분히 설명하지 못했습니다'));
  assert.ok(!text(view).includes('차이를 아직 찾지 못했습니다'));
});

test('an exact question pair joins its PV card without replacing the root or question meaning', () => {
  const comparison = question();
  comparison.lineIds.reverse();
  comparison.detail.reference_move = 'e2e3';
  comparison.detail.alternative_move = 'd2d3';
  const view = render([primary(), comparison, pv()]);
  assert.equal(nodes(view).filter(node => node.sel === 'article.move-review__pv-reading').length, 1);
  assert.equal(nodes(view).filter(node => node.sel === 'article.move-review__scene').length, 0);
  assert.equal(text(view).split(explanation).length - 1, 1);
  assert.equal(text(view).split(comparison.detail.text as string).length - 1, 1);
  assert.ok(!text(view).includes(pvExplanation));
});

test('a question from a later prefix keeps its separate scene', () => {
  const comparison = question();
  comparison.detail.prefix_moves_uci = ['d2d3'];
  comparison.detail.reference_move = 'h7h6';
  comparison.detail.alternative_move = 'g7g6';
  const view = render([comparison, pv()]);
  assert.equal(nodes(view).filter(node => node.sel === 'article.move-review__pv-reading').length, 1);
  assert.equal(nodes(view).filter(node => node.sel === 'article.move-review__scene').length, 1);
  assert.ok(text(view).includes(comparison.detail.text as string));
  assert.ok(text(view).includes(pvExplanation));
  assert.deepEqual(nodes(view).filter(node =>
    node.sel === 'article.move-review__pv-reading' || node.sel === 'article.move-review__scene')
    .map(node => node.sel), ['article.move-review__pv-reading', 'article.move-review__scene']);
});

test('an answered context question is not promoted into the visible PV comparison', () => {
  const comparison = question();
  comparison.selection = 'context';
  const view = render([primary(), comparison, pv()]);
  assert.equal(nodes(view).filter(node => node.sel === 'article.move-review__pv-reading').length, 1);
  assert.equal(text(view).split(explanation).length - 1, 1);
  assert.ok(!text(view).includes(comparison.detail.text as string));
});

test('completed grouped comparisons retain their exact work, prefix, and bounded move pool', () => {
  const origin = { kind: 'generator', producer: 'inline', source_refs: [] } as const;
  const proposals: CommentaryProposal[] = [{
    id: 'proposal', admission: 'generated',
    origin: Object.fromEntries(['question', 'candidates', 'priority', 'evidence']
      .map(key => [key, { ...origin, source_refs: [] }])) as CommentaryProposal['origin'],
    requirements: ['h7h5', 'g8f6'].map((alternative, index) => ({
      id: `pair-${index}`, kind: 'compare_moves', state: 'answered', stop_reason: null,
      prefix_moves_uci: ['d2d3'], reference_move: 'h7h6', alternative_move: alternative,
      work_id: 'work:2', line_id: null, occurrence_id: null, position_id: null,
      answer: { ...(pv().detail as CommentaryPvComparisonDetail).answer, work_id: 'work:2' },
    })),
  }];
  const search: CommentaryResponseSearch = {
    work_id: 'work:2', execution_key_sha256: 'a'.repeat(64), engine_profile: 'inline',
    required_depth: 8, multi_pv: 3, root_move: 'd2d3', root_move_occurrence_id: 'root',
    child_position_id: 'after', prefix_moves_uci: ['d2d3'], ply_offset: 1,
    root_restriction: { kind: 'restricted', moves_uci: ['h7h6', 'h7h5', 'g8f6'] },
    candidates: [],
  };
  // Answer and path validation precedes this ownership check in the full decoder.
  assert.equal(proposalContainsResponsePool(proposals, search), true);
  const rejects = (mutate: (proposal: CommentaryProposal, search: CommentaryResponseSearch) => void) => {
    const changed = structuredClone(proposals), receipt = structuredClone(search);
    mutate(changed[0], receipt);
    assert.equal(proposalContainsResponsePool(changed, receipt), false);
  };
  rejects((_, receipt) => {
    if (receipt.root_restriction.kind === 'restricted') receipt.root_restriction.moves_uci.push('b8c6');
  });
  rejects((_, receipt) => {
    if (receipt.root_restriction.kind === 'restricted') receipt.root_restriction.moves_uci.pop();
  });
  rejects(proposal => {
    const pair = proposal.requirements[1];
    if (pair.kind === 'compare_moves') pair.prefix_moves_uci = ['e2e3'];
  });
  rejects(proposal => { proposal.requirements[1].work_id = 'work:3'; });
  rejects(proposal => { proposal.requirements[1].answer!.work_id = 'work:3'; });
  rejects(proposal => { proposal.requirements[1].state = 'stopped'; });
  rejects(proposal => { proposal.requirements[1].stop_reason = 'search_budget_exhausted'; });
  rejects(proposal => { proposal.requirements[1].state = 'awaiting_engine'; });
  const prior = structuredClone(search);
  prior.work_id = 'work:1';
  prior.multi_pv = 2;
  prior.root_restriction = { kind: 'restricted', moves_uci: ['h7h6', 'h7h5'] };
  assert.equal(proposalContainsResponsePool(proposals, prior), true,
    'an independently validated old pair remains owned after a deeper question reopens');
  const retired = structuredClone(proposals);
  retired[0].requirements.forEach(reading => {
    reading.state = 'stopped';
    reading.stop_reason = 'prerequisite_unresolved';
    reading.answer = null;
    reading.work_id = null;
  });
  assert.equal(proposalContainsResponsePool(retired, search), false,
    'the union of current questions alone cannot certify an old three-move work');
  const issued = structuredClone(search);
  issued.proposal_ownership = {
    work_id: search.work_id, execution_key_sha256: search.execution_key_sha256,
    question_refs: retired[0].requirements.map(reading => ({ proposal_id: 'proposal', requirement_id: reading.id })),
  };
  assert.equal(proposalContainsResponsePool(retired, issued), true);
  const rejectsRetired = (mutate: (proposals: CommentaryProposal[], receipt: CommentaryResponseSearch) => void) => {
    const changed = structuredClone(retired), receipt = structuredClone(issued);
    mutate(changed, receipt);
    assert.equal(proposalContainsResponsePool(changed, receipt), false);
  };
  rejectsRetired((_, receipt) => { receipt.work_id = 'work:3'; });
  rejectsRetired((_, receipt) => { receipt.execution_key_sha256 = 'b'.repeat(64); });
  rejectsRetired((_, receipt) => { receipt.proposal_ownership!.question_refs.pop(); });
  rejectsRetired((_, receipt) => { receipt.proposal_ownership!.question_refs[0].proposal_id = 'other'; });
  rejectsRetired((_, receipt) => { receipt.proposal_ownership!.question_refs[0].requirement_id = 'other'; });
  rejectsRetired((_, receipt) => { receipt.proposal_ownership!.question_refs.push(receipt.proposal_ownership!.question_refs[0]); });
  rejectsRetired((_, receipt) => { receipt.prefix_moves_uci = ['e2e3']; });
  rejectsRetired((_, receipt) => {
    if (receipt.root_restriction.kind === 'restricted') receipt.root_restriction.moves_uci[2] = 'b8c6';
  });
  rejectsRetired((changed, _) => {
    const pair = changed[0].requirements[1];
    if (pair.kind === 'compare_moves') pair.reference_move = 'h7h5';
  });
  assert.ok(retired[0].requirements.every(reading => reading.state === 'stopped' && reading.answer === null));
});

test('a second pair retains its own card and distinct question meaning', () => {
  const comparison = question();
  comparison.lineIds = ['played', 'third'];
  comparison.detail.alternative_move = 'g1f3';
  const second = pv();
  second.id = 'second-pv';
  second.lineIds = ['played', 'third'];
  (second.detail.answer as CommentaryPvComparisonDetail['answer']).alternative_line_id = 'third';
  const view = render([primary(), comparison, pv(), second], 'en-US', undefined, {}, [{
    id: 'third',
    startFen,
    kind: 'analysis',
    occurrenceIds: ['third-root'],
    positionIds: ['before', 'third-after'],
    moves: [{ uci: 'g1f3', label: 'Nf3', fenAfter: 'rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1' }],
  }]);
  const cards = nodes(view).filter(node => node.sel === 'article.move-review__pv-reading');
  assert.equal(cards.length, 2);
  assert.ok(text(cards[0]).includes('d3 ↔ e3'));
  assert.ok(text(cards[0]).includes(explanation));
  assert.ok(!text(cards[0]).includes(comparison.detail.text as string));
  assert.ok(text(cards[1]).includes('d3 ↔ Nf3'));
  assert.ok(text(cards[1]).includes(comparison.detail.text as string));
  assert.equal(nodes(view).filter(node => node.sel === 'article.move-review__scene').length, 0);
});

test('a reversed exact pair shows its shared question and effect position once', () => {
  const comparison = question(), reading = pv();
  comparison.detail.effects = [{
    scope: 'line_history', line_ids: ['played'], prefix_moves_uci: [], through_plies: [2],
    actor: { square: 'c1', role: 'bishop', side: 'white', position_id: 'after' },
    target: null, square: 'h6', occurrence_ids: ['root', 'reply'], evidence_ids: [],
  }];
  const answer = reading.detail.answer as CommentaryPvComparisonDetail['answer'];
  answer.evaluation_relation = 'worse';
  answer.alternative_evaluation = { ...answer.alternative_evaluation, white_score: { kind: 'cp', value: 100 } };
  const pinned: unknown[] = [];
  const view = render([primary(), comparison, reading, reversedPv(reading)], 'en-US', undefined,
    { pinFrame: frame => pinned.push(frame) });
  const cards = nodes(view).filter(node => node.sel === 'article.move-review__pv-reading');
  assert.equal(cards.length, 1);
  assert.ok(text(cards[0]).includes('d3 ↔ e3'));
  assert.equal(text(view).split(comparison.detail.text as string).length - 1, 1);
  assert.equal(text(view).split(explanation).length - 1, 1);
  const links = nodes(cards[0]).filter(node => node.sel?.startsWith('button') &&
    text(node).includes('Position used in this explanation'));
  assert.equal(links.length, 1);
  const click = links[0].data?.on?.click;
  if (typeof click === 'function') click(new Event('click'), links[0]);
  assert.deepEqual(pinned, [{ proofId: 'played', ply: 2, phase: 'after' }]);
});

test('reversed pair consolidation preserves existing first-card order', () => {
  const view = render([question(), reversedPv(), pv()]);
  const cards = nodes(view).filter(node => node.sel === 'article.move-review__pv-reading');
  assert.equal(cards.length, 1);
  assert.ok(text(cards[0]).includes('e3 ↔ d3'));
});

test('an inquiry referencing the reversed pair retains a link to the displayed comparison', () => {
  const reversed = reversedPv();
  const view = render([question(), pv(), reversed], 'en-US', undefined, {}, [], [{
    id: 'inquiry', scope: 'root_choice', source: 'automatic_observation', root_occurrence_id: 'root',
    anchor_occurrence_ids: [], ordering_source_ids: [], position_id: 'before',
    reference_move: 'e2e3', alternative_move: 'd2d3', reasons: [], state: 'answered', stop_reason: null,
    answer: reversed.detail.answer as CommentaryPvComparisonDetail['answer'],
    explanation_candidate_ids: [reversed.id],
  }]);
  const inquiry = nodes(view).find(node => node.sel === 'article.move-review__inquiry')!;
  const links = nodes(inquiry).filter(node => node.sel === 'div.move-review__inquiry-reading-links');
  assert.equal(links.length, 1);
  assert.ok(text(links[0]).includes('d3 ↔ e3'));
  assert.equal(nodes(view).filter(node => node.sel === 'article.move-review__pv-reading').length, 1);
});

test('a reversed pair retains a distinct receipt, occurrence, effect, or continuation link', () => {
  const mutations: ((candidate: CommentaryCandidate) => void)[] = [
    candidate => { (candidate.detail.answer as CommentaryPvComparisonDetail['answer']).work_id = 'other-work'; },
    candidate => { (candidate.detail.answer as CommentaryPvComparisonDetail['answer']).reference_occurrence_id = 'other-root'; },
    candidate => { (candidate.detail.answer as CommentaryPvComparisonDetail['answer']).evaluation_relation = 'better'; },
    candidate => { candidate.detail.continuation_candidate_ids = ['question']; },
    candidate => { candidate.detail.effects = [{
      scope: 'line_history', line_ids: ['alternative'], prefix_moves_uci: [], through_plies: [2],
      actor: { square: 'c1', role: 'bishop', side: 'white', position_id: 'alternative-after' },
      target: null, square: 'h6', occurrence_ids: ['alternative-root', 'alternative-reply'], evidence_ids: [],
    }]; },
  ];
  for (const mutate of mutations) {
    const reversed = reversedPv();
    mutate(reversed);
    const view = render([primary(), question(), pv(), reversed]);
    const cards = nodes(view).filter(node => node.sel === 'article.move-review__pv-reading');
    assert.equal(cards.length, 2);
    if (reversed.detail.effects) {
      assert.ok(text(cards[1]).includes(pvExplanation));
      assert.equal(nodes(cards[1]).filter(node => node.sel?.startsWith('button') &&
        text(node).includes('Position used in this explanation')).length, 1);
    }
  }
});

test('a merged card keeps distinct PV histories and the exact position links from every explanation', () => {
  const root = primary(), comparison = question(), reading = pv();
  const effect = {
    scope: 'line_history',
    line_ids: ['played'],
    prefix_moves_uci: [],
    through_plies: [2],
    actor: { square: 'c1', role: 'bishop', side: 'white', position_id: 'after' },
    target: null,
    square: 'h6',
    occurrence_ids: ['root', 'reply'],
    evidence_ids: [],
  };
  root.detail.effects = [effect];
  comparison.detail.effects = [effect];
  reading.detail.effects = [{
    ...effect,
    line_ids: ['alternative'],
    occurrence_ids: ['alternative-root', 'alternative-reply'],
    actor: { ...effect.actor, position_id: 'alternative-after' },
  }];
  const pinned: unknown[] = [];
  const view = render([root, comparison, reading], 'en-US', undefined, { pinFrame: frame => pinned.push(frame) });
  assert.equal(nodes(view).filter(node => node.sel === 'article.move-review__pv-reading').length, 1);
  for (const expected of [explanation, comparison.detail.text as string, pvExplanation])
    assert.equal(text(view).split(expected).length - 1, 1);
  const links = nodes(view).filter(node => node.sel?.startsWith('button') &&
    text(node).includes('Position used in this explanation'));
  assert.equal(links.length, 2);
  for (const link of links) {
    const click = link.data?.on?.click;
    assert.equal(typeof click, 'function');
    if (typeof click === 'function') click(new Event('click'), link);
  }
  assert.deepEqual(pinned, [
    { proofId: 'played', ply: 2, phase: 'after' },
    { proofId: 'alternative', ply: 2, phase: 'after' },
  ]);
});

test('only included explanation references expose their exact later position once', () => {
  const candidate = primary();
  const effect = {
    scope: 'pair_difference',
    line_ids: ['played', 'alternative'],
    prefix_moves_uci: [],
    through_plies: [2, 1],
    actor: { square: 'c1', role: 'bishop', side: 'white', position_id: 'before' },
    target: null,
    square: 'h6',
    occurrence_ids: ['root', 'reply', 'alternative-root'],
    evidence_ids: [],
  };
  const label = 'Position used in this explanation';
  assert.ok(!text(render([candidate, pv()])).includes(label));
  candidate.detail.effects = [effect, effect];
  let pinned: unknown;
  const view = render([candidate, pv()], 'en-US', undefined, { pinFrame: frame => { pinned = frame; } });
  const links = nodes(view).filter(node => node.sel?.startsWith('button') && text(node).includes(label));
  assert.equal(links.length, 1);
  assert.ok(text(links[0]).includes('h6'));
  const click = links[0].data?.on?.click;
  assert.equal(typeof click, 'function');
  if (typeof click === 'function') click(new Event('click'), links[0]);
  assert.deepEqual(pinned, { proofId: 'played', ply: 2, phase: 'after' });
});

test('merged comparison prose leads with each owned terminal and shares its ending button with effects', () => {
  for (const kind of ['checkmate', 'stalemate', 'insufficient_material', 'fivefold_repetition', 'seventy_five_move_rule'] as const) {
    // Already admitted renderer inputs: canonical terminal rules are not recomputed here.
    const path: MoveReviewObservations['paths'][number] = {
      id: 'terminal', kind: 'played', startFen, occurrenceIds: ['root', 'terminal-end'],
      moves: [{ uci: 'd2d3', label: 'd3', fenAfter: playedFen }, { uci: 'h7h6', label: 'h6', fenAfter: replyFen }],
      lineSummary: { scope: 'observed_line', perspective: 'white', premise_ids: ['terminal-summary'], events: [],
        ending: { kind, ply_offset: 1, occurrence_id: 'terminal-end' } },
    };
    const main = primary(), reading = pv(), comparison = question();
    main.lineIds = reading.lineIds = comparison.lineIds = ['alternative', 'terminal'];
    main.detail.reference_line_id = 'alternative';
    main.detail.candidate_line_id = 'terminal';
    const answer = (reading.detail as CommentaryPvComparisonDetail).answer;
    answer.reference_line_id = 'alternative';
    answer.alternative_line_id = 'terminal';
    answer.reference_occurrence_id = 'alternative-root';
    answer.alternative_occurrence_id = 'root';
    main.detail.effects = [{ scope: 'line_history', line_ids: ['terminal'], prefix_moves_uci: [], through_plies: [2],
      actor: { position_id: 'after', side: 'white', role: 'bishop', square: 'c1' }, target: null, square: 'h6',
      occurrence_ids: ['root', 'terminal-end'], evidence_ids: [] }];
    let pinned: unknown;
    const view = render([main, comparison, reading], 'en-US', undefined,
      { pinFrame: frame => { pinned = frame; } }, [path]);
    const cards = nodes(view).filter(node => node.sel === 'article.move-review__pv-reading');
    assert.equal(cards.length, 1);
    const children = (cards[0].children ?? []).filter((node): node is VNode => typeof node === 'object' && !!node);
    const paragraphs = (cards[0].children ?? []).filter((node): node is VNode =>
      typeof node === 'object' && !!node && (node.sel === 'p' || !!node.sel?.startsWith('p.')));
    const label = moveReviewCopy('en-US').terminalLabels[kind];
    assert.deepEqual(paragraphs[0].data?.attrs,
      { lang: 'en-US', 'data-terminal': kind, 'data-line': path.id, 'data-ply': 2 });
    assert.ok(text(paragraphs[0]).includes(label.toLowerCase()) &&
      path.moves.every(move => text(paragraphs[0]).includes(move.label)));
    assert.equal(children.find(node => node.sel === 'p' || node.sel?.startsWith('p.')), paragraphs[0],
      'the first paragraph of the whole card is the terminal, including styled generic paragraphs');
    const genericIndex = children.findIndex(node => node.sel === 'details.move-review__comparison-records');
    assert.ok(children.indexOf(paragraphs[0]) < children.findIndex(node => node.sel === 'p.move-review__pv-quality'));
    assert.ok(children.indexOf(paragraphs[0]) < genericIndex);
    assert.ok(children.findIndex(node => text(node) === explanation) < genericIndex);
    assert.equal(terminalParagraphs(cards[0]).length, 1);
    assert.ok(paragraphs.some(node => text(node) === explanation));
    const links = nodes(cards[0]).filter(node => node.sel?.startsWith('button') &&
      text(node).includes('Position used in this explanation'));
    assert.equal(links.length, 1, 'the terminal and existing effect share their exact ending frame');
    assert.ok(text(links[0]).includes(label));
    const click = links[0].data?.on?.click;
    if (typeof click === 'function') click(new Event('click'), links[0]);
    assert.deepEqual(pinned, { proofId: 'terminal', ply: 2, phase: 'after' });
    assert.ok(nodes(cards[0]).some(node => node.sel === 'button.move-review__line-event' &&
      text(node).includes(label)), 'the detailed summary uses the existing automatic-terminal caption too');
  }
});

test('a standalone comparison owns its one-move terminal without borrowing another arm or malformed ending', () => {
  const path: MoveReviewObservations['paths'][number] = {
    id: 'terminal', kind: 'played', startFen, occurrenceIds: ['root'],
    moves: [{ uci: 'd2d3', label: 'd3', fenAfter: playedFen }],
    lineSummary: { scope: 'observed_line', perspective: 'white', premise_ids: ['terminal-summary'], events: [],
      ending: { kind: 'insufficient_material', ply_offset: 0, occurrence_id: 'root' } },
  };
  const main = primary();
  const hidden = { ...primary(), id: 'hidden', selection: 'context' as const, lineIds: ['terminal'] };
  const unmatched = render([main, hidden], 'en-US', undefined, {}, [path]);
  assert.equal(terminalParagraphs(unmatched).length, 0);
  assert.ok(!text(unmatched).includes('Position used in this explanation'));
  main.lineIds = ['terminal', 'alternative'];
  main.detail.reference_line_id = 'terminal';
  let pinned: unknown;
  const view = render([main], 'en-US', undefined, { pinFrame: frame => { pinned = frame; } }, [path]);
  const card = nodes(view).find(node => node.sel === 'article.move-review__scene')!;
  const paragraphs = (card.children ?? []).filter((node): node is VNode =>
    typeof node === 'object' && !!node && (node.sel === 'p' || !!node.sel?.startsWith('p.')));
  const firstParagraph = nodes(card).find(node => node.sel === 'p' || node.sel?.startsWith('p.'));
  assert.equal(firstParagraph, paragraphs[0], 'a standalone card also starts with its terminal paragraph');
  assert.deepEqual(paragraphs[0].data?.attrs,
    { lang: 'en-US', 'data-terminal': 'insufficient_material', 'data-line': path.id, 'data-ply': 1 });
  assert.ok(text(paragraphs[0]).includes(path.moves[0].label));
  assert.ok(paragraphs.findIndex(node => text(node) === explanation) > 0);
  const link = nodes(card).find(node => node.sel?.startsWith('button') &&
    text(node).includes('Position used in this explanation'))!;
  const click = link.data?.on?.click;
  if (typeof click === 'function') click(new Event('click'), link);
  assert.deepEqual(pinned, { proofId: 'terminal', ply: 1, phase: 'after' });
  for (const ending of [
    { kind: 'line_end' as const, ply_offset: 0, occurrence_id: 'root' },
    { kind: 'checkmate' as const, ply_offset: 1, occurrence_id: 'root' },
    { kind: 'checkmate' as const, ply_offset: 0, occurrence_id: 'foreign' },
  ]) {
    const changed = structuredClone(path);
    changed.lineSummary!.ending = ending;
    const result = render([main], 'en-US', undefined, {}, [changed]);
    assert.equal(terminalParagraphs(result).length, 0);
    assert.ok(!text(result).includes('Position used in this explanation'));
  }
});

test('owned terminals lead comparison cards without reordering peers or borrowing hidden and other-occurrence endings', () => {
  const standalone = question();
  standalone.lineIds = ['played', 'question-line'];
  standalone.detail.text = 'A separate question remains available.';
  const paths: MoveReviewObservations['paths'] = [{
    id: 'question-line', kind: 'analysis', startFen, occurrenceIds: ['question-root'],
    moves: [{ uci: 'e2e3', label: 'e3', fenAfter: alternativeFen }],
  }];
  const readings = ['ordinary-1', 'terminal-1', 'ordinary-2', 'terminal-2'].map(id => {
    const candidate = pv();
    candidate.id = id;
    candidate.lineIds = ['played', id];
    candidate.detail.text = `Explanation for ${id}.`;
    const answer = (candidate.detail as CommentaryPvComparisonDetail).answer;
    answer.alternative_line_id = id;
    answer.alternative_occurrence_id = `${id}-root`;
    paths.push({
      id, kind: 'analysis', startFen, occurrenceIds: [`${id}-root`, `${id}-end`],
      moves: [{ uci: 'e2e3', label: 'e3', fenAfter: alternativeFen },
        { uci: 'h7h6', label: 'h6', fenAfter: replyFen }],
      ...(id.startsWith('terminal') ? { lineSummary: {
        scope: 'observed_line' as const, perspective: 'white' as const, premise_ids: [`${id}-summary`], events: [],
        ending: { kind: 'checkmate' as const, ply_offset: 1, occurrence_id: `${id}-end` },
      } } : {}),
    });
    return candidate;
  });
  for (const mode of ['owned', 'other-occurrence', 'hidden'] as const) {
    const candidates = structuredClone([standalone, ...readings]);
    for (const candidate of candidates.filter(candidate => candidate.id.startsWith('terminal'))) {
      if (mode === 'other-occurrence') candidate.rootOccurrenceId = 'another-root';
      if (mode === 'hidden') {
        candidate.selection = 'context';
        candidate.detail = { type: 'root_comparison', reference_line_id: 'question-line',
          candidate_line_id: candidate.id, material_relation: 'equal', text: candidate.detail.text };
      }
    }
    const before = structuredClone(candidates), pinned: unknown[] = [];
    const view = render(candidates, 'en-US', undefined, { pinFrame: frame => pinned.push(frame) }, paths);
    const cards = nodes(view).filter(node =>
      node.sel === 'article.move-review__pv-reading' || node.sel === 'article.move-review__scene');
    assert.deepEqual(cards.map(node => node.data?.attrs?.id ?? 'standalone'), mode === 'owned'
      ? ['move-review-pv-terminal-1', 'move-review-pv-terminal-2', 'standalone',
          'move-review-pv-ordinary-1', 'move-review-pv-ordinary-2']
      : ['standalone', 'move-review-pv-ordinary-1', 'move-review-pv-ordinary-2']);
    assert.deepEqual(candidates, before, 'only display order changes; candidate selection and evidence stay intact');
    for (const card of cards) {
      assert.ok(nodes(card).some(node => node.sel?.startsWith('button')), 'every comparison keeps its position links');
      const expected = card.data?.attrs?.id
        ? `Explanation for ${String(card.data.attrs.id).replace('move-review-pv-', '')}.`
        : standalone.detail.text as string;
      assert.ok(text(card).includes(expected), 'every displayed comparison retains its concrete explanation');
    }
    if (mode === 'owned') {
      assert.deepEqual(terminalParagraphs(cards[0]).map(node => node.data?.attrs),
        [{ lang: 'en-US', 'data-terminal': 'checkmate', 'data-line': 'terminal-1', 'data-ply': 2 }]);
      const ending = nodes(cards[0]).find(node => node.sel?.startsWith('button') &&
        text(node).includes('Position used in this explanation'))!;
      const click = ending.data?.on?.click;
      assert.equal(typeof click, 'function');
      if (typeof click === 'function') click(new Event('click'), ending);
      assert.deepEqual(pinned, [{ proofId: 'terminal-1', ply: 2, phase: 'after' }]);
    } else assert.equal(cards.flatMap(terminalParagraphs).length, 0);
  }
});

test('current root comparisons precede a child terminal while preserving each exact question and frame', () => {
  const root = question();
  root.id = 'root-question';
  root.lineIds = ['played', 'root-alternative'];
  root.detail.text = 'The current root choice.';
  const child = question();
  child.id = 'child-question';
  child.lineIds = ['child-terminal', 'child-alternative'];
  child.detail.prefix_moves_uci = ['d2d3'];
  child.detail.reference_move = 'h7h6';
  child.detail.alternative_move = 'g7g6';
  child.detail.perspective = 'black';
  child.detail.text = 'A separate later choice.';
  const paths: MoveReviewObservations['paths'] = [
    { id: 'root-alternative', kind: 'analysis', startFen, occurrenceIds: ['other-root'],
      moves: [{ uci: 'e2e3', label: 'e3', fenAfter: alternativeFen }] },
    { id: 'child-terminal', kind: 'analysis', startFen, occurrenceIds: ['root', 'child-end'],
      moves: [{ uci: 'd2d3', label: 'd3', fenAfter: playedFen },
        { uci: 'h7h6', label: 'h6', fenAfter: replyFen }],
      lineSummary: { scope: 'observed_line', perspective: 'white', premise_ids: ['child-summary'], events: [],
        ending: { kind: 'checkmate', ply_offset: 1, occurrence_id: 'child-end' } } },
    { id: 'child-alternative', kind: 'analysis', startFen, occurrenceIds: ['root', 'other-child'],
      moves: [{ uci: 'd2d3', label: 'd3', fenAfter: playedFen },
        { uci: 'g7g6', label: 'g6', fenAfter: replyFen }] },
  ];
  const candidates = [child, root, pv()], before = structuredClone(candidates), pinned: unknown[] = [];
  const view = render(candidates, 'en-US', undefined, { pinFrame: frame => pinned.push(frame) }, paths);
  const cards = nodes(view).filter(node =>
    node.sel === 'article.move-review__pv-reading' || node.sel === 'article.move-review__scene');
  assert.deepEqual(cards.map(node => node.data?.attrs?.id ?? node.data?.key),
    [':scene-root-question', 'move-review-pv-pv', ':scene-child-question']);
  assert.ok(text(cards[0]).includes(root.detail.text as string));
  assert.ok(text(cards[2]).includes(child.detail.text as string));
  assert.deepEqual(terminalParagraphs(cards[2]).map(node => node.data?.attrs),
    [{ lang: 'en-US', 'data-terminal': 'checkmate', 'data-line': 'child-terminal', 'data-ply': 2 }]);
  const button = nodes(cards[2]).find(node => node.sel?.startsWith('button') &&
    text(node).includes('Position used in this explanation'))!;
  const click = button.data?.on?.click;
  assert.equal(typeof click, 'function');
  if (typeof click === 'function') click(new Event('click'), button);
  assert.deepEqual(pinned, [{ proofId: 'child-terminal', ply: 2, phase: 'after' }]);
  assert.deepEqual(candidates, before);
});

test('the displayed root explanation precedes other root questions while current terminals still lead', () => {
  const alternatives: MoveReviewObservations['paths'] = [
    { id: 'root-a3', kind: 'analysis', startFen, occurrenceIds: ['a3-root'],
      moves: [{ uci: 'a2a3', label: 'a3', fenAfter: alternativeFen }] },
    { id: 'root-h3', kind: 'analysis', startFen, occurrenceIds: ['h3-root'],
      moves: [{ uci: 'h2h3', label: 'h3', fenAfter: alternativeFen }] },
  ];
  const questions = alternatives.map(path => {
    const candidate = question();
    candidate.id = path.id;
    candidate.lineIds = ['played', path.id];
    candidate.detail.alternative_move = path.moves[0].uci;
    candidate.detail.text = `A separate root question about ${path.moves[0].label}.`;
    return candidate;
  });
  const terminalPath: MoveReviewObservations['paths'][number] = {
    id: 'root-terminal', kind: 'analysis', startFen, occurrenceIds: ['terminal-root', 'terminal-end'],
    moves: [{ uci: 'b2b3', label: 'b3', fenAfter: alternativeFen },
      { uci: 'h7h6', label: 'h6', fenAfter: replyFen }],
    lineSummary: { scope: 'observed_line', perspective: 'white', premise_ids: ['terminal-summary'], events: [],
      ending: { kind: 'checkmate', ply_offset: 1, occurrence_id: 'terminal-end' } },
  };
  const terminal = pv();
  terminal.id = 'terminal-pv';
  terminal.lineIds = ['played', terminalPath.id];
  const terminalAnswer = (terminal.detail as CommentaryPvComparisonDetail).answer;
  terminalAnswer.alternative_line_id = terminalPath.id;
  terminalAnswer.alternative_occurrence_id = 'terminal-root';
  for (const includeTerminal of [false, true]) {
    const candidates = [...questions, primary(), pv(), ...(includeTerminal ? [terminal] : [])];
    const before = structuredClone(candidates), pinned: unknown[] = [];
    const view = render(candidates, 'en-US', undefined, { pinFrame: frame => pinned.push(frame) },
      [...alternatives, ...(includeTerminal ? [terminalPath] : [])]);
    const cards = nodes(view).filter(node =>
      node.sel === 'article.move-review__pv-reading' || node.sel === 'article.move-review__scene');
    assert.deepEqual(cards.map(node => node.data?.attrs?.id ?? node.data?.key),
      [...(includeTerminal ? ['move-review-pv-terminal-pv'] : []), 'move-review-pv-pv', ':scene-root-a3', ':scene-root-h3']);
    const main = cards[includeTerminal ? 1 : 0];
    assert.equal(text(main).split(explanation).length - 1, 1);
    assert.ok(!text(main).includes(questions[0].detail.text as string));
    assert.ok(text(cards.at(-2)!).includes(questions[0].detail.text as string));
    assert.ok(text(cards.at(-1)!).includes(questions[1].detail.text as string));
    const button = nodes(main).find(node => node.sel?.startsWith('button') &&
      text(node).includes('Inspect the comparison line') && text(node).includes('d3'))!;
    assert.ok(button);
    const click = button.data?.on?.click;
    assert.equal(typeof click, 'function');
    if (typeof click === 'function') click(new Event('click'), button);
    assert.deepEqual(pinned, [{ proofId: 'played', ply: 1, phase: 'after' }]);
    assert.deepEqual(candidates, before);
  }
});

test('a current one-move automatic ending survives without a comparison or accepted score', () => {
  const path: MoveReviewObservations['paths'][number] = {
    id: 'played-terminal', kind: 'played', startFen, occurrenceIds: ['root'],
    moves: [{ uci: 'd2d3', label: 'd3', fenAfter: playedFen }],
    lineSummary: { scope: 'observed_line', perspective: 'white', premise_ids: ['terminal-summary'], events: [],
      ending: { kind: 'insufficient_material', ply_offset: 0, occurrence_id: 'root' } },
  };
  const pinned: unknown[] = [];
  const view = render([], 'en-US', undefined, { pinFrame: frame => pinned.push(frame) }, [path]);
  const card = nodes(view).find(node => node.sel === 'article.move-review__scene')!;
  assert.ok(card);
  assert.deepEqual(terminalParagraphs(card).map(node => node.data?.attrs),
    [{ lang: 'en-US', 'data-terminal': 'insufficient_material', 'data-line': path.id, 'data-ply': 1 }]);
  assert.ok(text(terminalParagraphs(card)[0]).includes(path.moves[0].label));
  assert.equal(nodes(view).filter(node => node.sel === 'article.move-review__scene').length, 1);
  const button = nodes(card).find(node => node.sel?.startsWith('button'))!;
  const click = button.data?.on?.click;
  assert.equal(typeof click, 'function');
  if (typeof click === 'function') click(new Event('click'), button);
  assert.deepEqual(pinned, [{ proofId: 'played-terminal', ply: 1, phase: 'after' }]);
  const pinnedView = render([], 'en-US', undefined, {}, [path], [], {
    evidenceExpanded: false, pinnedFrame: { proofId: 'played-terminal', ply: 1, phase: 'after' },
  });
  assert.ok(nodes(pinnedView).some(node => node.sel === 'section.move-review__comparison-preview'),
    'the automatic ending has an in-card board even without a comparison candidate');
  for (const change of [
    (value: typeof path) => { value.kind = 'analysis'; },
    (value: typeof path) => { value.startFen = alternativeFen; },
    (value: typeof path) => { value.moves[0].uci = 'e2e3'; },
    (value: typeof path) => { value.moves[0].fenAfter = alternativeFen; },
    (value: typeof path) => { value.occurrenceIds[0] = 'foreign'; value.lineSummary!.ending.occurrence_id = 'foreign'; },
    (value: typeof path) => { value.lineSummary!.ending.kind = 'line_end'; },
    (value: typeof path) => { value.lineSummary!.ending.ply_offset = 1; },
    (value: typeof path) => {
      value.moves.push({ uci: 'h7h6', label: 'h6', fenAfter: replyFen });
      value.occurrenceIds.push('later');
      value.lineSummary!.ending = { kind: 'checkmate', ply_offset: 1, occurrence_id: 'later' };
    },
  ]) {
    const other = structuredClone(path);
    change(other);
    assert.ok(!nodes(render([], 'en-US', undefined, {}, [other]))
      .some(node => node.sel === 'article.move-review__scene'));
  }
  const main = primary();
  main.lineIds = ['played-terminal', 'alternative'];
  main.detail.reference_line_id = 'played-terminal';
  assert.equal(terminalParagraphs(render([main], 'en-US', undefined, {}, [path])).length, 1);
});

test('a check sequence owns its current mover, checked king, exact reply, and observation premises', () => {
  const rootFen = '7k/8/8/8/8/8/8/R6K w - - 0 1';
  const fens = [rootFen, 'R6k/8/8/8/8/8/8/7K b - - 1 1',
    'R7/7k/8/8/8/8/8/7K w - - 2 2',
    '8/R6k/8/8/8/8/8/7K b - - 3 2'];
  const check: MoveReviewObservation = {
    kind: 'check_responses', phase: 'after', occurrenceId: 'root', side: 'white', role: 'rook',
    from: 'a1', to: 'a8', premiseIds: ['check-proof'], checked_side: 'black', king_square: 'h8',
    checkers: [{ role: 'rook', square: 'a8' }], controlled_king_destinations: [], terminal: 'ongoing',
    responses: [{ modes: ['king_move'], resource: {
      movement: { side: 'black', from: 'h8', to: 'h7', before_role: 'king', after_role: 'king' },
      move_uci: 'h8h7', capture: null,
    } }],
  };
  const response: MoveReviewObservation = {
    kind: 'check_response', phase: 'before', occurrenceId: 'reply', side: 'black', role: 'king',
    from: 'h8', to: 'h7', premiseIds: ['response-proof'], responding_side: 'black', king_square: 'h8',
    checkers: [{ role: 'rook', square: 'a8' }], response: check.responses[0],
  };
  const laterCheck: MoveReviewObservation = { ...check, occurrenceId: 'later', from: 'a8', to: 'a7',
    premiseIds: ['later-check-proof'], king_square: 'h7', checkers: [{ role: 'rook', square: 'a7' }],
    responses: [{ modes: ['king_move'], resource: {
      movement: { side: 'black', from: 'h7', to: 'h6', before_role: 'king', after_role: 'king' },
      move_uci: 'h7h6', capture: null,
    } }],
  };
  // Admitted typed observations isolate selection ownership. Full wire proof admission
  // is exercised separately against actual runtime responses.
  const observations = {
    paths: [{ id: 'played', kind: 'played', startFen: rootFen,
      occurrenceIds: ['root', 'reply', 'later'], positionIds: ['before', 'after', 'reply-position', 'later-position'],
      moves: ['a1a8', 'h8h7', 'a8a7'].map((uci, index) => ({ uci, label: uci, fenAfter: fens[index + 1] })) }],
    statements: [check, response, laterCheck],
    positions: fens.map((fen, index) => ({ id: ['before', 'after', 'reply-position', 'later-position'][index], fen })),
  } as unknown as MoveReviewObservations;
  const effect: CommentaryEffect = {
    scope: 'check_sequence', line_ids: ['played'], prefix_moves_uci: [], through_plies: [2],
    actor: { position_id: 'after', side: 'white', role: 'rook', square: 'a8' },
    target: { position_id: 'after', side: 'black', role: 'king', square: 'h8' }, square: 'h8',
    occurrence_ids: ['root', 'reply'], evidence_ids: ['root', 'reply', 'check-proof', 'response-proof'],
  };
  const selection = { version: 2, scenes: [], candidates: [{
    id: 'comparison', kind: 'comparison', selection: 'selected', root_occurrence_id: 'root',
    occurrence_ids: ['root', 'reply', 'later'], position_ids: ['before', 'after', 'reply-position', 'later-position'],
    line_ids: ['played'], evidence_ids: ['comparison-proof'], source_refs: [], detail: {
      type: 'root_comparison', scope: 'observed_lines', comparison_evidence_id: 'comparison-proof',
      comparison_kind: 'played_vs_best', reference_line_id: null, candidate_line_id: 'played',
      missing_line_ids: ['unavailable-reference'], material_relation: 'unavailable',
      material_unavailable_reason: 'missing_path', text: 'Ra8 checks; the returned reply is Kh7.', effects: [effect],
    },
  }] };
  const context = { rootFen, playedMove: 'a1a8', engineProfile: 'inline-selection-test' };
  assert.ok(decodeCommentarySelection(selection, observations, context));
  const mutations: [string, (effect: CommentaryEffect) => void][] = [
    ['foreign line', row => { row.line_ids = ['other']; }],
    ['foreign prefix', row => { row.prefix_moves_uci = ['a1a8']; }],
    ['wrong king', row => { row.target = { position_id: 'after', side: 'white', role: 'king', square: 'h1' }; row.square = 'h1'; }],
    ['wrong actor', row => { row.actor = { position_id: 'after', side: 'white', role: 'king', square: 'h1' }; }],
    ['wrong actor position', row => { row.actor.position_id = 'before'; }],
    ['wrong king position', row => { row.target!.position_id = 'before'; }],
    ['wrong square', row => { row.square = 'h7'; }],
    ['truncated reply', row => { row.through_plies = [1]; }],
    ['missing reply occurrence', row => { row.occurrence_ids = ['root']; }],
    ['foreign occurrence', row => { row.occurrence_ids = ['root', 'other']; }],
    ['missing check premise', row => { row.evidence_ids = row.evidence_ids.filter(id => id !== 'check-proof'); }],
    ['missing response premise', row => { row.evidence_ids = row.evidence_ids.filter(id => id !== 'response-proof'); }],
  ];
  for (const [name, mutate] of mutations) {
    const forged = structuredClone(selection);
    mutate(forged.candidates[0].detail.effects[0]);
    assert.equal(decodeCommentarySelection(forged, observations, context), undefined, name);
  }

  const continued = structuredClone(selection);
  const continuedEffect = continued.candidates[0].detail.effects[0];
  continuedEffect.through_plies = [3];
  continuedEffect.occurrence_ids.push('later');
  continuedEffect.evidence_ids.push('later', 'later-check-proof');
  assert.ok(decodeCommentarySelection(continued, observations, context),
    'the same checking turn continues through the next actual check');
  const unseeded = structuredClone(observations);
  unseeded.statements = [laterCheck];
  assert.equal(decodeCommentarySelection(continued, unseeded, context), undefined,
    'a distant check cannot replace the missing current check and response');

  const kingReply = structuredClone(selection);
  const kingEffect = kingReply.candidates[0].detail.effects[0];
  kingEffect.actor = kingEffect.target = { position_id: 'after', side: 'black', role: 'king', square: 'h7' };
  kingEffect.square = 'h7';
  kingEffect.through_plies = [1];
  kingEffect.occurrence_ids = ['root'];
  kingEffect.evidence_ids = ['root', 'response-proof'];
  const kingObservations = structuredClone(observations);
  kingObservations.paths[0].startFen = fens[1];
  kingObservations.paths[0].moves = [{ uci: 'h8h7', label: 'Kh7', fenAfter: fens[2] }];
  kingObservations.paths[0].occurrenceIds = ['root'];
  kingObservations.paths[0].positionIds = ['before', 'after'];
  kingObservations.positions![0].fen = fens[1];
  kingObservations.positions![1].fen = fens[2];
  kingObservations.statements = [{ ...response, occurrenceId: 'root' }];
  kingReply.candidates[0].occurrence_ids = ['root'];
  kingReply.candidates[0].position_ids = ['before', 'after'];
  assert.ok(decodeCommentarySelection(kingReply, kingObservations,
    { ...context, rootFen: fens[1], playedMove: 'h8h7' }), 'a legal king response can be both the actor and the protected king');
});

test('functional effects require their exact sustained relation and actual legal use', () => {
  // Small legal paths and admitted resource rows isolate effect ownership. No fixture
  // helpers are loaded; full wire admission is checked against runtime responses separately.
  const cases = [
    { name: 'incoming support with delayed recapture', scope: 'support_handoff',
      fen: '4r2k/8/8/8/4N3/2N5/8/3R3K w - - 0 1',
      moves: ['d1e1', 'h8h7', 'c3b5', 'e8e4', 'h1h2', 'h7h6', 'e1e4'],
      target: 'e4', square: 'e4', loss: 3, rays: [] },
    { name: 'outgoing support', scope: 'support_handoff',
      fen: '4r2k/8/8/8/4N3/2N5/8/4R2K w - - 0 1',
      moves: ['c3b5', 'e8e4', 'e1e4'], target: 'e4', square: 'e4', loss: 1, rays: [] },
    { name: 'target movement alone is not a support handoff', scope: 'support_handoff',
      fen: '4r2k/8/8/8/4P3/2N5/8/3R3K w - - 0 1',
      moves: ['d1e1', 'h8h7', 'e4e5', 'e8e5', 'e1e5'],
      target: 'e4', square: 'e4', rejected: true, rays: [] },
    { name: 'a later provider move cannot end support already lost when the target moved', scope: 'support_handoff',
      fen: '4r2k/8/8/8/4P3/2N5/8/3R3K w - - 0 1',
      moves: ['d1e1', 'h8h7', 'e4e5', 'h7h6', 'c3b5', 'e8e5', 'e1e5'],
      target: 'e4', square: 'e4', rejected: true, rays: [] },
    { name: 'replacement blocker', scope: 'blocker_handoff',
      fen: 'b6k/8/8/8/4P3/2N5/7K/8 w - - 0 1',
      moves: ['c3d5', 'h8h7', 'e4e5', 'h7h6', 'h2h1'], target: 'a8', square: 'h1',
      rays: [{ from: 'a8', squares: ['b7', 'c6', 'd5', 'e4', 'f3', 'g2', 'h1'] }] },
    { name: 'an earlier sole blocker cannot borrow a later sole interval', scope: 'blocker_handoff',
      fen: 'b6k/8/8/8/4B3/2N5/7K/8 w - - 0 1',
      moves: ['c3d5', 'h8g8', 'e4f5', 'g8h8', 'f5e4', 'h8g8', 'e4f5', 'g8h8', 'h2h1'],
      target: 'a8', square: 'h1', rejected: true,
      rays: [{ from: 'a8', squares: ['b7', 'c6', 'd5', 'e4', 'f3', 'g2', 'h1'] }] },
    { name: 'move through the vacated square', scope: 'clearance_use',
      fen: '7k/8/8/8/8/8/8/RB5K w - - 0 1',
      moves: ['b1c2', 'h8g8', 'a1d1'], target: 'a1', square: 'b1',
      rays: [{ from: 'a1', squares: ['b1', 'c1', 'd1', 'e1', 'f1', 'g1', 'h1'] }] },
    { name: 'use the open ray from the new position', scope: 'clearance_use',
      fen: '7k/3p4/8/8/3N4/8/R7/7K w - - 0 1',
      moves: ['d4f5', 'h8g8', 'a2d2'], target: 'a2', square: 'd4',
      rays: [{ from: 'd2', squares: ['d3', 'd4', 'd5', 'd6', 'd7', 'd8'] }] },
    { name: 'a reopened passage cannot borrow the earlier clearance', scope: 'clearance_use',
      fen: '7k/3p4/4b3/8/3N4/8/R7/7K w - - 0 1',
      moves: ['d4f5', 'e6d5', 'h1h2', 'd5e6', 'a2d2'], target: 'a2', square: 'd4', rejected: true,
      rays: [{ from: 'd2', squares: ['d3', 'd4', 'd5', 'd6', 'd7', 'd8'] }] },
    { name: 'a functional use can own its later attack without inventing a root attack', scope: 'clearance_use',
      fen: 'r7/1k1p4/8/8/3N4/8/R6R/6K1 w - - 0 1',
      moves: ['d4f5', 'a8h8', 'a2d2', 'h8h7'], target: 'a2', square: 'd4', use: 3,
      rays: [{ from: 'd2', squares: ['d3', 'd4', 'd5', 'd6', 'd7', 'd8'] }] },
    { name: 'new control supports the original pawn when it later arrives', scope: 'support_use',
      fen: '7k/1pp5/8/8/3P4/8/8/7K b - - 0 1',
      moves: ['b7b6', 'h1h2', 'c7c5', 'd4c5', 'b6c5'], target: 'c7', square: 'c5', loss: 3, rays: [] },
    { name: 'new support follows the same friendly rook to the eventual exchange square', scope: 'support_use',
      fen: '7k/3r4/2r5/8/8/4N3/2P5/7K b - - 0 1',
      moves: ['d7c7', 'h1h2', 'c6c2', 'e3c2', 'c7c2'], target: 'c6', square: 'c2', loss: 3, rays: [] },
    { name: 'new support of an occupied square needs no departing defender', scope: 'support_use',
      fen: '4r2k/8/8/8/4N3/8/8/3R3K w - - 0 1',
      moves: ['d1e1', 'h8h7', 'h1h2', 'e8e4', 'e1e4'], target: 'e4', square: 'e4', loss: 3, rays: [] },
    { name: 'new support owns the exact later loss check and its recapture response', scope: 'support_use',
      fen: '4r2k/8/8/8/4N2K/8/P7/3R4 w - - 0 1',
      moves: ['d1e1', 'h8h7', 'a2a3', 'e8e4', 'e1e4'], target: 'e4', square: 'e4', loss: 3, lossCheck: true, rays: [] },
    { name: 'a handoff can own its loss check before the later actual recapture', scope: 'support_handoff',
      fen: '4r2k/8/8/8/4N2K/2N5/8/3R4 w - - 0 1',
      moves: ['d1e1', 'h8h7', 'c3b5', 'e8e4', 'h4h3', 'h7h6', 'e1e4'],
      target: 'e4', square: 'e4', loss: 3, lossCheck: true, rays: [] },
    { name: 'a different occupant cannot donate a future support square', scope: 'support_use',
      fen: '7k/1pp5/8/2N5/3P4/8/8/7K b - - 0 1',
      moves: ['b7b6', 'c5b3', 'c7c5', 'd4c5', 'b6c5'], target: 'c7', square: 'c5', rejected: true, rays: [] },
    { name: 'an already available defense is not new support', scope: 'support_use',
      fen: '4r2k/8/8/8/4N3/8/4R3/7K w - - 0 1',
      moves: ['e2e1', 'e8e4', 'e1e4'], target: 'e4', square: 'e4', rejected: true, rays: [] },
    { name: 'a provider cannot move before the claimed stationary defense is used', scope: 'support_use',
      fen: '4r2k/8/8/8/4N3/8/8/3R3K w - - 0 1',
      moves: ['d1e1', 'h8h7', 'e1e2', 'e8e4', 'e2e4'], target: 'e4', square: 'e4', rejected: true, rays: [] },
    { name: 'the current knight next creates an attack whose history stays linked', scope: 'piece_route',
      fen: '7k/8/8/8/8/4p3/8/1N5K w - - 0 1',
      moves: ['b1a3', 'h8g8', 'a3c2', 'e3e2'], target: 'e3', square: 'c2', route: 'attack', use: 3, rays: [] },
    { name: 'the current knight next supports a friendly pawn under attack', scope: 'piece_route',
      fen: '7k/8/1b6/8/8/4P3/8/1N5K w - - 0 1',
      moves: ['b1a3', 'h8g8', 'a3c2'], target: 'e3', square: 'c2', route: 'support', rays: [] },
    { name: 'the next support has no observed opposing pressure', scope: 'piece_route',
      fen: '7k/8/8/8/8/4P3/8/1N5K w - - 0 1',
      moves: ['b1a3', 'h8g8', 'a3c2'], target: 'e3', square: 'c2', route: 'support', rejected: true, rays: [] },
    { name: 'a route cannot skip an earlier move of the same knight', scope: 'piece_route',
      fen: '7k/8/8/3p4/8/8/8/1N5K w - - 0 1',
      moves: ['b1a3', 'h8g8', 'a3b5', 'g8h8', 'b5c3'], target: 'd5', square: 'c3', route: 'attack', rejected: true, rays: [] },
    { name: 'the next knight move evades the same original bishop', scope: 'piece_route',
      fen: '5b1k/8/8/8/8/8/8/1N5K w - - 0 1',
      moves: ['b1a3', 'h8g8', 'a3c2'], target: 'f8', square: 'c2', route: 'evade', rays: [] },
  ];
  for (const sample of cases) {
    const board = Chess.fromSetup(parseFen(sample.fen).unwrap()).unwrap();
    const positions: NonNullable<MoveReviewObservations['positions']> = [], fens: string[] = [];
    const material: NonNullable<NonNullable<MoveReviewObservations['paths'][number]['lineSummary']>['material']> = {
      events: [], net_cp: 0, max_gain_cp: 0, max_loss_cp: 0, recovered_after_loss: false,
    };
    for (let count = 0; count <= sample.moves.length; count++) {
      const observed = (square: string) => {
        const piece = board.board.get(parseSquare(square)!);
        return piece && { side: piece.color, role: piece.role, square: square as Key };
      };
      fens.push(makeFen(board.toSetup()));
      positions.push({ id: `p${count}`, fen: fens[count], premise_id: `position-${count}`,
        controls: { complete: true, pieces: [...board.board].map(([square, piece]) => {
          const squares = [...attacks(piece, square, board.board.occupied)].map(makeSquare);
          return { piece: observed(makeSquare(square))!, squares,
            source_ids: squares.map(to => `control-${count}-${makeSquare(square)}-${to}`) };
        }) },
        rays: { complete: true, lines: sample.rays.filter(ray => observed(ray.from)).map(ray => ({
          piece: observed(ray.from)!, squares: ray.squares as Key[], source_id: `ray-${count}-${ray.from}`,
          occupants: ray.squares.flatMap(square => observed(square) ? [observed(square)!] : []),
        })) },
        legal_moves: { complete: true, moves: [...board.allDests()].flatMap(([from, dests]) =>
          [...dests].map(to => ({ uci: makeSquare(from) + makeSquare(to), destination: makeSquare(to),
            ...(board.board.get(to) ? { captured_square: makeSquare(to) } : {}),
            source_id: `legal-${count}-${makeSquare(from)}${makeSquare(to)}` }))) },
        pawn_states: { complete: true, pawns: [] }, pawn_files: { complete: true, files: [] },
        pawn_advances: { complete: true, paths: [] },
      });
      if (count === sample.moves.length) break;
      const move = parseUci(sample.moves[count])!;
      assert.ok('from' in move && board.isLegal(move), sample.name);
      if (!('from' in move)) throw Error('expected a board move');
      const piece = board.board.get(move.from)!, captured = board.board.get(move.to);
      if (captured) {
        const value = { pawn: 100, knight: 300, bishop: 300, rook: 500, queen: 900, king: 0 }[captured.role];
        const delta = piece.color === 'white' ? value : -value;
        material.net_cp += delta;
        material.events.push({ ply_offset: count, occurrence_id: `o${count}`, move_uci: sample.moves[count],
          side: piece.color, from: makeSquare(move.from), to: makeSquare(move.to), before_role: piece.role,
          after_role: piece.role, capture: { role: captured.role, square: makeSquare(move.to), value_cp: value,
            recapture: material.events.length ? 'proven' : 'excluded' },
          promotion_gain_cp: 0, delta_cp: delta, balance_cp: material.net_cp });
      }
      board.play(move);
    }
    const occurrences = sample.moves.map((_move, index) => `o${index}`);
    const use = sample.use ?? sample.moves.length;
    const proofThrough = (through: number) => [...occurrences.slice(0, through), 'summary',
      ...(sample.route && through >= use ? ['route-proof'] : []), ...positions.slice(0, through + 1).flatMap(position => [position.premise_id,
      ...position.controls.pieces.flatMap(control => control.source_ids),
      ...position.rays.lines.map(ray => ray.source_id), ...position.legal_moves.moves.map(move => move.source_id)])];
    const evidence = proofThrough(use);
    const observations: MoveReviewObservations = { statements: [], positions, paths: [{
      id: 'played', kind: 'played', startFen: sample.fen, occurrenceIds: occurrences,
      positionIds: positions.map(position => position.id),
      moves: sample.moves.map((uci, index) => ({ uci, label: uci, fenAfter: fens[index + 1] })),
      lineSummary: { scope: 'observed_line', perspective: 'white', premise_ids: ['summary'], events: [], material,
        ending: { kind: 'line_end', ply_offset: sample.moves.length - 1, occurrence_id: occurrences.at(-1)! } },
    }] };
    if (sample.route) {
      const point = (count: number, controllerSquare: string, targetSquare: string) => {
        const controller = positions[count].controls.pieces.find(row => row.piece.square === controllerSquare)!;
        const target = positions[count].controls.pieces.find(row => row.piece.square === targetSquare)!.piece;
        const offset = controller.squares.indexOf(target.square);
        return { controller: controller.piece, target, state: offset < 0 ? 'absent' as const : 'present' as const,
          premiseIds: [positions[count].premise_id, ...(offset < 0 ? [] : [controller.source_ids[offset]])] };
      };
      const from = sample.moves[use - 1].slice(0, 2), to = sample.moves[use - 1].slice(2, 4);
      const receiving = sample.target;
      const before = sample.route === 'evade' ? point(use - 1, receiving, from) : point(use - 1, from, receiving);
      const after = sample.route === 'evade' ? point(use, receiving, to) : point(use, to, receiving);
      const mover = positions[use].controls.pieces.find(row => row.piece.square === to)!.piece;
      const common = { occurrenceId: occurrences[use - 1], side: mover.side, role: mover.role,
        from, to, premiseIds: ['route-proof'] };
      observations.statements.push({ ...common, kind: 'piece_control',
        relation: before.controller.side === before.target.side ? 'support' : 'attack', before, after });
    }
    const actor = positions[1].controls.pieces.find(row => row.piece.square === sample.moves[0].slice(2, 4))!.piece;
    const target = positions[1].controls.pieces.find(row => row.piece.square === sample.target)!.piece;
    const effect: CommentaryEffect = { scope: sample.scope as CommentaryEffect['scope'], line_ids: ['played'],
      prefix_moves_uci: [], through_plies: [use], actor: { ...actor, position_id: 'p1' },
      target: { ...target, position_id: 'p1' }, square: sample.square as Key,
      occurrence_ids: occurrences.slice(0, use), evidence_ids: evidence };
    const selection = { version: 2, scenes: [], candidates: [{
      id: 'comparison', kind: 'comparison', selection: 'selected', root_occurrence_id: 'o0',
      occurrence_ids: occurrences, position_ids: positions.map(position => position.id),
      line_ids: ['played'], evidence_ids: ['comparison-proof'], source_refs: [], detail: {
        type: 'root_comparison', scope: 'observed_lines', comparison_evidence_id: 'comparison-proof',
        comparison_kind: 'played_vs_best', reference_line_id: null, candidate_line_id: 'played',
        missing_line_ids: ['unavailable-reference'], material_relation: 'unavailable',
        material_unavailable_reason: 'missing_path', text: sample.name, effects: [effect],
      },
    }] };
    const context = { rootFen: sample.fen, playedMove: sample.moves[0], engineProfile: 'inline-selection-test' };
    if (sample.rejected) {
      assert.equal(decodeCommentarySelection(selection, observations, context), undefined, sample.name);
      continue;
    }
    assert.ok(decodeCommentarySelection(selection, observations, context), sample.name);
    for (const [name, mutate] of [
      ['wrong line', (row: CommentaryEffect) => { row.line_ids = ['foreign']; }],
      ['wrong prefix', (row: CommentaryEffect) => { row.prefix_moves_uci = [sample.moves[0]]; }],
      ['wrong actor position', (row: CommentaryEffect) => { row.actor.position_id = 'p0'; }],
      ['wrong target position', (row: CommentaryEffect) => { row.target!.position_id = 'p0'; }],
      ['wrong square', (row: CommentaryEffect) => { row.square = 'a3'; }],
      ['missing use', (row: CommentaryEffect) => { row.through_plies = [1]; }],
      ['missing occurrence', (row: CommentaryEffect) => { row.occurrence_ids = occurrences.slice(1); }],
      ['missing position', (row: CommentaryEffect) => { row.evidence_ids = evidence.filter(id => id !== 'position-1'); }],
      ['missing summary', (row: CommentaryEffect) => { row.evidence_ids = evidence.filter(id => id !== 'summary'); }],
      ['missing legal use', (row: CommentaryEffect) => {
        row.evidence_ids = evidence.filter(id => id !== `legal-${use - 1}-${sample.moves[use - 1]}`);
      }],
    ] as const) {
      const forged = structuredClone(selection);
      mutate(forged.candidates[0].detail.effects[0]);
      assert.equal(decodeCommentarySelection(forged, observations, context), undefined, `${sample.name}: ${name}`);
    }
    if (sample.loss !== undefined) {
      const missing = structuredClone(observations);
      missing.positions![sample.loss + 1].legal_moves.moves = [];
      assert.equal(decodeCommentarySelection(selection, missing, context), undefined,
        'a later unpin cannot prove that the support was usable immediately after the loss');
    }
    if (sample.lossCheck) {
      const checked = structuredClone(observations), loss = sample.loss!, responsePly = loss + 1;
      const rootBefore = positions[0].controls.pieces.find(row => row.piece.square === sample.moves[0].slice(0, 2))!;
      const rootAfter = positions[1].controls.pieces.find(row => row.piece.square === actor.square)!;
      checked.statements.push({ kind: 'piece_control', relation: 'support', occurrenceId: 'o0',
        side: actor.side, role: actor.role, from: rootBefore.piece.square, to: actor.square,
        premiseIds: ['root-support'],
        before: { controller: rootBefore.piece, target, state: 'absent', premiseIds: ['position-0'] },
        after: { controller: rootAfter.piece, target, state: 'present', premiseIds: ['position-1'] } });
      const king = positions[1].controls.pieces.find(row => row.piece.side === actor.side && row.piece.role === 'king')!.piece;
      const checker = positions[loss + 1].controls.pieces.find(row => row.piece.square === sample.moves[loss].slice(2, 4))!.piece;
      const replyFrom = sample.moves[responsePly].slice(0, 2) as Key, replyTo = sample.moves[responsePly].slice(2, 4) as Key;
      const replyActor = positions[responsePly].controls.pieces.find(row => row.piece.square === replyFrom)!.piece;
      const check: Extract<MoveReviewObservation, { kind: 'check_responses' }> = {
        kind: 'check_responses', phase: 'after', occurrenceId: occurrences[loss],
        side: checker.side, role: checker.role, from: sample.moves[loss].slice(0, 2), to: checker.square,
        premiseIds: ['loss-check'], checked_side: king.side, king_square: king.square,
        checkers: [checker], controlled_king_destinations: [], terminal: 'ongoing',
        responses: [{ modes: [replyActor.role === 'king' ? 'king_move' : 'capture_checker'], resource: {
          movement: { side: replyActor.side, from: replyFrom, to: replyTo, before_role: replyActor.role, after_role: replyActor.role },
          move_uci: sample.moves[responsePly], capture: replyTo === checker.square ? checker : null,
        } }],
      };
      checked.statements.push(check, { kind: 'check_response', phase: 'before', occurrenceId: occurrences[responsePly],
        side: replyActor.side, role: replyActor.role, from: replyFrom, to: replyTo,
        premiseIds: ['loss-response'], responding_side: king.side, king_square: king.square,
        checkers: [checker], response: check.responses[0] });
      const sequence: CommentaryEffect = { ...effect, scope: 'check_sequence',
        target: { ...king, position_id: 'p1' }, square: king.square, through_plies: [loss + 2],
        occurrence_ids: ['o0', occurrences[loss], occurrences[responsePly]],
        evidence_ids: ['o0', occurrences[loss], occurrences[responsePly], 'loss-check', 'loss-response'] };
      const linked = structuredClone(selection);
      linked.candidates[0].detail.effects = [sequence, effect];
      assert.ok(decodeCommentarySelection(linked, checked, context), sample.name);
      linked.candidates[0].detail.effects.pop();
      assert.equal(decodeCommentarySelection(linked, checked, context), undefined,
        'a later check cannot borrow an absent support-use parent');
      const foreign = structuredClone(selection);
      const unrelated = structuredClone(sequence);
      unrelated.occurrence_ids = ['o0', occurrences[2], occurrences[3]];
      unrelated.through_plies = [4];
      foreign.candidates[0].detail.effects = [unrelated, effect];
      assert.equal(decodeCommentarySelection(foreign, checked, context), undefined,
        'the support parent does not authorize another earlier occurrence as its loss');
    }
    if (sample.scope === 'support_use') {
      const incomplete = structuredClone(observations);
      incomplete.positions![0].controls.complete = false;
      assert.equal(decodeCommentarySelection(selection, incomplete, context), undefined,
        'new support needs a complete before-position proving the absent relation');
      const missing = structuredClone(selection);
      missing.candidates[0].detail.effects[0].evidence_ids = evidence.filter(id => !id.startsWith(`control-1-${actor.square}-`));
      assert.equal(decodeCommentarySelection(missing, observations, context), undefined,
        'the initial new control must be owned by this support explanation');
      const broken = structuredClone(observations);
      broken.positions![sample.loss!].controls.pieces.find(row => row.piece.square === actor.square)!.squares = [];
      assert.equal(decodeCommentarySelection(selection, broken, context), undefined,
        'the same provider must still support the actual target at its loss');
    }
    if (sample.scope === 'piece_route') {
      const packetRefs = structuredClone(observations), normalized = structuredClone(selection);
      const positionRef = (count: number) => `inline-owner:evidence:position-occurrence:p${count}`;
      packetRefs.positions!.forEach((position, count) => { position.premise_id = positionRef(count); });
      normalized.candidates[0].detail.effects[0].evidence_ids = evidence.map(id =>
        id.startsWith('position-') ? positionRef(Number(id.slice('position-'.length))) : id);
      for (const statement of packetRefs.statements) if (statement.kind === 'piece_control')
        for (const [point, count] of [[statement.before, use - 1], [statement.after, use]] as const)
          point.premiseIds = point.premiseIds.map(id => id.startsWith('position-') ? positionRef(count) :
            `inline-owner:evidence:relation:position:p${count}:geometric_control:${id}`);
      assert.ok(decodeCommentarySelection(normalized, packetRefs, context),
        'the exact packet control ref and public source ID identify the same owned fact');
      for (const replacement of ['another-owner:evidence:', 'inline-owner:evidence:relation:position:wrong-']) {
        const unrelated = structuredClone(packetRefs);
        for (const statement of unrelated.statements) if (statement.kind === 'piece_control')
          for (const point of [statement.before, statement.after])
            point.premiseIds = point.premiseIds.map(id => id.includes(':geometric_control:')
              ? replacement.startsWith('another') ? id.replace('inline-owner:evidence:', replacement)
                : id.replace('inline-owner:evidence:relation:position:', replacement) : id);
        assert.equal(decodeCommentarySelection(normalized, unrelated, context), undefined,
          'a matching source suffix from another owner or position cannot authorize this fact');
      }
      const noObservation = structuredClone(observations);
      noObservation.statements = [];
      assert.equal(decodeCommentarySelection(selection, noObservation, context), undefined,
        'a route cannot infer a new role from final geometry alone');
      const noProof = structuredClone(selection);
      noProof.candidates[0].detail.effects[0].evidence_ids = evidence.filter(id => id !== 'route-proof');
      assert.equal(decodeCommentarySelection(noProof, observations, context), undefined,
        'the actual new-role observation belongs to the explanation');
      if (sample.route === 'support') {
        const unowned = structuredClone(selection);
        unowned.candidates[0].detail.effects[0].evidence_ids = evidence.filter(id => id !== 'control-3-b6-e3');
        assert.equal(decodeCommentarySelection(unowned, observations, context), undefined,
          'a supported pawn needs owned evidence that an enemy still attacks it');
      }
      if (sample.use !== undefined) {
        const history: CommentaryEffect = { ...effect, scope: 'line_history', through_plies: [sample.moves.length],
          attack_ply: use, square: target.square, occurrence_ids: ['o0', 'o2', 'o3'],
          evidence_ids: proofThrough(sample.moves.length) };
        const linked = structuredClone(selection);
        linked.candidates[0].detail.effects = [history, effect];
        assert.ok(decodeCommentarySelection(linked, observations, context),
          'the route can authorize its exact later attack independently of effect array order');
        linked.candidates[0].detail.effects.pop();
        assert.equal(decodeCommentarySelection(linked, observations, context), undefined,
          'the later attack cannot survive without its same-candidate route');
      }
    }
    if (sample.scope === 'blocker_handoff') {
      const incomplete = structuredClone(observations);
      incomplete.positions![3].controls.complete = false;
      assert.equal(decodeCommentarySelection(selection, incomplete, context), undefined,
        'the sole blocker interval requires complete control observations at every position');
      const controlled = structuredClone(observations);
      controlled.positions![3].controls.pieces.find(row => row.piece.square === sample.target)!
        .squares.push(sample.square as Key);
      assert.equal(decodeCommentarySelection(selection, controlled, context), undefined,
        'the stationary slider must not control the destination during the sole blocker interval');
    }
    if (sample.use !== undefined && sample.scope === 'clearance_use') {
      const anchorPiece = (square: string) => ({
        ...positions[1].controls.pieces.find(row => row.piece.square === square)!.piece, position_id: 'p1',
      });
      const history: CommentaryEffect = { scope: 'line_history', line_ids: ['played'], prefix_moves_uci: [],
        through_plies: [sample.moves.length], attack_ply: use, actor: anchorPiece('a2'), target: anchorPiece('d7'),
        square: 'd7', occurrence_ids: ['o0', 'o2', 'o3'], evidence_ids: proofThrough(sample.moves.length) };
      const attached = structuredClone(selection);
      // The child precedes its parent on the wire; proof admission must not depend on array order.
      attached.candidates[0].detail.effects = [history, effect];
      assert.ok(decodeCommentarySelection(attached, observations, context));
      for (const [name, mutate] of [
        ['missing parent', (rows: CommentaryEffect[]) => { rows.pop(); }],
        ['root attack invented', (rows: CommentaryEffect[]) => { delete rows[0].attack_ply; }],
        ['attack at root', (rows: CommentaryEffect[]) => { rows[0].attack_ply = 1; }],
        ['fractional attack ply', (rows: CommentaryEffect[]) => { rows[0].attack_ply = 2.5; }],
        ['attack at endpoint', (rows: CommentaryEffect[]) => { rows[0].attack_ply = sample.moves.length; }],
        ['wrong attack mover', (rows: CommentaryEffect[]) => { rows[0].actor = anchorPiece('h2'); }],
        ['wrong original target', (rows: CommentaryEffect[]) => { rows[0].target = anchorPiece('a8'); rows[0].square = 'a8'; }],
        ['future actor anchor', (rows: CommentaryEffect[]) => { rows[0].actor = { ...rows[0].actor, square: 'd2', position_id: 'p3' }; }],
        ['short history', (rows: CommentaryEffect[]) => { rows[0].through_plies = [3]; }],
        ['foreign prefix', (rows: CommentaryEffect[]) => { rows[0].prefix_moves_uci = ['d4f5']; }],
        ['foreign line', (rows: CommentaryEffect[]) => { rows[0].line_ids = ['foreign']; }],
        ['missing attack occurrence', (rows: CommentaryEffect[]) => { rows[0].occurrence_ids = ['o0', 'o3']; }],
        ['missing root occurrence', (rows: CommentaryEffect[]) => { rows[0].occurrence_ids = ['o2', 'o3']; }],
        ['missing attack position proof', (rows: CommentaryEffect[]) => { rows[0].evidence_ids = rows[0].evidence_ids.filter(id => id !== 'position-3'); }],
        ['missing attack control proof', (rows: CommentaryEffect[]) => { rows[0].evidence_ids = rows[0].evidence_ids.filter(id => id !== 'control-3-d2-d7'); }],
        ['missing parent proof', (rows: CommentaryEffect[]) => { rows[0].evidence_ids = rows[0].evidence_ids.filter(id => id !== 'legal-0-d4f5'); }],
        ['invalid parent use', (rows: CommentaryEffect[]) => { rows[1].through_plies = [2]; }],
        ['invalid parent ownership', (rows: CommentaryEffect[]) => { rows[1].actor.position_id = 'p0'; }],
        ['cyclic attack parent', (rows: CommentaryEffect[]) => { rows[1] = structuredClone(rows[0]); }],
        ['attack field on functional scope', (rows: CommentaryEffect[]) => { rows[1].attack_ply = 2; }],
      ] as const) {
        const forged = structuredClone(attached);
        mutate(forged.candidates[0].detail.effects);
        assert.equal(decodeCommentarySelection(forged, observations, context), undefined, name);
      }
      const reply = structuredClone(attached);
      reply.candidates[0].detail.effects = [{ ...history, attack_ply: 2, actor: anchorPiece('a8'),
        target: anchorPiece('h2'), square: 'h2', occurrence_ids: ['o0', 'o1', 'o3'] }];
      assert.ok(decodeCommentarySelection(reply, observations, context), 'the immediate reply needs no functional parent');
      const originalAttack = structuredClone(reply);
      const originalEffect = originalAttack.candidates[0].detail.effects[0];
      delete originalEffect.attack_ply;
      originalEffect.target = anchorPiece('a2');
      originalEffect.square = 'a2';
      assert.ok(decodeCommentarySelection(originalAttack, observations, context),
        'an existing root attack keeps its original no-field contract');
      const foreignParent = structuredClone(attached);
      const other = structuredClone(selection.candidates[0]);
      other.id = 'another-comparison';
      foreignParent.candidates[0].detail.effects = [history];
      foreignParent.candidates.push(other);
      assert.equal(decodeCommentarySelection(foreignParent, observations, context), undefined,
        'a functional effect in another candidate cannot authorize this history');
      const missingReplyProof = structuredClone(reply);
      missingReplyProof.candidates[0].detail.effects[0].evidence_ids = history.evidence_ids.filter(id => id !== 'control-2-h8-h2');
      assert.equal(decodeCommentarySelection(missingReplyProof, observations, context), undefined);
      const falseControl = structuredClone(observations);
      falseControl.positions![3].controls.pieces.find(row => row.piece.square === 'd2')!.squares = [];
      assert.equal(decodeCommentarySelection(attached, falseControl, context), undefined,
        'a referenced source cannot replace the actual same-piece attack');
    }
  }
});

test('a response inquiry uses exact geometric pressure or its owned en-passant capture', () => {
  const rootFen = '7k/8/8/8/3p4/8/4P3/7K w - - 0 1';
  const childFen = '7k/8/8/8/3pP3/8/8/7K b - e3 0 1';
  const legal = [
    { uci: 'd4e3', destination: 'e3', captured_square: 'e4', source_id: 'ep' },
    ...['d4d3', 'h8g8', 'h8g7', 'h8h7'].map(uci => ({ uci, destination: uci.slice(2), source_id: uci })),
  ];
  // Already admitted position resources isolate selection's ownership check.
  // Full wire decoding of freshly emitted runtime responses is checked separately.
  const observations = {
    paths: [{ id: 'played', kind: 'played', startFen: rootFen,
      occurrenceIds: ['e4'], positionIds: ['root', 'child'],
      moves: [{ uci: 'e2e4', label: 'e4', fenAfter: childFen }] }],
    statements: [],
    positions: [
      { id: 'root', fen: rootFen, controls: { pieces: [] } },
      { id: 'child', fen: childFen, controls: { pieces: [] }, legal_moves: { complete: true, moves: legal } },
    ],
  } as unknown as MoveReviewObservations;
  const inquiry = {
    id: 'response', scope: 'target_response', source: 'automatic_observation',
    root_occurrence_id: 'e4', root_move: 'e2e4', root_move_occurrence_id: 'e4', child_position_id: 'child',
    original_target: { position_id: 'root', side: 'white', role: 'pawn', square: 'e2' },
    target: { position_id: 'child', side: 'white', role: 'pawn', square: 'e4' },
    related_inquiry_ids: [], pressure_source_ids: [] as string[], capture_moves: ['d4e3'],
    decline_moves: legal.slice(1).map(move => move.uci),
    move_source_ids: Object.fromEntries(legal.map(move => [move.uci, move.source_id])),
    reference_move: 'd4e3', alternative_move: 'h8g8', state: 'deferred', stop_reason: null, answer: null,
  };
  const selection = { version: 2, candidates: [], scenes: [], response_inquiries: [inquiry] };
  const context = { rootFen, playedMove: 'e2e4', engineProfile: 'inline-selection-test' };
  const decode = () => decodeCommentarySelection(selection, observations, context);
  assert.ok(decode(), 'the exact child EP capture supplies its own legal source');
  for (const change of [
    (row: typeof inquiry) => { row.target.square = 'd4'; },
    (row: typeof inquiry) => { row.child_position_id = 'root'; },
    (row: typeof inquiry) => { row.root_move_occurrence_id = 'foreign'; },
    (row: typeof inquiry) => { row.move_source_ids.d4e3 = 'foreign'; },
    (row: typeof inquiry) => { row.pressure_source_ids = ['ep']; },
    (row: typeof inquiry) => { row.capture_moves = ['d4d3']; },
  ]) {
    const forged = structuredClone(selection);
    change(forged.response_inquiries[0]);
    assert.equal(decodeCommentarySelection(forged, observations, context), undefined);
  }
  legal[0].destination = 'e4';
  assert.equal(decode(), undefined, 'empty geometry cannot justify an ordinary capture');
  legal[0].destination = 'e3';
  context.rootFen = observations.paths[0].startFen = observations.positions![0].fen =
    '7k/8/8/8/3p4/3b4/4P3/7K w - - 0 1';
  observations.paths[0].moves[0].fenAfter = observations.positions![1].fen =
    '7k/8/8/8/3pP3/3b4/8/7K b - e3 0 1';
  legal.splice(1, 1, ...['d3c4', 'd3b5', 'd3a6', 'd3c2', 'd3b1', 'd3e2', 'd3f1']
    .map(uci => ({ uci, destination: uci.slice(2), source_id: uci })));
  legal.push({ uci: 'd3e4', destination: 'e4', captured_square: 'e4', source_id: 'bishop-capture' });
  inquiry.capture_moves = ['d4e3', 'd3e4'];
  inquiry.decline_moves = legal.filter(move => !('captured_square' in move) || move.captured_square !== 'e4')
    .map(move => move.uci);
  inquiry.move_source_ids = Object.fromEntries(legal.map(move => [move.uci, move.source_id]));
  const pressure = (square: string, source: string) => ({
    piece: { side: 'black', role: 'bishop', square: 'd3' }, squares: [square], source_ids: [source],
  });
  observations.positions![0].controls.pieces.push(pressure('e2', 'root-pressure') as any);
  observations.positions![1].controls.pieces.push(pressure('e4', 'child-pressure') as any);
  inquiry.pressure_source_ids = ['root-pressure', 'child-pressure'];
  assert.ok(decode(), 'geometric sources are the exact root and child union');
  for (const sources of [[], ['root-pressure'], ['child-pressure'], ['root-pressure', 'child-pressure', 'ep']]) {
    inquiry.pressure_source_ids = sources;
    assert.equal(decode(), undefined, 'EP does not excuse missing or unrelated geometric sources');
  }
});


test('explanation positions distinguish identical SAN in different lines and preview without changing the position', () => {
  const candidate = primary();
  candidate.detail.effects = [{
    scope: 'pair_difference', line_ids: ['played', 'alternative'], through_plies: [2, 2],
    prefix_moves_uci: [], actor: null, target: null, square: 'h6',
    occurrence_ids: ['reply', 'alternative-reply'], evidence_ids: [],
  }];
  const before = structuredClone(candidate);
  const previews: unknown[] = [], pins: unknown[] = [];
  let clears = 0;
  const view = render([candidate, pv()], 'en-US', undefined, {
    previewFrame: frame => previews.push(frame), pinFrame: frame => pins.push(frame),
    clearPreview: () => clears++,
  });
  const links = nodes(view).filter(node => node.sel === 'button.move-review__explanation-position');
  assert.equal(links.length, 2);
  assert.ok(text(links[0]).includes('1…h6') && text(links[0]).includes('d3 line'));
  assert.ok(text(links[1]).includes('1…h6') && text(links[1]).includes('e3 line'));
  const focus = links[1].data?.on?.focus;
  if (typeof focus === 'function') focus(new Event('focus'), links[1]);
  assert.deepEqual(previews, [{ proofId: 'alternative', ply: 2, phase: 'after' }]);
  assert.deepEqual(pins, [], 'focus previews without pinning');
  const click = links[1].data?.on?.click;
  if (typeof click === 'function') click(new Event('click'), links[1]);
  const card = nodes(view).find(node => node.sel === 'article.move-review__pv-reading')!;
  const leave = card.data?.on?.mouseleave;
  if (typeof leave === 'function') leave(new Event('mouseleave'), card);
  assert.deepEqual(pins, [{ proofId: 'alternative', ply: 2, phase: 'after' }]);
  assert.equal(clears, 1);
  assert.deepEqual(candidate, before);
});

test('the in-card board keeps its owned position, before/after selection, and keyboard dismissal', () => {
  const candidate = primary();
  const frame = { proofId: 'alternative', ply: 2, phase: 'after' as const };
  const pins: unknown[] = [];
  let cleared = 0;
  const actions = { pinFrame: (frame: unknown) => pins.push(frame), clearPreview: () => cleared++, clearPin: () => cleared++ };
  const view = render([candidate, pv()], 'en-US', undefined, actions, [], [],
    { evidenceExpanded: false, pinnedFrame: frame });
  const preview = nodes(view).find(node => node.sel === 'section.move-review__comparison-preview')!;
  assert.ok(preview && text(preview).includes('Alternative · 1…h6'));
  const player = nodes(preview).find(node => node.sel === 'input' && node.data?.attrs?.type === 'range')!;
  assert.equal(player.data?.props?.value, 2);
  const input = new Event('input');
  Object.defineProperty(input, 'target', { value: { value: '0' } });
  trigger(player, 'input', input);
  assert.deepEqual(pins, [{ proofId: 'alternative', ply: 1, phase: 'before' }]);
  const keydown = preview.data?.on?.keydown;
  const escape = new Event('keydown', { cancelable: true });
  Object.defineProperty(escape, 'key', { value: 'Escape' });
  if (typeof keydown === 'function') keydown(escape, preview);
  assert.equal(escape.defaultPrevented, true);
  assert.equal(cleared, 2);
  for (const invalid of [
    { ...frame, proofId: 'foreign' }, { ...frame, ply: 0 }, { ...frame, ply: 3 },
    { ...frame, ply: 1.5 }, { ...frame, highlightKey: 'unknown' },
  ]) {
    const invalidView = render([candidate, pv()], 'en-US', undefined, {}, [], [],
      { evidenceExpanded: false, hoveredFrame: invalid });
    assert.ok(!nodes(invalidView).some(node => node.sel === 'section.move-review__comparison-preview'));
  }
  const card = nodes(view).find(node => node.sel === 'article.move-review__pv-reading')!;
  const prose = (card.children ?? []).filter((node): node is VNode => typeof node === 'object' && !!node &&
    node.sel === 'p.move-review__commentary' && node.data?.attrs?.lang === 'en-US');
  assert.ok(prose.some(node => text(node) === explanation), 'the complete commentary remains a direct paragraph');
  const records = nodes(card).find(node => node.sel === 'details.move-review__comparison-records')!;
  assert.ok(records && !records.data?.attrs?.open);
  candidate.detail.effects = [{ scope: 'line_history', line_ids: ['alternative'], through_plies: [2],
    prefix_moves_uci: [], actor: null, target: null, square: 'h6', occurrence_ids: ['alternative-reply'], evidence_ids: [] }];
  const relationView = render([candidate, pv()], 'en-US', undefined, {}, [], [], {
    evidenceExpanded: false, pinnedFrame: { ...frame, inspectSquare: 'h6' },
  });
  const relationLink = nodes(relationView).find(node => node.sel === 'button.move-review__explanation-position')!;
  assert.equal(relationLink.data?.attrs?.['aria-pressed'], 'false', 'a relation inspection is not a plain-position pin');
});

test('SAN uses the exact before-position for special moves and distinct piece choices', () => {
  const cases = [
    ['r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1', 'e1g1', 'O-O'],
    ['r3k2r/8/8/8/8/8/8/R4RK1 b kq - 1 1', 'e8c8', 'O-O-O'],
    ['7k/5K2/6Q1/8/8/8/8/8 w - - 0 1', 'g6g7', 'Qg7#'],
    ['7k/8/5KQ1/8/8/8/8/8 w - - 0 1', 'g6h6', 'Qh6+'],
    ['7k/P7/8/8/8/8/8/7K w - - 0 1', 'a7a8q', 'a8=Q+'],
    ['7k/P7/8/8/8/8/8/7K w - - 0 1', 'a7a8n', 'a8=N'],
    ['7k/8/8/3pP3/8/8/8/7K w - d6 0 1', 'e5d6', 'exd6'],
    ['7k/8/8/8/8/R7/8/R6K w - - 0 1', 'a1a2', 'R1a2'],
    ['7k/8/8/8/8/1N6/8/1N3N1K w - - 0 1', 'b1d2', 'Nb1d2'],
    ['7k/8/8/8/8/8/8/1N3N1K w - - 0 1', 'b1d2', 'Nbd2'],
    ['6k1/8/8/8/8/4N3/8/1N5K w - - 2 2', 'b1d2', 'Nd2'],
  ];
  for (const [fen, move, expected] of cases) assert.equal(moveReviewSan(fen, move), expected);
  assert.equal(moveReviewSan(startFen, 'e2e5'), '?');
});

test('localized commentary retains exact SAN and occurrence links in both interface locales', () => {
  const original = [primary(), question(), pv()];
  const before = structuredClone(original);
  const san: string[][] = [];
  for (const locale of ['en-US', 'ko-KR'] as const) {
    const pinned: unknown[] = [];
    const view = render(original, locale, undefined, { pinFrame: frame => pinned.push(frame) });
    const all = nodes(view);
    assert.equal(view.data?.attrs?.lang, locale);
    assert.ok(all.some(node => node.sel === 'h2#analyse-move-review-title' && text(node) === moveReviewCopy(locale).title));
    const paragraphs = all.filter(node => node.sel === 'p.move-review__commentary' && node.data?.attrs?.lang === locale);
    assert.ok(paragraphs.some(node => locale === 'en-US' ? text(node) === explanation : /[가-힣]/.test(text(node))));
    assert.ok(paragraphs.every(node => !/\b[a-h][1-8][a-h][1-8][qrbn]?\b/.test(text(node))));
    san.push(all.filter(node => node.sel === 'button.move-review__inline-san').map(text).sort());
    const link = all.find(node => node.sel?.startsWith('button') && text(node) ===
      `${locale === 'ko-KR' ? '비교 수순 보기' : 'Inspect the comparison line'} · d3`)!;
    assert.ok(link);
    const click = link.data?.on?.click;
    if (typeof click === 'function') click(new Event('click'), link);
    assert.deepEqual(pinned, [{ proofId: 'played', ply: 1, phase: 'after' }]);
  }
  assert.deepEqual(san[0], san[1]);
  assert.deepEqual(original, before, 'rendering must not modify UCI or evidence ownership');
  const documentBefore = Object.getOwnPropertyDescriptor(globalThis, 'document');
  try {
    Object.defineProperty(globalThis, 'document', { configurable: true, value: { documentElement: { lang: 'ko' } } });
    assert.equal(normalizeMoveReviewLocale(), 'ko-KR');
    assert.equal(normalizeMoveReviewLocale('en'), 'en-US');
    Object.defineProperty(globalThis, 'document', { configurable: true, value: { documentElement: { lang: 'en' } } });
    assert.equal(normalizeMoveReviewLocale(), 'en-US');
  } finally {
    if (documentBefore) Object.defineProperty(globalThis, 'document', documentBefore);
    else Reflect.deleteProperty(globalThis, 'document');
  }
});
