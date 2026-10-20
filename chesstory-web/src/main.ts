import { h, init, attributesModule, classModule, propsModule, eventListenersModule, type VNode } from 'snabbdom';
import { Chessground } from '@lichess-org/chessground';
import type { Api } from '@lichess-org/chessground/api';
import type { Config } from '@lichess-org/chessground/config';
import { chessgroundDests } from 'chessops/compat';
import { parseSan } from 'chessops/san';
import { makeUci, parseSquare } from 'chessops/util';
import { GameRecord, position, type GameNode } from './game';
import { BrowserEngine } from './engine/browserEngine';
import { moveReviewEngineProfile } from './engine/types';
import { MoveReviewCoordinator } from './review/moveReviewCoordinator';
import { createMoveReviewRuntimeSource } from './review/moveReviewRuntimeSource';
import { moveReviewBudgets, moveReviewCopy, moveReviewSans, updateMoveReviewViewForJob, type MoveReviewJobState,
  type MoveReviewViewState, type MoveReviewLocale } from './review/moveReview';
import { renderMoveReview, renderMoveReviewBoard, type MoveReviewPanelProps } from './review/view/moveReview';
import type { Key, Color, Tree } from './review/types';

const patch = init([attributesModule, classModule, propsModule, eventListenersModule]);
let vnode: VNode | Element = document.getElementById('app')!;
let game = GameRecord.empty(), path = '', source = '', message = '', moveInput = '';
let locale: MoveReviewLocale = 'ko-KR', orientation: Color = 'white', dark = false;
let job: MoveReviewJobState = { kind: 'idle', reason: 'root' };
let view: MoveReviewViewState = { evidenceExpanded: false };
let coordinator: MoveReviewCoordinator | undefined, reviewEngine: BrowserEngine | undefined;
let liveEngine: BrowserEngine | undefined, engineOn = false, engineState = 'Off';
let evaluation: Tree.LocalEval | undefined, generation = 0;
let reviewStarted = 0, reviewElapsed = 0;
let promotion: { from: Key; to: Key } | undefined;
const storageKey = 'chesstories.workspace.v1';
try {
  const saved = JSON.parse(localStorage.getItem(storageKey) ?? 'null');
  if (saved) {
    game = GameRecord.parse(saved.pgn);
    path = game.nodes.has(saved.path) ? saved.path : '';
    locale = saved.locale === 'en-US' ? 'en-US' : 'ko-KR';
    dark = saved.dark === true;
  }
} catch { message = '저장된 기보를 읽지 못했습니다. PGN 파일을 다시 불러오세요.'; }

const t = (ko: string, en: string) => locale === 'ko-KR' ? ko : en;
const current = () => game.nodes.get(path)!;
const button = (text: string, action: () => void, disabled = false, attrs: Record<string, string> = {}) =>
  h('button', { attrs: { type: 'button', disabled, ...attrs }, on: { click: action } }, text);

function save(): void {
  try { localStorage.setItem(storageKey, JSON.stringify({ pgn: game.export(), path, locale, dark })); }
  catch { message = t('자동 저장에 실패했습니다. PGN 저장으로 보관하세요.', 'Autosave failed. Download the PGN to keep your work.'); }
}

function stopReview(): void {
  const previous = coordinator; coordinator = undefined; previous?.destroy();
  reviewEngine?.destroy(); reviewEngine = undefined;
  job = { kind: 'idle', reason: 'root' };
  view = { evidenceExpanded: false };
  reviewStarted = reviewElapsed = 0;
}

function go(next: string): void {
  if (!game.nodes.has(next)) return;
  stopReview(); path = next; promotion = undefined;
  save(); startAnalysis(); redraw();
}

function load(): void {
  try {
    const next = GameRecord.parse(source);
    stopReview(); game = next; path = ''; message = t('기보를 불러왔습니다.', 'Game imported.');
    save(); startAnalysis(); redraw();
  } catch (error) { message = error instanceof Error ? error.message : 'Import failed'; redraw(); }
}

function play(uci: string): void {
  try {
    const next = game.append(current(), uci);
    moveInput = ''; message = ''; go(next.path);
  } catch (error) { message = String(error); redraw(); }
}

function boardMove(from: Key, to: Key): void {
  const pos = position(current().fen);
  if (pos.board.get(parseSquare(from)!)?.role === 'pawn' && (to[1] === '8' || to[1] === '1')) {
    promotion = { from, to }; redraw();
  } else play(from + to);
}

function startAnalysis(): void {
  const ownGeneration = ++generation;
  liveEngine?.destroy(); liveEngine = undefined; evaluation = undefined;
  if (!engineOn || coordinator?.isPreemptingLiveEngine()) { engineState = engineOn ? 'Paused' : 'Off'; return; }
  const node = current();
  if (position(node.fen).isEnd()) { engineState = 'Finished'; return; }
  engineState = 'Running';
  liveEngine = new BrowserEngine();
  void liveEngine.analyse(game.root.fen, game.ancestors(path).slice(1).map(n => n.uci!), node.fen,
    ev => {
      if (ownGeneration !== generation) return;
      evaluation = ev;
      if (ev.bestmove !== undefined) engineState = 'Ready';
      redraw();
    }, error => {
      if (ownGeneration !== generation) return;
      engineState = 'Unavailable'; message = error; redraw();
    });
}

function explain(): void {
  const subject = game.subject(path);
  if (!subject) return;
  stopReview();
  reviewStarted = performance.now();
  reviewEngine = new BrowserEngine();
  const engine = reviewEngine;
  coordinator = new MoveReviewCoordinator(locale, {
    engineProfile: moveReviewEngineProfile,
    source: createMoveReviewRuntimeSource(engine.execute),
    suspendLiveEngine() { ++generation; liveEngine?.destroy(); liveEngine = undefined; engineState = engineOn ? 'Paused' : 'Off'; },
    resumeLiveEngine() { engine.destroy(); if (coordinator) startAnalysis(); },
    stateChanged(next) {
      updateMoveReviewViewForJob(view, job, next);
      job = next;
      if (reviewStarted && ['completed', 'position-action', 'abstained', 'fault'].includes(job.kind))
        reviewElapsed = performance.now() - reviewStarted;
      redraw();
    },
  });
  coordinator.settle(subject); coordinator.activate();
}

function panelProps(): MoveReviewPanelProps {
  return { job, view, copy: moveReviewCopy(locale), locale, orientation,
    liveEnginePaused: !!coordinator?.isPreemptingLiveEngine(), actions: {
      selectCandidate(uci) { view.selectedCandidateUci = uci; redraw(); },
      toggleEvidence() { view.evidenceExpanded = !view.evidenceExpanded; redraw(); },
      toggleProof(id) { view.expandedProofId = view.expandedProofId === id ? undefined : id; redraw(); },
      previewFrame(frame) { view.hoveredFrame = frame; redraw(); },
      clearPreview() { view.hoveredFrame = undefined; redraw(); },
      pinFrame(frame) { view.pinnedFrame = frame; view.hoveredFrame = undefined; redraw(); },
      clearPin() { view.pinnedFrame = undefined; redraw(); },
      inspectFrame(frame) { view.boardFrame = frame; redraw(); },
      clearInspection() { view.boardFrame = undefined; redraw(); },
      retry: explain, cancel: () => { coordinator?.deactivate(); redraw(); },
    } };
}

function board(): VNode {
  const node = current(), pos = position(node.fen);
  const config: Config = {
    fen: node.fen, orientation, turnColor: pos.turn, check: pos.isCheck(), coordinates: true,
    animation: { enabled: false }, disableContextMenu: true,
    lastMove: node.uci ? [node.uci.slice(0, 2) as Key, node.uci.slice(2, 4) as Key] : undefined,
    movable: { free: false, color: pos.turn, dests: promotion ? new Map() : chessgroundDests(pos), events: { after: boardMove } },
    premovable: { enabled: false }, drawable: { enabled: true },
  };
  return h('div.board-square', [h('div.cg-wrap.is2d', { key: 'board', hook: {
    insert(v) { (v.elm as HTMLElement & { cg?: Api }).cg = Chessground(v.elm as HTMLElement, config); },
    update(_old, v) { (v.elm as HTMLElement & { cg?: Api }).cg?.set(config); },
    destroy(v) { (v.elm as HTMLElement & { cg?: Api }).cg?.destroy(); },
  } })]);
}

function moveLabel(node: GameNode): string {
  return `${Math.ceil(node.ply / 2)}${node.ply % 2 ? '.' : '…'} ${node.san}`;
}

function moves(parent: GameNode, level = 0): VNode[] {
  if (!parent.children.length) return [];
  const first = parent.children[0];
  const nodeButton = (node: GameNode) => h('button.move', { key: node.path,
    class: { selected: node.path === path }, attrs: { type: 'button', 'aria-current': node.path === path ? 'step' : 'false' },
    on: { click: () => go(node.path) } }, moveLabel(node));
  return [nodeButton(first), ...parent.children.slice(1).map(child => h('span.variation', { key: child.path }, [
    '(', nodeButton(child), ...moves(child, level + 1), ')',
  ])), ...moves(first, level)];
}

function download(): void {
  const url = URL.createObjectURL(new Blob([game.export()], { type: 'application/x-chess-pgn;charset=utf-8' }));
  const link = document.createElement('a'); link.href = url; link.download = 'chesstories.pgn'; link.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}

function redraw(): void {
  const node = current(), props = panelProps(), inspection = renderMoveReviewBoard(props);
  document.documentElement.dataset.theme = dark ? 'dark' : 'light';
  document.documentElement.lang = locale === 'ko-KR' ? 'ko' : 'en';
  vnode = patch(vnode, h('div#app', [
    h('header.app-header', [h('div.brand', 'Chesstories'), h('span.subtitle', t('기보와 해설', 'Game & commentary')),
      h('div.preferences', [
        h('select', { attrs: { 'aria-label': 'Language' }, props: { value: locale },
          on: { change: event => { locale = (event.target as HTMLSelectElement).value as MoveReviewLocale; if (coordinator) coordinator.locale = locale; save(); redraw(); } } },
        [h('option', { attrs: { value: 'ko-KR' }, props: { selected: locale === 'ko-KR' } }, '한국어'),
          h('option', { attrs: { value: 'en-US' }, props: { selected: locale === 'en-US' } }, 'English')]),
        button(t(dark ? '밝게' : '어둡게', dark ? 'Light' : 'Dark'), () => { dark = !dark; save(); redraw(); }),
      ]),
    ]),
    h('main.workspace', [
      h('section.position-column', { attrs: { 'aria-label': t('체스판과 기보', 'Board and game') } }, [
        h('div.position-heading', [h('h1', game.title() || t('분석 기보', 'Analysis game')), h('span', node.san ? moveLabel(node) : t('시작 국면', 'Starting position'))]),
        inspection?.board ?? board(),
        ...(inspection ? [inspection.notice] : []),
        h('nav.board-controls', { attrs: { 'aria-label': t('기보 이동', 'Game navigation') } }, [
          button('⏮', () => go(''), !path, { 'aria-label': t('처음', 'First position') }),
          button('←', () => go(path.slice(0, -2)), !path, { 'aria-label': t('이전 수', 'Previous move') }),
          button('→', () => go(node.children[0].path), !node.children.length, { 'aria-label': t('다음 수', 'Next move') }),
          button('⏭', () => { let last = node; while (last.children.length) last = last.children[0]; go(last.path); }, !node.children.length, { 'aria-label': t('마지막', 'Last position') }),
          button(t('방향 전환', 'Flip board'), () => { orientation = orientation === 'white' ? 'black' : 'white'; redraw(); }),
        ]),
        ...(promotion ? [h('div.promotion', { attrs: { role: 'group', 'aria-label': t('승격 기물', 'Promotion piece') } },
          ['q', 'r', 'b', 'n'].map((role, i) => button(['Queen', 'Rook', 'Bishop', 'Knight'][i], () => {
            const pending = promotion!; promotion = undefined; play(pending.from + pending.to + role);
          })))] : []),
        h('form.move-entry', { on: { submit: event => {
          event.preventDefault(); const move = parseSan(position(node.fen), moveInput.trim());
          play(move ? makeUci(move) : moveInput.trim());
        } } }, [h('input', { attrs: { 'aria-label': t('수 입력', 'Enter move'), placeholder: 'e4, Nf3, e2e4', autocomplete: 'off' },
          props: { value: moveInput }, on: { input: event => moveInput = (event.target as HTMLInputElement).value } }),
          h('button', { attrs: { type: 'submit' } }, t('두기', 'Play move'))]),
        h('section.engine-reference', [h('div.section-heading', [h('h2', 'Engine reference'),
          button(`Engine / ${engineOn ? 'On' : 'Off'}`, () => { engineOn = !engineOn; startAnalysis(); redraw(); }, false, { 'aria-pressed': String(engineOn) })]),
          h('p.engine-state', `${engineState}${evaluation ? ` · D${evaluation.depth}` : ''} · Stockfish 18 · 2 threads`),
          ...(evaluation?.pvs.map(pv => h('p.engine-line', [h('strong', pv.mate !== undefined ? `#${pv.mate}` : `${(pv.cp ?? 0) >= 0 ? '+' : ''}${((pv.cp ?? 0) / 100).toFixed(2)}`),
            h('span', moveReviewSans(node.fen, pv.moves).slice(0, 12).join(' '))])) ?? []),
        ]),
        h('section.game-score', [h('div.section-heading', [h('h2', t('기보', 'Game score')), button(t('PGN 저장', 'Download PGN'), download)]),
          h('div.moves', moves(game.root)),
          ...(node.data?.comments?.map(comment => h('p.game-comment', comment)) ?? []),
        ]),
        h('details.import', [h('summary', t('PGN · FEN 불러오기', 'Import PGN · FEN')),
          h('textarea', { attrs: { 'aria-label': 'PGN or FEN', rows: 6, spellcheck: 'false', placeholder: t('기보(PGN) 또는 국면(FEN)을 붙여 넣으세요.', 'Paste a PGN game or FEN position.') },
            props: { value: source }, on: { input: event => source = (event.target as HTMLTextAreaElement).value } }),
          h('div.import-actions', [button(t('불러오기', 'Import'), load), h('label.file-button', [t('파일 선택', 'Choose file'),
            h('input', { attrs: { type: 'file', accept: '.pgn,.txt' }, on: { change: async event => {
              const file = (event.target as HTMLInputElement).files?.[0]; if (!file) return;
              if (file.size > 1_000_000) { message = '최대 1MB입니다.'; redraw(); return; }
              try { source = await file.text(); load(); } catch { message = '파일을 읽지 못했습니다.'; redraw(); }
            } } })])]),
          h('p.help', t('한 경기씩 불러옵니다. 변형·주석은 PGN 저장에 함께 보존됩니다.', 'Import one game at a time. Variations and comments are preserved in PGN downloads.')),
        ]),
        h('details.fen', [h('summary', t('현재 FEN', 'Current FEN')), h('input', { attrs: { readonly: true, 'aria-label': 'Current FEN' }, props: { value: node.fen } })]),
      ]),
      h('section.commentary-column', { attrs: { 'aria-label': t('해설', 'Commentary') } }, [
        h('div.commentary-heading', [h('h2', t('이 수의 의미', 'Why this move')),
          button(t(`해설 · ${moveReviewBudgets.singleMs / 1000}초`, `Explain · ${moveReviewBudgets.singleMs / 1000} seconds`),
            explain, !path || job.kind === 'loading'),
        ]),
        h('output#review-status', { attrs: { 'aria-live': 'polite', 'data-state': job.kind, 'data-elapsed-ms': Math.round(reviewElapsed) } },
          job.kind === 'loading' ? t('수의 목적과 응수를 살펴보고 있습니다.', 'Examining the move and its replies…') :
          reviewElapsed ? `${job.kind} · ${(reviewElapsed / 1000).toFixed(2)}s` : t('기보에서 수를 고르거나 판 위에 수를 두세요.', 'Choose a move in the score or play on the board.')),
        renderMoveReview(props),
      ]),
    ]),
    h('footer', [h('p.message', { attrs: { role: 'status' } }, message),
      h('span', t('이 브라우저에 자동 저장됩니다. 기보를 보관하려면 PGN 저장을 사용하세요.', 'Autosaved in this browser. Download the PGN to keep a portable copy.'))]),
  ]));
}

window.addEventListener('keydown', event => {
  if ((event.target as HTMLElement).closest('input, textarea, select, button') || event.altKey || event.ctrlKey || event.metaKey) return;
  if (event.key === 'ArrowLeft' && path) { event.preventDefault(); go(path.slice(0, -2)); }
  if (event.key === 'ArrowRight' && current().children.length) { event.preventDefault(); go(current().children[0].path); }
});
window.addEventListener('pagehide', () => { coordinator?.destroy(); reviewEngine?.destroy(); liveEngine?.destroy(); });
redraw();
