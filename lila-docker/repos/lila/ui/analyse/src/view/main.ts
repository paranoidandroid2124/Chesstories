import * as licon from 'lib/licon';
import { type VNode, hl } from 'lib/view';
import { playable } from 'lib/game';
import { render as trainingView } from './roundTraining';
import type AnalyseCtrl from '../ctrl';
import { view as keyboardView } from '../keyboard';

import { viewContext, renderBoard, renderMain, renderTools, renderUnderboard } from './components';
import { boardRectSig } from './boardRect';
import { displayColumns } from 'lib/device';
import { renderControls } from './controls';

let resizeCache: {
  columns: number;
  main: HTMLElement | null;
  board: HTMLElement | null;
  tools: HTMLElement | null;
  side: HTMLElement | null;
  controls: HTMLElement | null;
  underboard: HTMLElement | null;
  observer: ResizeObserver | null;
  raf: number | null;
  boardRectSig: string;
};

export default function () {
  return function (ctrl: AnalyseCtrl): VNode {
    resizeCache ??= resizeHandler(ctrl);
    return analyseView(ctrl);
  };
}

function analyseView(ctrl: AnalyseCtrl): VNode {
  bindLayoutObserver(ctrl);
  const ctx = viewContext(ctrl);
  return renderMain(
    ctx,
    ctrl.keyboardHelp && keyboardView(ctrl),
    renderSide(),
    renderBoard(ctx),
    renderTools(ctx),
    renderControls(ctrl),
    renderUnderboard(ctx),
    trainingView(ctrl),
    !ctrl.synthetic &&
      playable(ctrl.data) &&
      hl(
        'div.back-to-game',
        hl(
          'a.button.button-empty.text',
          {
            attrs: {
              href: `/${ctrl.data.game.id}/${ctrl.data.player.color}`,
              'data-icon': licon.Back,
            },
          },
          'Back to game',
        ),
      ),
  );
}

function renderSide(): VNode | undefined {
  return undefined;
}

function resizeHandler(ctrl: AnalyseCtrl) {
  window.addEventListener('resize', () => {
    scheduleBoardSync(ctrl);
    if (resizeCache.columns !== displayColumns()) ctrl.redraw();
    resizeCache.columns = displayColumns();
  });
  return {
    columns: displayColumns(),
    main: null,
    board: null,
    tools: null,
    side: null,
    controls: null,
    underboard: null,
    observer: null,
    raf: null,
    boardRectSig: '',
  };
}

function bindLayoutObserver(ctrl: AnalyseCtrl): void {
  const main = document.querySelector('main.analyse') as HTMLElement | null;
  if (!main || resizeCache.main === main) return;

  resizeCache.observer?.disconnect();
  resizeCache.main = main;
  resizeCache.board = main.querySelector('.analyse__board') as HTMLElement | null;
  resizeCache.tools = main.querySelector('.analyse__tools') as HTMLElement | null;
  resizeCache.side = main.querySelector('.analyse__side') as HTMLElement | null;
  resizeCache.controls = main.querySelector('.analyse__controls') as HTMLElement | null;
  resizeCache.underboard = main.querySelector('.analyse__underboard') as HTMLElement | null;
  resizeCache.boardRectSig = '';

  resizeCache.observer = new ResizeObserver(() => scheduleBoardSync(ctrl));
  resizeCache.observer.observe(main);
  if (resizeCache.board) resizeCache.observer.observe(resizeCache.board);
  if (resizeCache.tools) resizeCache.observer.observe(resizeCache.tools);
  if (resizeCache.side) resizeCache.observer.observe(resizeCache.side);
  if (resizeCache.controls) resizeCache.observer.observe(resizeCache.controls);
  if (resizeCache.underboard) resizeCache.observer.observe(resizeCache.underboard);

  scheduleBoardSync(ctrl);
}

function scheduleBoardSync(ctrl: AnalyseCtrl): void {
  if (resizeCache.raf !== null) cancelAnimationFrame(resizeCache.raf);
  resizeCache.raf = requestAnimationFrame(() => {
    requestAnimationFrame(() => {
      resizeCache.raf = null;
      syncBoard(ctrl);
    });
  });
}

function syncBoard(ctrl: AnalyseCtrl): void {
  if (!resizeCache.main) return;

  if (!resizeCache.board || !resizeCache.main.contains(resizeCache.board))
    resizeCache.board = resizeCache.main.querySelector('.analyse__board') as HTMLElement | null;
  const board = resizeCache.board;
  if (!board) return;
  resizeCache.observer?.observe(board);

  const rect = board.getBoundingClientRect();
  const sig = boardRectSig(rect);
  if (sig === resizeCache.boardRectSig) return;

  resizeCache.boardRectSig = sig;
  ctrl.withCg(cg => cg.redrawAll());
}
