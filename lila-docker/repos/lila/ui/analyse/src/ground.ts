import { h, type VNode } from 'snabbdom';
import type { Elements } from '@lichess-org/chessground/types';
import resizeHandle from 'lib/chessgroundResize';
import { storage } from 'lib/storage';
import type AnalyseCtrl from './ctrl';
import * as Prefs from 'lib/prefs';
import { Chessground as makeChessground } from '@lichess-org/chessground';
import { boardCoordsToViewConfig } from './boardWorkspace';

export const render = (ctrl: AnalyseCtrl): VNode =>
  h('div.cg-wrap.cgv' + ctrl.cgVersion.js, {
    hook: {
      insert: vnode => ctrl.setChessground(makeChessground(vnode.elm as HTMLElement, makeConfig(ctrl))),
    },
  });

function makeConfig(ctrl: AnalyseCtrl): CgConfig {
  const d = ctrl.data,
    pref = d.pref,
    labelConfig = boardCoordsToViewConfig(ctrl.boardCoords()),
    opts = ctrl.makeCgOpts();
  const config = {
    turnColor: opts.turnColor,
    fen: opts.fen,
    check: opts.check,
    lastMove: opts.lastMove,
    orientation: ctrl.bottomColor(),
    coordinates: labelConfig.coordinates,
    coordinatesOnSquares: labelConfig.coordinatesOnSquares,
    addPieceZIndex: false,
    addDimensionsCssVarsTo: document.body,
    touchIgnoreRadius: 0,
    viewOnly: false,
    movable: {
      free: false,
      color: opts.movable!.color,
      dests: opts.movable!.dests,
      showDests: pref.destination,
      rookCastle: pref.rookCastle,
    },
    events: {
      move: ctrl.userMove,
      insert(elements: Elements) {
        resizeHandle(elements, Prefs.ShowResizeHandle.Always, ctrl.node.ply);
      },
    },
    premovable: {
      enabled: opts.premovable!.enabled,
      showDests: pref.destination,
      events: {
        set: ctrl.onPremoveSet,
      },
    },
    draggable: {
      enabled: pref.moveEvent !== Prefs.MoveEvent.Click,
      showGhost: pref.highlight,
    },
    selectable: {
      enabled: pref.moveEvent !== Prefs.MoveEvent.Drag,
    },
    drawable: {
      enabled: true,
      eraseOnMovablePieceClick: true,
      defaultSnapToValidMove: storage.boolean('arrow.snap').getOrDefault(true),
    },
    highlight: {
      lastMove: pref.highlight,
      check: pref.highlight,
    },
    animation: {
      duration: pref.animationDuration,
    },
    disableContextMenu: true,
  };

  return config;
}
