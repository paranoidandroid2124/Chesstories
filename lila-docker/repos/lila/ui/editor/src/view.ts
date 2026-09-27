import { h, type VNode } from 'snabbdom';
import * as licon from 'lib/licon';
import { copyMeInput, icon } from 'lib/view';
import type { MouchEvent, NumberPair } from '@lichess-org/chessground/types';
import { dragNewPiece } from '@lichess-org/chessground/drag';
import { eventPosition, opposite } from '@lichess-org/chessground/util';
import { parseFen } from 'chessops/fen';
import { parseSquare, makeSquare } from 'chessops/util';
import type EditorCtrl from './ctrl';
import chessground from './chessground';
import type { Selected, CastlingToggle, EditorState, EndgamePosition, OpeningPosition } from './interfaces';
import { fenToEpd } from 'lib/game/chess';

function castleCheckBox(ctrl: EditorCtrl, id: CastlingToggle, label: string): VNode {
  const input = h('input', {
    attrs: { type: 'checkbox', 'aria-label': `${id === id.toUpperCase() ? 'White' : 'Black'} ${label}` },
    props: { checked: ctrl.castlingToggles[id] },
    on: {
      change(e) {
        ctrl.setCastlingToggle(id, (e.target as HTMLInputElement).checked);
      },
    },
  });
  return h('label', [input, label]);
}

function optgroup(name: string, opts: VNode[]): VNode {
  return h('optgroup', { attrs: { label: name } }, opts);
}

function controls(ctrl: EditorCtrl, state: EditorState): VNode {
  const endgamePosition2option = function (pos: EndgamePosition): VNode {
    return h('option', { attrs: { value: pos.epd || pos.fen, 'data-fen': pos.fen } }, pos.name);
  };

  const buttonStart = (withIcon?: string) =>
    h(
      'button.button.button-empty',
      { on: { click: ctrl.startPosition }, attrs: { 'aria-label': 'Start position' } },
      [...(withIcon ? [icon('reload')] : []), 'Start position'],
    );
  const buttonClear = (withIcon?: string) =>
    h(
      'button.button.button-empty',
      { on: { click: ctrl.clearBoard }, attrs: { 'aria-label': 'Clear board' } },
      [...(withIcon ? [icon('trash')] : []), 'Clear board'],
    );

  return h('div.board-editor__tools', [
    h('div.metadata', [
      h(
        'div.color',
        h(
          'select',
          {
            attrs: { 'aria-label': 'Side to move' },
            on: {
              change(e) {
                ctrl.setTurn((e.target as HTMLSelectElement).value as Color);
              },
            },
            props: { value: ctrl.turn },
          },
          (['whitePlays', 'blackPlays'] as const).map(function (key) {
            return h(
              'option',
              {
                attrs: { value: key[0] === 'w' ? 'white' : 'black', selected: key[0] === ctrl.turn[0] },
              },
              key === 'whitePlays' ? 'White to play' : 'Black to play',
            );
          }),
        ),
      ),
      h('div.castling', [
        h('strong', 'Castling'),
        h('div', [
          h('span.castling__color', 'White'),
          castleCheckBox(ctrl, 'K', 'O-O'),
          castleCheckBox(ctrl, 'Q', 'O-O-O'),
        ]),
        h('div', [
          h('span.castling__color', 'Black'),
          castleCheckBox(ctrl, 'k', 'O-O'),
          castleCheckBox(ctrl, 'q', 'O-O-O'),
        ]),
      ]),
      h('div.enpassant', [
        h('label', { attrs: { for: 'enpassant-select' } }, 'En passant'),
        h(
          'select#enpassant-select',
          {
            on: {
              change(e) {
                ctrl.setEnPassant(parseSquare((e.target as HTMLSelectElement).value));
              },
            },
            props: { value: ctrl.epSquare ? makeSquare(ctrl.epSquare) : '' },
          },
          ['', ...[ctrl.turn === 'black' ? 3 : 6].flatMap(r => 'abcdefgh'.split('').map(f => f + r))].map(
            key =>
              h(
                'option',
                {
                  attrs: {
                    value: key,
                    selected: (key ? parseSquare(key) : undefined) === ctrl.epSquare,
                    hidden: Boolean(key && !state.enPassantOptions.includes(key)),
                    disabled: Boolean(key && !state.enPassantOptions.includes(key)) /*Safari*/,
                  },
                },
                key || 'None',
              ),
          ),
        ),
      ]),
    ]),
    ...(ctrl.cfg.embed || !ctrl.cfg.positions || !ctrl.cfg.endgamePositions
      ? []
      : [
        (() => {
          const positionOption = (pos: OpeningPosition): VNode =>
            h(
              'option',
              { attrs: { value: pos.epd || pos.fen, 'data-fen': pos.fen } },
              pos.eco ? `${pos.eco} ${pos.name}` : pos.name,
            );
          const epd = fenToEpd(state.fen);
          const value =
            (
              ctrl.cfg.positions.find(p => p.fen.startsWith(epd)) ||
              ctrl.cfg.endgamePositions.find(p => p.epd === epd)
            )?.epd || '';
          return h(
            'select.positions',
            {
              attrs: { 'aria-label': 'Starting position' },
              props: { value },
              on: {
                insert(vnode) {
                  (vnode.elm as HTMLSelectElement).value = fenToEpd(state.fen);
                },
                change(e) {
                  const el = e.target as HTMLSelectElement;
                  const value = el.selectedOptions[0].getAttribute('data-fen');
                  if (!value || !ctrl.setFen(value)) el.value = '';
                },
              },
            },
            [
              h('option', { attrs: { value: '' } }, 'Set the board'),
              optgroup('Popular openings', ctrl.cfg.positions.map(positionOption)),
              optgroup('Endgame positions', ctrl.cfg.endgamePositions.map(endgamePosition2option)),
            ],
          );
        })(),
      ]),
    ...(ctrl.cfg.embed
      ? [h('div.actions', [buttonStart(), buttonClear()])]
      : [
        h('div.actions', [
          buttonStart(licon.Reload),
          buttonClear(licon.Trash),
          h(
            'button.button.button-empty.text',
            {
              attrs: { 'aria-label': 'Flip board' },
              on: {
                click() {
                  ctrl.chessground!.toggleOrientation();
                  ctrl.onChange();
                },
              },
            },
            [icon('chasingArrows'), 'Flip board'],
          ),
          ...(ctrl.resetFen ? [h('button.button.button-empty.board-editor__undo', {
            attrs: { type: 'button' }, on: { click: ctrl.undoReset },
          }, [icon('reload'), 'Undo board reset'])] : []),
          h(
            'a',
            {
              attrs: {
                'aria-label': 'Review this position',
                'aria-disabled': String(!state.legalFen),
                ...(!state.legalFen ? { 'aria-describedby': 'editor-position-error', tabindex: -1 } : {}),
                rel: 'nofollow',
                ...(state.legalFen
                  ? { href: ctrl.makeAnalysisUrl(state.legalFen, ctrl.bottomColor()) }
                  : {}),
              },
              class: {
                button: true,
                'board-editor__review': true,
                text: true,
                disabled: !state.legalFen,
              },
            },
            [icon('microscope'), 'Review this position'],
          ),
        ]),
        ...(!state.legalFen ? [h('p#editor-position-error.board-editor__error', { attrs: { role: 'status' } },
          'Add one king for each side, keep pawns off the first and last ranks, and check whose turn it is.')] : []),
      ]),
  ]);
}

function inputs(ctrl: EditorCtrl, fen: string): VNode | undefined {
  if (ctrl.cfg.embed) return;
  return h('div.copyables', [
    h('p', [
      h('strong', 'FEN'),
      h('input', {
        attrs: { spellcheck: 'false', enterkeyhint: 'done', 'aria-label': 'FEN',
          'aria-invalid': String(ctrl.fenError), ...(ctrl.fenError ? { 'aria-describedby': 'editor-fen-error' } : {}) },
        props: { value: ctrl.fenInput ?? fen },
        on: {
          input(e) {
            ctrl.fenInput = (e.target as HTMLInputElement).value;
            ctrl.fenError = !parseFen(ctrl.fenInput.trim()).isOk;
            ctrl.redraw();
          },
          change(e) { ctrl.setFen((e.target as HTMLInputElement).value.trim()); },
          keydown(e) {
            if (e.key === 'Enter') {
              e.preventDefault();
              const el = e.target as HTMLInputElement;
              if (ctrl.setFen(el.value.trim())) el.blur();
            }
          },
        },
      }),
    ]),
    ...(ctrl.fenError ? [h('div#editor-fen-error.board-editor__fen-error', { attrs: { role: 'alert' } }, [
      h('span', 'Check the FEN format. Your board has not changed.'),
      h('button.button.button-empty', { attrs: { type: 'button' }, on: { click: () => {
        ctrl.fenInput = undefined; ctrl.fenError = false; ctrl.redraw();
      } } }, 'Restore board FEN'),
    ])] : []),
    h('p', [h('strong.name', 'URL'), copyMeInput(ctrl.makeEditorUrl(fen, ctrl.bottomColor()), { 'aria-label': 'Position URL' })]),
  ]);
}

// can be 'pointer', 'trash', or [color, role]
function selectedToClass(s: Selected): string {
  return s === 'pointer' || s === 'trash' ? s : s.join(' ');
}

let lastTouchMovePos: NumberPair | undefined;

function sparePieces(ctrl: EditorCtrl, color: Color, _orientation: Color, position: 'top' | 'bottom'): VNode {
  const selectedClass = selectedToClass(ctrl.selected());

  const pieces = ['king', 'queen', 'rook', 'bishop', 'knight', 'pawn'].map(function (role) {
    return [color, role];
  });

  return h(
    'div',
    { attrs: { class: ['spare', 'spare-' + position, 'spare-' + color].join(' ') } },
    ['pointer', ...pieces, 'trash'].map((s: Selected) => {
      const className = selectedToClass(s);
      const attrs = {
        class: className,
        ...(s !== 'pointer' && s !== 'trash'
          ? {
            'data-color': s[0],
            'data-role': s[1],
          }
          : {}),
      };
      const selectedSquare =
        selectedClass === className &&
        (!ctrl.chessground ||
          !ctrl.chessground.state.draggable.current ||
          !ctrl.chessground.state.draggable.current.newPiece);
      return h(
        'div',
        {
          attrs: { role: 'button', tabindex: 0, 'aria-label': s === 'pointer' ? 'Move pieces' : s === 'trash' ? 'Remove pieces' : `${color} ${s[1]}`, 'aria-pressed': String(selectedSquare) },
          class: {
            'no-square': true,
            pointer: s === 'pointer',
            trash: s === 'trash',
            'selected-square': selectedSquare,
          },
          on: {
            keydown: event => {
              if (event.key === 'Enter' || event.key === ' ') {
                event.preventDefault();
                ctrl.selected(s);
                ctrl.redraw();
              }
            },
            mousedown: onSelectSparePiece(ctrl, s, 'mouseup'),
            touchstart: onSelectSparePiece(ctrl, s, 'touchend'),
            touchmove: e => {
              lastTouchMovePos = eventPosition(e);
            },
          },
        },
        [h('div', [s === 'pointer' || s === 'trash' ? icon(s) : h('piece', { attrs })])],
      );
    }),
  );
}

function onSelectSparePiece(ctrl: EditorCtrl, s: Selected, upEvent: string): (e: MouchEvent) => void {
  return function (e: MouchEvent): void {
    e.preventDefault();
    if (s === 'pointer' || s === 'trash') {
      ctrl.selected(s);
      ctrl.redraw();
    } else {
      ctrl.selected('pointer');

      dragNewPiece(ctrl.chessground!.state, { color: s[0], role: s[1] }, e, true);

      document.addEventListener(
        upEvent,
        (e: MouchEvent) => {
          const eventPos = eventPosition(e) || lastTouchMovePos;
          if (eventPos && ctrl.chessground!.getKeyAtDomPos(eventPos)) ctrl.selected('pointer');
          else ctrl.selected(s);
          ctrl.redraw();
        },
        { once: true },
      );
    }
  };
}

function makeCursor(selected: Selected): string {
  if (selected === 'pointer') return 'pointer';

  const name = selected === 'trash' ? 'trash' : selected.join('-');
  const url = site.asset.url('cursors/' + name + '.cur');

  return `url('${url}'), default !important`;
}

export default function (ctrl: EditorCtrl): VNode {
  const state = ctrl.getState();
  const color = ctrl.bottomColor();

  return h('div.board-editor', { attrs: { style: `cursor: ${makeCursor(ctrl.selected())}` } }, [
    sparePieces(ctrl, opposite(color), color, 'top'),
    h('div.main-board', [chessground(ctrl)]),
    sparePieces(ctrl, color, color, 'bottom'),
    controls(ctrl, state),
    inputs(ctrl, state.legalFen || state.fen),
  ]);
}
