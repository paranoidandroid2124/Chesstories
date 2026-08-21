import { repeater, myUserId } from 'lib';
import * as licon from 'lib/licon';
import { storage } from 'lib/storage';
import { type VNode, onInsert, hl, icon, domDialog } from 'lib/view';
import { displayColumns, isTouchDevice } from 'lib/device';
import { addPointerListeners } from 'lib/pointer';
import * as control from '../control';
import type AnalyseCtrl from '../ctrl';

type Action = 'first' | 'prev' | 'next' | 'last' | 'scrub-help' | 'opening-explorer' | 'menu' | 'analysis';

export function renderControls(ctrl: AnalyseCtrl) {
  const canJumpPrev = ctrl.path !== '',
    canJumpNext = !!ctrl.node.children[0],
    columns = displayColumns(),
    touchDevice = isTouchDevice(),
    mobileUi = columns === 1 && touchDevice,
    activeTool = ctrl.activeControlBarTool();
  const explorerOpen = activeTool === 'opening-explorer';
  const menuOpen = activeTool === 'action-menu';

  return hl(
    'div.analyse__controls.analyse-controls',
    {
      hook: onInsert(el =>
        addPointerListeners(el, {
          click: e => clickControl(ctrl, e),
          hscrub: touchDevice ? dx => scrubControl(ctrl, dx) : undefined,
          hold: e => holdControl(ctrl, e),
        }),
      ),
    },
    [
      ctrl.isCevalAllowed() && renderAnalysisToggle(ctrl, activeTool),
      hl('div.jumps', [
        !mobileUi && jumpButton(licon.JumpFirst, 'first', canJumpPrev),
        jumpButton(licon.LessThan, 'prev', canJumpPrev),
        mobileUi &&
          !scrubHelpAcknowledged() &&
          hl('i.scrub-help', { attrs: { 'data-act': 'scrub-help' } }, [icon(licon.InfoCircle as any)]),
        jumpButton(licon.GreaterThan, 'next', canJumpNext),
        !mobileUi &&
          jumpButton(licon.JumpLast, 'last', ctrl.node !== ctrl.mainline[ctrl.mainline.length - 1]),
      ]),
      hl(
        'button.fbt',
        {
          attrs: {
            title: explorerOpen ? 'Close opening book and tablebase' : 'Open opening book and tablebase',
            'data-act': 'opening-explorer',
            'aria-label': explorerOpen
              ? 'Close opening book and tablebase'
              : 'Open opening book and tablebase',
            'aria-expanded': explorerOpen ? 'true' : 'false',
            ...(explorerOpen ? { 'aria-controls': 'analyse-opening-explorer' } : {}),
          },
          class: {
            hidden: !ctrl.explorer.allowed(),
            active: activeTool === 'opening-explorer',
          },
        },
        [icon(licon.Book as any)],
      ),
      hl(
        'button.fbt',
        {
          class: { active: activeTool === 'action-menu' },
          attrs: {
            title: menuOpen ? 'Close board settings' : 'Open board settings',
            'data-act': 'menu',
            'aria-label': menuOpen ? 'Close board settings' : 'Open board settings',
            'aria-expanded': menuOpen ? 'true' : 'false',
            ...(menuOpen ? { 'aria-controls': 'analyse-action-menu' } : {}),
          },
        },
        [icon(licon.Hamburger as any)],
      ),
    ],
  );
}

function renderAnalysisToggle(ctrl: AnalyseCtrl, activeTool: string | false): VNode {
  const active = ctrl.showCeval(),
    latent = active && !!activeTool;
  const label = active ? 'Hide live engine' : 'Show live engine';
  return hl(
    'button.fbt.fbt--engine-toggle',
    {
      attrs: {
        title: label,
        'data-act': 'analysis',
        'aria-label': label,
        'aria-pressed': active ? 'true' : 'false',
      },
      class: { active, latent, computing: ctrl.ceval.isComputing },
    },
    [icon(licon.Cogs as any), hl('span.label', 'Engine')],
  );
}

function holdControl(ctrl: AnalyseCtrl, e: PointerEvent) {
  if (!(e.target instanceof HTMLElement)) return;
  const target = e.target.closest<HTMLElement>('[data-act]');
  if (!target || target.matches('button:disabled,[aria-disabled="true"]')) return;
  const action = target.dataset.act as Action;
  if (action === 'prev' || action === 'next') {
    repeater(() => {
      control[action](ctrl);
      ctrl.redraw();
    });
  } else clickControl(ctrl, e);
}

function clickControl(ctrl: AnalyseCtrl, e: PointerEvent) {
  if (!(e.target instanceof HTMLElement)) return;
  const target = e.target.closest<HTMLElement>('[data-act]');
  if (!target || target.matches('button:disabled,[aria-disabled="true"]')) return;
  const action = target.dataset.act as Action;
  if (!action) return;
  if (action === 'prev') control.prev(ctrl);
  else if (action === 'next') control.next(ctrl);
  else if (action === 'first') control.first(ctrl);
  else if (action === 'last') control.last(ctrl);
  else if (action === 'scrub-help') scrubHelp(ctrl);
  else if (action === 'opening-explorer') ctrl.toggleExplorer();
  else if (action === 'menu') ctrl.toggleActionMenu();
  else if (action === 'analysis') {
    if (ctrl.activeControlBarTool()) {
      ctrl.explorer.disable();
      ctrl.actionMenu(false);
    }
    ctrl.showCeval(!ctrl.showCeval());
  }
  ctrl.redraw();
}

let last: number[] = [];

function scrubControl(ctrl: AnalyseCtrl, dx: number | 'pointerup') {
  if (dx === 'pointerup') {
    const v = last.slice(-3).reduce((a, b) => a + b, 0) / Math.min(last.length, 3);
    if (v > 16) control.last(ctrl);
    else if (v < -16) control.first(ctrl);
    last = [];
  } else {
    if (dx > 0) control.next(ctrl);
    else control.prev(ctrl);
    last.push(dx);
  }
  ctrl.redraw();
}

const jumpLabels: Record<string, string> = {
  first: 'Go to the first move',
  prev: 'Go to the previous move',
  next: 'Go to the next move',
  last: 'Go to the last move',
};

const jumpButton = (iconName: string, effect: string, enabled: boolean): VNode => {
  const label = jumpLabels[effect] ?? effect;
  const attrs: Record<string, string | boolean> = {
    'data-act': effect,
    title: label,
    'aria-label': label,
  };
  if (!enabled) attrs.disabled = true;
  return hl('button.fbt.move', { class: { disabled: !enabled }, attrs }, [icon(iconName as any)]);
};

function scrubHelp(ctrl: AnalyseCtrl) {
  domDialog({
    htmlText: $html`
      <p>
        Swipe left or right on the button bar below the board to go to game start or end.
      </p>
      <p>
        Move your finger slowly to scrub through moves one by one.
      </p>
      <button class="button">OK</button>`,
    actions: [{ selector: 'button', result: 'ok' }],
    noCloseButton: true,
    show: true,
  }).then(dlg => {
    scrubHelpAcknowledged(dlg.returnValue === 'ok');
    ctrl.redraw();
  });
}

function scrubHelpAcknowledged(ack?: boolean) {
  const key = `analyse.help.scrub-acknowledged.${myUserId() ?? 'anon'}`;
  const acknowledged = storage.boolean(key);
  if (ack === undefined) return acknowledged.get();
  if (ack) acknowledged.set(true);
  return ack;
}
