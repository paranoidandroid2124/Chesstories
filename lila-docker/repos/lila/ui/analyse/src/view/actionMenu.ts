import { displayColumns } from 'lib/device';
import type { VNode, ToggleSettings } from 'lib/view';
import { bind, hl, icon, toggle } from 'lib/view';
import type AnalyseCtrl from '../ctrl';
import { clamp } from 'lib/algo';

const ctrlToggle = (t: ToggleSettings, ctrl: AnalyseCtrl) => toggle(t, ctrl.redraw);

type BoardSettingsOpts = {
  closeOnChange?: boolean;
};

const flipBoard = (ctrl: AnalyseCtrl, closeMenu: () => void) => {
  ctrl.flip();
  closeMenu();
  ctrl.redraw();
};

const setInlineMoveList = (ctrl: AnalyseCtrl, closeMenu: () => void, inline: boolean) => {
  ctrl.treeView.modePreference(inline ? 'inline' : 'column');
  closeMenu();
  ctrl.redraw();
};

const setVariationControls = (ctrl: AnalyseCtrl, closeMenu: () => void, enabled: boolean) => {
  ctrl.disclosureMode(enabled);
  closeMenu();
  ctrl.redraw();
};

export function view(ctrl: AnalyseCtrl): VNode {
  return hl('div.action-menu', { attrs: { id: 'analyse-action-menu', role: 'region', 'aria-label': 'Board settings' } }, [
    hl('div.action-menu__heading', [
      hl('strong', 'Board settings'),
      hl('button.button.button-empty', { attrs: { type: 'button', 'aria-label': 'Close board settings' }, hook: bind('click', () => {
        ctrl.actionMenu(false);
        ctrl.redraw();
        document.querySelector<HTMLButtonElement>('[data-act="menu"]')?.focus();
      }) }, 'Close'),
    ]),
    ...boardSettingsView(ctrl),
  ]);
}

export function boardSettingsView(ctrl: AnalyseCtrl, opts: BoardSettingsOpts = {}): VNode[] {
  const closeOnChange = opts.closeOnChange ?? false;
  const closeMenu = () => {
    if (closeOnChange) ctrl.actionMenu.toggle();
  };

  return [
    hl('div.action-menu__tools', [
      hl(
        'button',
        {
          hook: bind('click', () => flipBoard(ctrl, closeMenu)),
          attrs: { type: 'button', title: 'Hotkey: f' },
        },
        [icon('chasingArrows'), 'Flip board'],
      ),
    ]),
    displayColumns() > 1 && hl('h2', 'Display'),
    ctrlToggle(
      {
        name: 'Inline notation',
        title: 'Shift+I',
        id: 'inline',
        checked: ctrl.treeView.modePreference() === 'inline',
        change: v => setInlineMoveList(ctrl, closeMenu, v),
      },
      ctrl,
    ),
    ctrlToggle(
      {
        name: 'Line branches',
        title: 'Show controls to expand or hide side lines',
        id: 'disclosure',
        checked: ctrl.disclosureMode(),
        change: v => setVariationControls(ctrl, closeMenu, v),
      },
      ctrl,
    ),
    renderVariationOpacitySlider(ctrl),
  ].filter(Boolean) as VNode[];
}

function renderVariationOpacitySlider(ctrl: AnalyseCtrl): VNode {
  const value = ctrl.variationArrowOpacity() || 0;
  const set = (value: number) => { ctrl.variationArrowOpacity(value); ctrl.redraw(); };
  return hl('div.setting.action-menu__range', [
    hl('label', { attrs: { for: 'variation-emphasis' } }, 'Line emphasis'),
    hl('output', { attrs: { for: 'variation-emphasis' } }, Math.round(value * 100) + '%'),
    hl('input#variation-emphasis.range', {
      attrs: { min: 0, max: 1, step: 0.1, type: 'range', 'aria-valuetext': Math.round(value * 100) + '%' },
      props: { value },
      on: {
        input: event => set(Number((event.target as HTMLInputElement).value)),
        wheel: event => {
          event.preventDefault();
          set(clamp(value + (event.deltaY > 0 ? -0.1 : 0.1), { min: 0, max: 1 }));
        },
      },
    }),
    hl('small', 'Faint'),
    hl('small', 'Strong'),
    hl('p', 'Adjust the arrows that show side lines on the board.'),
  ]);
}
