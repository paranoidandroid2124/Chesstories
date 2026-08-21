import * as licon from 'lib/licon';
import { displayColumns } from 'lib/device';
import type { VNode, ToggleSettings } from 'lib/view';
import { bind, hl, toggle } from 'lib/view';
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
  return hl('div.action-menu', { attrs: { id: 'analyse-action-menu' } }, boardSettingsView(ctrl));
}

export function boardSettingsView(ctrl: AnalyseCtrl, opts: BoardSettingsOpts = {}): VNode[] {
  const closeOnChange = opts.closeOnChange ?? true;
  const closeMenu = () => {
    if (closeOnChange) ctrl.actionMenu.toggle();
  };

  return [
    hl('div.action-menu__tools', [
      hl(
        'button',
        {
          hook: bind('click', () => flipBoard(ctrl, closeMenu)),
          attrs: { type: 'button', 'data-icon': licon.ChasingArrows, title: 'Hotkey: f' },
        },
        'Flip board',
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
  return hl('span.setting.action-menu__range', [
    hl('label', 'Line emphasis'),
    hl('input.range', {
      key: 'variation-arrows',
      attrs: { min: 0, max: 1, step: 0.1, type: 'range', value: ctrl.variationArrowOpacity() || 0 },
      props: { value: ctrl.variationArrowOpacity() || 0 },
      hook: {
        insert: (vnode: VNode) => {
          const input = vnode.elm as HTMLInputElement;
          input.addEventListener('input', () => {
            ctrl.variationArrowOpacity(parseFloat(input.value));
          });
          input.addEventListener('wheel', e => {
            e.preventDefault();
            ctrl.variationArrowOpacity(
              clamp((ctrl.variationArrowOpacity() || 0) + (e.deltaY > 0 ? -0.1 : 0.1), {
                min: 0,
                max: 1,
              }),
            );
          });
        },
      },
    }),
  ]);
}
