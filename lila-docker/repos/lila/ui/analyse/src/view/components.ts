import { view as cevalView, renderEval as normalizeEval } from 'lib/ceval';
import { parseFen } from 'chessops/fen';
import { defined } from 'lib';
import * as licon from 'lib/licon';
import { type VNode, type LooseVNodes, bind, onInsert, icon, hl } from 'lib/view';
import { displayColumns, isMobile } from 'lib/device';
import * as materialView from 'lib/game/view/material';

import { view as actionMenu } from './actionMenu';
import explorerView from '../explorer/explorerView';
import { view as forkView } from '../fork';
import renderClocks from './clocks';
import * as control from '../control';
import * as chessground from '../ground';
import type AnalyseCtrl from '../ctrl';
import type { ConcealOf } from '../interfaces';
import * as pgnExport from '../pgnExport';
import { spinnerVdom as spinner, stepwiseScroll } from 'lib/view';
import * as Prefs from 'lib/prefs';
import statusView from 'lib/game/view/status';
import { plyToTurn } from 'lib/game/chess';
import { dispatchChessgroundResize } from 'lib/chessgroundResize';
import pgnImport, { renderPgnError } from '../pgnImport';
import {
  normalizeInlinePgn,
  pgnInputError,
  reviewStudyCreateGate,
  type PgnDraftStatus,
} from '../pgnPipeline';
import { storage } from 'lib/storage';
import { renderMoveReview } from './moveReview';

interface ViewContext {
  ctrl: AnalyseCtrl;
  concealOf?: ConcealOf;
  playerBars: VNode[] | undefined;
  playerStrips: [VNode, VNode] | undefined;
  gaugeOn: boolean;
  needsInnerCoords: boolean;
}

export function viewContext(ctrl: AnalyseCtrl): ViewContext {
  const playerBars = undefined;
  const gaugeOn = ctrl.showEvalGauge();
  return {
    ctrl,
    concealOf: makeConcealOf(ctrl),
    playerBars,
    playerStrips: renderPlayerStrips(ctrl),
    gaugeOn,
    needsInnerCoords: ctrl.showCapturedMaterial() || !!playerBars,
  };
}

export function renderMain(ctx: ViewContext, ...kids: LooseVNodes[]): VNode {
  const { ctrl, playerBars, gaugeOn, needsInnerCoords } = ctx;
  return hl(
    'main.analyse.variant-' + ctrl.data.game.variant.key,
    {
      attrs: {
        'data-active-tool': ctrl.activeControlBarTool(),
        'data-active-mode': ctrl.activeControlMode(),
      },
      hook: {
        insert: () => {
          forceInnerCoords(ctrl, needsInnerCoords);
        },
        update(_, _2) {
          forceInnerCoords(ctrl, needsInnerCoords);
        },
        postpatch(old, vnode) {
          if (old.data!.gaugeOn !== gaugeOn) dispatchChessgroundResize();
          vnode.data!.gaugeOn = gaugeOn;
        },
      },
      class: {
        'gauge-on': gaugeOn,
        'has-players': !!playerBars,
        'analyse-hunter': ctrl.opts.hunter,
        'analyse--notebook': ctrl.isStudy(),
      },
    },
    kids,
  );
}

type WorkspaceToolId = 'opening-explorer' | 'action-menu';
type NotebookGlyphKind = 'bookmark' | 'page' | 'section';

type WorkspaceTool = {
  id: WorkspaceToolId;
  label: string;
  summary: string;
  icon: string;
  active: boolean;
  open: () => void;
};

function notebookGlyphNodes(kind: NotebookGlyphKind): VNode[] {
  switch (kind) {
    case 'bookmark':
      return [
        hl('path', { attrs: { d: 'M10 6.5h12a2 2 0 0 1 2 2v17l-8-4.8-8 4.8v-17a2 2 0 0 1 2-2Z' } }),
        hl('path', { attrs: { d: 'M12 11h8' } }),
        hl('path', { attrs: { d: 'M12 14.5h8' } }),
      ];
    case 'page':
      return [
        hl('path', {
          attrs: { d: 'M11 5.5h8.5L25 11v14.5a2 2 0 0 1-2 2H11a2 2 0 0 1-2-2v-18a2 2 0 0 1 2-2Z' },
        }),
        hl('path', { attrs: { d: 'M19.5 5.5V11H25' } }),
        hl('path', { attrs: { d: 'M12.5 15h9' } }),
        hl('path', { attrs: { d: 'M12.5 18.5h9' } }),
        hl('path', { attrs: { d: 'M12.5 22h6.5' } }),
      ];
    case 'section':
      return [
        hl('path', { attrs: { d: 'M8 9.5a2 2 0 0 1 2-2h9l5 5v11a2 2 0 0 1-2 2H10a2 2 0 0 1-2-2v-14Z' } }),
        hl('path', { attrs: { d: 'M10.5 7.5v-2a2 2 0 0 1 2-2H21l3 3' } }),
        hl('path', { attrs: { d: 'M13 15h7' } }),
        hl('path', { attrs: { d: 'M13 18.5h7' } }),
        hl('path', { attrs: { d: 'M13 22h4.5' } }),
      ];
  }
}

function renderNotebookGlyph(kind: NotebookGlyphKind, extraClass?: string): VNode {
  return hl(
    'span.notebook-glyph',
    {
      class: {
        [`notebook-glyph--${kind}`]: true,
        [extraClass || '']: !!extraClass,
      },
    },
    [
      hl(
        'svg',
        {
          attrs: {
            viewBox: '0 0 32 32',
            fill: 'none',
            stroke: 'currentColor',
            'stroke-width': '1.7',
            'stroke-linecap': 'round',
            'stroke-linejoin': 'round',
            'aria-hidden': 'true',
          },
        },
        notebookGlyphNodes(kind),
      ),
    ],
  );
}

function notebookGlyphForTool(tool: WorkspaceTool): NotebookGlyphKind {
  switch (tool.id) {
    case 'opening-explorer':
      return 'bookmark';
    case 'action-menu':
      return 'section';
  }
}

function renderWorkspaceToolIcon(ctrl: AnalyseCtrl, tool: WorkspaceTool): VNode {
  return ctrl.isStudy()
    ? renderNotebookGlyph(notebookGlyphForTool(tool), 'analyse__workspace-glyph')
    : icon(tool.icon as any);
}

function workspaceTools(ctrl: AnalyseCtrl): WorkspaceTool[] {
  const tools: WorkspaceTool[] = [
    {
      id: 'opening-explorer',
      label: 'Opening book',
      summary: 'Opening games and tablebase positions',
      icon: licon.Book,
      active: ctrl.activeControlBarTool() === 'opening-explorer',
      open: ctrl.toggleExplorer,
    },
  ];

  tools.push({
    id: 'action-menu',
    label: 'Board view',
    summary: 'Coordinates, moves, and review cues',
    icon: licon.Hamburger,
    active: ctrl.activeControlBarTool() === 'action-menu',
    open: ctrl.toggleActionMenu,
  });

  return tools;
}

function workspaceToolControls(ctrl: AnalyseCtrl, tool: WorkspaceTool): string | undefined {
  if (!tool.active) return;
  if (tool.id === 'opening-explorer') return ctrl.explorer.allowed() ? 'analyse-opening-explorer' : undefined;
  return ctrl.actionMenu() ? 'analyse-action-menu' : undefined;
}

function renderWorkspaceDock(ctrl: AnalyseCtrl): VNode {
  const activeTool = ctrl.activeControlBarTool();
  const tools = workspaceTools(ctrl);
  return hl(`section.analyse__workspace-dock${activeTool ? '' : '.is-idle'}`, [
    hl('div.analyse__workspace-dock-head', [
      hl('h2', 'Reference desk'),
      hl(
        'span',
        activeTool
          ? 'The current position stays fixed while you inspect another source.'
          : 'Opening records and board settings stay attached to this position.',
      ),
    ]),
    hl(
      'div.analyse__workspace-dock-grid',
      tools.map(tool => {
        const controls = workspaceToolControls(ctrl, tool);
        return hl(
          'button.analyse__workspace-tool',
          {
            key: tool.id,
            attrs: {
              type: 'button',
              title: tool.summary,
              'aria-label': `${tool.active ? 'Close' : 'Open'} ${tool.label}`,
              'data-tool-id': tool.id,
              'aria-pressed': tool.active ? 'true' : 'false',
              'aria-expanded': tool.active ? 'true' : 'false',
              ...(controls ? { 'aria-controls': controls } : {}),
            },
            hook: bind('click', tool.open, ctrl.redraw),
            class: {
              active: tool.active,
            },
          },
          [
            hl('span.analyse__workspace-tool-icon', [renderWorkspaceToolIcon(ctrl, tool)]),
            hl('span.analyse__workspace-tool-copy', [hl('strong', tool.label), hl('span', tool.summary)]),
          ],
        );
      }),
    ),
  ]);
}

export function renderTools({ ctrl, concealOf }: ViewContext) {
  const showCeval = ctrl.isCevalAllowed() && ctrl.showCeval();
  const activeTool = explorerView(ctrl);
  return hl('div.analyse__tools', [
    showCeval && cevalView.renderCeval(ctrl),
    showCeval && cevalView.renderPvs(ctrl),
    ctrl.moveReviewAvailable() && renderMoveReview(ctrl.moveReviewPanelProps()),
    renderMoveList(ctrl, concealOf),
    forkView(ctrl, concealOf),
    displayColumns() > 1 && renderWorkspaceDock(ctrl),
    activeTool,
    ctrl.actionMenu() && actionMenu(ctrl),
  ]);
}

export function renderBoard({ ctrl, playerBars, playerStrips, gaugeOn }: ViewContext, skipInfo = false) {
  return hl('div.analyse__board-wrap', [
    gaugeOn && cevalView.renderHorizontalGauge(ctrl),
    hl(
      'div.analyse__board.main-board',
      {
        hook:
          'ontouchstart' in window || !storage.boolean('scrollMoves').getOrDefault(true)
            ? undefined
            : bind(
                'wheel',
                stepwiseScroll((e: WheelEvent, scroll: boolean) => {
                  const target = e.target as HTMLElement;
                  if (
                    target.tagName !== 'PIECE' &&
                    target.tagName !== 'SQUARE' &&
                    target.tagName !== 'CG-BOARD'
                  )
                    return;
                  if (scroll) {
                    e.preventDefault();
                    if (e.deltaY > 0) control.next(ctrl);
                    else if (e.deltaY < 0) control.prev(ctrl);
                    ctrl.redraw();
                  }
                }),
                undefined,
                false,
              ),
      },
      [
        !skipInfo && playerStrips,
        !skipInfo && playerBars?.[ctrl.bottomIsWhite() ? 1 : 0],
        chessground.render(ctrl),
        !skipInfo && playerBars?.[ctrl.bottomIsWhite() ? 0 : 1],
        ctrl.promotion.view(),
      ],
    ),
  ]);
}

export function renderUnderboard({ ctrl }: ViewContext) {
  return hl('div.analyse__underboard', [renderInputs(ctrl)]);
}

function renderInputs(ctrl: AnalyseCtrl): VNode | undefined {
  if (ctrl.ongoing) return;
  if (ctrl.isStudy()) {
    const studyPanel = renderStudyWorkspacePanel(ctrl);
    return studyPanel ? hl('div.copyables.copyables--workspace', [studyPanel]) : undefined;
  }
  if (!ctrl.data.userAnalysis) return;
  if (ctrl.redirecting) return spinner();
  const currentPgn = pgnExport.renderFullTxt(ctrl);
  const currentInspection = inspectPgnDraft(currentPgn, currentPgn);
  const draftPgn = defined(ctrl.pgnInput) ? ctrl.pgnInput : currentPgn;
  const pgnInspection = inspectPgnDraft(draftPgn, currentPgn);
  const fenInspection = inspectFenDraft(
    defined(ctrl.fenInput) ? ctrl.fenInput : ctrl.node.fen,
    ctrl.node.fen,
  );
  const submitPgnDraft = () => {
    if (pgnInspection.status !== 'ready') return;
    const draft = defined(ctrl.pgnInput) ? ctrl.pgnInput : pgnExport.renderFullTxt(ctrl);
    if (draft !== pgnExport.renderFullTxt(ctrl)) ctrl.importPgn(draft);
  };
  return hl('div.copyables.copyables--workspace', [
    hl('div.analyse-review__summary-grid.copyables__summary', [
      summaryFact(pgnInspection.headline, 'game text'),
      summaryFact(
        pgnDraftReplaySize(pgnInspection),
        pgnInspection.preview ? 'moves to replay' : 'score text',
      ),
      summaryFact(fenInspection.headline, 'board position'),
    ]),
    renderStudyLaunchPanel(ctrl, pgnInspection.status),
    hl('div.copyables__panel', [
      hl('div.pair', [
        hl('label.name', 'Position setup'),
        hl('input.copyable', {
          attrs: { spellcheck: 'false', enterkeyhint: 'done' },
          hook: {
            insert: vnode => {
              const el = vnode.elm as HTMLInputElement;
              el.value = defined(ctrl.fenInput) ? ctrl.fenInput : ctrl.node.fen;
              const submitFen = () => {
                const nextFen = el.value.trim();
                if (nextFen === ctrl.node.fen || !parseFen(nextFen).isOk) return false;
                ctrl.changeFen(nextFen);
                return true;
              };
              el.addEventListener('change', () => {
                if (el.reportValidity()) submitFen();
              });
              el.addEventListener('keydown', (e: KeyboardEvent) => {
                if (e.key !== 'Enter' || e.shiftKey || e.ctrlKey || e.altKey || e.metaKey) return;
                if (el.reportValidity() && submitFen()) e.preventDefault();
              });
              el.addEventListener('input', () => {
                ctrl.fenInput = el.value;
                el.setCustomValidity(parseFen(el.value.trim()).isOk ? '' : 'Position setup needs fixes');
                requestAnimationFrame(ctrl.redraw);
              });
            },
            postpatch: (_, vnode) => {
              const el = vnode.elm as HTMLInputElement;
              if (!defined(ctrl.fenInput)) {
                el.value = ctrl.node.fen;
                el.setCustomValidity('');
              } else if (el.value !== ctrl.fenInput) el.value = ctrl.fenInput;
            },
          },
        }),
      ]),
      renderInlineStatus(fenInspection.headline, fenInspection.message, fenInspection.status === 'invalid'),
    ]),
    hl('div.copyables__panel.copyables__panel--pgn', [
      hl('div.pair', [
        hl('label.name', 'Game text'),
        hl('textarea.copyable', {
          attrs: { spellcheck: 'false' },
          class: { 'is-error': !!ctrl.pgnError || pgnInspection.status === 'invalid' },
          hook: {
            ...onInsert((el: HTMLTextAreaElement) => {
              el.value = defined(ctrl.pgnInput) ? ctrl.pgnInput : pgnExport.renderFullTxt(ctrl);
              const importPgnIfDifferent = () =>
                el.value !== pgnExport.renderFullTxt(ctrl) && ctrl.importPgn(el.value);

              el.addEventListener('input', () => {
                ctrl.pgnInput = el.value;
                ctrl.pgnError = '';
                requestAnimationFrame(ctrl.redraw);
              });

              el.addEventListener('keypress', (e: KeyboardEvent) => {
                if (e.key !== 'Enter' || e.shiftKey || e.ctrlKey || e.altKey || e.metaKey || isMobile())
                  return;
                else if (importPgnIfDifferent()) e.preventDefault();
              });
              if (isMobile()) el.addEventListener('focusout', importPgnIfDifferent);
            }),
            postpatch: (_, vnode) => {
              (vnode.elm as HTMLTextAreaElement).value = defined(ctrl.pgnInput)
                ? ctrl.pgnInput
                : pgnExport.renderFullTxt(ctrl);
            },
          },
        }),
      ]),
      hl('div.bottom-item.bottom-actions', [
        hl(
          'button.button.button-thin.bottom-action.text',
          {
            attrs: pgnInspection.status !== 'ready' ? { disabled: true } : {},
            hook: bind('click', submitPgnDraft),
          },
          [icon(licon.PlayTriangle as any), ' Load game'],
        ),
        pgnInspection.status !== 'current' &&
          hl(
            'button.button.button-thin.bottom-action.text',
            {
              hook: bind('click', () => ctrl.resetImportDraft()),
            },
            [icon(licon.Reload as any), ' Reset draft'],
          ),
      ]),
      renderInlineStatus(
        pgnInspection.headline,
        pgnInspection.preview
          ? `${pgnInspection.message} ${pgnDraftPlayerDetail(pgnInspection)}`
          : pgnInspection.message,
        pgnInspection.status === 'invalid',
      ),
      hl(
        'div.bottom-item.bottom-error',
        { class: { 'is-error': !!ctrl.pgnError || pgnInspection.status === 'invalid' } },
        [icon(licon.CautionTriangle as any), renderPgnError(ctrl.pgnError || pgnInspection.error)],
      ),
      renderImportPreview(currentInspection, pgnInspection),
    ]),
  ]);
}

function renderStudyWorkspacePanel(ctrl: AnalyseCtrl): VNode | null {
  const study = ctrl.studyData();
  if (!study) return null;

  const currentUrl = ctrl.studyUrl();
  const studyTarget =
    currentUrl && typeof window !== 'undefined' ? new URL(currentUrl, window.location.origin) : null;
  const studyUrl =
    studyTarget &&
    studyTarget.pathname + studyTarget.search === window.location.pathname + window.location.search
      ? null
      : currentUrl;
  const actionMessage = ctrl.studyActionMessageText();
  const syncTone = ctrl.studyWriteError ? 'error' : ctrl.isStudyWriting() ? 'info' : 'success';
  const syncMessage = actionMessage || ctrl.studyStatusText();
  const access =
    study.visibility === 'private' ? 'Private' : study.visibility === 'unlisted' ? 'Link sharing' : 'Public';

  return hl('section.copyables__study.copyables__study--current', [
    hl('div.copyables__study-head', [
      hl('div.copyables__study-copy', [
        hl('h2', study.name),
        hl('span.copyables__study-subline', study.chapterName),
      ]),
      hl('div.copyables__study-actions', [
        studyUrl
          ? hl('a.button.button-thin.copyables__study-button', { attrs: { href: studyUrl } }, [
              renderNotebookGlyph('page', 'copyables__study-button-glyph'),
              ' Open study',
            ])
          : null,
        hl(
          'button.button.button-thin.copyables__study-button',
          {
            attrs: { type: 'button' },
            hook: bind('click', () => {
              void ctrl.copyStudyShareLink();
            }),
          },
          [renderNotebookGlyph('bookmark', 'copyables__study-button-glyph'), ' Copy study link'],
        ),
      ]),
    ]),
    hl('dl.copyables__study-meta-grid', [
      hl('div', [hl('dt', 'Chapter'), hl('dd', study.chapterName)]),
      hl('div', [hl('dt', 'Chapters'), hl('dd', `${study.chapters.length}`)]),
      hl('div', [hl('dt', 'Access'), hl('dd', access)]),
    ]),
    renderStudyStatus(syncMessage, actionMessage ? ctrl.studyActionToneValue() : syncTone),
  ]);
}

const studyVisibilityChoices = [
  {
    value: 'unlisted',
    title: 'Link sharing',
    help: 'Anyone with the link can open it.',
  },
  {
    value: 'private',
    title: 'Private',
    help: 'Only you can open it.',
  },
  {
    value: 'public',
    title: 'Public',
    help: 'Visible in public study lists.',
  },
] as const;

function closeStudyCreateSetup(ctrl: AnalyseCtrl): void {
  ctrl.closeStudyCreateSetup();
  requestAnimationFrame(() => document.getElementById('study-create-launch')?.focus());
}

function renderStudySetupForm(ctrl: AnalyseCtrl): VNode | null {
  if (!ctrl.studyCreateSetupVisible()) return null;
  const setup = ctrl.studyCreateSetupValues();
  const busy = ctrl.studyCreateBusy();

  return hl(
    'form.copyables__study-setup',
    {
      attrs: { id: 'study-create-setup', 'aria-labelledby': 'study-create-setup-heading' },
      hook: onInsert((el: HTMLFormElement) => {
        el.querySelector<HTMLInputElement>('input')?.focus();
        el.addEventListener('keydown', e => {
          if (e.key === 'Escape' && !ctrl.studyCreateBusy()) {
            e.preventDefault();
            closeStudyCreateSetup(ctrl);
          }
        });
        el.addEventListener('submit', e => {
          e.preventDefault();
          void ctrl.submitStudyCreateSetup();
        });
      }),
    },
    [
      hl('div.copyables__study-setup-head', [
        hl('h3', { attrs: { id: 'study-create-setup-heading' } }, 'Create study'),
        hl('span.copyables__study-subline', 'Save the loaded game with a first chapter and access setting.'),
      ]),
      hl('label.copyables__study-field', [
        hl('span', 'Study title'),
        hl('input', {
          attrs: {
            type: 'text',
            maxlength: 100,
            required: true,
            autocomplete: 'off',
            disabled: busy,
          },
          props: { value: setup.name },
          hook: bind('input', e => {
            ctrl.updateStudyCreateSetup({ name: (e.target as HTMLInputElement).value });
          }),
        }),
      ]),
      hl('label.copyables__study-field', [
        hl('span', 'First chapter'),
        hl('input', {
          attrs: {
            type: 'text',
            maxlength: 80,
            required: true,
            autocomplete: 'off',
            disabled: busy,
          },
          props: { value: setup.chapterName },
          hook: bind('input', e => {
            ctrl.updateStudyCreateSetup({ chapterName: (e.target as HTMLInputElement).value });
          }),
        }),
      ]),
      hl('fieldset.copyables__study-access', [
        hl('legend', 'Sharing'),
        hl(
          'div.copyables__study-access-options',
          studyVisibilityChoices.map(choice =>
            hl(
              'label.copyables__study-access-option',
              {
                class: { 'is-selected': setup.visibility === choice.value },
              },
              [
                hl('input', {
                  attrs: {
                    type: 'radio',
                    name: 'study-visibility',
                    value: choice.value,
                    disabled: busy,
                  },
                  props: { checked: setup.visibility === choice.value },
                  hook: bind('change', () => {
                    ctrl.updateStudyCreateSetup({ visibility: choice.value });
                  }),
                }),
                hl('span.copyables__study-access-copy', [
                  hl('strong', choice.title),
                  hl('span', choice.help),
                ]),
              ],
            ),
          ),
        ),
      ]),
      ctrl.studyCreateErrorText() ? renderStudyStatus(ctrl.studyCreateErrorText()!, 'error') : null,
      hl('div.copyables__study-setup-actions', [
        hl(
          'button.button.button-metal.copyables__study-button',
          {
            attrs: { type: 'button', disabled: busy },
            hook: bind('click', () => closeStudyCreateSetup(ctrl)),
          },
          'Cancel',
        ),
        hl(
          'button.button.copyables__study-button',
          {
            attrs: { type: 'submit', disabled: busy },
          },
          busy ? 'Creating...' : 'Create study',
        ),
      ]),
    ],
  );
}

function renderStudyLaunchPanel(ctrl: AnalyseCtrl, pgnStatus: PgnDraftStatus): VNode {
  const busy = ctrl.studyCreateBusy();
  const needsAuth = ctrl.studyNeedsAuth();
  const transferCount = ctrl.studyTransferCountValue();
  const error = ctrl.studyCreateErrorText();
  const setupOpen = ctrl.studyCreateSetupVisible();
  const createGate = reviewStudyCreateGate(pgnStatus);
  const createDisabled = busy || createGate.disabled;

  return hl('section.copyables__study.copyables__study--launch', [
    hl('div.copyables__study-head', [
      hl('div.copyables__study-copy', [
        hl('h2', 'Save this game as a study'),
        hl('span.copyables__study-subline', 'Keep the game, chapters, and notes together.'),
      ]),
      hl('div.copyables__study-actions', [
        createGate.disabled
          ? hl(
              'button.button.copyables__study-button',
              {
                attrs: { type: 'button', disabled: true },
              },
              createGate.buttonLabel,
            )
          : needsAuth
            ? hl('a.button.copyables__study-button', { attrs: { href: ctrl.studyLoginHref() } }, [
                renderNotebookGlyph('bookmark', 'copyables__study-button-glyph'),
                ' Sign in to create',
              ])
            : hl(
                'button.button.copyables__study-button',
                {
                  attrs: createDisabled
                    ? { type: 'button', disabled: true }
                    : {
                        id: 'study-create-launch',
                        type: 'button',
                        'aria-expanded': setupOpen ? 'true' : 'false',
                        ...(setupOpen ? { 'aria-controls': 'study-create-setup' } : {}),
                      },
                  hook: createDisabled
                    ? undefined
                    : bind('click', () => {
                        ctrl.openStudyCreateSetup();
                      }),
                },
                busy ? 'Creating...' : createGate.buttonLabel,
              ),
      ]),
    ]),
    renderStudyStatus(
      busy
        ? transferCount > 0
          ? `Creating the study and carrying over ${transferCount} saved line${transferCount === 1 ? '' : 's'}.`
          : 'Creating the study from the current game.'
        : createGate.message,
      busy ? 'info' : createGate.tone,
      busy ? undefined : createGate.title,
    ),
    error && !setupOpen ? renderStudyStatus(error, 'error') : null,
    renderStudySetupForm(ctrl),
  ]);
}

type FenDraftInspection = {
  status: 'current' | 'ready' | 'invalid';
  headline: string;
  message: string;
};

type PgnDraftInspection = {
  status: PgnDraftStatus;
  headline: string;
  message: string;
  chars: number;
  lines: number;
  normalized?: string;
  error?: string;
  preview?: {
    variant: string;
    plies: number;
    opening?: string;
  };
};

let lastPgnInspection: { draft: string; current: string; result: PgnDraftInspection } | undefined;

function inspectFenDraft(draft: string, currentFen: string): FenDraftInspection {
  const trimmed = draft.trim();
  if (!trimmed || trimmed === currentFen) {
    return {
      status: 'current',
      headline: 'Current position',
      message: 'Edit the position setup and press Enter to reopen the board from a new position.',
    };
  }
  if (!parseFen(trimmed).isOk) {
    return {
      status: 'invalid',
      headline: 'Position setup needs fixes',
      message: 'The board waits until the position setup can be read.',
    };
  }
  return {
    status: 'ready',
    headline: 'Ready to jump',
    message: 'Press Enter to relaunch from this board state.',
  };
}

function inspectPgnDraft(draft: string, currentPgn: string): PgnDraftInspection {
  if (lastPgnInspection?.draft === draft && lastPgnInspection.current === currentPgn)
    return lastPgnInspection.result;
  const chars = draft.trim().length;
  const lines = draft ? draft.split(/\r?\n/).length : 0;
  const normalized = normalizeInlinePgn(draft);
  const normalizedCurrent = normalizeInlinePgn(currentPgn);
  let result: PgnDraftInspection;
  if (!normalized) {
    result = {
      status: chars ? 'invalid' : 'empty',
      headline: chars ? 'Game text too long' : 'Draft empty',
      message: chars ? pgnInputError(draft) : 'Paste a game to load another game on this board.',
      chars,
      lines,
    };
  } else {
    try {
      const imported = pgnImport(normalized);
      const game = imported.game;
      const plies = Math.max(0, (game?.turns || imported.treeParts?.length || 1) - 1);
      const preview = {
        variant: game?.variant?.name || 'Chess',
        plies,
        opening: game?.opening?.name,
      };
      result =
        normalizedCurrent === normalized
          ? {
              status: 'current',
              headline: 'Current board game',
              message: 'The draft matches the game already loaded on this board.',
              chars,
              lines,
              normalized,
              preview,
            }
          : {
              status: 'ready',
              headline: 'Ready to load',
              message: 'Loading this game will replace the current board and moves.',
              chars,
              lines,
              normalized,
              preview,
            };
    } catch (err) {
      result = {
        status: 'invalid',
        headline: 'Game text needs fixes',
        message: 'The board waits until the game text can be read.',
        chars,
        lines,
        normalized,
        error: (err as Error).message,
      };
    }
  }
  lastPgnInspection = { draft, current: currentPgn, result };
  return result;
}

function renderInlineStatus(headline: string, message: string, error = false): VNode {
  return hl(`div.bottom-item.bottom-status.copyables__status${error ? '.is-error' : ''}`, [
    hl('strong', headline),
    hl('span', message),
  ]);
}

function summaryFact(value: string, label: string): VNode {
  return hl('div.analyse-review__summary-fact', [hl('strong', value), hl('span', label)]);
}

function pgnDraftReplaySize(inspection: PgnDraftInspection): string {
  return inspection.preview ? playedMoveLabel(inspection.preview.plies) : gameTextLineLabel(inspection.lines);
}

function pgnDraftPlayerDetail(inspection: PgnDraftInspection): string {
  if (inspection.preview) {
    return [inspection.preview.variant, playedMoveLabel(inspection.preview.plies), inspection.preview.opening]
      .filter(Boolean)
      .join(' • ');
  }
  if (inspection.chars > 0) return `${gameTextLineLabel(inspection.lines)} pasted`;
  return 'Paste a game to begin.';
}

function playedMoveLabel(plies: number): string {
  const count = Math.max(0, plies);
  return `${count} played move${count === 1 ? '' : 's'}`;
}

function gameTextLineLabel(lines: number): string {
  const count = Math.max(1, lines);
  return `${count} game text line${count === 1 ? '' : 's'}`;
}

function renderStudyStatus(message: string, tone: 'info' | 'success' | 'error', title?: string): VNode {
  const statusAttrs: Record<string, string> =
    tone === 'error'
      ? { role: 'alert', 'aria-atomic': 'true' }
      : { role: 'status', 'aria-live': 'polite', 'aria-atomic': 'true' };

  return hl(
    `div.copyables__study-status.copyables__study-status--${tone}`,
    {
      attrs: statusAttrs,
    },
    [
      hl(
        'strong',
        title || (tone === 'error' ? 'Study issue' : tone === 'info' ? 'Study in progress' : 'Study ready'),
      ),
      hl('span', message),
    ],
  );
}

function renderImportPreview(current: PgnDraftInspection, incoming: PgnDraftInspection): VNode {
  return hl('div.copyables__preview', [
    hl('div.copyables__preview-item', [
      hl('span.copyables__preview-label', 'On the board'),
      hl('strong', current.preview?.opening || current.preview?.variant || 'Current board'),
      hl('span', pgnDraftPlayerDetail(current)),
    ]),
    hl('div.copyables__preview-item', [
      hl('span.copyables__preview-label', 'Ready to load'),
      hl(
        'strong',
        incoming.preview?.opening ||
          incoming.preview?.variant ||
          (incoming.status === 'invalid' ? 'Game text needs fixes' : 'Awaiting draft'),
      ),
      hl('span', pgnDraftPlayerDetail(incoming)),
    ]),
  ]);
}

function renderResult(ctrl: AnalyseCtrl): VNode[] {
  const render = (result: string, status: string) => [hl('div.result', result), hl('div.status', status)];
  if (ctrl.data.game.status.id >= 30) {
    const winner = ctrl.data.game.winner;
    const result = winner === 'white' ? '1-0' : winner === 'black' ? '0-1' : '½-½';
    return render(result, statusView(ctrl.data));
  }
  return [];
}

export const renderIndexAndMove = (node: Tree.Node, withEval: boolean, withGlyphs: boolean): LooseVNodes =>
  node.san ? [renderIndex(node.ply, true), renderMoveNodes(node, withEval, withGlyphs)] : undefined;

export const renderIndex = (ply: Ply, withDots: boolean): VNode =>
  hl(`index.sbhint${ply}`, plyToTurn(ply) + (withDots ? (ply % 2 === 1 ? '.' : '...') : ''));

export function renderMoveNodes(
  node: Tree.Node,
  withEval: boolean,
  withGlyphs: boolean,
  ev?: Tree.ClientEval | Tree.ServerEval | false,
): LooseVNodes {
  ev ??= node.ceval ?? node.eval; // ev = false will override withEval
  const evalText = !ev
    ? ''
    : ev?.cp !== undefined
      ? normalizeEval(ev.cp)
      : ev?.mate !== undefined
        ? `#${ev.mate}`
        : '';
  return [
    hl('san', node.san!),
    withGlyphs && node.glyphs?.map(g => hl('glyph', { attrs: { title: g.name } }, g.symbol)),
    withEval && !!node.shapes?.length && hl('shapes'),
    withEval && evalText && hl('eval', evalText.replace('-', '−')),
  ];
}

const renderMoveList = (ctrl: AnalyseCtrl, concealOf?: ConcealOf): VNode =>
  hl('div.analyse__moves.areplay', { hook: ctrl.treeView.hook() }, [
    hl('div', [ctrl.treeView.render(concealOf), renderResult(ctrl)]),
  ]);

const renderMaterialDiffs = (ctrl: AnalyseCtrl): [VNode, VNode] =>
  materialView.renderMaterialDiffs(
    ctrl.showCapturedMaterial(),
    ctrl.bottomColor(),
    ctrl.node.fen,
    !!(ctrl.data.player.checks || ctrl.data.opponent.checks), // showChecks
    ctrl.nodeList,
    ctrl.node.ply,
  );

function makeConcealOf(_: AnalyseCtrl): ConcealOf | undefined {
  return undefined;
}

let prevForceInnerCoords: boolean;
function forceInnerCoords(ctrl: AnalyseCtrl, v: boolean) {
  if (ctrl.data.pref.coords === Prefs.Coords.Outside) {
    if (prevForceInnerCoords !== v) {
      prevForceInnerCoords = v;
      $('body').toggleClass('coords-in', v).toggleClass('coords-out', !v);
    }
  }
}

function renderPlayerStrips(ctrl: AnalyseCtrl): [VNode, VNode] | undefined {
  const renderPlayerStrip = (cls: string, materialDiff: VNode, clock?: VNode): VNode =>
    hl('div.analyse__player_strip.' + cls, [materialDiff, clock]);

  const clocks = renderClocks(ctrl, ctrl.path),
    whitePov = ctrl.bottomIsWhite(),
    materialDiffs = renderMaterialDiffs(ctrl);

  return [
    renderPlayerStrip('top', materialDiffs[0], clocks?.[whitePov ? 1 : 0]),
    renderPlayerStrip('bottom', materialDiffs[1], clocks?.[whitePov ? 0 : 1]),
  ];
}
