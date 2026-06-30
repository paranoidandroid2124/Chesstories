import { view as cevalView, renderEval as normalizeEval } from 'lib/ceval';
import { parseFen } from 'chessops/fen';
import { defined } from 'lib';
import * as licon from 'lib/licon';
import { type VNode, type LooseVNode, type LooseVNodes, bind, onInsert, icon, hl } from 'lib/view';
import { displayColumns, isMobile } from 'lib/device';
import * as materialView from 'lib/game/view/material';

import { view as actionMenu } from './actionMenu';
import explorerView from '../explorer/explorerView';
import { view as forkView } from '../fork';
import renderClocks from './clocks';
import * as control from '../control';
import * as chessground from '../ground';
import type AnalyseCtrl from '../ctrl';
import type {
  ConcealOf,
  ImportHistoryAccount,
  ImportHistoryAnalysis,
  ImportHistoryView,
} from '../interfaces';
import * as pgnExport from '../pgnExport';
import { spinnerVdom as spinner, stepwiseScroll } from 'lib/view';
import * as Prefs from 'lib/prefs';
import statusView from 'lib/game/view/status';
import { plyToTurn } from 'lib/game/chess';
import { dispatchChessgroundResize } from 'lib/chessgroundResize';
import pgnImport, { renderPgnError } from '../pgnImport';
import { normalizeInlinePgn, reviewStudyCreateGate, type PgnDraftStatus } from '../pgnPipeline';
import { storage } from 'lib/storage';
import { chesstoryBriefSections, type ChesstoryBriefSection } from '../chesstoryBrief';

interface ViewContext {
  ctrl: AnalyseCtrl;
  allowVideo?: boolean;
  concealOf?: ConcealOf;
  showCevalPvs: boolean;
  gamebookPlayView?: VNode;
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
    showCevalPvs: true,
    gamebookPlayView: undefined,
    playerBars,
    playerStrips: renderPlayerStrips(ctrl),
    gaugeOn,
    needsInnerCoords: ctrl.showCapturedMaterial() || !!playerBars,
  };
}

export function renderMain(ctx: ViewContext, ...kids: LooseVNodes[]): VNode {
  const { ctrl, playerBars, gaugeOn, gamebookPlayView, needsInnerCoords } = ctx;
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
        'gamebook-play': !!gamebookPlayView,
        'analyse-hunter': ctrl.opts.hunter,
        'analyse--notebook': ctrl.isStudy(),
      },
    },
    [renderSidebar(ctrl), ...kids],
  );
}

function renderSidebar(ctrl: AnalyseCtrl): VNode | undefined {
  return hl(
    'div.analyse__sidebar',
    workspaceTools(ctrl).map(tool =>
      hl(
        'button.fbt',
        {
          key: tool.id,
          attrs: {
            title: tool.summary,
            'data-act': tool.id,
          },
          hook: bind('click', tool.open, ctrl.redraw),
          class: {
            active: tool.active,
            busy: !!tool.busy,
          },
        },
        [renderWorkspaceToolIcon(ctrl, tool), hl('span.label', tool.label)],
      ),
    ),
  );
}

type WorkspaceToolId = 'opening-explorer' | 'action-menu';
type NotebookGlyphKind = 'notebook' | 'bookmark' | 'page' | 'section';

type WorkspaceTool = {
  id: WorkspaceToolId;
  label: string;
  summary: string;
  icon: string;
  active: boolean;
  busy?: boolean;
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
    default:
      return [
        hl('path', {
          attrs: {
            d: 'M8.5 7.5a2 2 0 0 1 2-2h10.5a3 3 0 0 1 3 3v15.5a2 2 0 0 1-2 2H11a2.5 2.5 0 0 1-2.5-2.5V7.5Z',
          },
        }),
        hl('path', { attrs: { d: 'M12 5.5v21' } }),
        hl('path', { attrs: { d: 'M15 11.5h6' } }),
        hl('path', { attrs: { d: 'M15 15h6' } }),
        hl('path', { attrs: { d: 'M15 18.5h4.5' } }),
        hl('path', { attrs: { d: 'M12 23.5c.7-1 1.7-1.5 3-1.5h9' } }),
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
    default:
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

function renderWorkspaceDock(ctrl: AnalyseCtrl): VNode {
  const activeTool = ctrl.activeControlBarTool();
  const tools = workspaceTools(ctrl);
  return hl(`section.analyse__workspace-dock${activeTool ? '' : '.is-idle'}`, [
    hl('div.analyse__workspace-dock-head', [
      hl('strong', activeTool ? 'Choose what to study' : 'Keep the board in sight'),
      hl(
        'span',
        activeTool
          ? 'Move between openings, candidate lines, and board view without losing the current move.'
          : 'Open a study view while the board and moves stay anchored.',
      ),
    ]),
    hl(
      'div.analyse__workspace-dock-grid',
      tools.map(tool =>
        hl(
          'button.analyse__workspace-card',
          {
            key: tool.id,
            attrs: {
              type: 'button',
              title: tool.summary,
              'data-tool-id': tool.id,
            },
            hook: bind('click', tool.open, ctrl.redraw),
            class: {
              active: tool.active,
              busy: !!tool.busy,
            },
          },
          [
            hl('span.analyse__workspace-card-icon', [renderWorkspaceToolIcon(ctrl, tool)]),
            hl('span.analyse__workspace-card-copy', [hl('strong', tool.label), hl('span', tool.summary)]),
            tool.busy ? hl('span.analyse__workspace-card-state', 'Working') : null,
          ],
        ),
      ),
    ),
  ]);
}

export function renderTools({ ctrl, concealOf, allowVideo }: ViewContext, embeddedVideo?: LooseVNode) {
  const showCeval = ctrl.isCevalAllowed() && ctrl.showCeval();
  const activeTool = explorerView(ctrl);
  return hl('div.analyse__tools', [
    allowVideo && embeddedVideo,
    showCeval && cevalView.renderCeval(ctrl),
    showCeval && cevalView.renderPvs(ctrl),
    ctrl.isStudy() && renderChesstoryBrief(ctrl),
    renderMoveList(ctrl, concealOf),
    forkView(ctrl, concealOf),
    displayColumns() > 1 && renderWorkspaceDock(ctrl),
    activeTool,
    ctrl.actionMenu() && actionMenu(ctrl),
  ]);
}

function renderChesstoryBrief(ctrl: AnalyseCtrl): VNode {
  const node = ctrl.getNode();
  const moveLabel = node.san
    ? `${plyToTurn(node.ply)}${node.ply % 2 === 1 ? '.' : '...'} ${node.san}`
    : 'Initial position';

  return hl('section.analyse__chesstory-brief', [
    hl('div.analyse__chesstory-brief-head', [
      hl('div.analyse__chesstory-brief-title', [
        hl('span', 'Chesstory review'),
        hl('strong', 'Opening to middlegame'),
      ]),
      hl('span.analyse__chesstory-brief-state', 'Not filled yet'),
    ]),
    hl('div.analyse__chesstory-brief-focus', [hl('span', 'Decision point'), hl('strong', moveLabel)]),
    hl(
      'div.analyse__chesstory-brief-list',
      chesstoryBriefSections().map(section => renderChesstoryBriefSection(section)),
    ),
  ]);
}

function renderChesstoryBriefSection(section: ChesstoryBriefSection): VNode {
  return hl('div.analyse__chesstory-brief-row', [
    hl('div.analyse__chesstory-brief-row-head', [
      hl('strong', section.title),
      section.pending ? hl('span', 'Add note') : null,
    ]),
    hl('p', section.body),
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
  const recentDrafts = ctrl
    .recentImportDrafts()
    .filter(draft => draft !== pgnInspection.normalized && draft !== currentInspection.normalized);
  const serverHistory = ctrl.opts.importHistory;
  const submitPgnDraft = () => {
    if (pgnInspection.status !== 'ready') return;
    const draft = defined(ctrl.pgnInput) ? ctrl.pgnInput : pgnExport.renderFullTxt(ctrl);
    if (draft !== pgnExport.renderFullTxt(ctrl)) ctrl.importPgn(draft);
  };
  return hl('div.copyables.copyables--workspace', [
    hl('div.analyse-review__summary-grid.copyables__summary', [
      compactSummaryCard(pgnInspection.headline, 'game text'),
      compactSummaryCard(
        pgnDraftReplaySize(pgnInspection),
        pgnInspection.preview ? 'moves to replay' : 'score text',
      ),
      compactSummaryCard(fenInspection.headline, 'board position'),
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
      recentDrafts.length ? renderRecentImportDrafts(ctrl, recentDrafts) : null,
      serverHistory ? renderServerImportHistory(serverHistory) : null,
    ]),
  ]);
}

function renderNotebookPanelCover(title: string, subtitle: string, detail: string): VNode {
  return hl('div.copyables__study-cover', [
    hl('div.copyables__study-cover-spine'),
    hl('div.copyables__study-cover-pages', [
      hl('span.copyables__study-cover-page'),
      hl('span.copyables__study-cover-page.copyables__study-cover-page--mid'),
      hl('span.copyables__study-cover-page.copyables__study-cover-page--inner'),
    ]),
    hl('div.copyables__study-cover-face', [
      hl('div.copyables__study-cover-seal', [
        renderNotebookGlyph('notebook', 'copyables__study-cover-glyph'),
      ]),
      hl('span.copyables__study-cover-eyebrow', 'Review study'),
      hl('strong.copyables__study-cover-title', title),
      hl('span.copyables__study-cover-subtitle', subtitle),
      hl('span.copyables__study-cover-detail', detail),
    ]),
  ]);
}

function renderStudyWorkspacePanel(ctrl: AnalyseCtrl): VNode | null {
  const study = ctrl.studyData();
  if (!study) return null;

  const node = ctrl.getNode();
  const moveLabel = node.san
    ? `${plyToTurn(node.ply)}${node.ply % 2 === 1 ? '.' : '...'} ${node.san}`
    : 'Initial position';
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
  const privateStudy = study.visibility === 'private';
  const copyLabel = privateStudy ? ' Copy private study link' : ' Copy review study link';
  const sharePill = privateStudy ? 'Only collaborators can open the link' : 'Share the exact section link';

  return hl('section.copyables__study.copyables__study--current', [
    hl('div.copyables__study-head', [
      renderNotebookPanelCover(study.name, 'Opening to middlegame', `${study.chapterName} review`),
      hl('div.copyables__study-copy', [
        hl('span.copyables__study-eyebrow', 'Review study'),
        hl('strong', study.name),
        hl(
          'span.copyables__study-subline',
          `Explain how ${study.chapterName} turns opening choices into middlegame plans.`,
        ),
      ]),
      hl('div.copyables__study-actions', [
        studyUrl
          ? hl('a.button.button-thin.copyables__study-button', { attrs: { href: studyUrl } }, [
              renderNotebookGlyph('page', 'copyables__study-button-glyph'),
              ' Open review study',
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
          [renderNotebookGlyph('bookmark', 'copyables__study-button-glyph'), copyLabel],
        ),
      ]),
    ]),
    hl('div.analyse-review__summary-grid.copyables__study-summary', [
      compactSummaryCard('Review record', 'mode'),
      compactSummaryCard('Opening plan', 'thread'),
      compactSummaryCard(`${study.chapters.length}`, 'sections'),
      compactSummaryCard(ctrl.isStudyWriting() ? 'Saving' : 'Ready', 'sync'),
    ]),
    hl(
      'div.copyables__study-thread',
      chesstoryBriefSections()
        .slice(0, 3)
        .map(section =>
          hl('div.copyables__study-thread-card', [
            hl('span', section.title),
            hl('strong', section.key === 'current-decision' ? moveLabel : section.body),
            hl(
              'p',
              section.key === 'current-decision'
                ? 'Attach notes and supporting lines to this move when it changes the plan.'
                : 'Use this slot before adding deeper variations.',
            ),
          ]),
        ),
    ),
    renderStudyStatusCard(syncMessage, actionMessage ? ctrl.studyActionToneValue() : syncTone),
    hl('div.copyables__study-pills', [
      studyFeaturePill('page', 'Moments, not move dumps'),
      studyFeaturePill('section', 'Opening to middlegame'),
      studyFeaturePill('bookmark', sharePill),
    ]),
  ]);
}

const studyVisibilityChoices = [
  {
    value: 'unlisted',
    title: 'Link sharing',
    help: 'Anyone with the link can review it.',
  },
  {
    value: 'private',
    title: 'Private',
    help: 'Only you can open this review study.',
  },
  {
    value: 'public',
    title: 'Public',
    help: 'Visible in public review study lists.',
  },
] as const;

function renderStudySetupModal(ctrl: AnalyseCtrl): VNode | null {
  if (!ctrl.studyCreateSetupVisible()) return null;
  const setup = ctrl.studyCreateSetupValues();
  const busy = ctrl.studyCreateBusy();

  return hl(
    'div.copyables__study-modal-mask',
    {
      hook: bind('click', e => {
        if (e.target === e.currentTarget) ctrl.closeStudyCreateSetup();
      }),
    },
    [
      hl(
        'form.copyables__study-modal',
        {
          hook: bind('submit', e => {
            e.preventDefault();
            void ctrl.submitStudyCreateSetup();
          }),
        },
        [
          hl('div.copyables__study-modal-head', [
            renderNotebookPanelCover('Review study', 'Opening to middlegame', 'Save the analysis'),
            hl('div.copyables__study-modal-copy', [
              hl('span.copyables__study-eyebrow', 'Review study setup'),
              hl('strong', 'Name this review study'),
              hl(
                'span.copyables__study-subline',
                'Save the loaded game as a study with playable lines and a section link. Add explanation notes after it opens.',
              ),
            ]),
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
            hl('span', 'First section'),
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
          ctrl.studyCreateErrorText() ? renderStudyStatusCard(ctrl.studyCreateErrorText()!, 'error') : null,
          hl('div.copyables__study-modal-actions', [
            hl(
              'button.button.button-metal.copyables__study-button',
              {
                attrs: { type: 'button', disabled: busy },
                hook: bind('click', () => {
                  ctrl.closeStudyCreateSetup();
                }),
              },
              'Cancel',
            ),
            hl(
              'button.button.copyables__study-button',
              {
                attrs: { type: 'submit', disabled: busy },
              },
              [
                renderNotebookGlyph('notebook', 'copyables__study-button-glyph'),
                busy ? ' Creating...' : ' Create review study',
              ],
            ),
          ]),
        ],
      ),
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
      renderNotebookPanelCover('Review study', 'Opening to middlegame', 'Make the game explainable'),
      hl('div.copyables__study-copy', [
        hl('span.copyables__study-eyebrow', 'Review study'),
        hl('strong', 'Turn this game into a review study'),
        hl(
          'span.copyables__study-subline',
          'Keep the game, key moves, and notes together so the opening can explain the middlegame.',
        ),
      ]),
      hl('div.copyables__study-actions', [
        createGate.disabled
          ? hl(
              'button.button.copyables__study-button',
              {
                attrs: { type: 'button', disabled: true },
              },
              [renderNotebookGlyph('notebook', 'copyables__study-button-glyph'), createGate.buttonLabel],
            )
          : needsAuth
            ? hl('a.button.copyables__study-button', { attrs: { href: ctrl.studyLoginHref() } }, [
                renderNotebookGlyph('bookmark', 'copyables__study-button-glyph'),
                ' Sign in to create',
              ])
            : hl(
                'button.button.copyables__study-button',
                {
                  attrs: createDisabled ? { type: 'button', disabled: true } : { type: 'button' },
                  hook: createDisabled
                    ? undefined
                    : bind('click', () => {
                        ctrl.openStudyCreateSetup();
                      }),
                },
                [
                  renderNotebookGlyph('notebook', 'copyables__study-button-glyph'),
                  busy ? ' Creating review...' : ` ${createGate.buttonLabel}`,
                ],
              ),
      ]),
    ]),
    hl('div.analyse-review__summary-grid.copyables__study-summary', [
      compactSummaryCard('Opening idea', 'start'),
      compactSummaryCard('Plan thread', 'middle'),
      compactSummaryCard('Board cues', 'evidence'),
    ]),
    renderStudyStatusCard(
      busy
        ? transferCount > 0
          ? `Creating the review study and carrying over ${transferCount} saved line${transferCount === 1 ? '' : 's'}.`
          : 'Creating the review study from the current game.'
        : createGate.message,
      busy ? 'info' : createGate.tone,
      busy ? undefined : createGate.title,
    ),
    error && !setupOpen ? renderStudyStatusCard(error, 'error') : null,
    hl('div.copyables__study-pills', [
      studyFeaturePill('page', 'Opening-to-plan notes'),
      studyFeaturePill('section', 'Playable lines'),
      studyFeaturePill('bookmark', 'Shareable section link'),
    ]),
    renderStudySetupModal(ctrl),
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
  const normalized = normalizeInlinePgn(draft);
  const normalizedCurrent = normalizeInlinePgn(currentPgn);
  const lines = draft ? draft.split(/\r?\n/).length : 0;
  const chars = draft.trim().length;
  let result: PgnDraftInspection;
  if (!normalized) {
    result = {
      status: 'empty',
      headline: 'Draft empty',
      message: 'Paste a game to load another game on this board.',
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

function compactSummaryCard(value: string, label: string): VNode {
  return hl('div.analyse-review__summary-card', [hl('strong', value), hl('span', label)]);
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

function renderStudyStatusCard(message: string, tone: 'info' | 'success' | 'error', title?: string): VNode {
  return hl(`div.copyables__study-status.copyables__study-status--${tone}`, [
    hl(
      'strong',
      title ||
        (tone === 'error'
          ? 'Review study issue'
          : tone === 'info'
            ? 'Review study in progress'
            : 'Review study ready'),
    ),
    hl('span', message),
  ]);
}

function studyFeaturePill(kind: NotebookGlyphKind, label: string): VNode | null {
  return label
    ? hl('span.copyables__study-pill', [
        renderNotebookGlyph(kind, 'copyables__study-pill-glyph'),
        hl('span', label),
      ])
    : null;
}

function renderImportPreview(current: PgnDraftInspection, incoming: PgnDraftInspection): VNode {
  return hl('div.copyables__preview', [
    hl('div.copyables__preview-card', [
      hl('span.copyables__preview-label', 'On the board'),
      hl('strong', current.preview?.opening || current.preview?.variant || 'Current board'),
      hl('span', pgnDraftPlayerDetail(current)),
    ]),
    hl('div.copyables__preview-card', [
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

function renderRecentImportDrafts(ctrl: AnalyseCtrl, drafts: string[]): VNode {
  return hl('div.copyables__recent', [
    hl('div.copyables__recent-head', [
      hl('strong', 'Recent game drafts'),
      hl('span', 'Games you loaded in this session.'),
    ]),
    hl(
      'div.copyables__recent-list',
      drafts.map((draft, index) => {
        const inspection = inspectPgnDraft(draft, '');
        return hl(
          'button.copyables__recent-item',
          {
            key: draft.slice(0, 40) + index,
            attrs: { type: 'button' },
            hook: bind('click', () => ctrl.useImportDraft(draft)),
          },
          [
            hl('strong', inspection.preview?.opening || inspection.preview?.variant || `Draft ${index + 1}`),
            hl('span', pgnDraftPlayerDetail(inspection)),
          ],
        );
      }),
    ),
  ]);
}

function renderServerImportHistory(history: ImportHistoryView): VNode | undefined {
  const hasAnalyses = !!history.recentAnalyses?.length;
  const hasAccounts = !!history.recentAccounts?.length;
  if (!hasAnalyses && !hasAccounts) return renderEmptySavedHistory();
  const blocks: VNode[] = [];
  if (hasAnalyses) {
    blocks.push(
      hl('div.copyables__recent', [
        hl('div.copyables__recent-head', [
          hl('strong', 'Recent games'),
          hl('span', 'Games you can reopen from any signed-in device.'),
        ]),
        hl(
          'div.copyables__recent-list',
          history.recentAnalyses.map((entry, index) =>
            renderSavedAnalysisEntry(entry, history.currentAnalysisId === entry.id, index === 0),
          ),
        ),
      ]),
    );
  }
  if (hasAccounts) {
    blocks.push(
      hl('div.copyables__recent', [
        hl('div.copyables__recent-head', [
          hl('strong', 'Recent players'),
          hl('span', 'Jump back into saved player game lists without retyping usernames.'),
        ]),
        hl(
          'div.copyables__recent-list',
          history.recentAccounts.map((entry, index) => renderSavedAccountEntry(entry, index === 0)),
        ),
      ]),
    );
  }
  return blocks.length ? hl('div.copyables__history-stack', blocks) : undefined;
}

function renderEmptySavedHistory(): VNode {
  return hl('div.copyables__recent.copyables__recent--empty', [
    hl('div.copyables__recent-head', [
      hl('strong', 'No saved games yet'),
      hl(
        'span',
        'Load a pasted game or player game once, and it will stay here for quick reopen on any signed-in device.',
      ),
    ]),
    hl('div.copyables__empty-actions', [
      hl('a.copyables__recent-link', { attrs: { href: '/import' } }, 'Open recent games'),
      hl('span.copyables__recent-link-note', 'or paste a game below to start your saved game list.'),
    ]),
  ]);
}

function renderSavedAnalysisEntry(entry: ImportHistoryAnalysis, current: boolean, priority: boolean): VNode {
  const supportLine = analysisSupportLine(entry);
  return hl(
    'a.copyables__recent-item',
    {
      key: entry.id,
      attrs: { href: entry.href },
      class: { 'is-current': current, 'is-priority': priority },
    },
    [
      hl('div.copyables__recent-kicker', [
        entry.providerLabel
          ? renderHistoryBadge(
              entry.providerLabel,
              'copyables__badge--provider',
              providerToneClass(entry.provider),
            )
          : null,
        renderHistoryBadge(
          sourceTypeLabel(entry.sourceType),
          `copyables__badge--${sourceTypeTone(entry.sourceType)}`,
        ),
        current
          ? renderHistoryBadge('Current', 'copyables__badge--current')
          : priority
            ? renderHistoryBadge('Latest', 'copyables__badge--priority')
            : null,
      ]),
      hl('div.copyables__recent-body', [
        hl('div.copyables__recent-title-row', [
          hl('strong', current ? `${entry.title} (Current)` : entry.title),
          hl('span.copyables__recent-cta', current ? 'On board' : priority ? 'Open latest' : 'Open'),
        ]),
        hl('span.copyables__recent-subline', analysisMetaLine(entry)),
        supportLine ? hl('span.copyables__recent-foot', supportLine) : null,
        hl(
          'span.copyables__meta',
          `${current ? 'Opened and active' : 'Opened'} ${formatImportTimestamp(entry.openedAt)}`,
        ),
      ]),
    ],
  );
}

function renderSavedAccountEntry(entry: ImportHistoryAccount, priority: boolean): VNode {
  return hl(
    'a.copyables__recent-item',
    {
      key: `${entry.provider}:${entry.username}`,
      attrs: { href: entry.href },
      class: { 'is-priority': priority },
    },
    [
      hl('div.copyables__recent-kicker', [
        renderHistoryBadge(
          entry.providerLabel,
          'copyables__badge--provider',
          providerToneClass(entry.provider),
        ),
        priority ? renderHistoryBadge('Latest', 'copyables__badge--priority') : null,
        entry.lastAnalysedAt ? renderHistoryBadge('Studied', 'copyables__badge--activity') : null,
      ]),
      hl('div.copyables__recent-body', [
        hl('div.copyables__recent-title-row', [
          hl('strong', `@${entry.username}`),
          hl('span.copyables__recent-cta', priority ? 'Open latest' : 'Open'),
        ]),
        hl('span.copyables__recent-subline', `${entry.analysisCount} saved games`),
        hl(
          'span.copyables__recent-foot',
          priority
            ? 'Fastest way back to your most recently loaded player games.'
            : 'Reopen this player game list without retyping the username.',
        ),
        hl('span.copyables__meta', `Active ${formatImportTimestamp(entry.activityAt)}`),
      ]),
    ],
  );
}

function analysisMetaLine(entry: ImportHistoryAnalysis): string {
  const line = [
    entry.username ? `@${entry.username}` : undefined,
    entry.playedAtLabel && entry.playedAtLabel !== '-' ? entry.playedAtLabel : undefined,
    entry.result,
    entry.speed && entry.speed !== '-' ? entry.speed : undefined,
  ]
    .filter(Boolean)
    .join(' • ');
  return line || 'Saved game ready to reopen.';
}

function analysisSupportLine(entry: ImportHistoryAnalysis): string | undefined {
  const line = [
    entry.opening,
    entry.variant && entry.variant !== entry.opening ? entry.variant : undefined,
    entry.sourceType === 'manual' ? 'Pasted game' : 'Imported game',
  ]
    .filter(Boolean)
    .join(' • ');
  return line || undefined;
}

function formatImportTimestamp(value: string | undefined): string {
  if (!value) return 'recently';
  const date = new Date(value);
  return Number.isNaN(date.getTime()) ? value : date.toLocaleString();
}

function renderHistoryBadge(label: string, ...classes: string[]): VNode {
  const suffix = classes
    .filter(Boolean)
    .map(cls => `.${cls}`)
    .join('');
  return hl(`span.copyables__badge${suffix}`, label);
}

function providerToneClass(provider: string | undefined): string {
  if (provider === 'chesscom') return 'copyables__badge--chesscom';
  if (provider === 'lichess') return 'copyables__badge--lichess';
  return '';
}

function sourceTypeLabel(sourceType: string): string {
  return sourceType === 'manual' ? 'Pasted game' : 'Imported game';
}

function sourceTypeTone(sourceType: string): string {
  return sourceType === 'manual' ? 'manual' : 'imported';
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
