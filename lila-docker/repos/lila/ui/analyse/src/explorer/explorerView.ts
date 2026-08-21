import type { VNode } from 'snabbdom';
import * as licon from 'lib/licon';
import { displayLocale, numberFormat } from 'lib/format';
import { bind, dataIcon, type MaybeVNode, type LooseVNodes, hl } from 'lib/view';
import { moveArrowAttributes } from './explorerUtil';
import type AnalyseCtrl from '../ctrl';
import {
  isOpening,
  isTablebase,
  type TablebaseCategory,
  type OpeningData,
  type OpeningMoveStats,
} from './interfaces';
import { MAX_DEPTH } from './explorerCtrl';
import { showTablebase } from './tablebaseView';

type ExplorerViewOpts = {
  force?: boolean;
  closable?: boolean;
};

function resultBar(move: OpeningMoveStats): VNode {
  const sum = move.white + move.draws + move.black;
  const section = (key: 'white' | 'black' | 'draws') => {
    const percent = (move[key] * 100) / sum;
    return hl(
      'span.' + key,
      { attrs: { style: 'width: ' + Math.round((move[key] * 1000) / sum) / 10 + '%' } },
      percent > 12 ? Math.round(percent) + (percent > 20 ? '%' : '') : '',
    );
  };
  return hl('div.bar', ['white', 'draws', 'black'].map(section));
}

function showMoveTable(ctrl: AnalyseCtrl, data: OpeningData): VNode | null {
  if (!data.moves.length) return null;
  const sumTotal = data.white + data.black + data.draws;
  const movesWithCurrent =
    data.moves.length > 1
      ? [
          ...data.moves,
          {
            white: data.white,
            black: data.black,
            draws: data.draws,
            uci: '',
            san: 'Σ',
          } as OpeningMoveStats,
        ]
      : data.moves;

  return hl('table.moves', [
    hl('thead', [
      hl('tr', [
        hl('th', 'Move'),
        hl('th', { attrs: { colspan: 2 } }, 'Games'),
        hl('th', 'White / Draw / Black'),
      ]),
    ]),
    hl(
      'tbody',
      moveArrowAttributes(ctrl, { fen: data.fen, onClick: (_, uci) => uci && ctrl.explorerMove(uci) }),
      movesWithCurrent.map(move => {
        const total = move.white + move.draws + move.black;
        return hl(`tr${move.uci ? '' : '.sum'}`, { key: move.uci, attrs: { 'data-uci': move.uci } }, [
          hl(
            'td',
            { attrs: { title: move.opening ? `${move.opening.eco}: ${move.opening.name}` : '' } },
            move.san,
          ),
          hl('td', ((total / sumTotal) * 100).toFixed(0) + '%'),
          hl('td', bigNumberFormatter ? bigNumberFormatter.format(total) : numberFormat(total)),
          hl('td', { attrs: { title: moveStatsTooltip(ctrl, move) } }, resultBar(move)),
        ]);
      }),
    ),
  ]);
}

const bigNumberFormatter =
  window.Intl && Intl.NumberFormat ? new Intl.NumberFormat(displayLocale, { notation: 'compact' }) : null;

function moveStatsTooltip(ctrl: AnalyseCtrl, move: OpeningMoveStats): string {
  if (!move.uci) return 'Total';
  if (ctrl.explorer.opts.showRatings) {
    if (move.averageRating) return `Average rating: ${move.averageRating}`;
    if (move.averageOpponentRating)
      return `Performance rating: ${move.performance}, average opponent: ${move.averageOpponentRating}`;
  }
  return '';
}

const closeButton = (ctrl: AnalyseCtrl): VNode =>
  hl(
    'button.button.button-empty.text',
    { attrs: dataIcon(licon.X), hook: bind('click', ctrl.toggleExplorer, ctrl.redraw) },
    'Close',
  );

const showEmpty = (ctrl: AnalyseCtrl, data: OpeningData | undefined, opts: ExplorerViewOpts): VNode => {
  const isTooDeep = ctrl.explorer.root.node.ply >= MAX_DEPTH;
  return hl('div.data.empty', [
    explorerTitle(ctrl, opts),
    openingTitle(ctrl, data),
    hl('div.message', [
      hl('strong', isTooDeep ? 'Max depth reached' : 'No line found'),
      hl('p.notice', 'Try another line'),
    ]),
  ]);
};

const showGameEnd = (ctrl: AnalyseCtrl, title: string, opts: ExplorerViewOpts): VNode =>
  hl('div.data.empty', [
    explorerTitle(ctrl, opts),
    hl('div.title', 'Game over'),
    hl('div.message', [hl('i', { attrs: dataIcon(licon.InfoCircle) }), hl('h3', title)]),
  ]);

const openingTitle = (ctrl: AnalyseCtrl, data?: OpeningData) => {
  const opening = data?.opening;
  const title = opening ? `${opening.eco} ${opening.name}` : '';
  return hl(
    'div.title',
    { attrs: opening ? { title } : {} },
    opening
      ? [hl('a', { attrs: { href: `/opening/${opening.name}`, target: '_blank' } }, title)]
      : [showTitle(ctrl.data.game.variant)],
  );
};

let lastShow: MaybeVNode;
export const clearLastShow = () => {
  lastShow = undefined;
};

function show(ctrl: AnalyseCtrl, opts: ExplorerViewOpts): MaybeVNode {
  const data = ctrl.explorer.current();
  if (data && isOpening(data)) {
    const moveTable = showMoveTable(ctrl, data);
    if (moveTable)
      lastShow = hl('div.data', [
        explorerTitle(ctrl, opts),
        data?.opening && openingTitle(ctrl, data),
        moveTable,
      ]);
    else lastShow = showEmpty(ctrl, data, opts);
  } else if (data && isTablebase(data)) {
    const row = (category: TablebaseCategory, title: string, tooltip?: string) =>
      showTablebase(
        ctrl,
        data.fen,
        title,
        tooltip,
        data.moves.filter(m => m.category === category),
      );
    if (data.moves.length)
      lastShow = hl('div.data', [
        row('loss', 'Winning'),
        row('unknown', 'Unknown'),
        row('syzygy-loss', 'Win or 50 moves by prior mistake', 'Unknown due to rounding'),
        row('maybe-loss', 'Win or 50 moves'),
        row('blessed-loss', 'Win prevented by 50 move rule'),
        row('draw', 'Drawn'),
        row('cursed-win', 'Loss saved by 50 move rule'),
        row('maybe-win', 'Loss or 50 moves'),
        row('syzygy-win', 'Loss or 50 moves by prior mistake', 'Unknown due to rounding'),
        row('win', 'Losing'),
      ]);
    else if (data.checkmate) lastShow = showGameEnd(ctrl, 'Checkmate', opts);
    else if (data.stalemate) lastShow = showGameEnd(ctrl, 'Stalemate', opts);
    else lastShow = showEmpty(ctrl, undefined, opts);
  }
  return lastShow;
}

const explorerTitle = (ctrl: AnalyseCtrl, opts: ExplorerViewOpts) => {
  const active = (nodes: LooseVNodes, title: string) =>
    hl(
      'span.active.text.masters',
      {
        attrs: { title, ...dataIcon(licon.Book) },
      },
      nodes,
    );
  const masterDbInfo = '2 million games from top rated FIDE players from 1952 to 2024-08';
  return hl('div.explorer-title', [
    hl('div.explorer-title__dbs', [active([hl('strong', 'Masters'), ' database'], masterDbInfo)]),
    opts.closable === false ? null : closeButton(ctrl),
  ]);
};

function showTitle(variant: Variant) {
  if (variant.key === 'standard' || variant.key === 'fromPosition') return 'Opening book';
  return `${variant.name} opening book`;
}

function showFailing(ctrl: AnalyseCtrl, opts: ExplorerViewOpts) {
  return hl('div.data.empty', [
    explorerTitle(ctrl, opts),
    hl('div.title', showTitle(ctrl.data.game.variant)),
    hl('div.failing.message', [
      hl('h3', 'Oops, sorry!'),
      hl('p.notice', ctrl.explorer.failing()?.toString()),
    ]),
  ]);
}

let lastFen: FEN = '';

export function renderExplorerPanel(ctrl: AnalyseCtrl, opts: ExplorerViewOpts = {}): VNode | undefined {
  const explorer = ctrl.explorer;
  if (!opts.force && !explorer.enabled()) return;
  const data = explorer.current(),
    loading = explorer.loading() || (!data && !explorer.failing()),
    content = explorer.failing() ? showFailing(ctrl, opts) : show(ctrl, opts);
  return hl(
    'section.explorer-box.sub-box',
    {
      attrs: { id: 'analyse-opening-explorer' },
      class: { loading, reduced: !!explorer.failing() || explorer.movesAway() > 2 },
      hook: {
        insert: vnode => ((vnode.elm as HTMLElement).scrollTop = 0),
        postpatch(_, vnode) {
          if (!data || lastFen === data.fen) return;
          (vnode.elm as HTMLElement).scrollTop = 0;
          lastFen = data.fen;
        },
      },
    },
    [hl('div.overlay'), content],
  );
}

export default function (ctrl: AnalyseCtrl): VNode | undefined {
  return renderExplorerPanel(ctrl);
}
