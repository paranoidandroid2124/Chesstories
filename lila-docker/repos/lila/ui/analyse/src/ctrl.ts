import { playable, playedTurns, readDests, writeDests, validUci } from 'lib/game';
import * as keyboard from './keyboard';
import { treeReconstruct, plyColor } from './util';
import { plural } from './view/util';
import type { AnalyseOpts, AnalyseData, JustCaptured, StudyView } from './interfaces';
import type { Api as ChessgroundApi } from '@lichess-org/chessground/api';
import { Autoplay, type AutoplayDelay } from './autoplay';
import { makeTree, treePath, treeOps, type TreeWrapper } from 'lib/tree';
import { compute as computeAutoShapes } from './autoShape';
import type { Config as ChessgroundConfig } from '@lichess-org/chessground/config';
import type { CevalHandler, EvalMeta, CevalOpts, Work } from 'lib/ceval';
import { CevalCtrl, isEvalBetter } from 'lib/ceval';
import { TreeView } from './treeView/treeView';
import type { Prop, Toggle } from 'lib';
import {
  defined,
  prop,
  toggle,
  throttle,
  requestIdleCallback,
  propWithEffect,
  myUserId,
  myUsername,
} from 'lib';
import { preferenceLocalStorage } from 'lib/cookieConsent';
import { pubsub } from 'lib/pubsub';
import type { DrawShape } from '@lichess-org/chessground/draw';
import { scalachessCharPair } from 'chessops/compat';
import { ForkCtrl } from './fork';
import { nextGlyphSymbol, add3or5FoldGlyphs } from './nodeFinder';
import { opposite, parseUci, makeSquare, roleToChar } from 'chessops/util';
import { type Outcome, isNormal } from 'chessops/types';
import { makeFen, parseFen } from 'chessops/fen';
import { setupPosition } from 'chessops/variant';
import { makeSanAndPlay } from 'chessops/san';
import { makeUci } from 'chessops';
import { storedBooleanProp, storedProp, tempStorage } from 'lib/storage';
import { PromotionCtrl } from 'lib/game/promotion';
import ExplorerCtrl from './explorer/explorerCtrl';
import { tablebase as loadTablebase } from './explorer/explorerXhr';
import type { TablebaseCategory } from './explorer/interfaces';
import { uciToMove } from '@lichess-org/chessground/util';
import { IdbTree } from './idbTree';
import pgnImport from './pgnImport';
import * as pgnExport from './pgnExport';
import { emptyPgnError, normalizeInlinePgn, submitPgnToImportPipeline } from './pgnPipeline';
import * as studyApi from './studyApi';
import { decodeChesstoryMoveMeaningResponse, type ChesstoryMoveMeaningPayload } from './chesstoryBrief';
import { defaultInit, jsonHeader, xhrHeader, ensureOk } from 'lib/xhr';

import type { PgnError } from 'chessops/pgn';

import { confirm } from 'lib/view';
import { displayColumns } from 'lib/device';
import { dispatchChessgroundResize } from 'lib/chessgroundResize';
import * as Prefs from 'lib/prefs';
import {
  applyBoardLabelMode,
  boardLabelModeFromCoords,
  boardLabelModeToCoords,
  type BoardLabelMode,
} from './boardWorkspace';

const recentImportStorageKey = 'analyse.import-recents.v1';
const boardLabelModes = new Set<BoardLabelMode>(['off', 'inside', 'rim', 'full']);

interface AnalyseHistoryState {
  analysePly: Ply;
}

interface ChesstoryMoveMeaningRequest {
  fen: FEN;
  playedMoveUci: Uci;
  variations: Array<{
    moves: Uci[];
    scoreCp: number;
    mate?: number;
    depth: number;
  }>;
  ply: Ply;
  openingContext?: {
    eco?: string;
    name?: string;
    family?: string;
  };
  movePrefixUci: Uci[];
  probeResults?: ChesstoryProbeResult[];
}

interface ChesstoryProbeRequest {
  id: string;
  fen: FEN;
  moves: Uci[];
  depth: number;
  purpose?: string;
  multiPv?: number;
  baselineEvalCp?: number;
  candidateMove?: Uci;
  opponentResourceMove?: Uci;
  depthFloor?: number;
  horizon?: string;
  variationHash?: string;
  comparisonFen?: FEN;
}

interface ChesstoryProbeResult {
  id: string;
  fen: FEN;
  replyLines?: Array<{
    moves: Uci[];
    scoreCp: number;
    mate?: number;
    depth: number;
  }>;
  purpose?: string;
  probedMove?: Uci;
  depth?: number;
  candidateMove?: Uci;
  opponentResourceMove?: Uci;
  horizon?: string;
  variationHash?: string;
  generatedAtEpochMs?: number;
  tablebase?: {
    terminal: { fen: FEN; wdl: number; dtm?: number };
    comparison: { fen: FEN; wdl: number; dtm?: number };
    legalMoves: Array<{ moveUci: Uci; resultingWdl: number; resultingDtm?: number }>;
  };
}

interface ChesstoryProbeBatch {
  results: ChesstoryProbeResult[];
  failed: boolean;
}

function loginHref(): string {
  return `/login?referrer=${encodeURIComponent(location.pathname + location.search)}`;
}

export default class AnalyseCtrl implements CevalHandler {
  data: AnalyseData;
  element: HTMLElement;
  tree: TreeWrapper;
  chessground: ChessgroundApi;
  ceval: CevalCtrl;
  idbTree: IdbTree = new IdbTree(this);
  actionMenu: Toggle = toggle(false);
  isEmbed: boolean;

  // current tree state, cursor, and denormalized node lists
  path: Tree.Path;
  node: Tree.Node;
  nodeList: Tree.Node[];
  mainline: Tree.Node[];

  // sub controllers
  autoplay: Autoplay;
  explorer: ExplorerCtrl;
  fork: ForkCtrl;
  promotion: PromotionCtrl;

  // state flags
  justPlayed?: string; // pos
  justCaptured?: JustCaptured;
  redirecting = false;
  onMainline = true;
  synthetic: boolean; // false if coming from a real game
  ongoing: boolean; // true if real game is ongoing
  private cevalEnabledProp = storedBooleanProp('engine.enabled', false);

  // display flags
  flipped = false;
  showComments = true; // whether to display comments in the move tree
  variationArrowOpacity: Prop<number | false>;
  showGauge: Prop<boolean>;
  private showCevalProp: Prop<boolean> = storedBooleanProp('analyse.show-engine', !!this.cevalEnabledProp());
  private boardLabelModeProp!: Prop<BoardLabelMode>;
  private showCapturedProp!: Prop<boolean>;
  possiblyShowMoveAnnotationsOnBoard = storedBooleanProp('analyse.show-move-annotation', true);
  keyboardHelp: boolean = location.hash === '#keyboard';
  threatMode: Prop<boolean> = prop(false);
  disclosureMode = storedBooleanProp('analyse.disclosure.enabled', false);

  treeView: TreeView;
  cgVersion = {
    js: 1, // increment to recreate chessground
    dom: 1,
  };

  // underboard inputs
  fenInput?: string;
  pgnInput?: string;
  pgnError?: string;
  private recentImportDraftsCache?: string[];

  // study write queue (HTTP only, no sockets)
  private studyWriteQueue: Array<() => Promise<void>> = [];
  private studyWriting = false;
  studyWriteError?: string;
  private studyCreateLoading = false;
  private studyCreateError: string | null = null;
  private studyCreateSetupOpen = false;
  private studyCreateForm: Required<studyApi.StudyCreateSetup> = {
    name: 'Game review',
    chapterName: 'Opening to middlegame',
    visibility: 'unlisted',
  };
  private studyActionMessage: string | null = null;
  private studyActionTone: 'info' | 'success' | 'error' = 'info';
  private studyActionTimer?: number;
  private studyTransferCount = 0;
  private chesstoryBriefKey?: string;
  private chesstoryBriefLoadingKey?: string;
  private chesstoryBriefFailureKey?: string;
  private chesstoryBriefRetryAfter = 0;
  private chesstoryBriefGeneration = 0;
  private chesstoryBriefAbort?: AbortController;
  private chesstoryProbeCancel?: () => void;
  private chesstoryBriefPayload?: ChesstoryMoveMeaningPayload;
  private chesstoryBriefLoading = false;
  private chesstoryBriefUnavailable = false;

  // other paths
  initialPath: Tree.Path;
  contextMenuPath?: Tree.Path;
  gamePath?: Tree.Path;
  pendingCopyPath: Prop<Tree.Path | null>;
  pendingDeletionPath: Prop<Tree.Path | null>;

  // misc
  requestInitialPly?: number; // start ply from the URL location hash
  cgConfig: any; // latest chessground config (useful for revert)
  pvUciQueue: Uci[] = [];
  private restoringHistory = false;

  constructor(
    readonly opts: AnalyseOpts,
    readonly redraw: Redraw,
  ) {
    this.data = opts.data;
    this.element = opts.element;
    this.isEmbed = !!opts.embed;
    this.treeView = new TreeView(this);
    this.promotion = new PromotionCtrl(
      this.withCg,
      () => this.withCg(g => g.set(this.cgConfig)),
      this.redraw,
    );
    if (opts.inlinePgn) this.data = this.changePgn(opts.inlinePgn, false) || this.data;

    this.initialize(this.data, false);
    this.initWorkspacePrefs();
    this.syncWorkspacePrefs();
    this.initCeval();
    this.pendingCopyPath = propWithEffect(null, this.redraw);
    this.pendingDeletionPath = propWithEffect(null, this.redraw);
    this.initialPath = this.makeInitialPath();
    this.setPath(this.initialPath);

    this.showGround();

    this.variationArrowOpacity = this.makeVariationOpacityProp();
    this.resetAutoShapes();
    this.explorer.setNode();

    if (location.hash === '#menu') requestIdleCallback(this.actionMenu.toggle, 500);
    this.startCeval();
    keyboard.bind(this);
    this.installHistoryNavigation();

    const url = new URL(window.location.href);
    const urlEngine = url.searchParams.get('engine');
    if (urlEngine) {
      try {
        this.ceval.engines.select(urlEngine);
        this.cevalEnabled(true);
        this.threatMode(false);
      } catch (e) {
        console.info(e);
      }
      url.searchParams.delete('engine');
      window.history.replaceState(window.history.state, '', `${url.pathname}${url.search}${url.hash}`);
    }

    pubsub.on('jump', (ply: string) => {
      this.jumpToMain(parseInt(ply));
      this.redraw();
    });

    pubsub.on('ply.trigger', () =>
      pubsub.emit('ply', this.node.ply, this.tree.lastMainlineNode(this.path).ply === this.node.ply),
    );
    this.mergeIdbThenShowTreeView();
  }

  private studyRef(): studyApi.StudyRef | null {
    const s = this.opts.study as { id?: string; chapterId?: string } | undefined;
    if (!s?.id || !s?.chapterId) return null;
    return { id: s.id, chapterId: s.chapterId };
  }

  studyData(): StudyView | undefined {
    return this.opts.study;
  }

  isStudy = (): boolean => !!this.studyData()?.id && !!this.studyData()?.chapterId;

  canWriteStudy(): boolean {
    const s = this.opts.study as { canWrite?: boolean } | undefined;
    return !!s?.canWrite;
  }

  private enqueueStudyWrite(task: (ref: studyApi.StudyRef) => Promise<void>): void {
    if (!this.canWriteStudy() || this.studyWriteError) return;
    const ref = this.studyRef();
    if (!ref) return;

    // Bind the study/chapter at enqueue-time so chapter switches don't corrupt queued writes.
    const bound = () => task(ref);
    this.studyWriteQueue.push(bound);
    if (!this.studyWriting) void this.flushStudyWrites();
    this.redraw();
  }

  private async flushStudyWrites(): Promise<void> {
    if (this.studyWriting) return;
    this.studyWriting = true;
    this.redraw();
    try {
      while (this.studyWriteQueue.length) {
        const task = this.studyWriteQueue.shift();
        if (task) await task();
      }
    } catch (e) {
      this.studyWriteQueue = [];
      this.studyWriteError = e instanceof Error ? e.message : String(e);
      console.warn('Study sync failed', e);
    } finally {
      this.studyWriting = false;
      this.redraw();
    }
  }

  isStudyWriting(): boolean {
    return this.studyWriting || this.studyWriteQueue.length > 0;
  }

  studyCommentText(path: Tree.Path): string {
    const uid = myUserId();
    if (!uid) return '';
    const node = this.tree.nodeAtPath(path);
    const comment = node?.comments?.find(c => typeof c.by === 'object' && c.by.id === uid);
    return comment?.text || '';
  }

  setStudyComment(path: Tree.Path, text: string): void {
    const uid = myUserId();
    const normalized = text.trim().length ? text : '';
    if (uid) {
      const name = myUsername() || uid;
      this.tree.updateAt(path, node => {
        const comments = (node.comments || []).slice();
        const idx = comments.findIndex(c => typeof c.by === 'object' && c.by.id === uid);
        if (!normalized) {
          if (idx >= 0) comments.splice(idx, 1);
        } else if (idx >= 0) {
          comments[idx].text = normalized;
        } else {
          comments.push({
            id: `local-${Date.now()}`,
            by: { id: uid, name },
            text: normalized,
          });
        }
        node.comments = comments.length ? comments : undefined;
      });
      this.redraw();
    }

    this.enqueueStudyWrite(async ref => {
      const res = await studyApi.setNodeComment(ref, path, text);
      this.tree.updateAt(res.path as Tree.Path, node => {
        node.comments = res.node.comments;
      });
      this.redraw();
    });
  }

  studyLoginHref = (): string => loginHref();

  studyNeedsAuth = (): boolean => !myUserId();

  studyUrl = (chapterId?: string): string | null => {
    const study = this.studyData();
    if (!study?.id || !study?.chapterId) return null;
    if (!chapterId || chapterId === study.chapterId)
      return study.url || `/study/${study.id}/${study.chapterId}`;
    return study.chapters.find(chapter => chapter.id === chapterId)?.url || `/study/${study.id}/${chapterId}`;
  };

  studyCreateBusy = (): boolean => this.studyCreateLoading;

  studyCreateErrorText = (): string | null => this.studyCreateError;

  studyCreateSetupVisible = (): boolean => this.studyCreateSetupOpen;

  studyCreateSetupValues = (): Required<studyApi.StudyCreateSetup> => this.studyCreateForm;

  openStudyCreateSetup = (): void => {
    if (this.studyCreateLoading) return;
    this.studyCreateError = null;
    this.studyCreateForm = this.defaultStudyCreateSetup();
    this.studyCreateSetupOpen = true;
    this.redraw();
  };

  closeStudyCreateSetup = (): void => {
    if (this.studyCreateLoading) return;
    this.studyCreateSetupOpen = false;
    this.redraw();
  };

  updateStudyCreateSetup = (patch: Partial<studyApi.StudyCreateSetup>): void => {
    this.studyCreateForm = { ...this.studyCreateForm, ...patch };
    this.redraw();
  };

  studyTransferCountValue = (): number => this.studyTransferCount;

  studyActionMessageText = (): string | null => this.studyActionMessage;

  studyActionToneValue = (): 'info' | 'success' | 'error' => this.studyActionTone;

  studyStatusText = (): string => {
    if (this.studyWriteError) return `Review study sync paused: ${this.studyWriteError}`;
    if (this.isStudyWriting()) return 'Saving notes to this section...';
    return this.canWriteStudy()
      ? 'Notes auto-save to this section.'
      : 'This review study section is read-only. You can still share the current section link.';
  };

  private setStudyActionMessage(message: string | null, tone: 'info' | 'success' | 'error' = 'info'): void {
    if (this.studyActionTimer !== undefined) {
      window.clearTimeout(this.studyActionTimer);
      this.studyActionTimer = undefined;
    }
    this.studyActionMessage = message;
    this.studyActionTone = tone;
    if (message) {
      this.studyActionTimer = window.setTimeout(() => {
        this.studyActionMessage = null;
        this.redraw();
      }, 2200);
    }
    this.redraw();
  }

  copyStudyShareLink = async (): Promise<void> => {
    const url = this.studyUrl();
    if (!url) return;
    try {
      await navigator.clipboard.writeText(new URL(url, location.origin).toString());
      const privateStudy = this.studyData()?.visibility === 'private';
      this.setStudyActionMessage(
        privateStudy
          ? 'Private study link copied. Only collaborators can open it.'
          : 'Review study link copied.',
        'success',
      );
    } catch (e) {
      console.warn('Study link copy failed', e);
      this.setStudyActionMessage('Copy failed. Open the study link directly instead.', 'error');
    }
  };

  submitStudyCreateSetup = async (): Promise<void> => {
    await this.createStudyFromCurrentAnalysis(this.studyCreateForm);
  };

  private createStudyFromCurrentAnalysis = async (setup: studyApi.StudyCreateSetup): Promise<void> => {
    if (this.studyCreateLoading) return;
    if (!myUserId()) {
      location.assign(this.studyLoginHref());
      return;
    }

    this.studyCreateLoading = true;
    this.studyCreateError = null;
    this.studyTransferCount = 0;
    this.redraw();

    const currentPgn = pgnExport.renderFullTxt(this);

    try {
      const created = await studyApi.createStudyFromAnalysis({
        pgn: currentPgn,
        orientation: this.getOrientation(),
        ...setup,
      });

      location.assign(created.url);
      return;
    } catch (e) {
      if (e instanceof studyApi.StudyApiError) {
        if (e.status === 401) {
          location.assign(this.studyLoginHref());
          return;
        }
        this.studyCreateError = e.message || 'Review study creation failed.';
      } else this.studyCreateError = e instanceof Error ? e.message : 'Review study creation failed.';
    } finally {
      this.studyCreateLoading = false;
      this.studyTransferCount = 0;
      this.redraw();
    }
  };

  private defaultStudyCreateSetup(): Required<studyApi.StudyCreateSetup> {
    const white = this.playerName('white');
    const black = this.playerName('black');
    const opening = this.data.game.opening?.name?.trim();
    return {
      name: `${white} vs ${black} review`.slice(0, 100),
      chapterName: (opening ? `${opening}: opening to middlegame` : 'Opening to middlegame').slice(0, 80),
      visibility: 'unlisted',
    };
  }

  private playerName(color: Color): string {
    const player = this.data.player.color === color ? this.data.player : this.data.opponent;
    return player.user?.username || player.name || (color === 'white' ? 'White' : 'Black');
  }

  initialize(data: AnalyseData, merge: boolean): void {
    this.data = data;
    this.synthetic = data.game.id === 'synthetic';
    this.ongoing = !this.synthetic && playable(data);
    this.treeView.hidden = true;
    const prevTree = merge && this.tree.root;
    this.tree = makeTree(treeReconstruct(this.data.treeParts, this.data.sidelines));
    if (prevTree) this.tree.merge(prevTree);
    const mainline = treeOps.mainlineNodeList(this.tree.root);
    if (this.data.game.status.name === 'draw') {
      if (add3or5FoldGlyphs(mainline)) this.data.game.threefold = true;
    }

    this.autoplay = new Autoplay(this);
    if (this.explorer) this.explorer.destroy();
    this.explorer = new ExplorerCtrl(this, this.opts.explorer, this.explorer);
    this.gamePath = this.synthetic || this.ongoing ? undefined : treePath.fromNodeList(mainline);
    this.fork = new ForkCtrl(this);

    site.sound.preloadBoardSounds();
  }

  private makeInitialPath = (): string => {
    // if correspondence, always use latest actual move to set 'current' style
    if (this.ongoing) return treePath.fromNodeList(treeOps.mainlineNodeList(this.tree.root));
    const loc = window.location,
      hashPly = loc.hash === '#last' ? this.tree.lastPly() : parseInt(loc.hash.slice(1)),
      startPly = hashPly >= 0 ? hashPly : this.opts.inlinePgn ? this.tree.lastPly() : undefined;
    if (defined(startPly)) {
      this.requestInitialPly = startPly;
      const mainline = treeOps.mainlineNodeList(this.tree.root);
      return treeOps.takePathWhile(mainline, n => n.ply <= startPly);
    } else return treePath.root;
  };

  private setPath = (path: Tree.Path): void => {
    this.path = path;
    this.nodeList = this.tree.getNodeList(path);
    this.node = treeOps.last(this.nodeList) as Tree.Node;
    this.mainline = treeOps.mainlineNodeList(this.tree.root);
    this.onMainline = this.tree.pathIsMainline(path);
    this.fenInput = undefined;
    this.pgnInput = undefined;
    this.idbTree.saveMoves();
    this.idbTree.revealNode();
  };

  flip = () => {
    this.flipped = !this.flipped;
    this.chessground?.set({
      orientation: this.bottomColor(),
    });
    this.onChange();
    this.redraw();
  };

  topColor(): Color {
    return opposite(this.bottomColor());
  }

  bottomColor(): Color {
    return this.flipped ? opposite(this.data.orientation) : this.data.orientation;
  }

  bottomIsWhite = () => this.bottomColor() === 'white';

  getOrientation(): Color {
    return this.bottomColor();
  }

  getNode(): Tree.Node {
    return this.node;
  }

  chesstoryBrief(): { payload?: ChesstoryMoveMeaningPayload; loading: boolean; unavailable: boolean } {
    return {
      payload: this.chesstoryBriefPayload,
      loading: this.chesstoryBriefLoading,
      unavailable: this.chesstoryBriefUnavailable,
    };
  }

  requestChesstoryBrief(): void {
    const request = this.chesstoryBriefRequest();
    const key = request
      ? JSON.stringify({
          request,
          probeCapabilities: {
            replyMultipv: this.isCevalAllowed() && this.cevalEnabled() && this.ceval.available(),
            endgameTablebase: !!this.opts.explorer.tablebaseEndpoint,
          },
        })
      : '';
    if (!request) {
      this.chesstoryBriefAbort?.abort();
      this.chesstoryProbeCancel?.();
      this.chesstoryBriefGeneration++;
      this.chesstoryBriefKey = undefined;
      this.chesstoryBriefLoadingKey = undefined;
      this.chesstoryBriefFailureKey = undefined;
      this.chesstoryBriefAbort = undefined;
      this.chesstoryProbeCancel = undefined;
      this.chesstoryBriefPayload = undefined;
      this.chesstoryBriefLoading = false;
      this.chesstoryBriefUnavailable = true;
      return;
    }
    if (
      key === this.chesstoryBriefKey ||
      key === this.chesstoryBriefLoadingKey ||
      (key === this.chesstoryBriefFailureKey && Date.now() < this.chesstoryBriefRetryAfter)
    )
      return;

    this.chesstoryBriefAbort?.abort();
    this.chesstoryProbeCancel?.();
    const abort = new AbortController();
    const generation = ++this.chesstoryBriefGeneration;
    const isActive = (): boolean =>
      generation === this.chesstoryBriefGeneration && this.chesstoryBriefLoadingKey === key;
    this.chesstoryBriefKey = undefined;
    this.chesstoryBriefLoadingKey = key;
    this.chesstoryBriefAbort = abort;
    this.chesstoryBriefPayload = undefined;
    this.chesstoryBriefLoading = true;
    this.chesstoryBriefUnavailable = false;

    fetch('/api/chess-judgment/move-meaning', {
      ...defaultInit,
      signal: abort.signal,
      method: 'post',
      headers: {
        ...jsonHeader,
        ...xhrHeader,
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(request),
    })
      .then(ensureOk)
      .then(res => res.json() as Promise<unknown>)
      .then(async raw => {
        if (!isActive()) return;
        const decoded = decodeChesstoryMoveMeaningResponse(raw);
        if (!decoded?.ok) throw new Error('Invalid Chesstory response contract');
        let data = decoded;
        let review = data.move_review;
        const probeResults: ChesstoryProbeResult[] = [];
        const seenProbeIds = new Set<string>();
        const probeBudget = 12;
        let probeFailed = false;
        for (let round = 0; round < 6 && probeResults.length < probeBudget; round++) {
          const batch = await this.chesstoryProbeResults(
            data.probe_requests,
            seenProbeIds,
            probeBudget - probeResults.length,
            isActive,
          );
          if (!isActive()) return;
          probeFailed ||= batch.failed;
          if (!batch.results.length) break;
          probeResults.push(...batch.results);
          const nextRaw = await fetch('/api/chess-judgment/move-meaning', {
            ...defaultInit,
            signal: abort.signal,
            method: 'post',
            headers: {
              ...jsonHeader,
              ...xhrHeader,
              'Content-Type': 'application/json',
            },
            body: JSON.stringify({ ...request, probeResults }),
          })
            .then(ensureOk)
            .then(res => res.json() as Promise<unknown>);
          if (!isActive()) return;
          const next = decodeChesstoryMoveMeaningResponse(nextRaw);
          if (!next?.ok) throw new Error('Invalid Chesstory response contract');
          data = next;
          review = data.move_review;
        }
        if (!isActive()) return;
        this.chesstoryBriefPayload = review?.renderable ? review : undefined;
        this.chesstoryBriefUnavailable = !this.chesstoryBriefPayload;
        if (probeFailed && !this.chesstoryBriefPayload) {
          this.chesstoryBriefFailureKey = key;
          this.chesstoryBriefRetryAfter = Date.now() + 3000;
        } else {
          this.chesstoryBriefKey = key;
          this.chesstoryBriefFailureKey = undefined;
        }
      })
      .catch(e => {
        if (!isActive()) return;
        console.info('Chesstory brief unavailable', e);
        this.chesstoryBriefPayload = undefined;
        this.chesstoryBriefUnavailable = true;
        this.chesstoryBriefFailureKey = key;
        this.chesstoryBriefRetryAfter = Date.now() + 3000;
      })
      .finally(() => {
        if (isActive()) {
          this.chesstoryBriefLoadingKey = undefined;
          this.chesstoryBriefAbort = undefined;
          this.chesstoryBriefLoading = false;
          this.redraw();
        }
      });
  }

  private async chesstoryProbeResults(
    rawRequests: unknown,
    seenProbeIds: Set<string>,
    limit: number,
    isActive: () => boolean,
  ): Promise<ChesstoryProbeBatch> {
    const canRunEngineProbe = this.isCevalAllowed() && this.cevalEnabled() && this.ceval.available();
    const canRunTablebaseProbe = !!this.opts.explorer.tablebaseEndpoint;
    const requests = Array.isArray(rawRequests)
      ? rawRequests.filter(
          (request): request is ChesstoryProbeRequest =>
            !!request &&
            typeof request === 'object' &&
            typeof (request as ChesstoryProbeRequest).id === 'string' &&
            typeof (request as ChesstoryProbeRequest).fen === 'string' &&
            !seenProbeIds.has((request as ChesstoryProbeRequest).id) &&
            Array.isArray((request as ChesstoryProbeRequest).moves) &&
            (request as ChesstoryProbeRequest).moves.every(
              move => typeof move === 'string' && validUci(move),
            ) &&
            (((request as ChesstoryProbeRequest).purpose === 'reply_multipv' &&
              canRunEngineProbe &&
              ((request as ChesstoryProbeRequest).moves.length === 0 ||
                ((request as ChesstoryProbeRequest).moves.length === 1 &&
                  (request as ChesstoryProbeRequest).moves[0] ===
                    (request as ChesstoryProbeRequest).opponentResourceMove))) ||
              ((request as ChesstoryProbeRequest).purpose === 'endgame_tablebase' &&
                canRunTablebaseProbe &&
                (request as ChesstoryProbeRequest).moves.length > 0 &&
                typeof (request as ChesstoryProbeRequest).comparisonFen === 'string')),
        )
      : [];
    if (!requests.length) return { results: [], failed: false };

    const wasEnabled = !!this.cevalEnabled();
    const hasEngineProbe = requests.some(request => request.purpose === 'reply_multipv');
    const results: ChesstoryProbeResult[] = [];
    let failed = false;
    if (hasEngineProbe) this.ceval.stop();
    try {
      for (const request of requests.slice(0, limit)) {
        if (!isActive()) break;
        seenProbeIds.add(request.id);
        const result =
          request.purpose === 'endgame_tablebase'
            ? await this.runChesstoryTablebaseProbe(request)
            : await this.runChesstoryProbe(request, isActive);
        if (result && isActive()) results.push(result);
        else if (isActive()) failed = true;
      }
    } finally {
      if (hasEngineProbe) {
        this.ceval.stop();
        if (wasEnabled) this.startCeval();
      }
    }
    return { results, failed };
  }

  private async runChesstoryTablebaseProbe(
    request: ChesstoryProbeRequest,
  ): Promise<ChesstoryProbeResult | undefined> {
    if (!request.comparisonFen) return;
    try {
      const [terminal, comparison] = await Promise.all([
        loadTablebase(this.opts.explorer.tablebaseEndpoint, request.fen),
        loadTablebase(this.opts.explorer.tablebaseEndpoint, request.comparisonFen),
      ]);
      const terminalWdl = this.chesstoryTablebaseWdl(terminal.category);
      const comparisonWdl = this.chesstoryTablebaseWdl(comparison.category);
      if (
        terminalWdl !== -2 ||
        comparisonWdl !== 2 ||
        terminal.dtm === undefined ||
        comparison.dtm === undefined
      )
        return;
      const legalMoves = request.moves.flatMap(move => {
        const result = terminal.moves.find(candidate => candidate.uci === move);
        const resultingWdl = result && this.chesstoryTablebaseWdl(result.category);
        return result && resultingWdl !== undefined && result.dtm !== undefined
          ? [{ moveUci: move, resultingWdl, resultingDtm: result.dtm }]
          : [];
      });
      if (legalMoves.length !== request.moves.length) return;
      return {
        id: request.id,
        fen: request.fen,
        purpose: request.purpose,
        variationHash: request.variationHash,
        generatedAtEpochMs: Date.now(),
        tablebase: {
          terminal: { fen: request.fen, wdl: terminalWdl, dtm: terminal.dtm },
          comparison: {
            fen: request.comparisonFen,
            wdl: comparisonWdl,
            dtm: comparison.dtm,
          },
          legalMoves,
        },
      };
    } catch (e) {
      console.info('Chesstory tablebase probe unavailable', e);
      return;
    }
  }

  private chesstoryTablebaseWdl(category: TablebaseCategory): number | undefined {
    switch (category) {
      case 'win':
        return 2;
      case 'cursed-win':
        return 1;
      case 'draw':
        return 0;
      case 'blessed-loss':
        return -1;
      case 'loss':
        return -2;
      default:
        return;
    }
  }

  private runChesstoryProbe(
    request: ChesstoryProbeRequest,
    isActive: () => boolean,
  ): Promise<ChesstoryProbeResult | undefined> {
    const currentFen = request.moves.length
      ? parseFen(request.fen)
          .chain(setup => setupPosition('chess', setup))
          .unwrap(
            pos => {
              for (const uci of request.moves) {
                const move = parseUci(uci);
                if (!move || !pos.isLegal(move)) return;
                pos.play(move);
              }
              return makeFen(pos.toSetup());
            },
            () => undefined,
          )
      : request.fen;
    if (!currentFen || !isActive()) return Promise.resolve(undefined);
    return new Promise(resolve => {
      let done = false;
      const requiredPvs = Math.max(1, request.multiPv ?? 1);
      const depthFloor = Math.max(1, request.depthFloor ?? request.depth ?? 1);
      const timeout = window.setTimeout(() => finish(), Math.min(12000, Math.max(4000, depthFloor * 700)));
      const cancel = (): void => finish();
      const finish = (ev?: Tree.LocalEval): void => {
        if (done) return;
        done = true;
        window.clearTimeout(timeout);
        if (this.chesstoryProbeCancel === cancel) this.chesstoryProbeCancel = undefined;
        if (isActive()) this.ceval.stop();
        resolve(ev && isActive() ? this.chesstoryProbeResult(request, ev) : undefined);
      };
      this.chesstoryProbeCancel = cancel;
      const work: Work = {
        variant: this.data.game.variant.key,
        threads: this.ceval.threads,
        hashSize: this.ceval.hashSize,
        gameId: undefined,
        stopRequested: false,
        initialFen: request.fen,
        currentFen,
        moves: request.moves,
        path: `chesstory-probe:${request.id}`,
        ply: this.plyFromFen(currentFen),
        search: { depth: request.depth || depthFloor },
        multiPv: requiredPvs,
        threatMode: false,
        emit: (ev: Tree.LocalEval) => {
          if (
            isActive() &&
            ev.depth >= depthFloor &&
            ev.pvs.filter(pv => pv.moves.length && this.chesstoryEvalCp(pv) !== undefined).length >=
              requiredPvs
          )
            finish(ev);
        },
      };
      this.ceval.resume(work);
    });
  }

  private chesstoryProbeResult(
    request: ChesstoryProbeRequest,
    ev: Tree.LocalEval,
  ): ChesstoryProbeResult | undefined {
    const replyLines = ev.pvs
      .flatMap(pv => {
        const scoreCp = this.chesstoryEvalCp(pv);
        return pv.moves.length && scoreCp !== undefined
          ? [
              {
                moves: [...request.moves, ...pv.moves],
                scoreCp,
                mate: pv.mate,
                depth: ev.depth,
              },
            ]
          : [];
      });
    if (ev.depth < (request.depthFloor ?? request.depth ?? 1) || replyLines.length < (request.multiPv ?? 1))
      return;
    return {
      id: request.id,
      fen: request.fen,
      replyLines,
      purpose: request.purpose,
      probedMove: request.candidateMove,
      depth: ev.depth,
      candidateMove: request.candidateMove,
      opponentResourceMove: request.opponentResourceMove,
      horizon: request.horizon,
      variationHash: request.variationHash,
      generatedAtEpochMs: Date.now(),
    };
  }

  private chesstoryEvalCp(score: { cp?: number; mate?: number }): number | undefined {
    if (typeof score.cp === 'number' && Number.isFinite(score.cp)) return score.cp;
    if (typeof score.mate === 'number' && Number.isFinite(score.mate) && score.mate !== 0)
      return score.mate > 0 ? 10000 : -10000;
    return;
  }

  private plyFromFen(fen: FEN): Ply {
    const parts = fen.split(/\s+/);
    const fullmove = Math.max(1, Number(parts[5]) || 1);
    return ((fullmove - 1) * 2 + (parts[1] === 'b' ? 1 : 0)) as Ply;
  }

  private chesstoryBriefRequest(): ChesstoryMoveMeaningRequest | undefined {
    if (
      !this.isStudy() ||
      this.data.game.variant.key !== 'standard' ||
      this.nodeList.length < 2
    )
      return;
    const played = this.node;
    const before = this.nodeList[this.nodeList.length - 2];
    if (!played.uci) return;

    const beforeEval = before.ceval || before.eval;
    if (!beforeEval?.pvs?.length) return;

    const variations = beforeEval.pvs
      .flatMap(pv => {
        const moves = Array.isArray(pv.moves) ? pv.moves : pv.moves.split(' ').filter(Boolean);
        const scoreCp = this.chesstoryEvalCp(pv);
        return moves.length && scoreCp !== undefined
          ? [{ moves, scoreCp, mate: pv.mate, depth: beforeEval.depth || 0 }]
          : [];
      });
    if (!variations.length) return;
    const playedVariationIndex = variations.findIndex(v => v.moves[0] === played.uci);
    const playedVariation =
      playedVariationIndex < 0 ? undefined : variations[playedVariationIndex];
    const playedIsTerminal = parseFen(played.fen)
      .chain(setup => setupPosition('chess', setup))
      .unwrap(
        pos => pos.isEnd(),
        () => false,
      );

    if (!playedVariation || (playedVariation.moves.length < 2 && !playedIsTerminal)) {
      const playedEval = played.ceval || played.eval;
      if (!playedEval) return;
      const playedPv = playedEval.pvs?.find(pv => {
        const moves = Array.isArray(pv.moves) ? pv.moves : pv.moves.split(' ').filter(Boolean);
        return moves.length > 0;
      });
      const suffix = playedPv
        ? Array.isArray(playedPv.moves)
          ? playedPv.moves
          : playedPv.moves.split(' ').filter(Boolean)
        : [];
      const scoreCp = this.chesstoryEvalCp(playedPv || playedEval);
      if (!suffix.length && !playedIsTerminal) return;
      if (scoreCp === undefined) return;
      const completePlayedVariation = {
        moves: [played.uci, ...suffix],
        scoreCp,
        mate: playedPv?.mate ?? playedEval.mate,
        depth: playedEval.depth || 0,
      };
      if (playedVariationIndex < 0) variations.push(completePlayedVariation);
      else variations[playedVariationIndex] = completePlayedVariation;
    }

    return {
      fen: before.fen,
      playedMoveUci: played.uci,
      variations,
      ply: before.ply,
      openingContext: this.data.game.opening
        ? {
            eco: this.data.game.opening.eco,
            name: this.data.game.opening.name,
            family: this.data.game.opening.eco?.slice(0, 1),
          }
        : undefined,
      movePrefixUci: this.nodeList
        .slice(1, -1)
        .map(n => n.uci)
        .filter((uci): uci is Uci => !!uci),
    };
  }

  turnColor(): Color {
    return plyColor(this.node.ply);
  }

  togglePlay(delay: AutoplayDelay): void {
    this.autoplay.toggle(delay);
    this.actionMenu(false);
  }

  private showGround(): void {
    this.onChange();
    if (!defined(this.node.dests)) this.getDests();
    this.withCg(cg => {
      cg.set(this.makeCgOpts());
      this.setAutoShapes();
      if (this.node.shapes) cg.setShapes(this.node.shapes.slice() as DrawShape[]);
    });
  }

  private getDests: () => void = throttle(800, () => {
    if (defined(this.node.dests)) return;
    const path = this.path;
    this.position(this.node).unwrap(
      pos => {
        const dests = new Map<Key, Key[]>();
        for (const [orig, destSet] of pos.allDests()) {
          dests.set(makeSquare(orig) as Key, Array.from(destSet, makeSquare) as Key[]);
        }
        this.addDests(writeDests(dests), path);
      },
      _ => this.addDests('', path),
    );
  });

  serverMainline = () => this.mainline.slice(0, playedTurns(this.data) + 1);

  makeCgOpts(): ChessgroundConfig {
    const node = this.node,
      color = this.turnColor(),
      dests = readDests(this.node.dests),
      movableColor = !dests || dests.size > 0 ? color : undefined,
      config: ChessgroundConfig = {
        fen: node.fen,
        turnColor: color,
        movable: {
          color: movableColor,
          dests: (movableColor === color && dests) || new Map(),
        },
        check: !!node.check,
        lastMove: uciToMove(node.uci),
      };
    if (!dests && !node.check) {
      // premove while dests are loading from server
      // can't use when in check because it highlights the wrong king
      config.turnColor = opposite(color);
      config.movable!.color = color;
    }
    config.premovable = {
      enabled: config.movable!.color && config.turnColor !== config.movable!.color,
    };
    this.cgConfig = config;
    return config;
  }

  setChessground = (cg: CgApi) => {
    this.chessground = cg;

    this.setAutoShapes();
    if (this.node.shapes) this.chessground.setShapes(this.node.shapes.slice() as DrawShape[]);
    this.cgVersion.dom = this.cgVersion.js;
  };

  private onChange: () => void = throttle(300, () => {
    pubsub.emit('analysis.change', this.node.fen, this.path);
  });

  private installHistoryNavigation(): void {
    if (this.opts.study) return;
    this.syncHref('replace');
    window.addEventListener('popstate', this.onHistoryPopState);
  }

  private hrefForPly(ply: Ply): string {
    const url = new URL(window.location.href);
    const search = url.searchParams.toString();
    const base = `${url.pathname}${search ? `?${search}` : ''}`;
    return ply > this.tree.root.ply ? `${base}#${ply}` : base;
  }

  private syncHref(mode: 'replace' | 'push'): void {
    if (this.opts.study || this.restoringHistory) return;
    const url = this.hrefForPly(this.node.ply);
    const state: AnalyseHistoryState = { analysePly: this.node.ply };
    if (mode === 'push') {
      const current = `${window.location.pathname}${window.location.search}${window.location.hash}`;
      if (current === url) window.history.replaceState(state, '', url);
      else window.history.pushState(state, '', url);
      return;
    }
    window.history.replaceState(state, '', url);
  }

  private onHistoryPopState = (event: PopStateEvent): void => {
    if (this.opts.study) return;
    const state = event.state as AnalyseHistoryState | null;
    const ply =
      typeof state?.analysePly === 'number' ? state.analysePly : parseInt(window.location.hash.slice(1), 10);
    const targetPath = this.mainlinePlyToPath(Number.isFinite(ply) && ply >= 0 ? ply : this.tree.root.ply);
    if (targetPath === this.path) return;
    this.restoringHistory = true;
    try {
      this.autoplay.stop();
      this.withCg(cg => cg.selectSquare(null));
      this.jump(targetPath);
      this.redraw();
    } finally {
      this.restoringHistory = false;
    }
  };

  playedLastMoveMyself = () =>
    !!this.justPlayed && !!this.node.uci && this.node.uci.startsWith(this.justPlayed);

  jump(path: Tree.Path, historyMode: 'replace' | 'push' = 'replace'): void {
    const pathChanged = path !== this.path,
      isForwardStep = pathChanged && path.length === this.path.length + 2;
    if (this.path !== path)
      this.treeView.requestAutoScroll(treeOps.distance(this.path, path) > 8 ? 'instant' : 'smooth');
    this.setPath(path);
    if (pathChanged) {
      if (isForwardStep) site.sound.move(this.node);
      this.threatMode(false);
      this.ceval?.stop();
      this.startCeval();
      site.sound.saySan(this.node.san, true);
    }
    this.justPlayed = this.justCaptured = undefined;
    this.explorer.setNode();
    this.syncHref(historyMode);
    this.promotion.cancel();
    pubsub.emit('ply', this.node.ply, this.tree.lastMainlineNode(this.path).ply === this.node.ply);
    this.showGround();
    this.pluginUpdate(this.node.fen);
  }

  userJump = (path: Tree.Path): void => {
    this.autoplay.stop();
    this.withCg(cg => cg.selectSquare(null));
    this.jump(path, 'push');
  };

  canJumpTo = (_path: Tree.Path): boolean => true;

  userJumpIfCan(path: Tree.Path, sideStep = false): void {
    if (path === this.path || !this.canJumpTo(path)) return;
    if (sideStep) {
      // when stepping lines, anchor the chessground animation at the parent
      this.node = this.tree.nodeAtPath(path.slice(0, -2));
      this.chessground?.set(this.makeCgOpts());
      this.chessground?.state.dom.redrawNow(true);
    }
    this.userJump(path);
  }

  mainlinePlyToPath(ply: Ply): Tree.Path {
    return treeOps.takePathWhile(this.mainline, n => n.ply <= ply);
  }

  jumpToMain = (ply: Ply): void => {
    this.userJump(this.mainlinePlyToPath(ply));
  };

  jumpToIndex = (index: number): void => {
    this.jumpToMain(index + 1 + this.tree.root.ply);
  };

  jumpToGlyphSymbol(color: Color, symbol: string): void {
    const node = nextGlyphSymbol(color, symbol, this.mainline, this.node.ply);
    if (node) this.jumpToMain(node.ply);
    this.redraw();
  }

  reloadData(data: AnalyseData, merge: boolean): void {
    this.initialize(data, merge);
    this.syncWorkspacePrefs();
    this.redirecting = false;
    this.setPath(treePath.root);
    this.initCeval();
    this.cgVersion.js++;
    this.mergeIdbThenShowTreeView();
  }

  changePgn(pgn: string, andReload: boolean): AnalyseData | undefined {
    this.pgnError = '';
    const normalized = normalizeInlinePgn(pgn);
    if (!normalized) {
      this.pgnError = emptyPgnError;
      requestAnimationFrame(this.redraw);
      return undefined;
    }
    try {
      const data: AnalyseData = {
        ...pgnImport(normalized),
        orientation: this.bottomColor(),
        pref: this.data.pref,
      } as AnalyseData;
      if (andReload) {
        this.reloadData(data, false);
        this.userJump(this.mainlinePlyToPath(this.tree.lastPly()));
        this.redraw();
      }
      return data;
    } catch (err) {
      this.pgnError = (err as PgnError).message;
      requestAnimationFrame(this.redraw);
    }
    return undefined;
  }

  importPgn(rawPgn: string): boolean {
    this.pgnError = '';
    if (!submitPgnToImportPipeline(rawPgn)) {
      this.pgnError = emptyPgnError;
      requestAnimationFrame(this.redraw);
      return false;
    }
    this.rememberRecentImportDraft(rawPgn);
    this.redirecting = true;
    this.redraw();
    return true;
  }

  changeFen(fen: FEN): void {
    this.redirecting = true;
    window.location.href =
      '/analysis/' +
      this.data.game.variant.key +
      '/' +
      encodeURIComponent(fen).replace(/%20/g, '_').replace(/%2F/g, '/');
  }

  userMove = (orig: Key, dest: Key, capture?: JustCaptured): void => {
    this.justPlayed = orig;
    if (
      !this.promotion.start(orig, dest, {
        submit: (orig, dest, prom) => this.sendMove(orig, dest, capture, prom),
      })
    )
      this.sendMove(orig, dest, capture);
  };

  sendMove = (orig: Key, dest: Key, capture?: JustCaptured, prom?: Role): void => {
    if (capture) this.justCaptured = capture;
    const before = { fen: this.node.fen, path: this.path };
    const uci = (orig + dest + (prom ? roleToChar(prom) : '')) as Uci;
    this.applyUci(uci);
    this.redraw();
    if (this.path !== before.path) {
      this.enqueueStudyWrite(ref =>
        studyApi
          .anaMove(ref, {
            orig,
            dest,
            fen: before.fen,
            path: before.path,
            variant: this.data.game.variant.key,
            promotion: prom,
            ch: ref.chapterId,
          })
          .then(() => {}),
      );
    }
  };

  onPremoveSet = () => {};

  addNode(node: Tree.Node, path: Tree.Path) {
    this.idbTree.onAddNode(node, path);
    const newPath = this.tree.addNode(node, path);
    if (!newPath) {
      console.log("Can't addNode", node, path);
      return this.redraw();
    }

    this.jump(newPath, 'push');

    this.redraw();
    const queuedUci = this.pvUciQueue.shift();
    if (queuedUci) this.playUci(queuedUci, this.pvUciQueue);
    else this.chessground.playPremove();
  }

  addDests(dests: string, path: Tree.Path): void {
    this.tree.addDests(dests, path);
    if (path === this.path) {
      this.showGround();
      this.pluginUpdate(this.node.fen);
      if (this.outcome()) this.ceval.stop();
    }
    this.withCg(cg => cg.playPremove());
  }

  async deleteNode(path: Tree.Path): Promise<void> {
    this.pendingDeletionPath(null);
    const node = this.tree.nodeAtPath(path);
    if (!node) return;
    const count = treeOps.countChildrenAndComments(node);
    if (
      (count.nodes >= 10 || count.comments > 0) &&
      !(await confirm(
        'Delete ' +
          plural('move', count.nodes) +
          (count.comments ? ' and ' + plural('comment', count.comments) : '') +
          '?',
      ))
    )
      return;
    if (path) this.enqueueStudyWrite(ref => studyApi.deleteNode(ref, path));
    this.tree.deleteNodeAt(path);
    if (treePath.contains(this.path, path)) this.userJump(treePath.init(path));
    else this.jump(this.path);
    this.redraw();
  }

  allowedEval(node: Tree.Node = this.node): Tree.ClientEval | Tree.ServerEval | false | undefined {
    return this.cevalEnabled() ? node.ceval : false;
  }

  outcome(node?: Tree.Node): Outcome | undefined {
    return this.position(node || this.node).unwrap(
      pos => pos.outcome(),
      _ => undefined,
    );
  }

  position(node: Tree.Node): ReturnType<typeof setupPosition> {
    const setup = parseFen(node.fen).unwrap();
    return setupPosition('chess', setup);
  }

  private applyUci(uci: Uci): void {
    const path = this.path;
    this.position(this.node).unwrap(
      pos => {
        const move = parseUci(uci);
        if (!move || !pos.isLegal(move)) return this.jump(path);

        const ply = this.node.ply + 1;
        const san = makeSanAndPlay(pos, move);
        const setup = pos.toSetup();
        const node: Tree.Node = {
          id: scalachessCharPair(move),
          ply,
          san,
          fen: makeFen(setup),
          uci: makeUci(move),
          children: [],
          check: pos.isCheck() ? makeSquare(setup.board.kingOf(pos.turn)!) : undefined,
        };
        this.addNode(node, path);
      },
      _ => this.jump(path),
    );
  }

  promote(path: Tree.Path, toMainline: boolean): void {
    if (path) this.enqueueStudyWrite(ref => studyApi.promoteNode(ref, path, toMainline));
    this.tree.promoteAt(path, toMainline);
    this.jump(path);
  }

  forceVariation(path: Tree.Path, force: boolean): void {
    if (path) this.enqueueStudyWrite(ref => studyApi.forceVariationNode(ref, path, force));
    this.tree.forceVariationAt(path, force);
    this.jump(path);
  }

  visibleChildren(node = this.node): Tree.Node[] {
    return node.children.filter(kid => !kid.comp);
  }

  reset(): void {
    this.showGround();
    this.redraw();
  }

  encodeNodeFen(): FEN {
    return this.node.fen.replace(/\s/g, '_');
  }

  nextNodeBest() {
    return treeOps.withMainlineChild(this.node, (n: Tree.Node) => validUci(n.eval?.best));
  }

  setAutoShapes = (): void => {
    if (!site.blindMode) this.chessground?.setAutoShapes(computeAutoShapes(this));
  };

  private onNewCeval = (ev: Tree.ClientEval, path: Tree.Path, isThreat?: boolean): void => {
    this.tree.updateAt(path, (node: Tree.Node) => {
      if (node.fen !== ev.fen && !isThreat) return;

      if (isThreat) {
        const threat = ev as Tree.LocalEval;
        if (!node.threat || isEvalBetter(threat, node.threat)) node.threat = threat;
      } else if (!node.ceval || isEvalBetter(ev, node.ceval)) node.ceval = ev;
      else if (!ev.cloud) {
        if (node.ceval?.cloud && this.ceval.isDeeper()) node.ceval = ev;
      }

      if (path === this.path) {
        this.setAutoShapes();
        this.redraw();
      }
    });
  };

  private initCeval(): void {
    const opts: CevalOpts = {
      variant: this.data.game.variant,
      initialFen: this.data.game.initialFen,
      emit: (ev: Tree.ClientEval, work: EvalMeta) => this.onNewCeval(ev, work.path, work.threatMode),
      onUciHover: this.setAutoShapes,
      redraw: this.redraw,
      onSelectEngine: () => {
        this.initCeval();
        this.redraw();
      },
    };
    if (this.ceval) this.ceval.init(opts);
    else this.ceval = new CevalCtrl(opts);
  }

  isCevalAllowed = () =>
    !this.ongoing && (this.synthetic || !playable(this.data)) && !location.search.includes('evals=0');

  cevalEnabled = (enable?: boolean): boolean | 'force' => {
    const state = this.cevalEnabledProp() && this.isCevalAllowed() && !this.ceval.isPaused;
    if (enable === undefined) return state;
    this.showCevalProp(enable);
    this.cevalEnabledProp(enable);
    if (enable && this.ceval.isPaused) this.ceval.resume();
    if (enable !== state) {
      if (enable) this.startCeval();
      else {
        this.threatMode(false);
        this.ceval.stop();
      }
      this.setAutoShapes();
      this.ceval.showEnginePrefs(false);
      this.redraw();
    }
    return enable;
  };

  startCeval = () => {
    if (!this.ceval.download) this.ceval.stop();
    if (this.node.threefold || !this.cevalEnabled() || this.outcome()) return;
    this.ceval.start(this.path, this.nodeList, undefined, this.threatMode());
  };

  clearCeval(): void {
    this.tree.removeCeval();
    this.startCeval();
  }

  showVariationArrows() {
    if (!this.allowLines()) return false;
    const kids = this.variationArrowOpacity() ? this.node.children : [];
    return Boolean(kids.filter(x => !x.comp).length);
  }

  showAnalysis() {
    return this.cevalEnabled() && this.isCevalAllowed();
  }

  showMoveGlyphs = (): boolean => true;

  showMoveAnnotationsOnBoard = (): boolean =>
    this.possiblyShowMoveAnnotationsOnBoard() && this.showMoveGlyphs();

  showEvalGauge(): boolean {
    return (
      this.showGauge() &&
      displayColumns() > 1 &&
      this.showAnalysis() &&
      this.isCevalAllowed() &&
      !this.outcome()
    );
  }

  boardLabelMode = (): BoardLabelMode => this.boardLabelModeProp();

  boardCoords = (): Prefs.Coords => boardLabelModeToCoords(this.boardLabelModeProp());

  setBoardLabelMode = (mode: BoardLabelMode): void => {
    if (!boardLabelModes.has(mode)) return;
    this.boardLabelModeProp(mode);
    this.syncWorkspacePrefs();
    applyBoardLabelMode(this.chessground, mode);
    requestAnimationFrame(dispatchChessgroundResize);
    this.redraw();
  };

  showCapturedMaterial = (): boolean => this.showCapturedProp();

  setShowCapturedMaterial = (show: boolean): void => {
    this.showCapturedProp(show);
    this.syncWorkspacePrefs();
    requestAnimationFrame(dispatchChessgroundResize);
    this.redraw();
  };

  setShowEvalGauge = (show: boolean): void => {
    this.showGauge(show);
    requestAnimationFrame(dispatchChessgroundResize);
    this.redraw();
  };

  resetOrientation = (): void => {
    if (!this.flipped) return;
    this.flipped = false;
    this.chessground?.set({
      orientation: this.bottomColor(),
    });
    this.onChange();
    this.redraw();
  };

  showCeval = (show?: boolean) => {
    const barMode = this.activeControlMode();
    if (show === undefined) return displayColumns() > 1 || barMode === 'ceval';
    this.ceval.showEnginePrefs(false);
    this.showCevalProp(show);
    if (show) this.cevalEnabled(true);
    return show;
  };

  activeControlMode = () => (this.showCevalProp() ? 'ceval' : false);

  private initWorkspacePrefs() {
    const defaultBoardLabelMode = boardLabelModeFromCoords(this.data.pref.coords);
    this.boardLabelModeProp = storedProp<BoardLabelMode>(
      'analyse.board-view.labels',
      defaultBoardLabelMode,
      str => (boardLabelModes.has(str as BoardLabelMode) ? (str as BoardLabelMode) : defaultBoardLabelMode),
      v => v,
    );
    this.showCapturedProp = storedBooleanProp('analyse.board-view.material', !!this.data.pref.showCaptured);
    this.showGauge = storedBooleanProp('analyse.board-view.gauge', true);
  }

  private syncWorkspacePrefs() {
    this.data.pref.coords = this.boardCoords();
    this.data.pref.showCaptured = this.showCapturedMaterial();
  }

  private rememberRecentImportDraft(rawPgn: string) {
    const normalized = normalizeInlinePgn(rawPgn);
    if (!normalized) return;
    const next = [normalized, ...this.recentImportDrafts().filter(pgn => pgn !== normalized)].slice(0, 4);
    this.recentImportDraftsCache = next;
    tempStorage.set(recentImportStorageKey, JSON.stringify(next));
  }

  activeControlBarTool() {
    return this.actionMenu() ? 'action-menu' : this.explorer.enabled() ? 'opening-explorer' : false;
  }

  allowLines() {
    return true;
  }

  toggleDiscloseOf(path = this.path.slice(0, -2)) {
    const disclose = this.idbTree.discloseOf(this.tree.nodeAtPath(path), this.tree.pathIsMainline(path));
    if (disclose) this.idbTree.setCollapsed(path, disclose === 'expanded');
    return Boolean(disclose);
  }

  toggleThreatMode = (v = !this.threatMode()) => {
    if (v === this.threatMode()) return;
    if (this.node.check || !this.showAnalysis()) return;
    if (!this.cevalEnabled()) return;
    this.threatMode(v);
    this.setAutoShapes();
    this.startCeval();
    this.redraw();
  };

  togglePossiblyShowMoveAnnotationsOnBoard = (v: boolean): void => {
    this.possiblyShowMoveAnnotationsOnBoard(v);
    this.resetAutoShapes();
    this.redraw();
  };

  recentImportDrafts = (): string[] => {
    if (this.recentImportDraftsCache) return this.recentImportDraftsCache;
    const raw = tempStorage.get(recentImportStorageKey);
    if (!raw) return (this.recentImportDraftsCache = []);
    try {
      const parsed = JSON.parse(raw);
      this.recentImportDraftsCache = Array.isArray(parsed)
        ? parsed.filter((v): v is string => typeof v === 'string')
        : [];
    } catch (_) {
      this.recentImportDraftsCache = [];
    }
    return this.recentImportDraftsCache;
  };

  useImportDraft = (draft: string): void => {
    this.pgnInput = draft;
    this.pgnError = '';
    this.redirecting = false;
    this.redraw();
  };

  resetImportDraft = (): void => {
    this.fenInput = undefined;
    this.pgnInput = undefined;
    this.pgnError = '';
    this.redirecting = false;
    this.redraw();
  };

  toggleActionMenu = () => {
    if (!this.actionMenu()) {
      if (this.explorer.enabled()) this.explorer.toggle();
    }
    this.actionMenu.toggle();
  };

  toggleExplorer = (): void => {
    if (!this.explorer.allowed()) return;
    if (!this.explorer.enabled()) {
      this.actionMenu(false);
    }
    this.explorer.toggle();
  };

  withCg = <A>(f: (cg: ChessgroundApi) => A): A | undefined =>
    this.chessground && this.cgVersion.js === this.cgVersion.dom ? f(this.chessground) : undefined;

  playUci = (uci: Uci, uciQueue?: Uci[]) => {
    this.pvUciQueue = uciQueue ?? [];
    const move = parseUci(uci)!;
    if (!isNormal(move)) return;
    const to = makeSquare(move.to);
    const piece = this.chessground.state.pieces.get(makeSquare(move.from));
    const capture = this.chessground.state.pieces.get(to);
    this.sendMove(
      makeSquare(move.from),
      to,
      capture && piece && capture.color !== piece.color ? capture : undefined,
      move.promotion,
    );
  };

  playUciList(uciList: Uci[]): void {
    this.pvUciQueue = uciList;
    const firstUci = this.pvUciQueue.shift();
    if (firstUci) this.playUci(firstUci, this.pvUciQueue);
  }

  explorerMove(uci: Uci): void {
    this.playUci(uci);
    this.explorer.loading(true);
  }

  playBestMove(): void {
    const uci = this.node.ceval?.pvs[0].moves[0] || this.nextNodeBest();
    if (uci) this.playUci(uci);
  }

  pluginMove = (orig: Key, dest: Key, prom: Role | undefined): void => {
    const capture = this.chessground.state.pieces.get(dest);
    this.sendMove(orig, dest, capture, prom);
  };

  toggleVariationArrows = () => {
    const trueValue = this.variationArrowOpacity(false);
    this.variationArrowOpacity(trueValue === 0 ? 0.6 : -trueValue);
  };

  private makeVariationOpacityProp(): Prop<number | false> {
    const store = preferenceLocalStorage();
    let value = parseFloat(store?.getItem('analyse.variation-arrow-opacity') || '0');
    if (isNaN(value) || value < -1 || value > 1) value = 0;
    return (v?: number | false) => {
      if (v === false) return value;
      if (v === undefined || isNaN(v)) return value > 0 ? value : false;
      value = Math.min(1, Math.max(-1, v));
      store?.setItem('analyse.variation-arrow-opacity', value.toString());
      this.setAutoShapes();
      this.chessground.redrawAll();
      this.redraw();
      return value;
    };
  }

  private pluginUpdate = (fen: string) => {
    // If controller and chessground board states differ, ignore this update. Once the chessground
    // state is updated to match, pluginUpdate will be called again.
    if (!fen.startsWith(this.chessground?.getFen())) return;
  };

  showBestMoveArrows = () => false;

  private resetAutoShapes = () => {
    if (
      this.showBestMoveArrows() ||
      this.possiblyShowMoveAnnotationsOnBoard() ||
      this.variationArrowOpacity()
    )
      this.setAutoShapes();
    else this.chessground?.setAutoShapes([]);
  };

  private async mergeIdbThenShowTreeView() {
    await this.idbTree.merge();
    this.treeView.hidden = false;
    this.idbTree.revealNode();
    this.redraw();
  }
}
