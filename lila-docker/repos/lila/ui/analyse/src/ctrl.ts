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
import type { CevalHandler, EvalMeta, CevalOpts } from 'lib/ceval';
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
import { storedBooleanProp, storedProp } from 'lib/storage';
import { PromotionCtrl } from 'lib/game/promotion';
import ExplorerCtrl from './explorer/explorerCtrl';
import { uciToMove } from '@lichess-org/chessground/util';
import { IdbTree } from './idbTree';
import pgnImport from './pgnImport';
import * as pgnExport from './pgnExport';
import { normalizeInlinePgn, pgnInputError, submitPgnToImportPipeline } from './pgnPipeline';
import * as studyApi from './studyApi';
import {
  moveReviewCopy,
  moveReviewEngineOutcomeAtRequiredDepth,
  moveReviewProofById,
  moveReviewSubjectFromNodeList,
  moveReviewSubjectKey,
  moveReviewVerdictLabel,
  normalizeMoveReviewLocale,
  type MoveReviewCopy,
  type MoveReviewFrameSelection,
  type MoveReviewJobState,
  type MoveReviewLocale,
  type MoveReviewProof,
  type MoveReviewSubject,
  type MoveReviewVerdictSymbol,
  type MoveReviewViewState,
  type IssuedMoveReviewEngineWork,
  type MoveReviewEngineOutcome,
} from './moveReview';
import {
  MoveReviewCoordinator,
  type MoveReviewCoordinatorHost,
  type MoveReviewPreparation,
} from './moveReviewCoordinator';
import { createMoveReviewRuntimeSource } from './moveReviewRuntimeSource';
import { mergeMoveReviewProofIntoStudy } from './moveReviewStudy';
import type { MoveReviewPanelProps } from './view/moveReview';

import type { PgnError } from 'chessops/pgn';

import { confirm } from 'lib/view';
import { displayColumns } from 'lib/device';
import * as Prefs from 'lib/prefs';
import { boardLabelModeFromCoords, boardLabelModeToCoords, type BoardLabelMode } from './boardWorkspace';

const boardLabelModes = new Set<BoardLabelMode>(['off', 'inside', 'rim', 'full']);

interface AnalyseHistoryState {
  analysePly: Ply;
}

const reviewResumeMarker = 'chesstory-review-pgn\n';

function loginHref(): string {
  return `/login?referrer=${encodeURIComponent('/analysis?resumeReview=1')}`;
}

function takeResumedReviewPgn(): string | undefined {
  const url = new URL(location.href);
  if (url.searchParams.get('resumeReview') !== '1') return;
  url.searchParams.delete('resumeReview');
  history.replaceState(history.state, '', `${url.pathname}${url.search}${url.hash}`);
  if (!window.name.startsWith(reviewResumeMarker)) return;
  const pgn = normalizeInlinePgn(window.name.slice(reviewResumeMarker.length));
  window.name = '';
  return pgn;
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
  private moveReviewCoordinator?: MoveReviewCoordinator;
  private moveReviewJob: MoveReviewJobState = { kind: 'idle', reason: 'disabled' };
  private moveReviewView: MoveReviewViewState = { evidenceExpanded: false };
  private moveReviewLocale?: MoveReviewLocale;
  private moveReviewCopyValue?: MoveReviewCopy;
  private moveReviewSubjectIdentity?: string;
  private moveReviewEngineFailure?: () => void;
  private moveReviewAdded?: { subjectKey: string; proofId: string; path: Tree.Path };
  private moveReviewAnnotations = new Map<Tree.Path, { symbol: MoveReviewVerdictSymbol; label: string }>();
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
    const initialPgn = opts.inlinePgn || takeResumedReviewPgn();
    if (initialPgn) this.data = this.changePgn(initialPgn, false) || this.data;

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
    this.initMoveReview();
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

  prepareStudyLogin = (): void => {
    const pgn = normalizeInlinePgn(pgnExport.renderFullTxt(this));
    if (pgn) window.name = reviewResumeMarker + pgn;
  };

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
      this.prepareStudyLogin();
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
          this.prepareStudyLogin();
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

  moveReviewAvailable(): boolean {
    return !!this.moveReviewCoordinator;
  }

  moveReviewPanelProps(): MoveReviewPanelProps {
    return {
      job: this.moveReviewJob,
      view: this.moveReviewView,
      copy: this.moveReviewCopyValue!,
      locale: this.moveReviewLocale!,
      orientation: this.getOrientation(),
      canWrite: this.canWriteStudy(),
      liveEnginePaused: !!this.moveReviewCoordinator?.isPreemptingLiveEngine() && !!this.cevalEnabled(),
      addedProofId:
        this.moveReviewAdded?.subjectKey === this.moveReviewSubjectIdentity
          ? this.moveReviewAdded?.proofId
          : undefined,
      actions: {
        selectCandidate: this.selectMoveReviewCandidate,
        toggleEvidence: this.toggleMoveReviewEvidence,
        toggleProof: this.toggleMoveReviewProof,
        previewFrame: this.previewMoveReviewFrame,
        clearPreview: this.clearMoveReviewPreview,
        pinFrame: this.pinMoveReviewFrame,
        clearPin: this.clearMoveReviewPin,
        retry: this.retryMoveReview,
        addProof: this.addMoveReviewProof,
        viewAddedLine: this.viewAddedMoveReviewProof,
      },
    };
  }

  moveReviewNotation(path: Tree.Path): { symbol: MoveReviewVerdictSymbol; label: string } | undefined {
    return this.moveReviewAnnotations.get(path);
  }

  private initMoveReview(): void {
    const mode = this.opts.moveReview?.mode;
    const variant = this.data.game.variant.key;
    if (!this.isStudy() || variant !== 'standard' || mode !== 'runtime') return;

    this.moveReviewLocale = normalizeMoveReviewLocale();
    this.moveReviewCopyValue = moveReviewCopy(this.moveReviewLocale);
    const host: MoveReviewCoordinatorHost = {
      prepare: (subject, signal) => this.prepareMoveReview(subject, signal),
      suspendLiveEngine: this.suspendLiveEngineForMoveReview,
      resumeLiveEngine: this.resumeLiveEngineAfterMoveReview,
      stateChanged: this.setMoveReviewState,
    };
    this.moveReviewCoordinator = new MoveReviewCoordinator(this.moveReviewLocale, host);
    const updateActivity = (): void => {
      if (document.visibilityState === 'hidden' || !document.hasFocus())
        this.moveReviewCoordinator?.deactivate();
      else this.moveReviewCoordinator?.activate();
    };
    document.addEventListener('visibilitychange', updateActivity);
    window.addEventListener('focus', updateActivity);
    window.addEventListener('blur', updateActivity);
    window.addEventListener('pagehide', () => this.moveReviewCoordinator?.deactivate());
    window.addEventListener('pageshow', updateActivity);
    updateActivity();
    this.scheduleMoveReviewForCurrentEdge();
  }

  private scheduleMoveReviewForCurrentEdge(): void {
    this.moveReviewCoordinator?.settle(
      moveReviewSubjectFromNodeList(this.data.game.variant.key, this.path, this.nodeList),
    );
  }

  private prepareMoveReview = async (
    subject: MoveReviewSubject,
    signal: AbortSignal,
  ): Promise<MoveReviewPreparation> => {
    const result = await this.ceval.preflightMoveReviewSelection({ variant: subject.variant, signal }, () =>
      this.moveReviewEngineFailure?.(),
    );
    if (!result.ok) {
      const engineUnavailable = result.failures.some(failure => failure.reason === 'preflight-failed');
      return {
        ok: false,
        reason: engineUnavailable ? 'engine-unavailable' : 'browser-unsupported',
        message: engineUnavailable
          ? this.moveReviewCopyValue!.engineUnavailable
          : this.moveReviewCopyValue!.browserUnsupported,
      };
    }
    return {
      ok: true,
      engineProfile: result.profile,
      source: createMoveReviewRuntimeSource(this.executeMoveReviewEngineWork),
    };
  };

  private executeMoveReviewEngineWork = (
    work: IssuedMoveReviewEngineWork,
    receivedAtMs: number,
    signal: AbortSignal,
  ): Promise<MoveReviewEngineOutcome> =>
    signal.aborted
      ? Promise.resolve({
          kind: 'executor_failed',
          executorElapsedMs: 0,
          observedNodes: 0,
          engineTimeMs: 0,
          failureCode: 'cancelled',
          diagnostic: 'The move review was cancelled before engine work started.',
        })
      : new Promise(resolve => {
          let done = false;
          let latestNodes = 0;
          let latestEngineTimeMs = 0;
          let previousEvaluation: Tree.LocalEval | undefined;
          const startedAt = receivedAtMs;
          const elapsed = (): number => Math.max(0, Math.floor(performance.now() - startedAt));
          let leaseTimer: number | undefined = undefined;
          const failure = (failureCode: string, diagnostic: string): MoveReviewEngineOutcome => ({
            kind: 'executor_failed',
            executorElapsedMs: elapsed(),
            observedNodes: latestNodes,
            engineTimeMs: latestEngineTimeMs,
            failureCode,
            diagnostic,
          });
          const finish = (outcome: MoveReviewEngineOutcome, stopWork = false): void => {
            if (done) return;
            done = true;
            if (leaseTimer !== undefined) window.clearTimeout(leaseTimer);
            signal.removeEventListener('abort', cancel);
            if (this.moveReviewEngineFailure === fail) this.moveReviewEngineFailure = undefined;
            if (stopWork) this.ceval.stopMoveReviewWork();
            resolve(outcome);
          };
          const cancel = (): void => finish(failure('cancelled', 'The move review was cancelled.'), true);
          const fail = (): void =>
            finish(
              failure('engine_failure', 'The browser engine stopped before producing an exact result.'),
              true,
            );
          this.moveReviewEngineFailure = fail;
          signal.addEventListener('abort', cancel, { once: true });
          const remainingLeaseMs = work.maxSearchElapsedMs - elapsed();
          if (remainingLeaseMs <= 0) {
            finish(failure('lease_expired', 'The browser engine exceeded its issued work lease.'), true);
            return;
          }
          leaseTimer = window.setTimeout(
            () =>
              finish(failure('lease_expired', 'The browser engine exceeded its issued work lease.'), true),
            remainingLeaseMs,
          );
          try {
            const started = this.ceval.startMoveReview(work.engineProfile, {
              variant: work.variant,
              initialFen: work.enginePositionInitialFen,
              currentFen: work.searchFen,
              moves: work.enginePositionMovesUci,
              path: `move-review:${work.workId}`,
              ply: this.plyFromFen(work.searchFen),
              multiPv: work.searchLimits.multiPv,
              searchLimits: work.searchLimits,
              rootMoves: work.rootRestriction.kind === 'restricted' ? work.rootRestriction.movesUci : [],
              observe: (nodes, engineTimeMs) => {
                latestNodes = Math.max(latestNodes, nodes);
                latestEngineTimeMs = Math.max(latestEngineTimeMs, engineTimeMs);
              },
              emit: evaluation => {
                latestNodes = Math.max(latestNodes, evaluation.nodes);
                latestEngineTimeMs = Math.max(latestEngineTimeMs, evaluation.millis);
                if (evaluation.depth === work.searchLimits.depth - 1 && evaluation.bestmove === undefined)
                  previousEvaluation = {
                    ...evaluation,
                    pvs: evaluation.pvs.map(pv => ({ ...pv, moves: [...pv.moves] })),
                  };
                const outcome = moveReviewEngineOutcomeAtRequiredDepth(
                  work,
                  evaluation,
                  previousEvaluation,
                  elapsed(),
                );
                if (outcome) finish(outcome, true);
                else if (evaluation.bestmove !== undefined) fail();
              },
            });
            if (!started) fail();
          } catch (_) {
            fail();
          }
        });

  private suspendLiveEngineForMoveReview = (): void => {
    if (this.cevalEnabled()) this.ceval.stop();
    this.redraw();
  };

  private resumeLiveEngineAfterMoveReview = (): void => {
    this.ceval.stopMoveReview();
    this.moveReviewEngineFailure = undefined;
    if (this.cevalEnabled()) this.startCeval();
    this.redraw();
  };

  private setMoveReviewState = (state: MoveReviewJobState): void => {
    this.moveReviewJob = state;
    const subject =
      state.kind === 'completed' || state.kind === 'position-action'
        ? state.snapshot.subject
        : state.kind === 'loading' ||
            state.kind === 'abstained' ||
            state.kind === 'fault' ||
            state.kind === 'unsupported'
          ? state.subject
          : undefined;
    const subjectKey = subject ? moveReviewSubjectKey(subject) : undefined;
    if (subjectKey !== this.moveReviewSubjectIdentity || state.kind === 'loading') {
      this.moveReviewSubjectIdentity = subjectKey;
      this.moveReviewView = {
        selectedCandidateUci: subject?.played.uci,
        evidenceExpanded: false,
      };
    }
    if (state.kind === 'completed') {
      const played = state.snapshot.evidence.candidates.find(candidate =>
        candidate.roles.includes('played'),
      )!;
      this.moveReviewView.selectedCandidateUci ??= played.uci;
      const review = played.review;
      if (review.kind === 'move-verdict' && !this.moveReviewView.expandedProofId && review.comparisonProof)
        this.moveReviewView.expandedProofId = review.comparisonProof.id;
      if (review.kind !== 'move-verdict' || review.core.verdictSymbol === 'none')
        this.moveReviewAnnotations.delete(subject!.after.path);
      else
        this.moveReviewAnnotations.set(subject!.after.path, {
          symbol: review.core.verdictSymbol,
          label: `${moveReviewVerdictLabel(review.core.verdictSymbol, this.moveReviewCopyValue!)}: ${subject!.played.san}`,
        });
    } else if (state.kind === 'position-action') {
      this.moveReviewAnnotations.delete(subject!.after.path);
    }
    this.redraw();
  };

  private selectMoveReviewCandidate = (uci: Uci): void => {
    this.moveReviewView = {
      selectedCandidateUci: uci,
      evidenceExpanded: this.moveReviewView.evidenceExpanded,
    };
    this.redraw();
  };

  private toggleMoveReviewEvidence = (): void => {
    this.moveReviewView.evidenceExpanded = !this.moveReviewView.evidenceExpanded;
    if (!this.moveReviewView.evidenceExpanded) this.moveReviewView.hoveredFrame = undefined;
    this.redraw();
  };

  private toggleMoveReviewProof = (proofId: string): void => {
    this.moveReviewView.expandedProofId =
      this.moveReviewView.expandedProofId === proofId ? undefined : proofId;
    this.redraw();
  };

  private previewMoveReviewFrame = (frame: MoveReviewFrameSelection): void => {
    this.moveReviewView.hoveredFrame = frame;
    this.redraw();
  };

  private clearMoveReviewPreview = (): void => {
    this.moveReviewView.hoveredFrame = undefined;
    this.redraw();
  };

  private pinMoveReviewFrame = (frame: MoveReviewFrameSelection): void => {
    this.moveReviewView.pinnedFrame = frame;
    this.moveReviewView.hoveredFrame = undefined;
    this.redraw();
  };

  private clearMoveReviewPin = (): void => {
    this.moveReviewView.pinnedFrame = undefined;
    this.redraw();
  };

  private retryMoveReview = (): void => this.moveReviewCoordinator?.retry();

  private currentMoveReviewProof(
    proofId: string,
  ): { subject: MoveReviewSubject; proof: MoveReviewProof } | undefined {
    if (this.moveReviewJob.kind !== 'completed') return;
    const { subject, evidence } = this.moveReviewJob.snapshot;
    for (const candidate of evidence.candidates) {
      const proof = moveReviewProofById(candidate.review, proofId);
      if (proof) return { subject, proof };
    }
    return;
  }

  private addMoveReviewProof = (proofId: string): void => {
    const selected = this.currentMoveReviewProof(proofId);
    if (!selected || !this.canWriteStudy()) return;
    const subjectKey = moveReviewSubjectKey(selected.subject);
    this.enqueueStudyWrite(async ref => {
      const path = await mergeMoveReviewProofIntoStudy(this.tree, ref, selected.subject, selected.proof);
      this.moveReviewAdded = { subjectKey, proofId, path };
      this.redraw();
    });
  };

  private viewAddedMoveReviewProof = (proofId: string): void => {
    if (
      this.moveReviewAdded?.proofId === proofId &&
      this.moveReviewAdded.subjectKey === this.moveReviewSubjectIdentity
    )
      this.userJump(this.moveReviewAdded.path);
  };

  private plyFromFen(fen: FEN): Ply {
    const parts = fen.split(/\s+/);
    const fullmove = Math.max(1, Number(parts[5]) || 1);
    return ((fullmove - 1) * 2 + (parts[1] === 'b' ? 1 : 0)) as Ply;
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
      this.scheduleMoveReviewForCurrentEdge();
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
    this.moveReviewCoordinator?.settle(undefined);
    this.initialize(data, merge);
    this.syncWorkspacePrefs();
    this.redirecting = false;
    this.setPath(treePath.root);
    this.initCeval();
    this.startCeval();
    this.moveReviewAnnotations.clear();
    this.scheduleMoveReviewForCurrentEdge();
    this.cgVersion.js++;
    this.mergeIdbThenShowTreeView();
  }

  changePgn(pgn: string, andReload: boolean): AnalyseData | undefined {
    this.pgnError = '';
    const normalized = normalizeInlinePgn(pgn);
    if (!normalized) {
      this.pgnError = pgnInputError(pgn);
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
      this.pgnError = pgnInputError(rawPgn);
      requestAnimationFrame(this.redraw);
      return false;
    }
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

  allowedEval(node: Tree.Node = this.node): Tree.ClientEval | false | undefined {
    return !this.cevalEnabled() ? false : node.ceval;
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
      } else if (!node.ceval || isEvalBetter(ev, node.ceval)) {
        node.ceval = ev;
      } else if (!ev.cloud) {
        if (node.ceval?.cloud && this.ceval.isDeeper()) {
          node.ceval = ev;
        }
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
        if (!this.moveReviewCoordinator?.isPreemptingLiveEngine()) this.initCeval();
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
    if (this.moveReviewCoordinator?.isPreemptingLiveEngine()) return;
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

  boardCoords = (): Prefs.Coords => boardLabelModeToCoords(this.boardLabelModeProp());

  showCapturedMaterial = (): boolean => this.showCapturedProp();

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
    const ceval = this.allowedEval();
    if (!ceval) return;
    const uci = ceval.pvs[0]?.moves[0] || this.nextNodeBest();
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
    const storedValue = storedProp(
      'analyse.variation-arrow-opacity',
      0,
      value => {
        const parsed = parseFloat(value);
        return isNaN(parsed) || parsed < -1 || parsed > 1 ? 0 : parsed;
      },
      value => value.toString(),
    );
    return (v?: number | false) => {
      const value = storedValue();
      if (v === false) return value;
      if (v === undefined || isNaN(v)) return value > 0 ? value : false;
      const nextValue = Math.min(1, Math.max(-1, v));
      storedValue(nextValue);
      this.setAutoShapes();
      this.chessground.redrawAll();
      this.redraw();
      return nextValue;
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
