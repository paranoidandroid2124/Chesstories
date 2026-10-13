import type { Tree } from '../review/types';
const defined = <T>(value: T | undefined): value is T => value !== undefined;
import type { Work } from './types';

export class Protocol {
  public engineName: string | undefined;
  readonly ready: Promise<void>;

  private work: Work | undefined;
  private currentEval: Tree.LocalEval | undefined;
  private expectedPvs = 1;
  private moveReviewPvs = new Map<number, Tree.PvData>();
  private moveReviewDepth = 0;
  private moveReviewSeldepth = 0;
  private moveReviewNodes = 0;
  private moveReviewMillis = 0;
  private moveReviewPreparing: Work | undefined;
  private initialReady = false;
  private resolveReady: (() => void) | undefined;

  private nextWork: Work | undefined;

  private send: ((cmd: string) => void) | undefined;
  private options: Map<string, string | number> = new Map<string, string>();

  constructor() {
    this.ready = new Promise(resolve => (this.resolveReady = resolve));
  }

  connected(send: (cmd: string) => void): void {
    this.send = send;
    this.moveReviewPreparing = undefined;
    this.initialReady = false;

    // Cache only options sent on this connection, not assumed engine defaults.
    this.options.clear();
    this.send('uci');
  }

  private setOption(name: string, value: string | number): void {
    value = value.toString();
    if (this.send && this.options.get(name) !== value) {
      this.send(`setoption name ${name} value ${value}`);
      this.options.set(name, value);
    }
  }

  disconnected(): void {
    if (this.work && this.currentEval) {
      this.currentEval.bestmove ??= '(none)';
      this.work.emit(this.currentEval);
    }
    this.work = undefined;
    this.send = undefined;
  }

  received(command: string): void {
    const parts = command.trim().split(/\s+/g);
    if (parts[0] === 'uciok') {
      // Analyse without contempt.
      this.setOption('UCI_AnalyseMode', 'true');
      this.setOption('Analysis Contempt', 'Off');

      // Both ordinary analysis and commentary use standard king-destination castling.
      this.setOption('UCI_Chess960', 'false');

      this.send?.('ucinewgame');
      this.send?.('isready');
    } else if (parts[0] === 'readyok') {
      if (this.moveReviewPreparing) this.startMoveReviewWork();
      else {
        this.initialReady = true;
        this.resolveReady?.();
        this.resolveReady = undefined;
        this.swapWork();
      }
    } else if (parts[0] === 'id' && parts[1] === 'name') this.engineName = parts.slice(2).join(' ');
    else if (parts[0] === 'bestmove') {
      const work = this.work;
      this.work = undefined;
      if (work) {
        const ceval = this.currentEval ?? { millis: 0, fen: work.currentFen, depth: 0, nodes: 0, pvs: [] };
        ceval.bestmove = parts[1];
        if (parts[2] === 'ponder') ceval.ponder = parts[3];
        work.emit(ceval);
      }
      this.swapWork();
      return;
    } else if (this.work && parts[0] === 'info') {
      let depth = 0,
        seldepth,
        nodes,
        multiPv = 1,
        millis,
        bound: Tree.PvData['bound'],
        isMate = false,
        povEv,
        moves: string[] = [];
      for (let i = 1; i < parts.length; i++) {
        switch (parts[i]) {
          case 'depth':
            depth = parseInt(parts[++i]);
            break;
          case 'nodes':
            nodes = parseInt(parts[++i]);
            break;
          case 'seldepth':
            seldepth = parseInt(parts[++i]);
            break;
          case 'multipv':
            multiPv = parseInt(parts[++i]);
            break;
          case 'time':
            millis = parseInt(parts[++i]);
            break;
          case 'score':
            isMate = parts[++i] === 'mate';
            povEv = parseInt(parts[++i]);
            if (parts[i + 1] === 'lowerbound' || parts[i + 1] === 'upperbound')
              bound = parts[++i] as typeof bound;
            break;
          case 'pv':
            moves = parts.slice(++i);
            i = parts.length;
            break;
        }
      }

      if (defined(nodes) || defined(millis))
        this.work.observe?.(nodes ?? 0, millis ?? 0);

      // Stop can be followed by final usage info before bestmove releases this work.
      // Preserve accounting, but do not admit another PV or evaluation after cancellation.
      if (this.work.stopRequested) return;

      // Sometimes we get #0. Let's just skip it.
      if (isMate && !povEv) return;

      if (!defined(nodes) || !defined(millis) || !defined(isMate) || !defined(povEv)) return;

      const ev = this.work.ply % 2 === 1 ? -povEv : povEv;

      // Ignore primary bound messages; preserve non-primary bound metadata for consumers that need exact lines.
      if (bound && multiPv === 1) {
        if (depth >= this.moveReviewDepth) this.moveReviewPvs.clear();
        return;
      }

      const pvData = {
        moves,
        cp: isMate ? undefined : ev,
        mate: isMate ? ev : undefined,
        depth,
        ...(bound ? { bound } : {}),
      };

      this.acceptMoveReviewPv(multiPv, depth, seldepth, nodes, millis, pvData);
    } else if (
      command &&
      !['Stockfish', 'id', 'option', 'info'].includes(parts[0]) &&
      !['Analysis Contempt', 'UCI_Variant', 'UCI_AnalyseMode'].includes(command.split(': ')[1])
    )
      console.warn(`SF: ${command}`);
  }

  private stop(): void {
    if (this.work && !this.work.stopRequested) {
      this.work.stopRequested = true;
      this.send?.('stop');
    }
  }

  private swapWork(): void {
    if (!this.send || this.work || this.moveReviewPreparing || !this.initialReady) return;
    this.prepareMoveReviewWork();
  }

  private prepareMoveReviewWork(): void {
    if (!this.send || !this.nextWork) return;
    const work = this.nextWork;

    this.currentEval = undefined;
    this.expectedPvs = work.multiPv;
    this.moveReviewPvs.clear();
    this.moveReviewDepth = 0;
    this.moveReviewSeldepth = 0;
    this.moveReviewNodes = 0;
    this.moveReviewMillis = 0;

    this.setOption('UCI_Variant', work.variant);
    // The dedicated standard review protocol uses king-destination castling
    // for its issued history, searchmoves restriction and reported PVs.
    this.setOption('UCI_Chess960', 'false');
    this.setOption('Threads', work.threads);
    this.setOption('Hash', work.hashSize || 16);
    this.setOption('MultiPV', Math.max(1, work.multiPv));

    this.send('setoption name Clear Hash');
    this.send('ucinewgame');
    this.moveReviewPreparing = work;
    this.send('isready');
  }

  private acceptMoveReviewPv(
    multiPv: number,
    depth: number,
    seldepth: number | undefined,
    nodes: number,
    millis: number,
    pv: Tree.PvData,
  ): void {
    const work = this.work;
    if (
      !work ||
      !Number.isSafeInteger(multiPv) ||
      multiPv < 1 ||
      multiPv > this.expectedPvs ||
      depth < this.moveReviewDepth
    )
      return;
    // Stockfish can replace its complete MultiPV set at the same depth. A
    // fresh primary starts that set; its secondary lines cannot borrow entries
    // from an earlier set, including one already emitted at this depth.
    if (depth > this.moveReviewDepth || multiPv === 1) {
      this.moveReviewDepth = depth;
      this.moveReviewPvs.clear();
      this.moveReviewSeldepth = 0;
      this.moveReviewNodes = 0;
      this.moveReviewMillis = 0;
    }
    if (multiPv !== 1 && !this.moveReviewPvs.has(1)) return;
    this.moveReviewPvs.set(multiPv, pv);
    this.moveReviewSeldepth = Math.max(this.moveReviewSeldepth, seldepth ?? 0);
    this.moveReviewNodes = Math.max(this.moveReviewNodes, nodes);
    this.moveReviewMillis = Math.max(this.moveReviewMillis, millis);
    const pvs = Array.from({ length: this.expectedPvs }, (_, index) => this.moveReviewPvs.get(index + 1));
    if (pvs.some(candidate => candidate === undefined)) return;
    const complete = pvs as Tree.PvData[];
    const primary = complete[0]!;
    this.currentEval = {
      fen: work.currentFen,
      depth,
      ...(this.moveReviewSeldepth ? { seldepth: this.moveReviewSeldepth } : {}),
      nodes: this.moveReviewNodes,
      millis: this.moveReviewMillis,
      cp: primary.cp,
      mate: primary.mate,
      pvs: complete,
    };
    this.moveReviewPvs.clear();
    work.emit(this.currentEval);
  }

  private startMoveReviewWork(): void {
    const work = this.moveReviewPreparing;
    this.moveReviewPreparing = undefined;
    if (!work || !this.send || this.nextWork !== work) {
      this.swapWork();
      return;
    }

    this.work = work;
    this.nextWork = undefined;

    this.send(['position fen', work.initialFen, 'moves', ...work.moves].join(' '));
    this.send(
      [
        'go',
        'depth',
        work.searchLimits.depth,
        'nodes',
        work.searchLimits.nodes,
        'movetime',
        work.searchLimits.movetimeMs,
        ...(work.rootMoves.length ? ['searchmoves', ...work.rootMoves] : []),
      ].join(' '),
    );
  }

  compute(nextWork: Work | undefined): void {
    this.nextWork = nextWork;
    this.stop();
    this.swapWork();
  }

  isComputing(): boolean {
    return !!this.work && !this.work.stopRequested;
  }
}
