import { randomToken } from 'lib/algo';
import {
  moveReviewCopy,
  moveReviewSubjectKey,
  type MoveReviewJobState,
  type MoveReviewLocale,
  type MoveReviewSnapshot,
  type MoveReviewSource,
  type MoveReviewSourceRequest,
  type MoveReviewSubject,
} from './moveReview';
import { MoveReviewRuntimeError } from './moveReviewRuntimeSource';

export type MoveReviewPreparation =
  | {
      ok: true;
      engineProfile: MoveReviewSourceRequest['engineProfile'];
      source: MoveReviewSource;
    }
  | {
      ok: false;
      reason: Extract<MoveReviewJobState, { kind: 'unsupported' }>['reason'];
      message: string;
    };

export interface MoveReviewCoordinatorHost {
  prepare(subject: MoveReviewSubject, signal: AbortSignal): Promise<MoveReviewPreparation>;
  suspendLiveEngine(): void;
  resumeLiveEngine(): void;
  stateChanged(state: MoveReviewJobState): void;
}

const lockName = 'chesstory.position-commentary.v6';
const debounceMillis = 250;

export class MoveReviewCoordinator {
  private generation = 0;
  private authorityGeneration = 0;
  private authorityAbort?: AbortController;
  private authorityRelease?: () => void;
  private hasAuthority = false;
  private active = false;
  private subject?: MoveReviewSubject;
  private completed?: Extract<MoveReviewSnapshot, { kind: 'completed' }>;
  private debounceTimer?: number;
  private abort?: AbortController;
  private computationActive = false;

  constructor(
    private readonly locale: MoveReviewLocale,
    private readonly host: MoveReviewCoordinatorHost,
  ) {}

  activate(): void {
    if (this.active) return;
    this.active = true;
    this.acquireAuthority();
  }

  deactivate(): void {
    if (!this.active) return;
    this.active = false;
    const incomplete = this.subject !== undefined && !this.completed;
    this.releaseAuthority();
    this.cancelAttempt();
    if (incomplete) this.host.stateChanged({ kind: 'loading', subject: this.subject! });
  }

  settle(subject: MoveReviewSubject | undefined): void {
    this.cancelAttempt();
    this.subject = subject;
    this.completed = undefined;
    if (!subject) {
      this.host.stateChanged({ kind: 'idle', reason: 'root' });
      return;
    }
    this.host.stateChanged({ kind: 'loading', subject });
    this.arm();
  }

  retry(): void {
    if (this.subject) this.settle(this.subject);
  }

  isPreemptingLiveEngine(): boolean {
    return this.computationActive;
  }

  destroy(): void {
    this.deactivate();
  }

  private arm(): void {
    if (!this.active || !this.hasAuthority || !this.subject || this.completed || this.abort) return;
    if (this.debounceTimer !== undefined) window.clearTimeout(this.debounceTimer);
    const subjectKey = moveReviewSubjectKey(this.subject);
    this.debounceTimer = window.setTimeout(() => {
      this.debounceTimer = undefined;
      if (this.subject && moveReviewSubjectKey(this.subject) === subjectKey) void this.start(this.subject);
    }, debounceMillis);
  }

  private async start(subject: MoveReviewSubject): Promise<void> {
    const generation = ++this.generation;
    const abort = new AbortController();
    this.abort = abort;
    this.computationActive = true;
    this.host.suspendLiveEngine();

    let completed: Extract<MoveReviewSnapshot, { kind: 'completed' }> | undefined;
    let positionAction: Extract<MoveReviewSnapshot, { kind: 'position-action' }> | undefined;
    let abstained = false;
    try {
      const prepared = await this.host.prepare(subject, abort.signal);
      if (!this.isCurrent(generation, subject) || abort.signal.aborted) return;
      if (!prepared.ok) {
        this.finishAttempt(abort);
        this.host.stateChanged({
          kind: 'unsupported',
          subject,
          reason: prepared.reason,
          message: prepared.message,
        });
        return;
      }

      const request: MoveReviewSourceRequest = {
        requestId: `move-review-${randomToken()}`,
        subject,
        engineProfile: prepared.engineProfile,
      };
      await prepared.source.run(
        request,
        incoming => {
          if (!this.isCurrent(generation, subject) || abort.signal.aborted) return;
          if (incoming.kind === 'abstained') {
            abstained = true;
            return;
          }
          if (incoming.kind === 'position-action') {
            positionAction = incoming;
            return;
          }
          if (incoming.kind === 'completed') completed = incoming;
        },
        abort.signal,
      );
      if (!this.isCurrent(generation, subject) || abort.signal.aborted) return;
      if (positionAction) {
        this.finishAttempt(abort);
        this.host.stateChanged({ kind: 'position-action', snapshot: positionAction });
        return;
      }
      if (abstained) {
        this.completed = undefined;
        this.finishAttempt(abort);
        this.host.stateChanged({ kind: 'abstained', subject });
        return;
      }
      if (!completed) throw new Error('move-review-source-ended');
      this.publishCompleted(completed, abort);
    } catch (error) {
      if (!this.isCurrent(generation, subject) || abort.signal.aborted) return;
      this.completed = undefined;
      this.finishAttempt(abort);
      this.host.stateChanged({
        kind: 'fault',
        subject,
        message: moveReviewCopy(this.locale).unavailable,
        retryable: error instanceof MoveReviewRuntimeError ? error.retryable : true,
      });
    }
  }

  private publishCompleted(
    completed: Extract<MoveReviewSnapshot, { kind: 'completed' }>,
    abort: AbortController,
  ): void {
    this.completed = completed;
    this.finishAttempt(abort);
    this.host.stateChanged({ kind: 'completed', snapshot: completed });
  }

  private isCurrent(generation: number, subject: MoveReviewSubject): boolean {
    return (
      generation === this.generation &&
      this.hasAuthority &&
      this.subject !== undefined &&
      moveReviewSubjectKey(this.subject) === moveReviewSubjectKey(subject)
    );
  }

  private cancelAttempt(): void {
    this.generation++;
    if (this.debounceTimer !== undefined) window.clearTimeout(this.debounceTimer);
    this.debounceTimer = undefined;
    this.abort?.abort();
    this.abort = undefined;
    this.finish();
  }

  private finishAttempt(abort: AbortController): void {
    if (this.abort === abort) this.abort = undefined;
    this.finish();
  }

  private finish(): void {
    if (!this.computationActive) return;
    this.computationActive = false;
    this.host.resumeLiveEngine();
  }

  private acquireAuthority(): void {
    const locks = typeof navigator === 'undefined' ? undefined : navigator.locks;
    if (!locks) {
      // ponytail: without Web Locks, independent server jobs are safer than another client lease protocol.
      this.hasAuthority = true;
      this.arm();
      return;
    }

    const generation = ++this.authorityGeneration;
    const abort = new AbortController();
    this.authorityAbort = abort;
    void locks
      .request(lockName, { mode: 'exclusive', signal: abort.signal }, async () => {
        if (generation !== this.authorityGeneration || !this.active) return;
        if (this.authorityAbort === abort) this.authorityAbort = undefined;
        this.hasAuthority = true;
        this.arm();
        await new Promise<void>(resolve => {
          this.authorityRelease = resolve;
        });
        if (generation !== this.authorityGeneration) return;
        this.authorityRelease = undefined;
        this.hasAuthority = false;
      })
      .catch(() => {
        if (generation !== this.authorityGeneration || abort.signal.aborted || !this.active) return;
        if (this.authorityAbort === abort) this.authorityAbort = undefined;
        this.hasAuthority = true;
        this.arm();
      });
  }

  private releaseAuthority(): void {
    this.authorityGeneration++;
    this.authorityAbort?.abort();
    this.authorityAbort = undefined;
    this.hasAuthority = false;
    this.authorityRelease?.();
    this.authorityRelease = undefined;
  }
}
