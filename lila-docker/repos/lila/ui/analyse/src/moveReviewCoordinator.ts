import { randomToken } from 'lib/algo';
import {
  moveReviewCacheKey,
  moveReviewCopy,
  moveReviewSubjectKey,
  MoveReviewMemoryLru,
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

type LeaseMessage = { type: 'claim'; token: string; order: number } | { type: 'release'; token: string };

const channelName = 'chesstory.position-commentary.v6';
const debounceMillis = 250;

export class MoveReviewCoordinator {
  private readonly cache = new MoveReviewMemoryLru<Extract<MoveReviewSnapshot, { kind: 'completed' }>>(64);
  private readonly token = randomToken();
  private readonly channel = new BroadcastChannel(channelName);
  private generation = 0;
  private leaseClock = 0;
  private claimOrder = 0;
  private ownerToken = this.token;
  private ownerClaimOrder = 0;
  private active = false;
  private subject?: MoveReviewSubject;
  private completed?: Extract<MoveReviewSnapshot, { kind: 'completed' }>;
  private debounceTimer?: number;
  private abort?: AbortController;
  private computationActive = false;

  constructor(
    private readonly locale: MoveReviewLocale,
    private readonly host: MoveReviewCoordinatorHost,
  ) {
    this.channel.onmessage = event => this.receiveLease(event.data as LeaseMessage);
  }

  activate(): void {
    this.active = true;
    this.claimOrder = ++this.leaseClock;
    this.ownerToken = this.token;
    this.ownerClaimOrder = this.claimOrder;
    this.channel.postMessage({
      type: 'claim',
      token: this.token,
      order: this.claimOrder,
    } satisfies LeaseMessage);
    this.arm();
  }

  deactivate(): void {
    this.active = false;
    const incomplete = this.subject !== undefined && !this.completed;
    this.cancelAttempt();
    if (incomplete) this.host.stateChanged({ kind: 'loading', subject: this.subject! });
    if (this.ownerToken === this.token)
      this.channel.postMessage({
        type: 'release',
        token: this.token,
      } satisfies LeaseMessage);
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
    this.channel.close();
  }

  private arm(): void {
    if (!this.active || this.ownerToken !== this.token || !this.subject || this.completed || this.abort)
      return;
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
    let cacheHit = false;
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
          const cached = this.cache.get(
            moveReviewCacheKey(subject, {
              engineProfile: incoming.engineProfile,
              judgmentRevision: incoming.judgmentRevision,
              annotationPolicyRevision: incoming.annotationPolicyRevision,
            }),
          );
          if (cached) {
            completed = cached;
            cacheHit = true;
            abort.abort();
          } else if (incoming.kind === 'completed') completed = incoming;
        },
        abort.signal,
      );
      if (cacheHit && completed && this.isCurrent(generation, subject)) {
        this.publishCompleted(completed, abort);
        return;
      }
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
      this.cache.set(
        moveReviewCacheKey(completed.subject, {
          engineProfile: completed.engineProfile,
          judgmentRevision: completed.judgmentRevision,
          annotationPolicyRevision: completed.annotationPolicyRevision,
        }),
        completed,
      );
      this.publishCompleted(completed, abort);
    } catch (error) {
      if (cacheHit && completed && this.isCurrent(generation, subject)) {
        this.publishCompleted(completed, abort);
        return;
      }
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
      this.ownerToken === this.token &&
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

  private receiveLease(message: LeaseMessage): void {
    if (!message || message.token === this.token) return;
    if (message.type === 'release') {
      if (this.ownerToken !== message.token || !this.active) return;
      this.ownerToken = this.token;
      this.ownerClaimOrder = this.claimOrder;
      this.channel.postMessage({
        type: 'claim',
        token: this.token,
        order: this.claimOrder,
      } satisfies LeaseMessage);
      this.arm();
      return;
    }
    this.leaseClock = Math.max(this.leaseClock, message.order);
    // An activation that observed another claim is causally newer. Truly
    // concurrent claims have the same order and converge by token.
    const remoteWins =
      message.order > this.ownerClaimOrder ||
      (message.order === this.ownerClaimOrder && message.token > this.ownerToken);
    if (!remoteWins) return;
    this.ownerToken = message.token;
    this.ownerClaimOrder = message.order;
    const incomplete = this.subject !== undefined && !this.completed;
    this.cancelAttempt();
    if (incomplete) this.host.stateChanged({ kind: 'loading', subject: this.subject! });
  }
}
