import { randomToken } from 'lib/algo';
import {
  moveReviewCopy,
  moveReviewBudgets,
  moveReviewFocusKey,
  moveReviewFrameForSubject,
  moveReviewObservations,
  moveReviewSubjectKey,
  type MoveReviewJobState,
  type MoveReviewLocale,
  type MoveReviewObservations,
  type MoveReviewSnapshot,
  type MoveReviewSource,
  type MoveReviewSourceRequest,
  type MoveReviewSubject,
  type MoveReviewRecord,
} from './moveReview';
import { MoveReviewRuntimeError } from './moveReviewRuntimeSource';
import { releaseCommentaryInvestigation } from './moveReviewSelection';

export interface MoveReviewCoordinatorHost {
  engineProfile: MoveReviewSourceRequest['engineProfile'];
  source: MoveReviewSource;
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
  private observations?: MoveReviewObservations;
  private retainedAssessment: Pick<MoveReviewJobState, 'assessment' | 'assessmentState' | 'assessmentScope' | 'verification'> =
    {};
  private debounceTimer?: number;
  private abort?: AbortController;
  private computationActive = false;
  private singleDeadlineAtMs?: number;
  private record?: MoveReviewRecord & {
    blockingFailure?: 'engine_unavailable' | 'browser_unsupported' | MoveReviewRuntimeError['code'];
    observations?: MoveReviewObservations;
    grades: Map<string, MoveReviewJobState>;
  };
  private selected?: MoveReviewSubject;

  constructor(
    private readonly locale: MoveReviewLocale,
    private readonly host: MoveReviewCoordinatorHost,
  ) {}

  activate(): void {
    if (this.active) return;
    this.active = true;
    this.arm();
  }

  deactivate(): void {
    if (!this.active) return;
    this.active = false;
    const interrupted = !!this.abort || this.debounceTimer !== undefined;
    this.releaseAuthority();
    this.cancelAttempt();
    if (interrupted && this.subject)
      this.publish({
        kind: 'abstained',
        subject: this.subject,
        stopCondition: 'cancelled',
        observations: this.observations,
      });
  }

  settle(subject: MoveReviewSubject | undefined, record?: MoveReviewRecord): void {
    if (record) {
      this.selected = subject;
      if (this.record?.key === record.key) {
        this.publishSelection();
        return;
      }
      this.cancelAttempt();
      this.record = {
        ...record,
        grades: new Map(),
      };
      this.observations = undefined;
      this.retainedAssessment = {};
      this.settleWork(record.subject);
      return;
    }
    this.record = undefined;
    this.selected = undefined;
    this.settleWork(subject);
  }

  private settleWork(subject: MoveReviewSubject | undefined): void {
    this.cancelAttempt();
    this.singleDeadlineAtMs = this.record ? undefined : performance.now() + moveReviewBudgets.singleMs;
    if (!subject || !this.subject || moveReviewSubjectKey(subject) !== moveReviewSubjectKey(this.subject)) {
      this.observations = undefined;
      this.retainedAssessment = {};
      const cached = subject && this.record?.grades.get(moveReviewFocusKey(subject));
      if (
        cached &&
        (cached.kind === 'completed' || cached.kind === 'position-action') &&
        moveReviewSubjectKey(cached.snapshot.subject) === moveReviewSubjectKey(subject)
      ) {
        const snapshot = cached.snapshot;
        this.retainedAssessment = {
          assessment:
            snapshot.kind === 'completed'
              ? { kind: 'completed', evidence: snapshot.evidence }
              : { kind: 'position-action', action: snapshot.action },
          assessmentState: snapshot.assessmentState,
          assessmentScope: snapshot.assessmentScope,
          verification: snapshot.verification,
        };
      }
    }
    this.subject = subject;
    this.completed = undefined;
    if (!subject) {
      this.releaseAuthority();
      this.publish({ kind: 'idle', reason: 'root' });
      return;
    }
    this.publish({ kind: 'loading', subject, observations: this.observations });
    this.arm();
  }

  retry(): void {
    if (this.record) {
      this.record.blockingFailure = undefined;
      this.settleWork(this.selected ?? this.subject ?? this.record.subject);
    } else if (this.subject) this.settle(this.subject);
  }

  isPreemptingLiveEngine(): boolean {
    return this.computationActive;
  }

  destroy(): void {
    this.deactivate();
    this.cancelAttempt();
    this.record?.grades.clear();
    this.record = undefined;
    this.subject = undefined;
    this.selected = undefined;
    this.completed = undefined;
    this.observations = undefined;
    this.retainedAssessment = {};
    this.host.stateChanged({ kind: 'idle', reason: 'disabled' });
  }

  private arm(): void {
    if (!this.active || !this.subject || this.completed || this.abort) return;
    if (this.record && this.record.grades.get(moveReviewFocusKey(this.subject))?.kind !== 'loading') return;
    if (!this.hasAuthority) {
      if (!this.authorityAbort) this.acquireAuthority();
      return;
    }
    if (this.debounceTimer !== undefined) window.clearTimeout(this.debounceTimer);
    const subjectKey = moveReviewSubjectKey(this.subject);
    this.debounceTimer = window.setTimeout(() => {
      this.debounceTimer = undefined;
      if (this.subject && moveReviewSubjectKey(this.subject) === subjectKey) void this.start(this.subject);
    }, debounceMillis);
  }

  private async start(subject: MoveReviewSubject): Promise<void> {
    if (!this.active || !this.hasAuthority) return;
    const generation = ++this.generation;
    const abort = new AbortController();
    this.abort = abort;
    this.computationActive = true;
    this.host.suspendLiveEngine();
    // Waiting for earlier moves or browser authority must not consume this move's execution time.
    const deadlineAtMs = this.record
      ? performance.now() + moveReviewBudgets.jobSafetyMs
      : this.singleDeadlineAtMs!;

    let completed: Extract<MoveReviewSnapshot, { kind: 'completed' }> | undefined;
    let positionAction: Extract<MoveReviewSnapshot, { kind: 'position-action' }> | undefined;
    let abstained: Extract<MoveReviewSnapshot, { kind: 'abstained' }> | undefined;
    try {
      if (performance.now() >= deadlineAtMs) throw new MoveReviewRuntimeError('deadline-exceeded', true);
      const request: MoveReviewSourceRequest = {
        requestId: `move-review-${randomToken()}`,
        subject,
        engineProfile: this.host.engineProfile,
        deadlineAtMs,
      };
      await this.host.source.run(
        request,
        incoming => {
          if (
            !this.isCurrent(generation, subject) ||
            abort.signal.aborted ||
            performance.now() >= deadlineAtMs
          )
            return;
          if (incoming.observations) {
            this.observations = incoming.observations;
            if (this.record && moveReviewFocusKey(subject) === moveReviewFocusKey(this.record.subject))
              this.record.observations = incoming.observations;
          }
          if (incoming.kind === 'awaiting-core' || incoming.kind === 'awaiting-evidence') {
            if (incoming.kind === 'awaiting-evidence' && incoming.assessment)
              this.retainedAssessment = {
                assessment: incoming.assessment,
                assessmentState: incoming.assessmentState,
                assessmentScope: incoming.assessmentScope,
                verification: incoming.verification,
              };
            this.publish({ kind: 'loading', subject, observations: this.observations });
            return;
          }
          if (incoming.kind === 'abstained') {
            abstained = incoming;
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
      if (performance.now() >= deadlineAtMs) throw new MoveReviewRuntimeError('deadline-exceeded', true);
      if (positionAction) {
        this.finishAttempt(abort);
        this.publish({ kind: 'position-action', snapshot: positionAction });
        return;
      }
      if (abstained) {
        if (
          this.record &&
          (abstained.stopCondition === 'engine_unavailable' ||
            abstained.stopCondition === 'browser_unsupported')
        )
          this.record.blockingFailure = abstained.stopCondition;
        this.completed = undefined;
        this.finishAttempt(abort);
        this.publish({
          kind: 'abstained',
          subject,
          stopCondition: abstained.stopCondition,
          observations: this.observations,
          ...(abstained.verification ? { verification: abstained.verification } : {}),
        });
        return;
      }
      if (!completed) throw new Error('move-review-source-ended');
      this.publishCompleted(completed, abort);
    } catch (error) {
      if (!this.isCurrent(generation, subject) || abort.signal.aborted) return;
      this.completed = undefined;
      this.finishAttempt(abort);
      if (
        (error instanceof MoveReviewRuntimeError && error.code === 'deadline-exceeded') ||
        performance.now() >= deadlineAtMs
      ) {
        this.publish({
          kind: 'abstained',
          subject,
          stopCondition: 'deadline_exceeded',
          observations: this.observations,
        });
        return;
      }
      if (this.record && error instanceof MoveReviewRuntimeError) this.record.blockingFailure = error.code;
      this.publish({
        kind: 'fault',
        subject,
        message: moveReviewCopy(this.locale).unavailable,
        retryable: error instanceof MoveReviewRuntimeError ? error.retryable : true,
        observations: this.observations,
      });
    } finally {
      if (this.isCurrent(generation, subject) && !abort.signal.aborted) {
        this.advanceRecord();
        if (!this.abort && this.debounceTimer === undefined) this.releaseAuthority();
      }
    }
  }

  private advanceRecord(): void {
    const record = this.record;
    if (!record || !this.active || !this.hasAuthority || record.blockingFailure || this.abort) return;
    // Advance chronologically only after the current source has ended.
    const next = record.subjects?.find(subject => !record.grades.has(moveReviewFocusKey(subject)));
    if (next) this.settleWork(next);
  }

  private publishCompleted(
    completed: Extract<MoveReviewSnapshot, { kind: 'completed' }>,
    abort: AbortController,
  ): void {
    this.completed = completed;
    this.finishAttempt(abort);
    this.publish({ kind: 'completed', snapshot: completed });
  }

  private publish(state: MoveReviewJobState): void {
    if (state.kind === 'completed' || state.kind === 'position-action' || state.kind === 'abstained' || state.kind === 'fault') {
      const observations = moveReviewObservations(state);
      if (observations) releaseCommentaryInvestigation(observations,
        state.kind === 'abstained' ? state.stopCondition : state.kind === 'fault' ? 'runtime_unavailable' : undefined);
    }
    if (state.kind === 'completed' || state.kind === 'position-action') {
      const snapshot = state.snapshot;
      this.retainedAssessment = {
        assessment:
          snapshot.kind === 'completed'
            ? { kind: 'completed', evidence: snapshot.evidence }
            : { kind: 'position-action', action: snapshot.action },
        assessmentState: snapshot.assessmentState,
        assessmentScope: snapshot.assessmentScope,
        verification: snapshot.verification,
      };
    }
    if (state.kind === 'abstained' || state.kind === 'fault') {
      const verification = state.verification ?? this.retainedAssessment.verification;
      this.retainedAssessment.verification =
        verification?.state === 'pending'
          ? {
              ...verification,
              state: 'stopped',
              stopReason: state.kind === 'abstained' ? state.stopCondition : 'runtime_unavailable',
            }
          : verification;
    }
    state = { ...this.retainedAssessment, ...state };
    if (!this.record) {
      this.host.stateChanged(state);
      return;
    }
    if (this.subject) this.record.grades.set(moveReviewFocusKey(this.subject), state);
    this.publishSelection();
  }

  private publishSelection(): void {
    const record = this.record!;
    const selected = this.selected;
    const recordReview: NonNullable<MoveReviewJobState['recordReview']> = {
      total: 0,
      assessed: 0,
      completed: 0,
      stopped: 0,
      scenes: [],
    };
    const seen = new Set<string>();
    for (const subject of record.subjects ?? [record.subject]) {
      const key = moveReviewFocusKey(subject);
      if (seen.has(key)) continue;
      seen.add(key);
      recordReview.total++;
      const grade = record.grades.get(key);
      if (!grade) continue;
      const ended = grade.kind === 'completed' || grade.kind === 'position-action';
      if (ended || grade.assessment) recordReview.assessed++;
      if (
        grade.kind === 'abstained' ||
        grade.kind === 'fault' ||
        (ended && (grade.verification?.state === 'stopped' || grade.assessmentState === 'needs_refresh'))
      )
        recordReview.stopped++;
      else if (ended) recordReview.completed++;
      const origin =
        grade.kind === 'completed' || grade.kind === 'position-action'
          ? grade.snapshot.subject
          : 'subject' in grade
            ? grade.subject
            : undefined;
      const observations = moveReviewObservations(grade);
      if (!origin || !observations?.selection) continue;
      const frame = moveReviewFrameForSubject(observations, origin, origin);
      if (!frame) continue;
      const root = observations.paths.find(path => path.id === frame.proofId)?.occurrenceIds[frame.ply - 1];
      const scenes = observations.selection.scenes.filter(scene => scene.rootOccurrenceId === root);
      if (!scenes.length) continue;
      const fen = subject.before.fen.split(' ');
      recordReview.scenes.push({
        path: subject.after.path,
        label: `${fen[5]}${fen[1] === 'b' ? '...' : '.'} ${subject.played.san}`,
        state: scenes.some(scene => scene.state === 'needs_explanation')
          ? 'needs_explanation'
          : scenes.some(scene => scene.state === 'partial')
            ? 'partial'
            : 'ready',
      });
    }
    const currentGrade = this.subject && record.grades.get(moveReviewFocusKey(this.subject));
    const cancelled = currentGrade?.kind === 'abstained' && currentGrade.stopCondition === 'cancelled';
    recordReview.status =
      recordReview.completed + recordReview.stopped === recordReview.total
        ? 'finished'
        : record.blockingFailure
          ? 'blocked'
          : !this.active || cancelled
            ? 'paused'
            : 'running';
    if (!selected) {
      this.host.stateChanged({
        kind: 'idle',
        reason: 'root',
        observations: record.observations,
        observationFrame: null,
        recordReview,
      });
      return;
    }
    const grade = record.grades.get(moveReviewFocusKey(selected));
    const ownObservations = grade && moveReviewObservations(grade);
    const observations = ownObservations ?? record.observations;
    const state: MoveReviewJobState = grade ?? {
      kind: 'unsearched',
      subject: selected,
      observationsPending:
        !observations && record.grades.get(moveReviewFocusKey(record.subject))?.kind === 'loading',
    };
    this.host.stateChanged({
      ...state,
      observations,
      recordReview,
      observationFrame: observations
        ? (moveReviewFrameForSubject(observations, ownObservations ? selected : record.subject, selected) ??
          null)
        : null,
    });
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
    if (this.retainedAssessment.verification?.state === 'pending')
      this.retainedAssessment.verification = {
        ...this.retainedAssessment.verification,
        state: 'stopped',
        stopReason: 'cancelled',
      };
    if (this.observations && (this.abort || this.debounceTimer !== undefined))
      releaseCommentaryInvestigation(this.observations, 'cancelled');
    if (this.record && this.subject && (this.abort || this.debounceTimer !== undefined))
      this.record.grades.set(moveReviewFocusKey(this.subject), {
        ...this.retainedAssessment,
        kind: 'abstained',
        subject: this.subject,
        stopCondition: 'cancelled',
        observations: this.observations,
      });
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
