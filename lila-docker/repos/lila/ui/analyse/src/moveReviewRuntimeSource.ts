import { defaultInit, jsonHeader, xhrHeader } from 'lib/xhr';
import {
  buildMoveReviewEngineWorkReport,
  buildMoveReviewJobRequest,
  decodeMoveReviewSnapshot,
  moveReviewBudgets,
  type IssuedMoveReviewEngineWork,
  type MoveReviewEngineOutcome,
  type MoveReviewSource,
} from './moveReview';

const positionCommentaryJobsPath = '/api/chess-judgment/position-commentary-jobs';

type MoveReviewEngineExecutor = (
  work: IssuedMoveReviewEngineWork,
  receivedAtMs: number,
  signal: AbortSignal,
  deadlineAtMs?: number,
) => Promise<MoveReviewEngineOutcome>;

interface ReceivedJson {
  body: unknown;
  receivedAtMs: number;
}

export class MoveReviewRuntimeError extends Error {
  constructor(
    readonly code:
      | 'offline'
      | 'rate-limit'
      | 'malformed-response'
      | 'runtime-unavailable'
      | 'deadline-exceeded',
    readonly retryable: boolean,
  ) {
    super(code);
    this.name = 'MoveReviewRuntimeError';
  }
}

export function createMoveReviewRuntimeSource(execute: MoveReviewEngineExecutor): MoveReviewSource {
  return {
    async run(request, emit, signal): Promise<void> {
      const deadlineAtMs = request.deadlineAtMs ?? performance.now() + moveReviewBudgets.singleMs;
      const abort = new AbortController();
      const forwardAbort = () => abort.abort();
      signal.addEventListener('abort', forwardAbort, { once: true });
      if (signal.aborted) forwardAbort();
      const remaining = () => Math.max(0, Math.ceil(deadlineAtMs - performance.now()));
      const checkDeadline = () => {
        if (remaining() <= 0) {
          abort.abort();
          throw new MoveReviewRuntimeError('deadline-exceeded', true);
        }
      };
      const deadlineTimer = setTimeout(() => abort.abort(), remaining());
      const activeSignal = abort.signal;
      let jobId: string | undefined;
      let released = false;
      const release = (): void => {
        if (jobId && !released) {
          released = true;
          void fetch(`${positionCommentaryJobsPath}/${jobId}`, {
            ...defaultInit,
            cache: 'no-store',
            method: 'delete',
            keepalive: true,
            headers: { ...jsonHeader, ...xhrHeader, Prefer: 'return=minimal' },
          }).catch(() => {});
        }
      };
      activeSignal.addEventListener('abort', release, { once: true });
      try {
        if (signal.aborted) return;
        checkDeadline();
        const created = await requestJson(
          positionCommentaryJobsPath,
          {
            method: 'post',
            body: JSON.stringify(
              buildMoveReviewJobRequest(
                request.requestId,
                request.subject,
                request.engineProfile,
                remaining(),
              ),
            ),
          },
          activeSignal,
          [201],
        );
        let snapshot = decodeMoveReviewSnapshot(created.body, {
          requestId: request.requestId,
          subject: request.subject,
          engineProfile: request.engineProfile,
        });
        created.body = undefined;
        if (!snapshot) throw new MoveReviewRuntimeError('malformed-response', false);
        jobId = snapshot.jobId;
        checkDeadline();
        const seenWorkIds = new Set<string>();
        const seenExecutionKeys = new Set<string>();
        let expectedWorkNumber = 0;
        let receivedAtMs = created.receivedAtMs;
        if (activeSignal.aborted) {
          release();
          return;
        }

        while (!activeSignal.aborted) {
          checkDeadline();
          if (
            snapshot.kind === 'completed' ||
            snapshot.kind === 'position-action' ||
            snapshot.kind === 'abstained'
          ) {
            emit(snapshot);
            checkDeadline();
            return;
          }
          checkDeadline();
          if (activeSignal.aborted) return;

          const workNumber = Number(snapshot.issuedEngineWork.workId.slice('work:'.length));
          if (
            // The producer owns the exploration/repair allowance. Transport keeps the
            // original deadline and exact work identity, not a second copy of that policy.
            workNumber !== expectedWorkNumber ||
            seenWorkIds.has(snapshot.issuedEngineWork.workId) ||
            seenExecutionKeys.has(snapshot.issuedEngineWork.executionKeySha256)
          )
            throw new MoveReviewRuntimeError('malformed-response', false);
          seenWorkIds.add(snapshot.issuedEngineWork.workId);
          seenExecutionKeys.add(snapshot.issuedEngineWork.executionKeySha256);
          expectedWorkNumber++;

          emit(snapshot);
          checkDeadline();
          if (activeSignal.aborted) return;

          const outcome = await execute(snapshot.issuedEngineWork, receivedAtMs, activeSignal, deadlineAtMs);
          checkDeadline();
          if (activeSignal.aborted) return;
          const reportBody = JSON.stringify(
            buildMoveReviewEngineWorkReport(snapshot.issuedEngineWork, outcome),
          );
          checkDeadline();
          const reported = await requestJson(
            `${positionCommentaryJobsPath}/${snapshot.jobId}/engine-work-reports`,
            {
              method: 'post',
              body: reportBody,
            },
            activeSignal,
            [200],
          );
          const next = decodeMoveReviewSnapshot(reported.body, {
            requestId: request.requestId,
            subject: request.subject,
            engineProfile: request.engineProfile,
            jobId: snapshot.jobId,
          });
          reported.body = undefined;
          if (!next) throw new MoveReviewRuntimeError('malformed-response', false);
          checkDeadline();
          snapshot = next;
          receivedAtMs = reported.receivedAtMs;
        }
      } catch (error) {
        if (!signal.aborted) checkDeadline();
        throw error;
      } finally {
        clearTimeout(deadlineTimer);
        signal.removeEventListener('abort', forwardAbort);
        activeSignal.removeEventListener('abort', release);
        // Both a consumed terminal result and an abandoned job have no server-side
        // reader left. The coordinator owns the last usable assessment and evidence.
        release();
      }
    },
  };
}

async function requestJson(
  url: string,
  init: Pick<RequestInit, 'method' | 'body'>,
  signal: AbortSignal,
  acceptedStatuses: readonly number[],
): Promise<ReceivedJson> {
  let response: Response;
  try {
    response = await fetch(url, {
      ...defaultInit,
      ...init,
      cache: 'no-store',
      signal,
      headers: { ...jsonHeader, ...xhrHeader, 'Content-Type': 'application/json' },
    });
  } catch (error) {
    if (signal.aborted) throw error;
    throw new MoveReviewRuntimeError('offline', true);
  }
  if (response.status === 429) throw new MoveReviewRuntimeError('rate-limit', true);
  if (!acceptedStatuses.includes(response.status))
    throw new MoveReviewRuntimeError('runtime-unavailable', response.status >= 500);
  const receivedAtMs = performance.now();
  try {
    return { body: await response.json(), receivedAtMs };
  } catch (_) {
    throw new MoveReviewRuntimeError('malformed-response', false);
  }
}
