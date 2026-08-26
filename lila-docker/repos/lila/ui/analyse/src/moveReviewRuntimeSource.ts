import { defaultInit, jsonHeader, xhrHeader } from 'lib/xhr';
import {
  buildMoveReviewEngineWorkReport,
  buildMoveReviewJobRequest,
  decodeMoveReviewSnapshot,
  type IssuedMoveReviewEngineWork,
  type MoveReviewEngineOutcome,
  type MoveReviewSource,
} from './moveReview';

const positionCommentaryJobsPath = '/api/chess-judgment/position-commentary-jobs';

type MoveReviewEngineExecutor = (
  work: IssuedMoveReviewEngineWork,
  receivedAtMs: number,
  signal: AbortSignal,
) => Promise<MoveReviewEngineOutcome>;

interface ReceivedJson {
  body: unknown;
  receivedAtMs: number;
}

export class MoveReviewRuntimeError extends Error {
  constructor(
    readonly code: 'offline' | 'rate-limit' | 'malformed-response' | 'runtime-unavailable',
    readonly retryable: boolean,
  ) {
    super(code);
    this.name = 'MoveReviewRuntimeError';
  }
}

export function createMoveReviewRuntimeSource(execute: MoveReviewEngineExecutor): MoveReviewSource {
  return {
    async run(request, emit, signal): Promise<void> {
      let jobId: string | undefined;
      let cancelled = false;
      let completed = false;
      const cancel = (): void => {
        if (jobId && !cancelled) {
          cancelled = true;
          void fetch(`${positionCommentaryJobsPath}/${jobId}`, {
            ...defaultInit,
            cache: 'no-store',
            method: 'delete',
            keepalive: true,
            headers: { ...jsonHeader, ...xhrHeader },
          }).catch(() => {});
        }
      };
      signal.addEventListener('abort', cancel, { once: true });
      try {
        const created = await requestJson(
          positionCommentaryJobsPath,
          {
            method: 'post',
            body: JSON.stringify(
              buildMoveReviewJobRequest(request.requestId, request.subject, request.engineProfile),
            ),
          },
          signal,
          [201],
        );
        let snapshot = decodeMoveReviewSnapshot(created.body, {
          requestId: request.requestId,
          subject: request.subject,
          engineProfile: request.engineProfile,
        });
        if (!snapshot) throw new MoveReviewRuntimeError('malformed-response', false);
        jobId = snapshot.jobId;
        const seenWorkIds = new Set<string>();
        const seenExecutionKeys = new Set<string>();
        let expectedWorkNumber = 0;
        let receivedAtMs = created.receivedAtMs;
        if (signal.aborted) {
          cancel();
          return;
        }

        while (!signal.aborted) {
          emit(snapshot);
          if (
            snapshot.kind === 'completed' ||
            snapshot.kind === 'position-action' ||
            snapshot.kind === 'abstained'
          ) {
            completed = true;
            return;
          }
          if (signal.aborted) return;

          const workNumber = Number(snapshot.issuedEngineWork.workId.slice('work:'.length));
          if (
            workNumber !== expectedWorkNumber ||
            seenWorkIds.has(snapshot.issuedEngineWork.workId) ||
            seenExecutionKeys.has(snapshot.issuedEngineWork.executionKeySha256)
          )
            throw new MoveReviewRuntimeError('malformed-response', false);
          seenWorkIds.add(snapshot.issuedEngineWork.workId);
          seenExecutionKeys.add(snapshot.issuedEngineWork.executionKeySha256);
          expectedWorkNumber++;

          const outcome = await execute(snapshot.issuedEngineWork, receivedAtMs, signal);
          if (signal.aborted) return;
          const reportBody = JSON.stringify(buildMoveReviewEngineWorkReport(snapshot.issuedEngineWork, outcome));
          const reported = await requestJson(
            `${positionCommentaryJobsPath}/${snapshot.jobId}/engine-work-reports`,
            {
              method: 'post',
              body: reportBody,
            },
            signal,
            [200],
          );
          const next = decodeMoveReviewSnapshot(reported.body, {
            requestId: request.requestId,
            subject: request.subject,
            engineProfile: request.engineProfile,
            jobId: snapshot.jobId,
          });
          if (!next) throw new MoveReviewRuntimeError('malformed-response', false);
          snapshot = next;
          receivedAtMs = reported.receivedAtMs;
        }
      } finally {
        signal.removeEventListener('abort', cancel);
        if (!completed) cancel();
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
