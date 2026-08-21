import { defaultInit, jsonHeader, xhrHeader } from 'lib/xhr';
import {
  buildMoveReviewEngineWorkReport,
  buildMoveReviewJobRequest,
  decodeMoveReviewSnapshot,
  moveReviewCompactReceipt,
  type IssuedMoveReviewEngineWork,
  type MoveReviewCompactReceipt,
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
          [200, 201],
        );
        const reportedReceipts: MoveReviewCompactReceipt[] = [];
        let snapshot = decodeMoveReviewSnapshot(created.body, {
          requestId: request.requestId,
          subject: request.subject,
          engineProfile: request.engineProfile,
          reportedReceipts,
        });
        if (!snapshot) throw new MoveReviewRuntimeError('malformed-response', false);
        jobId = snapshot.jobId;
        const judgmentRevision = snapshot.judgmentRevision;
        const annotationPolicyRevision = snapshot.annotationPolicyRevision;
        const generation = snapshot.generation;
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

          const outcome = await execute(snapshot.issuedEngineWork, receivedAtMs, signal);
          if (signal.aborted) return;
          const reportBody = JSON.stringify(buildMoveReviewEngineWorkReport(snapshot.issuedEngineWork, outcome));
          const expectedReceipt = moveReviewCompactReceipt(snapshot.issuedEngineWork, outcome);
          const reported = await requestJson(
            `${positionCommentaryJobsPath}/${snapshot.jobId}/engine-work-reports`,
            {
              method: 'post',
              body: reportBody,
            },
            signal,
            [200],
            true,
          );
          const next = decodeMoveReviewSnapshot(reported.body, {
            requestId: request.requestId,
            subject: request.subject,
            engineProfile: request.engineProfile,
            jobId: snapshot.jobId,
            judgmentRevision,
            annotationPolicyRevision,
            generation,
            reportedReceipts,
            submittedReceipt: expectedReceipt,
          });
          if (!next) throw new MoveReviewRuntimeError('malformed-response', false);
          if (next.kind === 'awaiting-core' || next.kind === 'awaiting-evidence')
            reportedReceipts.push(expectedReceipt);
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
  retryProcessing = false,
): Promise<ReceivedJson> {
  let response: Response | undefined;
  for (let attempt = 0; attempt < 2; attempt++) {
    try {
      response = await fetch(url, {
        ...defaultInit,
        ...init,
        cache: 'no-store',
        signal,
        headers: { ...jsonHeader, ...xhrHeader, 'Content-Type': 'application/json' },
      });
      if (retryProcessing && response.status === 503 && attempt === 0) {
        const processing = await response.json().catch(() => undefined);
        if (
          !processing ||
          typeof processing !== 'object' ||
          (processing as Record<string, unknown>).schema_version !==
            'chesstory.position-commentary.job-error.v6' ||
          (processing as Record<string, unknown>).error !== 'engine_work_report_processing'
        )
          break;
        const retryAfterSeconds = Number(response.headers.get('Retry-After') ?? '0');
        await abortableDelay(Math.max(0, Math.min(1_000, retryAfterSeconds * 1_000)), signal);
        response = undefined;
        continue;
      }
      break;
    } catch (error) {
      if (signal.aborted) throw error;
      if (attempt === 1) throw new MoveReviewRuntimeError('offline', true);
    }
  }
  if (!response) throw new MoveReviewRuntimeError('offline', true);
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

function abortableDelay(milliseconds: number, signal: AbortSignal): Promise<void> {
  return new Promise((resolve, reject) => {
    const finish = (): void => {
      signal.removeEventListener('abort', abort);
      resolve();
    };
    const timer = setTimeout(finish, milliseconds);
    const abort = (): void => {
      clearTimeout(timer);
      signal.removeEventListener('abort', abort);
      reject(signal.reason);
    };
    signal.addEventListener('abort', abort, { once: true });
    if (signal.aborted) abort();
  });
}
