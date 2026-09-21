import { renderBoardPreview } from 'lib/view/boardPreview';
import { hl, type VNode } from 'lib/view/snabbdom';
import type { DrawShape } from '@lichess-org/chessground/draw';
import { parseFen } from 'chessops/fen';
import { parseSquare } from 'chessops/util';
import {
  commentarySceneFrame,
  questionComparisonDetail,
  pvComparisonDetail,
  targetResponseDetail,
  lineContinuationDetail,
  continuationComparisonDetail,
  type CommentaryCandidate,
  type CommentaryEvaluation,
  type CommentaryInquiryAnswer,
  type CommentaryPvComparisonDetail,
  type CommentaryResponseInquiry,
  type CommentaryTargetResponseDetail,
  type CommentaryTrackedPiece,
  type CommentaryContinuationInquiry,
  type CommentaryLineContinuationDetail,
  type CommentaryContinuationComparisonDetail,
} from '../moveReviewSelection';
import {
  formatMoveReviewPercent,
  formatMoveReviewPercentagePointChange,
  moveReviewVerdictCodeLabel,
  moveReviewObservations,
  moveReviewFrameForSubject,
  selectedMoveReviewCandidate,
  type MoveReviewCandidate,
  type MoveReviewCandidateRole,
  type MoveReviewCandidateReview,
  type MoveReviewCopy,
  type MoveReviewCore,
  type MoveReviewDrawClaim,
  type MoveReviewEvidence,
  type MoveReviewFrameSelection,
  type MoveReviewJobState,
  type MoveReviewLocale,
  type MoveReviewObservation,
  type MoveReviewObservations,
  type MoveReviewProof,
  type MoveReviewVerdictSymbol,
  type MoveReviewViewState,
  type ObservedPiece,
  type LegalResource,
  type MovementResource,
  type PawnState,
  type StructuralSubject,
  type PositionResources,
  type ObservedLineSummary,
} from '../moveReview';

type MoveReviewVerdictTone = 'neutral' | 'inaccuracy' | 'mistake' | 'blunder';

interface MoveReviewPanelActions {
  jumpToScene?(path: Tree.Path): void;
  selectCandidate(uci: Uci): void;
  toggleEvidence(): void;
  toggleProof(proofId: string): void;
  previewFrame(frame: MoveReviewFrameSelection): void;
  clearPreview(): void;
  pinFrame(frame: MoveReviewFrameSelection): void;
  clearPin(): void;
  retry(): void;
  addProof(proofId: string): void;
  viewAddedLine(proofId: string): void;
}

export interface MoveReviewPanelProps {
  job: MoveReviewJobState;
  view: MoveReviewViewState;
  copy: MoveReviewCopy;
  locale: MoveReviewLocale;
  orientation: Color;
  canWrite: boolean;
  liveEnginePaused: boolean;
  addedProofId?: string;
  actions: MoveReviewPanelActions;
}

export function renderMoveReview(props: MoveReviewPanelProps): VNode {
  const snapshot =
    props.job.kind === 'completed' || props.job.kind === 'position-action' ? props.job.snapshot : undefined;
  const scope = snapshot?.assessmentScope ?? props.job.assessmentScope;
  const assessmentState = snapshot?.assessmentState ?? props.job.assessmentState;
  const verification = snapshot?.verification ?? props.job.verification;
  const ko = props.locale === 'ko-KR';
  if (verification?.state === 'pending' || verification?.state === 'stopped')
    props = { ...props, copy: { ...props.copy,
      completed: verification.state === 'pending'
        ? (verification.reason === 'initial_verification'
          ? (ko ? '첫 평가 · 착수 검증 중' : 'Initial assessment · verification pending')
          : (ko ? '착수 재검증 중' : 'Move verification pending'))
        : (ko ? '착수 검증 미완료' : 'Move verification incomplete') } };
  // A restricted search selects a reference within its admitted pool, without a global rank claim.
  if (scope === 'investigation_pair' || assessmentState === 'needs_refresh')
    props = {
      ...props,
      copy: {
        ...props.copy,
        ...(assessmentState === 'needs_refresh'
          ? { completed: ko ? '추가 평가 미완료' : 'Assessment refresh incomplete' }
          : {}),
        best: ko ? '비교 기준 수' : 'Comparison reference',
        runnerUp: ko ? '함께 비교한 수' : 'Other compared move',
        selectedReference: ko
          ? '함께 비교한 수 중 기준 수를 두었습니다'
          : 'Played the reference within the compared moves',
      },
    };
  const titleId = 'analyse-move-review-title';
  return hl(
    'section.analyse__move-review',
    {
      attrs: {
        'aria-labelledby': titleId,
        'data-state': props.job.kind,
        lang: props.locale,
      },
    },
    [
      hl('header.move-review__header', [
        hl(`h2#${titleId}`, props.copy.title),
        hl(
          'span.move-review__status',
          { attrs: { role: 'status', 'aria-live': 'polite', 'aria-atomic': 'true' } },
          moveReviewStateLabel(props.job, props.copy),
        ),
      ]),
      props.liveEnginePaused && hl('div.move-review__live-engine-note', props.copy.liveEnginePaused),
      assessmentState === 'needs_refresh' &&
        hl(
          'p.move-review__assessment-notice',
          ko
            ? '실제 수·기존 기준 수·새 후보를 같은 검색에서 확인하는 절차가 끝나지 않았습니다. 아래는 이전 비교의 평가입니다.'
            : 'The played move, previous reference, and new candidate still need a matched search. The assessment below is from the earlier comparison.',
        ),
      assessmentState === 'needs_refresh' && snapshot && renderGradeRequest(props),
      scope === 'investigation_pair' &&
        hl(
          'p.move-review__assessment-scope',
          ko
            ? '추가 조사에서 함께 확인한 수들을 기준으로 한 평가입니다. 모든 합법수 중 최선이라는 뜻은 아닙니다.'
            : 'This assessment uses moves checked together during the additional investigation. It does not establish the best of all legal moves.',
        ),
      renderPanelContent(props),
      !snapshot &&
        props.job.assessment &&
        hl('section.move-review__retained-assessment', [
          hl('h3', ko ? '이전에 확보한 평가' : 'Previously established assessment'),
          props.job.assessment.kind === 'completed'
            ? renderReviewBody(props, props.job.assessment.evidence)
            : renderPositionAction(props.job.assessment.action, props),
        ]),
      renderCommentarySelection(props),
      renderInvestigations(props),
      renderObservations(props),
    ],
  );
}

function pvReadings(observations: MoveReviewObservations, root: string | undefined) {
  return (
    observations.selection?.candidates.flatMap(candidate => {
      const detail = pvComparisonDetail(candidate);
      return detail && candidate.rootOccurrenceId === root ? [{ candidate, detail }] : [];
    }) ?? []
  );
}

function matchingPrimary(
  observations: MoveReviewObservations,
  candidate: CommentaryCandidate,
  detail: CommentaryPvComparisonDetail,
) {
  return observations.selection?.candidates.find(
    primary =>
      primary.kind === 'comparison' &&
      primary.detail.type !== 'pv_comparison' &&
      primary.rootOccurrenceId === candidate.rootOccurrenceId &&
      primary.detail.reference_line_id === detail.answer.reference_line_id &&
      primary.detail.candidate_line_id === detail.answer.alternative_line_id,
  );
}

function pvReadingId(candidate: CommentaryCandidate): string {
  return `move-review-pv-${candidate.id}`;
}

function comparisonEvaluation(value: CommentaryEvaluation, props: MoveReviewPanelProps): string {
  if (value.kind === 'exact_automatic_terminal')
    return value.terminal.kind === 'checkmate'
      ? `${props.copy.terminalLabels.checkmate} · ${props.copy.colorLabels[value.terminal.winner]}`
      : props.copy.terminalLabels[value.terminal.kind];
  const score = value.white_score;
  const text =
    score.kind === 'cp'
      ? `${score.value >= 0 ? '+' : ''}${(score.value / 100).toFixed(2)}`
      : `${score.value > 0 ? '+' : '-'}M${Math.abs(score.value)}`;
  return props.locale === 'ko-KR'
    ? `백 기준 ${text} · 깊이 ${value.depth}`
    : `${text} from White’s side · depth ${value.depth}`;
}

function renderComparisonPaths(
  answer: Pick<
    CommentaryInquiryAnswer,
    'reference_line_id' | 'alternative_line_id' | 'reference_evaluation' | 'alternative_evaluation'
  >,
  observations: MoveReviewObservations,
  props: MoveReviewPanelProps,
  ply = 1,
): VNode[] {
  return (
    [
      [answer.reference_line_id, answer.reference_evaluation],
      [answer.alternative_line_id, answer.alternative_evaluation],
    ] as const
  ).map(([lineId, value]) => {
    const path = observations.paths.find(path => path.id === lineId)!;
    const label = path.moves
      .slice(0, ply)
      .map(move => move.label)
      .join(' ');
    return hl('div', [
      hl('p', `${label} · ${comparisonEvaluation(value, props)}`),
      hl(
        'button.button.button-thin',
        {
          attrs: { type: 'button' },
          on: { click: () => showObservationFrame(props, { proofId: lineId, ply, phase: 'after' }) },
        },
        `${props.locale === 'ko-KR' ? '비교 수순 보기' : 'Inspect the comparison line'} · ${label}`,
      ),
      renderLineSummary(path, path.lineSummary, {
        ...props,
        actions: { ...props.actions, pinFrame: frame => showObservationFrame(props, frame) },
      }),
    ]);
  });
}

function renderPvReading(
  candidate: CommentaryCandidate,
  detail: CommentaryPvComparisonDetail,
  observations: MoveReviewObservations,
  current: MoveReviewFrameSelection | null | undefined,
  props: MoveReviewPanelProps,
): VNode {
  const ko = props.locale === 'ko-KR';
  const answer = detail.answer;
  const label = (id: string) => observations.paths.find(path => path.id === id)!.moves[0].label;
  const primary = matchingPrimary(observations, candidate, detail);
  const material = {
    equal: ko
      ? '잡히거나 승격한 기물의 종류별 수는 같습니다.'
      : 'The captured or promoted piece-role counts are the same.',
    different: ko
      ? '잡히거나 승격한 기물의 종류별 수가 다릅니다.'
      : 'The captured or promoted piece-role counts differ.',
    unavailable: ko
      ? '두 수순의 기물 교환 기록을 아직 모두 확보하지 못했습니다.'
      : 'The material records for both lines are not fully available.',
  };
  const events = {
    same: ko
      ? '기록된 포획·승격 사건의 시점, 기물과 칸은 같습니다.'
      : 'The recorded capture and promotion events match in timing, pieces and squares.',
    different: ko
      ? '기록된 포획·승격 사건의 시점, 기물 또는 칸이 다릅니다.'
      : 'The recorded capture and promotion events differ in timing, pieces or squares.',
    unavailable: ko
      ? '포획·승격 사건의 순서를 대조할 기록이 부족합니다.'
      : 'There is not enough evidence to compare the capture and promotion sequences.',
  };
  const root = {
    same: ko
      ? '첫 수에 기록된 전개·통제 변화는 같습니다. 같은 계획이라는 뜻은 아닙니다.'
      : 'The recorded development and control changes at the first moves match. This does not establish the same plan.',
    different: ko
      ? '첫 수에 기록된 전개·통제 변화가 다릅니다. 아래에서 해당 관측을 펼쳐 볼 수 있습니다.'
      : 'The first moves have different recorded development or control changes. Their observations can be expanded below.',
    unavailable: ko
      ? '첫 수의 전개·통제 변화를 대조할 관측이 부족합니다.'
      : 'There are not enough observations to compare development and control at the first moves.',
  };
  return hl(
    'article.move-review__pv-reading',
    { key: candidate.id, attrs: { id: pvReadingId(candidate), tabindex: -1 } },
    [
      hl('h4', `${label(answer.reference_line_id)} ↔ ${label(answer.alternative_line_id)}`),
      continuationLinks(candidate.detail.continuation_candidate_ids, props),
      continuationLinks(candidate.detail.continuation_comparison_ids, props),
      hl(
        'p.move-review__pv-quality',
        detail.quality_band === 'close'
          ? ko
            ? '현재 평가 기준에서 가까운 두 수입니다. 작은 검색값 차이만으로 체스적 우열을 확정하지 않습니다.'
            : 'The moves are close under the current assessment policy. A small search-score difference does not establish chess superiority.'
          : ko
            ? '현재 평가 기준에서 두 수의 평가가 나뉩니다. 아래는 차이와 함께 관측된 사실입니다.'
            : 'The moves are separated under the current assessment policy. The observations below accompany that difference.',
      ),
      hl('ul', [
        hl('li', material[detail.material_relation]),
        hl('li', events[detail.event_sequence_relation]),
        hl('li', root[detail.root_observation_relation]),
      ]),
      primary?.state === 'ready' && typeof primary.detail.text === 'string' && hl('p', primary.detail.text),
      candidate.state !== 'ready' &&
        hl(
          'p',
          ko
            ? '등록한 관측 항목에서는 차이를 아직 찾지 못했습니다. 두 수가 완전히 동등하다는 뜻은 아닙니다.'
            : 'No difference has been established in the recorded facets. This does not establish complete equivalence.',
        ),
      ...renderComparisonPaths(answer, observations, props),
      current &&
        detail.root_participants.length > 0 &&
        hl(
          'div',
          detail.root_participants.map(piece =>
            hl(
              'button.button.button-thin',
              {
                attrs: { type: 'button' },
                on: {
                  click: () =>
                    showObservationFrame(props, { ...current, phase: 'before', inspectSquare: piece.square }),
                },
              },
              `${ko ? '원래 위치에서 살펴보기' : 'Inspect at the original position'} · ${pieceText(piece, props.copy)}`,
            ),
          ),
        ),
    ],
  );
}

function renderInvestigations(props: MoveReviewPanelProps): VNode | undefined {
  const observations = moveReviewObservations(props.job);
  if (!observations?.selection || props.job.kind === 'idle') return;
  const subject =
    props.job.kind === 'completed' || props.job.kind === 'position-action'
      ? props.job.snapshot.subject
      : props.job.subject;
  const current =
    props.job.observationFrame !== undefined
      ? props.job.observationFrame
      : moveReviewFrameForSubject(observations, subject, subject);
  const root =
    current && observations.paths.find(path => path.id === current.proofId)?.occurrenceIds[current.ply - 1];
  const selected =
    observations.selection.inquiries?.filter(inquiry => inquiry.root_occurrence_id === root) ?? [];
  const responses =
    observations.selection.response_inquiries?.filter(inquiry => inquiry.root_occurrence_id === root) ?? [];
  const readings = pvReadings(observations, root || undefined);
  const continuations =
    observations.selection.continuation_inquiries?.filter(inquiry => inquiry.root_occurrence_id === root) ??
    [];
  const profiles = observations.selection.candidates.filter(
    candidate => candidate.rootOccurrenceId === root && lineContinuationDetail(candidate),
  );
  const continuationPairs = observations.selection.candidates.filter(
    candidate => candidate.rootOccurrenceId === root && continuationComparisonDetail(candidate),
  );
  // Only current candidates expose retained questions; an old outcome alone cannot revive one.
  const pendingComparisons = observations.selection.candidates.flatMap(candidate => {
    const detail = questionComparisonDetail(candidate);
    return detail && candidate.rootOccurrenceId === root && candidate.state === 'partial'
      ? [{ candidate, detail }]
      : [];
  });
  if (
    !selected.length &&
    !readings.length &&
    !responses.length &&
    !continuations.length &&
    !profiles.length &&
    !continuationPairs.length &&
    !pendingComparisons.length
  )
    return;
  const ko = props.locale === 'ko-KR';
  const label = (move: Uci) =>
    observations.paths.find(path => path.startFen === subject.before.fen && path.moves[0]?.uci === move)
      ?.moves[0]?.label ?? move;
  const links = (ids: string[] | undefined, title: string): VNode | undefined => {
    const linked = readings.filter(reading => ids?.includes(reading.candidate.id));
    return linked.length
      ? hl('div.move-review__inquiry-reading-links', [
          hl('span', title),
          ...linked.map(({ candidate, detail }) =>
            hl(
              'button.button.button-thin',
              {
                attrs: { type: 'button' },
                on: {
                  click: () => {
                    const target = document.getElementById(pvReadingId(candidate));
                    target?.scrollIntoView({ block: 'nearest' });
                    target?.focus({ preventScroll: true });
                  },
                },
              },
              `${observations.paths.find(path => path.id === detail.answer.reference_line_id)!.moves[0].label} ↔ ${observations.paths.find(path => path.id === detail.answer.alternative_line_id)!.moves[0].label}`,
            ),
          ),
        ])
      : undefined;
  };
  const reasonLabels = {
    engine_alternative: ko
      ? '기존 엔진 검색에서 나온 다른 후보입니다.'
      : 'Another candidate from the earlier engine search.',
    capture_option: ko
      ? '이 자리에서 잡을 수 있는 기물이 있습니다.'
      : 'A piece can be captured from this position.',
    same_destination: ko
      ? '다른 기물도 같은 칸으로 갈 수 있습니다.'
      : 'Another piece can move to the same square.',
    same_actor_choice: ko ? '같은 기물이 둘 수 있는 다른 수입니다.' : 'Another legal move by the same piece.',
    target_relocation: ko ? '공격받는 기물을 옮길 수 있습니다.' : 'The attacked piece can move.',
    attacker_capture: ko ? '공격하는 기물을 잡을 수 있습니다.' : 'The attacking piece can be captured.',
    target_support: ko
      ? '다른 기물이 공격받는 기물의 칸을 통제하는 수입니다.'
      : 'Another piece can move to control the attacked piece’s square.',
  };
  const stateLabels = {
    deferred: ko ? '아직 비교하지 않음' : 'Not yet compared',
    awaiting_engine: ko ? '두 수를 비교하는 중' : 'Comparing the two moves',
    answered: ko ? '두 수의 평가 비교를 확보함' : 'Evaluation comparison obtained',
    stopped: ko ? '추가 비교가 중단됨' : 'Additional comparison stopped',
    observed: ko ? '기존 관측 확보' : 'Existing observations obtained',
  };
  const sourceLabels = {
    engine_pv: ko ? '앞서 계산한 수순에서 찾음' : 'Found in an earlier calculated line',
    legal_root: ko ? '이 위치의 합법수에서 찾음' : 'Found among legal moves in this position',
    attacked_target: ko ? '공격받는 대상에서 질문을 만듦' : 'Question prompted by an attacked piece',
  };
  const sharedOccurrences = [
    ...selected.flatMap(inquiry => [
      ...inquiry.anchor_occurrence_ids,
      ...(inquiry.answer
        ? [inquiry.answer.reference_occurrence_id, inquiry.answer.alternative_occurrence_id]
        : []),
    ]),
    ...readings.flatMap(({ detail }) => detail.root_difference_occurrence_ids),
    ...responses.flatMap(inquiry => [
      inquiry.root_move_occurrence_id,
      ...(inquiry.answer
        ? [inquiry.answer.reference_occurrence_id, inquiry.answer.alternative_occurrence_id]
        : []),
    ]),
  ];
  const pendingQuestionTexts = new Set<string>();
  return hl('section.move-review__investigations', [
    hl('h3', ko ? '읽은 비교와 남은 질문' : 'Read comparisons and remaining questions'),
    ...readings.map(({ candidate, detail }) =>
      renderPvReading(candidate, detail, observations, current, props),
    ),
    readings.length > 0 &&
      hl(
        'p.move-review__pv-limit',
        ko
          ? '확인한 범위는 이 유한 수순의 기록된 항목입니다. 강제 결과나 평가 차이의 원인까지 입증한 것은 아닙니다.'
          : 'These findings cover the recorded facets of these finite lines. They do not establish a forced result or the cause of the evaluation difference.',
      ),
    ...selected.map(inquiry => {
      const hasReading = readings.some(reading =>
        inquiry.explanation_candidate_ids?.includes(reading.candidate.id),
      );
      return hl('article.move-review__inquiry', { key: inquiry.id }, [
        hl('h4', `${label(inquiry.reference_move)} ↔ ${label(inquiry.alternative_move)}`),
        hl(
          'p.move-review__inquiry-state',
          inquiry.state === 'awaiting_engine' &&
            (props.job.kind === 'fault' || props.job.kind === 'abstained')
            ? ko
              ? '추가 비교 결과를 받기 전에 검토가 중단됨'
              : 'Review stopped before the additional comparison result arrived'
            : stateLabels[inquiry.state],
        ),
        links(
          inquiry.ordering_candidate_ids,
          ko ? '조사 순서를 정한 근거' : 'Evidence used to order this question',
        ),
        links(
          inquiry.explanation_candidate_ids,
          ko ? '이 답에서 읽은 비교' : 'Comparison read from this answer',
        ),
        inquiry.next_evidence &&
          hl(
            'p.move-review__next-evidence',
            inquiry.next_evidence === 'compare_roots'
              ? ko
                ? '두 수의 평가 비교가 더 필요합니다. 이것은 새 계산이 예약되었다는 뜻은 아닙니다.'
                : 'A further evaluation comparison is needed. This does not mean another search is scheduled.'
              : inquiry.next_evidence === 'inspect_existing'
                ? ko
                  ? '기존 수순에서 확인한 관측 차이를 위 비교에서 살펴볼 수 있습니다.'
                  : 'The observed differences in the existing lines can be inspected in the comparison above.'
                : ko
                  ? '이 질문의 차이를 설명할 근거는 아직 미확정입니다.'
                  : 'Evidence explaining the difference in this question remains unresolved.',
          ),
        hl('details.move-review__inquiry-reasons', [
          hl('summary', ko ? '질문을 만든 관측' : 'Observations behind the question'),
          ...inquiry.reasons.map(reason =>
            hl('div.move-review__inquiry-reason', [
              hl('p', reasonLabels[reason.kind]),
              reason.source_name && hl('p', sourceLabels[reason.source_name]),
              reason.target &&
                hl(
                  'p',
                  `${ko ? '공격받는 대상' : 'Attacked target'}: ${pieceText(reason.target, props.copy)}`,
                ),
              reason.target &&
                current &&
                hl(
                  'button.button.button-thin',
                  {
                    attrs: { type: 'button' },
                    on: {
                      click: () =>
                        showObservationFrame(props, {
                          ...current,
                          phase: 'before',
                          inspectSquare: reason.target!.square,
                        }),
                    },
                  },
                  ko ? '대상이 받는 공격 살펴보기' : 'Inspect attacks on the target',
                ),
              current &&
                hl(
                  'button.button.button-thin',
                  {
                    attrs: { type: 'button' },
                    on: {
                      click: () =>
                        showObservationFrame(props, {
                          ...current,
                          phase: 'before',
                          inspectSquare: reason.actor.square,
                        }),
                    },
                  },
                  `${ko ? '질문의 근거 보기' : 'Inspect the basis'} · ${props.copy.roleLabels[reason.actor.role]} ${reason.actor.square}`,
                ),
              reason.source_line_id &&
                hl(
                  'button.button.button-thin',
                  {
                    attrs: { type: 'button' },
                    on: {
                      click: () =>
                        showObservationFrame(props, {
                          proofId: reason.source_line_id!,
                          ply: 1,
                          phase: 'after',
                        }),
                    },
                  },
                  ko ? '질문을 만든 기존 수순 보기' : 'Inspect the earlier line behind this question',
                ),
            ]),
          ),
        ]),
        inquiry.answer &&
          !hasReading &&
          hl('div.move-review__inquiry-answer', [
            ...renderComparisonPaths(inquiry.answer, observations, props),
            hl(
              'p',
              ko
                ? '평가 비교는 확보했지만 이 두 수의 차이에 연결된 관측 읽기는 아직 없습니다.'
                : 'The evaluation comparison is available, but no observation reading is linked to this pair yet.',
            ),
          ]),
        inquiry.assessment_effect === 'needs_refresh' &&
          hl(
            'p',
            ko
              ? '실제 수·기존 기준 수·새 후보를 같은 검색에서 확인하지 못해 이전 착수 평가는 아직 갱신되지 않았습니다.'
              : 'A matched search of the played move, previous reference, and new candidate is unfinished, so the earlier move assessment has not been refreshed.',
          ),
        inquiry.assessment_effect === 'refreshed' &&
          hl(
            'p',
            ko
              ? '실제 수와 새 대안을 다시 비교하여 착수 평가에 반영했습니다.'
              : 'A further comparison of the played move with the new alternative has updated the move assessment.',
          ),
        inquiry.stop_reason && hl('p', investigationStopText(inquiry.stop_reason, props)),
      ]);
    }),
    ...responses.flatMap((inquiry, index) =>
      inquiry.related_inquiry_ids.flatMap(id => {
        const peerIndex = responses.findIndex(peer => peer.id === id);
        return peerIndex > index
          ? [renderRelatedResponses([inquiry, responses[peerIndex]], observations, props)]
          : [];
      }),
    ),
    ...responses.map(inquiry =>
      renderResponseInquiry(inquiry, observations, props, stateLabels[inquiry.state]),
    ),
    ...continuations.map(inquiry => renderContinuationInquiry(inquiry, observations, props)),
    ...profiles.map(candidate =>
      renderContinuationProfile(candidate, lineContinuationDetail(candidate)!, observations, props),
    ),
    ...continuationPairs.map(candidate =>
      renderContinuationComparison(candidate, continuationComparisonDetail(candidate)!, observations, props),
    ),
    ...pendingComparisons.map(({ candidate, detail }) => {
      const selection = observations.selection!;
      const questions = selection.proposals ?? selection.questionOutcomes ?? [];
      const linked = detail.question_refs.flatMap(ref => {
        const question = questions.find(question => question.id === ref.proposal_id);
        if (!question) return [];
        const conditions = 'requirements' in question ? question.requirements : question.conditions;
        const condition = conditions.find(condition => condition.id === ref.requirement_id);
        return condition ? [{ question, condition }] : [];
      });
      const texts = linked.flatMap(({ question }) => question.question_text ? [question.question_text] : [])
        .filter(text => {
          if (pendingQuestionTexts.has(text)) return false;
          pendingQuestionTexts.add(text);
          return true;
        });
      const reasons = [...new Set(linked.flatMap(({ condition }) =>
        condition.stop_reason ? [condition.stop_reason] : [],
      ))];
      if (!reasons.length && detail.stop_reason) reasons.push(detail.stop_reason);
      const ply = detail.prefix_moves_uci.length;
      const moveLabel = (move: Uci) => candidate.lineIds.flatMap(id => {
        const path = observations.paths.find(path => path.id === id);
        return path?.moves[ply]?.uci === move ? [path.moves[ply].label] : [];
      })[0] ?? move;
      return hl('article.move-review__inquiry', { key: candidate.id }, [
        hl('h4', `${moveLabel(detail.reference_move)} ↔ ${moveLabel(detail.alternative_move)}`),
        ...texts.map(text => hl('p', text)),
        hl(
          'p.move-review__inquiry-state',
          props.job.kind === 'loading' && linked.some(({ condition }) => condition.state === 'awaiting_engine')
            ? stateLabels.awaiting_engine
            : ko ? '아직 답하지 못한 비교 질문' : 'Comparison question still unanswered',
        ),
        ...reasons.map(reason => hl('p', investigationStopText(reason, props))),
      ]);
    }),
    sharedOccurrences.length > 0 &&
      hl('section.move-review__shared-observations', [
        hl('h4', ko ? '함께 참조한 수의 관측' : 'Shared move observations'),
        ...renderInquiryObservations(observations, sharedOccurrences, props),
      ]),
  ]);
}

function renderContinuationComparison(
  candidate: CommentaryCandidate,
  detail: CommentaryContinuationComparisonDetail,
  observations: MoveReviewObservations,
  props: MoveReviewPanelProps,
): VNode {
  const ko = props.locale === 'ko-KR';
  return hl(
    'article.move-review__continuation-comparison',
    { key: candidate.id, attrs: { id: `continuation-${candidate.id}`, tabindex: -1 } },
    [
      hl('h4', ko ? '두 수순에서 확인한 차이' : 'Observed differences between the two lines'),
      ...detail.differences.map((difference, index) => {
        if (difference.kind === 'recapturer_support')
          return hl('div.move-review__recapturer-support', { key: `${candidate.id}:${index}` }, [
            hl(
              'p',
              `${pieceText(difference.original_subject, props.copy)} → ${pieceText(difference.original_target, props.copy)}: ${ko ? '같은 원 기물의 지원이 두 재포획 선택에서 다르게 변합니다.' : 'Support by the same original piece changes differently across the two recapture choices.'}`,
            ),
            ...[difference.left, difference.right].map((side, index) =>
              hl('div', [
                hl(
                  'p',
                  `${index === 0 ? (ko ? '왼쪽 수순' : 'Left line') : ko ? '오른쪽 수순' : 'Right line'}: ${continuationSupportLabel(side.before.support.state, ko)} → ${continuationSupportLabel(side.after.support.state, ko)}`,
                ),
                ...[side.before, side.after].map((point, at) =>
                  hl(
                    'button.button.button-thin',
                    {
                      attrs: { type: 'button' },
                      on: {
                        click: () =>
                          showObservationFrame(props, {
                            proofId: point.line_id,
                            ply: point.ply_count,
                            phase: 'after',
                          }),
                      },
                    },
                    at === 0 ? (ko ? '재포획 전' : 'Before recapture') : ko ? '재포획 후' : 'After recapture',
                  ),
                ),
              ]),
            ),
          ]);
        const text =
          difference.kind === 'loss_followup'
            ? ko
              ? '같은 포획 이후 관측한 후속 결과가 다릅니다.'
              : 'The observed follow-ups after the same capture differ.'
            : difference.kind === 'capture_timing'
              ? ko
                ? '같은 기물이 같은 대상을 잡지만 포획 시점이 다릅니다.'
                : 'The same original piece captures the same target at different times.'
              : difference.kind === 'legal_capture_availability'
                ? difference.matched_support_comparison_ids
                  ? ko
                    ? '대체 기물로 재포획한 직후, 한쪽 위치에만 이 포획을 할 합법수가 있습니다.'
                    : 'After the alternative recaptures, this legal capture is available in only one position.'
                  : ko
                    ? '같은 포획 이후, 한쪽 위치에만 이 포획을 할 합법수가 있습니다.'
                    : 'After the matched capture, this legal capture is available in only one position.'
                : difference.relation === 'preserved_with_different_provider'
                  ? ko
                    ? '다른 기물이 같은 원래 대상에 대한 지원을 이어갑니다.'
                    : 'A different piece preserves support for the same original target.'
                  : ko
                    ? '같은 원래 대상에 대한 지원 상태가 다릅니다.'
                    : 'Support for the same original target differs.';
        const subject =
          'original_actor' in difference ? difference.original_actor : difference.original_subject;
        const target =
          'original_victim' in difference ? difference.original_victim : difference.original_target;
        return hl('div', { key: `${candidate.id}:${index}` }, [
          hl('p', `${pieceText(subject, props.copy)} → ${pieceText(target, props.copy)}: ${text}`),
          'application' in difference.left &&
            difference.left.application &&
            hl(
              'p',
              ko
                ? '왕이 같은 대상에 접근한 위치에서 지원 상태와 왕의 합법 포획 가능 여부가 함께 달라집니다.'
                : 'At positions where the king contacts the same target, support and legal king-capture availability differ together.',
            ),
          ...(difference.kind === 'loss_followup' ? (difference.shared_attackers ?? []) : []).map(shared => {
            const path = observations.paths.find(path => path.id === difference.left.line_id)!;
            return hl('div.move-review__shared-attacker', [
              hl(
                'p',
                ko
                  ? `분기 위치의 ${pieceText(shared.actor, props.copy)}가 ${pieceText(shared.left.target, props.copy)}와 ${pieceText(shared.right.target, props.copy)}를 모두 통제하며, 두 수순에서 각각의 포획이 관측됩니다.`
                  : `${pieceText(shared.actor, props.copy)} controls both ${pieceText(shared.left.target, props.copy)} and ${pieceText(shared.right.target, props.copy)} at the branch point; the two lines show the respective captures.`,
              ),
              hl(
                'button.button.button-thin',
                {
                  attrs: { type: 'button' },
                  on: {
                    click: () =>
                      showObservationFrame(props, {
                        proofId: path.id,
                        ply: shared.ply_count,
                        phase: 'after',
                      }),
                  },
                },
                ko ? '두 대상의 통제 위치 보기' : 'Inspect control of both targets',
              ),
            ]);
          }),
          ...[difference.left, difference.right].map((point, side) => {
            const path = observations.paths.find(path => path.id === point.line_id)!;
            const detail =
              'support' in point
                ? `${pieceText(point.original_provider, props.copy)} (${ko ? '원래 기물' : 'original piece'}): ${continuationSupportLabel(point.support.state, ko)}`
                : 'loss_event_id' in point
                  ? `${point.observed_followup_plies}${ko ? '수의 후속 기록' : ' follow-up plies'} · ${ko ? '포획자 제거' : 'Capturer removal'}: ${point.capturer_removal_state === 'observed' ? (ko ? '관측됨' : 'observed') : ko ? '수순 끝까지 미관측' : 'not observed through line end'} · ${ko ? '이 손실에 연결된 다른 대상 포획' : 'Other-target gains linked to this loss'}: ${point.other_target_gain_state === 'observed' ? (ko ? '관측됨' : 'observed') : ko ? '수순 끝까지 연결된 관측 없음' : 'no linked observation through line end'}`
                  : undefined;
            return hl('div', [
              detail && hl('p', detail),
              'direct_recapture' in point &&
                point.direct_recapture &&
                hl(
                  'p.move-review__direct-recapture',
                  ko
                    ? '이 수순에서는 지원하던 기물이 뒤에 실제로 되잡습니다.'
                    : 'In this line, the supporting piece later makes the recapture.',
                ),
              'application' in point &&
                point.application &&
                hl(
                  'p.move-review__king-capture',
                  `${pieceText(point.application.attacker, props.copy)} → ${pieceText(point.application.target, props.copy)}: ${
                    point.application.moves_uci.length
                      ? ko
                        ? '이 위치에서 왕으로 잡는 합법수 있음'
                        : 'the king has a legal capture at this position'
                      : ko
                        ? '이 위치에서 왕으로 잡는 합법수 없음'
                        : 'the king has no legal capture at this position'
                  }`,
                ),
              hl(
                'button.button.button-thin',
                {
                  attrs: { type: 'button' },
                  on: {
                    click: () =>
                      showObservationFrame(props, { proofId: path.id, ply: point.ply_count, phase: 'after' }),
                  },
                },
                `${side === 0 ? (ko ? '왼쪽 수순' : 'Left line') : ko ? '오른쪽 수순' : 'Right line'}: ${path.moves[point.ply_count - 1].label}`,
              ),
            ]);
          }),
        ]);
      }),
      continuationLinks(detail.continuation_candidate_ids, props),
      hl(
        'p',
        ko
          ? '명시한 위치에서의 관측 차이이며, 최선 응수나 충분한 보상을 입증하지 않습니다.'
          : 'These are differences at the named positions, not proof of best replies or sufficient compensation.',
      ),
    ],
  );
}

function continuationLinks(ids: unknown, props: MoveReviewPanelProps): VNode | undefined {
  if (!Array.isArray(ids) || !ids.length) return;
  return hl('div.move-review__continuation-links', [
    hl('span', props.locale === 'ko-KR' ? '연결된 후속 관측' : 'Linked continuation observations'),
    ...ids.map((id, index) =>
      hl(
        'button.button.button-thin',
        {
          attrs: { type: 'button' },
          on: {
            click: () => {
              const target = document.getElementById(`continuation-${id}`);
              target?.scrollIntoView({ block: 'nearest' });
              target?.focus({ preventScroll: true });
            },
          },
        },
        `${props.locale === 'ko-KR' ? '수순' : 'Line'} ${index + 1}`,
      ),
    ),
  ]);
}

function renderContinuationInquiry(
  inquiry: CommentaryContinuationInquiry,
  observations: MoveReviewObservations,
  props: MoveReviewPanelProps,
): VNode {
  const ko = props.locale === 'ko-KR';
  const path = observations.paths.find(path => inquiry.source_line_ids.includes(path.id))!;
  const ply = inquiry.prefix_moves_uci.length;
  const prefix = path.moves
    .slice(0, ply)
    .map(move => move.label)
    .join(' ');
  const state = {
    deferred: ko ? '후속 선택지를 아직 조사하지 않음' : 'Continuation choices have not been surveyed',
    awaiting_engine: ko ? '이 위치의 합법수를 조사 중' : 'Surveying legal moves at this position',
    surveyed: ko
      ? '전체 합법 범위를 검색함 · 설명 완결 여부는 별도'
      : 'The full legal scope was searched; explanation completeness is separate',
    forced: ko
      ? '이 위치에서 합법수는 하나 · 이후 수순 전체의 강제성을 뜻하지 않음'
      : 'One legal move here; this does not establish a forced continuation',
    stopped: ko ? '후속 조사 중단' : 'Continuation investigation stopped',
    observed: ko
      ? '질문에 해당하는 후속 응수 또는 다른 결과를 관측함 · 전체 응수 검색은 별도'
      : 'A continuation addressing this question or another outcome was observed; a full reply survey is separate',
  }[inquiry.state];
  return hl(
    'article.move-review__continuation-inquiry',
    {
      key: inquiry.id,
      attrs: { id: `continuation-question-${inquiry.id}`, tabindex: -1 },
    },
    [
      hl('h4', `${prefix} ${ko ? '이후' : 'onward'}`),
      hl(
        'p',
        inquiry.state === 'awaiting_engine' && (props.job.kind === 'fault' || props.job.kind === 'abstained')
          ? ko
            ? '후속 검색 결과를 받기 전에 검토가 중단됨'
            : 'Review stopped before the continuation search arrived'
          : state,
      ),
      hl(
        'p',
        inquiry.kinds
          .map(kind =>
            kind === 'root_capture_response'
              ? ko
                ? '첫 포획 뒤 상대의 선택을 조사함'
                : 'Investigating the opponent’s choices after the first capture'
              : kind === 'check_response_not_observed'
                ? ko
                  ? '체크를 받은 쪽의 정확한 후속 응수를 확인하는 질문'
                  : 'A question about the exact continuation in response to a check'
                : kind === 'delayed_capturer_removal'
                  ? ko
                    ? '앞서 잡은 기물이 나중에 제거되는 수순이 관측됨'
                    : 'An earlier capturer is removed later in an observed line'
                  : kind === 'continuation_not_observed'
                    ? ko
                      ? '같은 검색의 다른 응수에서 관측한 후속 포획을 이 응수에서도 조사함'
                      : 'Investigating here a follow-up capture observed after another reply in the same search'
                    : kind === 'support_application_not_observed'
                      ? ko
                        ? '다른 재포획 수순에서 쓰인 지원 기능을 이 대안의 정확한 후속 수순에서도 조사함'
                        : 'Investigating this recapture’s exact continuation for a support use observed after another recapture'
                      : kind === 'capture_exposure_response'
                        ? ko
                          ? '가로막던 기물이 비켜 상대의 합법적인 포획이 열린 위치의 응수를 조사함'
                          : 'Investigating replies after a blocker vacates and a legal capture opens'
                        : kind === 'exposure_prevention_choice'
                          ? ko
                            ? '열린 포획이 실제로 이어져 손실이 남았으므로 그 직전 선택을 조사함'
                            : 'An observed exposed capture leaves a material loss, prompting investigation of the preceding choice'
                          : kind === 'capturer_reply'
                            ? ko
                              ? '앞서 포획한 기물에 상대의 통제가 닿는 위치에서 현재 쪽의 응수를 조사함'
                              : 'Investigating choices where an earlier capturer is under opposing control'
                            : kind === 'replacement_function_choice'
                              ? ko
                                ? '재포획할 기물이 여러 개이며, 그 뒤 지원 기능이 실제로 쓰인 수순이 관측되어 선택을 조사함'
                                : 'Several pieces can recapture; a later observed use of support prompts investigation of that choice'
                              : kind === 'material_followup'
                                ? ko
                                  ? '손해가 남은 수순 끝에 합법적인 포획 또는 체크 후보가 있어 그 위치의 전체 응수를 조사함'
                                  : 'A legal capture or check at this line’s end prompts a search of all choices for the side still down material'
                                : ko
                                  ? '손실 이후 다른 대상을 잡는 수순이 관측됨'
                                  : 'Another target is captured after a material loss',
          )
          .join(' · '),
      ),
      hl(
        'button.button.button-thin',
        {
          attrs: { type: 'button' },
          on: { click: () => showObservationFrame(props, { proofId: path.id, ply, phase: 'after' }) },
        },
        ko ? '조사 위치 보기' : 'Inspect this choice point',
      ),
      hl(
        'p',
        `${ko ? '관측한 다음 수' : 'Observed next moves'}: ${inquiry.observed_moves.join(', ') || (ko ? '없음' : 'none')}`,
      ),
      continuationLinks(inquiry.explanation_candidate_ids, props),
      inquiry.route_challenge &&
        hl('div.move-review__route-challenge', [
          hl(
            'p',
            {
              not_returned: ko
                ? '필요한 응수 후보 중 아직 반환되지 않은 것이 있음'
                : 'Some required reply candidates have not been returned',
              awaiting_engine: ko
                ? '누락된 응수 묶음 전체와 기준 응수를 함께 조사 중'
                : 'Searching the full missing reply classes together with the reference reply',
              sampled: ko
                ? '응수 표본 확보 · 같은 검색의 기준 응수 대조 여부는 아래에 표시'
                : 'Reply samples obtained; same-search reference comparisons are shown below',
              compared: ko
                ? '각 응수 묶음과 기준 응수의 비교 수순을 같은 검색에서 확보'
                : 'Each reply class has comparison lines with the reference from the same search',
              stopped: ko ? '응수 보완 조사 중단' : 'Reply follow-up search stopped',
            }[inquiry.route_challenge.state],
          ),
          ...inquiry.route_challenge.move_classes.map(group =>
            hl('div', [
              hl(
                'p',
                `${group.kind === 'replacement_recapture' ? (ko ? '재포획 후보' : 'Recapture candidate') : ko ? '지정 응수' : 'Specified replies'}: ${group.moves_uci.join(', ')} · ${group.sampled_work_ids.length ? (ko ? '표본 반환됨' : 'sample returned') : group.kind === 'replacement_recapture' ? (ko ? '재포획 대안 미관측' : 'recapture alternative not observed') : ko ? '표본 없음' : 'no sample'} · ${group.comparison ? (ko ? '같은 검색의 대조 있음' : 'same-search comparison available') : group.moves_uci.includes(inquiry.route_challenge!.reference_move) ? (ko ? '기준 응수 자체가 포함됨' : 'includes the reference itself') : ko ? '같은 검색의 대조 미확정' : 'same-search comparison unresolved'}`,
              ),
              ...(group.comparison
                ? [group.comparison.reference_line_id, group.comparison.challenge_line_id]
                : []
              ).map(id => {
                const branch = observations.paths.find(path => path.id === id)!;
                return hl(
                  'button.button.button-thin',
                  {
                    attrs: { type: 'button' },
                    on: {
                      click: () =>
                        showObservationFrame(props, {
                          proofId: id,
                          ply: branch.moves.length,
                          phase: 'after',
                        }),
                    },
                  },
                  branch.moves
                    .slice(ply)
                    .map(move => move.label)
                    .join(' '),
                );
              }),
            ]),
          ),
          hl(
            'p',
            ko
              ? '표본의 비교이며, 회수 불가능이나 충분한 보상을 확정하지 않습니다.'
              : 'These are sample comparisons; they do not establish impossible recovery or sufficient compensation.',
          ),
          inquiry.route_challenge.stop_reason &&
            hl('p', investigationStopText(inquiry.route_challenge.stop_reason, props)),
        ]),
      !!inquiry.unresolved_source_line_ids?.length &&
        hl('div.move-review__continuation-unresolved', [
          hl('p', ko ? '후속 결과의 확인이 남은 수순' : 'Lines whose follow-up remains unresolved'),
          ...inquiry.unresolved_source_line_ids.map(id => {
            const unresolved = observations.paths.find(path => path.id === id)!;
            return hl(
              'button.button.button-thin',
              {
                attrs: { type: 'button' },
                on: {
                  click: () =>
                    showObservationFrame(props, {
                      proofId: id,
                      ply: unresolved.moves.length,
                      phase: 'after',
                    }),
                },
              },
              unresolved.moves.map(move => move.label).join(' '),
            );
          }),
        ]),
      !!inquiry.followup_question_ids?.length &&
        hl('div.move-review__continuation-followups', [
          hl('p', ko ? '이어지는 조사 질문' : 'Further questions from this choice'),
          ...inquiry.followup_question_ids.map(id => {
            const followup = observations.selection!.continuation_inquiries!.find(
              question => question.id === id,
            )!;
            const followupPath = observations.paths.find(path => followup.source_line_ids.includes(path.id))!;
            return hl(
              'button.button.button-thin',
              {
                attrs: { type: 'button' },
                on: {
                  click: () => {
                    const target = document.getElementById(`continuation-question-${id}`);
                    target?.scrollIntoView({ block: 'nearest' });
                    target?.focus({ preventScroll: true });
                  },
                },
              },
              followupPath.moves
                .slice(0, followup.prefix_moves_uci.length)
                .map(move => move.label)
                .join(' '),
            );
          }),
        ]),
      inquiry.stop_reason && hl('p', investigationStopText(inquiry.stop_reason, props)),
    ],
  );
}

function continuationSupportLabel(state: string, ko: boolean): string {
  return {
    present: ko ? '통제 관측' : 'Control observed',
    absent: ko ? '통제 없음' : 'No control',
    actor_removed: ko ? '지원 기물 제거됨' : 'Supporting piece removed',
    target_removed: ko ? '대상 제거됨' : 'Target removed',
    unknown: ko ? '미확정' : 'Unresolved',
  }[state]!;
}

function renderContinuationProfile(
  candidate: CommentaryCandidate,
  detail: CommentaryLineContinuationDetail,
  observations: MoveReviewObservations,
  props: MoveReviewPanelProps,
): VNode {
  const ko = props.locale === 'ko-KR',
    path = observations.paths.find(path => path.id === detail.line_id)!;
  const eventById = new Map(detail.events.map(event => [event.id, event]));
  const button = (id: string) => {
    const event = eventById.get(id)!;
    return hl(
      'button.button.button-thin',
      {
        attrs: { type: 'button' },
        on: {
          click: () =>
            showObservationFrame(props, { proofId: path.id, ply: event.ply_offset + 1, phase: 'after' }),
        },
      },
      path.moves[event.ply_offset].label,
    );
  };
  const support = (state: string) => continuationSupportLabel(state, ko);
  const visibleSupport = detail.support_comparisons.filter(
    comparison => comparison.targets.length || comparison.state === 'unknown',
  );
  const loneEvent =
    !detail.capture_links.length &&
    !detail.other_target_gains.length &&
    !visibleSupport.length &&
    !detail.recovery_losses?.length &&
    !detail.open_loss_event_ids?.length &&
    detail.events[0];
  return hl(
    'article.move-review__continuation-profile',
    { key: candidate.id, attrs: { id: `continuation-${candidate.id}`, tabindex: -1 } },
    [
      hl('h4', `${ko ? '수순에서 이어진 변화' : 'Changes along the line'} · ${path.moves[0].label}`),
      hl(
        'p',
        candidate.state === 'partial'
          ? ko
            ? '일부 정체성 또는 지원 근거 미확정'
            : 'Some identities or support evidence remain unresolved'
          : ko
            ? '기록된 범위의 기물과 지원 관계를 연결함'
            : 'Pieces and support relations are linked within the recorded line',
      ),
      loneEvent &&
        hl('p', [
          button(loneEvent.id),
          hl(
            'span',
            path.moves.length === loneEvent.ply_offset + 1
              ? ko
                ? ' 이후의 후속 수순은 아직 없습니다.'
                : ' has no recorded continuation yet.'
              : loneEvent.actor_end.state === 'present'
                ? ko
                  ? '의 기물은 이 수순 끝에 남아 있습니다.'
                  : ' leaves its actor present through this line’s end.'
                : loneEvent.actor_end.state === 'captured'
                  ? ko
                    ? '의 기물은 이 수순에서 나중에 잡혔습니다.'
                    : ' has its actor captured later in this line.'
                  : ko
                    ? '의 기물 상태는 수순 끝에서 미확정입니다.'
                    : ' has an unresolved actor identity at line end.',
          ),
        ]),
      ...detail.capture_links.map(link =>
        hl('p', [
          button(link.loss_event_id),
          hl(
            'span',
            ko
              ? `의 포획자가 ${link.elapsed_plies}수 뒤 `
              : `'s capturer is removed ${link.elapsed_plies} plies later by `,
          ),
          button(link.capture_event_id),
        ]),
      ),
      ...detail.other_target_gains.map(link =>
        hl('p', [
          button(link.loss_event_id),
          hl(
            'span',
            ko ? '의 손실 이후, 다른 대상을 잡음: ' : ' is followed by a capture of another target: ',
          ),
          button(link.gain_event_id),
        ]),
      ),
      ...(detail.recovery_losses ?? []).map(link => {
        const recovery = detail.events.find(event => event.id === link.recovery_event_id)!;
        return hl('p', [
          button(link.loss_event_id),
          hl('span', ko ? ' 뒤 ' : ' is followed by '),
          button(link.recovery_event_id),
          hl(
            'span',
            ko
              ? `를 둔 ${recovery.original_actor ? pieceText(recovery.original_actor, props.copy) : '기물'}은 `
              : `; its ${recovery.original_actor ? pieceText(recovery.original_actor, props.copy) : 'actor'} is then captured by `,
          ),
          button(link.later_loss_event_id),
          ko && hl('span', '에서 다시 잡혔습니다.'),
        ]);
      }),
      ...[
        ...new Map(
          (detail.support_applications ?? []).map(use => [
            JSON.stringify([
              use.original_provider,
              use.original_target,
              use.ply_count,
              use.support,
              use.application,
            ]),
            use,
          ]),
        ).values(),
      ].map(use =>
        hl('div.move-review__support-application', [
          hl(
            'p',
            `${pieceText(use.original_provider, props.copy)} (${ko ? '원래 기물' : 'original piece'}) · ${pieceText(use.application.target, props.copy)}: ${
              ko
                ? '지원이 닿는 이 위치에서 상대 왕이 이 대상을 잡는 합법수는 없습니다.'
                : 'At this supported position, the opposing king has no legal capture of this target.'
            }`,
          ),
          hl(
            'button.button.button-thin',
            {
              attrs: { type: 'button' },
              on: {
                click: () =>
                  showObservationFrame(props, { proofId: path.id, ply: use.ply_count, phase: 'after' }),
              },
            },
            ko ? '지원이 쓰인 위치 보기' : 'Inspect where support is used',
          ),
        ]),
      ),
      ...(detail.direct_support_recaptures ?? []).map(use =>
        hl('div.move-review__direct-support-recapture', [
          hl(
            'p',
            ko
              ? `${pieceText(use.provider, props.copy)}가 지원하던 ${pieceText(use.target, props.copy)}이 잡힌 뒤, 같은 지원 기물이 실제로 되잡았습니다.`
              : `${pieceText(use.provider, props.copy)} supports ${pieceText(use.target, props.copy)} and later recaptures its capturer in this line.`,
          ),
          button(use.loss_event_id),
          button(use.recapture_event_id),
        ]),
      ),
      ...(detail.capture_exposures ?? []).map(exposure =>
        hl('div.move-review__capture-exposure', [
          hl(
            'p',
            ko
              ? `가로막던 기물이 비켜 ${pieceText(exposure.attacker, props.copy)}가 ${pieceText(exposure.target, props.copy)}을 잡는 합법수가 열렸습니다. 이득 여부는 후속 수순에 달려 있습니다.`
              : `A vacated blocker opens a legal capture by ${pieceText(exposure.attacker, props.copy)} of ${pieceText(exposure.target, props.copy)}. Its material outcome depends on the continuation.`,
          ),
          hl(
            'button.button.button-thin',
            {
              attrs: { type: 'button' },
              on: {
                click: () =>
                  showObservationFrame(props, {
                    proofId: path.id,
                    ply: exposure.prefix_moves_uci.length,
                    phase: 'after',
                  }),
              },
            },
            ko ? '포획이 열린 위치 보기' : 'Inspect the newly available capture',
          ),
        ]),
      ),
      !!detail.open_loss_event_ids?.length &&
        hl('p', [
          hl('span', ko ? '수순 끝에서 아직 남은 손실: ' : 'Material losses still open at this line’s end: '),
          ...detail.open_loss_event_ids.map(button),
        ]),
      ...visibleSupport.map(comparison =>
        hl('details', { key: comparison.id }, [
          hl(
            'summary',
            `${pieceText(comparison.before_actor, props.copy)} → ${pieceText(comparison.after_actor, props.copy)} · ${ko ? '지원 대상의 변화' : 'Changes in supported targets'}`,
          ),
          comparison.state === 'unknown' &&
            hl('p', ko ? '지원 관계의 전체 목록은 미확정' : 'The complete support inventory is unresolved'),
          ...comparison.targets.map(target =>
            hl(
              'p',
              `${pieceText(target.original_target, props.copy)}: ${support(target.before.state)} → ${support(target.after.state)}`,
            ),
          ),
          ...[comparison.before_position_id, comparison.after_position_id].map((positionId, index) => {
            const offset = path.positionIds!.indexOf(positionId);
            return hl(
              'button.button.button-thin',
              {
                attrs: { type: 'button' },
                on: {
                  click: () =>
                    showObservationFrame(props, {
                      proofId: path.id,
                      ply: Math.max(1, offset),
                      phase: offset === 0 ? 'before' : 'after',
                    }),
                },
              },
              index === 0 ? (ko ? '이전 위치' : 'Before') : ko ? '이후 위치' : 'After',
            );
          }),
        ]),
      ),
      hl(
        'p',
        ko
          ? '이 연결은 기록된 수순의 관측입니다. 충분한 보상, 강제 회수 또는 평가 차이의 원인을 입증하지 않습니다.'
          : 'These links describe the recorded line. They do not establish sufficient compensation, forced recovery, or the cause of an evaluation difference.',
      ),
    ],
  );
}

function occurrenceFrame(
  observations: MoveReviewObservations,
  occurrence: string,
  phase: 'before' | 'after',
): MoveReviewFrameSelection | undefined {
  const path = observations.paths.find(
    path => path.kind !== 'history' && path.occurrenceIds.includes(occurrence),
  );
  if (path) return lineOccurrenceFrame(path, occurrence, phase);
  return;
}

function lineOccurrenceFrame(
  path: MoveReviewObservations['paths'][number],
  occurrence: string,
  phase: 'before' | 'after' = 'after',
): MoveReviewFrameSelection {
  return { proofId: path.id, ply: path.occurrenceIds.indexOf(occurrence) + 1, phase };
}

function responseReadings(inquiry: CommentaryResponseInquiry, observations: MoveReviewObservations) {
  return observations.selection!.candidates.flatMap(candidate => {
    const detail = targetResponseDetail(candidate);
    return detail &&
      candidate.rootOccurrenceId === inquiry.root_occurrence_id &&
      detail.response_inquiry_id === inquiry.id
      ? [{ candidate, detail }]
      : [];
  });
}

function trackedPieceText(piece: CommentaryTrackedPiece, props: MoveReviewPanelProps): string {
  return (
    pieceText(piece.current, props.copy) +
    (piece.original.square !== piece.current.square || piece.original.role !== piece.current.role
      ? ` (${props.locale === 'ko-KR' ? '처음' : 'originally'} ${pieceText(piece.original, props.copy)})`
      : '')
  );
}

function recordedSupportText(
  capture: CommentaryTargetResponseDetail['branches'][number]['observed_recapture'],
  props: MoveReviewPanelProps,
): string {
  if (capture.state !== 'complete') return props.locale === 'ko-KR' ? '미확정' : 'Unresolved';
  return (
    capture.supported_targets.map(piece => trackedPieceText(piece, props)).join('; ') ||
    (props.locale === 'ko-KR'
      ? '기록된 지원 대상 없음(킹 제외)'
      : 'No recorded support targets (excluding kings)')
  );
}

function renderRelatedResponses(
  inquiries: [CommentaryResponseInquiry, CommentaryResponseInquiry],
  observations: MoveReviewObservations,
  props: MoveReviewPanelProps,
): VNode {
  const ko = props.locale === 'ko-KR';
  const original = occurrenceFrame(observations, inquiries[0].root_move_occurrence_id, 'before')!;
  const button = (text: string, frame: MoveReviewFrameSelection) =>
    hl(
      'button.button.button-thin',
      {
        attrs: { type: 'button' },
        on: { click: () => showObservationFrame(props, frame) },
      },
      text,
    );
  return hl(
    'section.move-review__related-responses',
    { key: inquiries.map(inquiry => inquiry.id).join(':') },
    [
      hl('h4', ko ? '같은 대상을 다루는 두 첫 수' : 'Two first moves concerning the same target'),
      button(
        `${ko ? '원래 대상' : 'Original target'} · ${pieceText(inquiries[0].original_target, props.copy)}`,
        { ...original, inspectSquare: inquiries[0].original_target.square },
      ),
      hl('table', [
        hl('thead', [
          hl(
            'tr',
            (ko
              ? ['첫 수', '대상을 잡는 응수', '원래 대상', '기록된 재포획 기물', '재포획 뒤 지원']
              : [
                  'First move',
                  'Capturing reply',
                  'Original target',
                  'Recorded recapturer',
                  'Support after recapture',
                ]
            ).map(label => hl('th', { attrs: { scope: 'col' } }, label)),
          ),
        ]),
        hl(
          'tbody',
          inquiries.flatMap(inquiry => {
            const readings = responseReadings(inquiry, observations);
            const reading =
              readings.find(({ detail }) => targetResponseAnswer(detail, inquiry)) ?? readings[0];
            const current = reading?.detail.branches.find(
              branch => branch.reply_move === inquiry.reference_move,
            );
            const pathOf = (branch: CommentaryTargetResponseDetail['branches'][number]) =>
              observations.paths.find(path => path.id === branch.line_id)!;
            const extendsBranch = (
              longer: CommentaryTargetResponseDetail['branches'][number],
              shorter: CommentaryTargetResponseDetail['branches'][number],
            ) =>
              pathOf(shorter).occurrenceIds.every((id, index) => pathOf(longer).occurrenceIds[index] === id);
            // This table reads the capture branch; an unresolved decline cannot erase its recorded facts.
            const captures = readings.flatMap(({ detail }) =>
              detail.branches.filter(
                branch =>
                  branch.reply_move === inquiry.reference_move &&
                  branch.observed_recapture.actor &&
                  current &&
                  extendsBranch(branch, current),
              ),
            );
            const maximal = captures.filter(
              branch =>
                !captures.some(
                  other =>
                    pathOf(other).occurrenceIds.length > pathOf(branch).occurrenceIds.length &&
                    extendsBranch(other, branch),
                ),
            );
            const unique = new Map(
              maximal.map(branch => [JSON.stringify(pathOf(branch).occurrenceIds), branch]),
            );
            const branches = unique.size ? [...unique.values()] : [current];
            const child = occurrenceFrame(observations, inquiry.root_move_occurrence_id, 'after')!;
            const firstMove = observations.paths.find(path => path.id === child.proofId)!.moves[child.ply - 1]
              .label;
            return branches.map((branch, index) => {
              const path = branch && pathOf(branch);
              const capture = branch?.observed_recapture;
              const recaptureFrame =
                capture?.occurrence_id && path ? lineOccurrenceFrame(path, capture.occurrence_id) : undefined;
              const targetState =
                branch?.target_state === 'captured'
                  ? ko
                    ? '잡힘'
                    : 'Captured'
                  : branch?.target_state === 'present'
                    ? ko
                      ? '남아 있음'
                      : 'Present'
                    : ko
                      ? '미확정'
                      : 'Unresolved';
              return hl(
                'tr',
                { key: index ? `${inquiry.root_move}:${branch!.line_id}` : inquiry.root_move },
                [
                  hl('th', { attrs: { scope: 'row' } }, [
                    button(firstMove, child),
                    ...(branches.length > 1
                      ? [
                          hl(
                            'span',
                            `${ko ? '여러 후속 관측' : 'Multiple recorded continuations'} · ${index + 1}/${branches.length}`,
                          ),
                        ]
                      : []),
                  ]),
                  hl(
                    'td',
                    branch && path
                      ? [
                          button(path.moves[path.occurrenceIds.indexOf(branch.reply_occurrence_id)].label, {
                            ...lineOccurrenceFrame(path, branch.reply_occurrence_id),
                            inspectSquare: inquiry.target.square,
                          }),
                          ...(branches.length > 1
                            ? [
                                button(
                                  path.moves
                                    .slice(2)
                                    .map(move => move.label)
                                    .join(' '),
                                  {
                                    proofId: path.id,
                                    ply: path.moves.length,
                                    phase: 'after',
                                  },
                                ),
                              ]
                            : []),
                        ]
                      : `${inquiry.reference_move} · ${ko ? '수순 미확보' : 'Line unavailable'}`,
                  ),
                  hl('td', targetState),
                  hl(
                    'td',
                    capture?.actor && recaptureFrame
                      ? [
                          button(trackedPieceText(capture.actor, props), {
                            ...recaptureFrame,
                            inspectSquare: capture.actor.current.square,
                          }),
                        ]
                      : capture?.state === 'not_observed'
                        ? ko
                          ? '기록되지 않음'
                          : 'Not recorded'
                        : ko
                          ? '미확정'
                          : 'Unresolved',
                  ),
                  hl('td', capture ? recordedSupportText(capture, props) : ko ? '미확정' : 'Unresolved'),
                ],
              );
            });
          }),
        ),
      ]),
      hl(
        'p',
        ko
          ? '각 행은 해당 첫 수 뒤에 기록된 응수와 재포획을 대조합니다. 강제 응수나 첫 수의 우열을 뜻하지 않습니다.'
          : 'Each row follows the replies and recaptures recorded after that first move. It does not establish forced replies or rank the first moves.',
      ),
    ],
  );
}

function renderResponseInquiry(
  inquiry: CommentaryResponseInquiry,
  observations: MoveReviewObservations,
  props: MoveReviewPanelProps,
  stateLabel: string,
): VNode {
  const ko = props.locale === 'ko-KR';
  const child = occurrenceFrame(observations, inquiry.root_move_occurrence_id, 'after')!;
  const rootPath = observations.paths.find(path => path.id === child.proofId)!;
  const rootLabel = rootPath.moves[child.ply - 1].label;
  const label = (move: Uci) =>
    observations.paths.find(path => path.moves[0]?.uci === inquiry.root_move && path.moves[1]?.uci === move)
      ?.moves[1]?.label ?? move;
  const readings = responseReadings(inquiry, observations);
  const hasAnswerReading = readings.some(({ detail }) => targetResponseAnswer(detail, inquiry));
  const selections = [inquiry.reference_selection, inquiry.alternative_selection];
  const selectionText = (index: number): string | undefined => {
    const selection = selections[index];
    if (!selection) return;
    const move = label(index === 0 ? inquiry.reference_move : inquiry.alternative_move);
    const prefix = `${move}: `;
    if (selection.source === 'legal_seed')
      return prefix + (ko ? '합법 응수에서 출발한 조사 후보' : 'A legal reply to investigate');
    if (selection.source === 'root_pv_hint')
      return (
        prefix + (ko ? '첫 수 분석 수순의 응수 단서' : 'A reply suggested by the original move analysis')
      );
    if (selection.source === 'observed_line')
      return prefix + (ko ? '기존에 관측한 수순의 응수' : 'A reply from an existing observed line');
    const search = observations.selection!.response_searches!.find(
      search => search.work_id === selection.work_id,
    )!;
    const scope =
      search.root_restriction.kind === 'unrestricted'
        ? ko
          ? '전체 합법 응수'
          : 'all legal replies'
        : `${search.root_restriction.moves_uci.length}${ko ? '개의 지정 응수' : search.root_restriction.moves_uci.length === 1 ? ' specified reply' : ' specified replies'}`;
    return (
      prefix +
      (ko
        ? `${scope}를 검색해 반환된 후보 중 이 부류에서 평가가 가장 높은 응수`
        : `The highest-scoring reply in this group among candidates returned by a search of ${scope}`)
    );
  };
  return hl('article.move-review__response-inquiry', { key: inquiry.id }, [
    hl('h4', ko ? `${rootLabel} 이후의 응수` : `Responses after ${rootLabel}`),
    hl(
      'p',
      ko
        ? `${pieceText(inquiry.target, props.copy)}을(를) 잡는 응수와 잡지 않는 응수를 대조합니다.`
        : `Compare a reply that captures ${pieceText(inquiry.target, props.copy)} with one that does not.`,
    ),
    hl(
      'p.move-review__inquiry-state',
      inquiry.state === 'awaiting_engine' && (props.job.kind === 'fault' || props.job.kind === 'abstained')
        ? ko
          ? '응수 비교 결과를 받기 전에 검토가 중단됨'
          : 'Review stopped before the response comparison arrived'
        : stateLabel,
    ),
    hl('div', [
      hl(
        'button.button.button-thin',
        {
          attrs: { type: 'button' },
          on: {
            click: () => showObservationFrame(props, { ...child, inspectSquare: inquiry.target.square }),
          },
        },
        ko ? '응수를 두기 전 대상 보기' : 'Inspect the target before the reply',
      ),
      hl(
        'button.button.button-thin',
        {
          attrs: { type: 'button' },
          on: {
            click: () =>
              showObservationFrame(props, {
                ...child,
                phase: 'before',
                inspectSquare: inquiry.original_target.square,
              }),
          },
        },
        `${ko ? '처음의 대상' : 'Original target'} · ${pieceText(inquiry.original_target, props.copy)}`,
      ),
    ]),
    hl('details.move-review__response-options', [
      hl('summary', ko ? '질문을 만든 합법 응수' : 'Legal replies behind the question'),
      hl(
        'p',
        `${ko ? '대상을 잡음' : 'Captures the target'}: ${inquiry.capture_moves.map(label).join(', ')}`,
      ),
      hl(
        'p',
        `${ko ? '대상을 잡지 않음' : 'Does not capture the target'}: ${inquiry.decline_moves.map(label).join(', ')}`,
      ),
      hl(
        'p',
        `${ko ? '이번 비교' : 'This comparison'}: ${label(inquiry.reference_move)} ↔ ${label(inquiry.alternative_move)}`,
      ),
    ]),
    selections.some(Boolean) &&
      hl('div.move-review__response-selection', [
        hl('p', ko ? '응수 선택 근거' : 'Why these replies were selected'),
        ...selections.map((_selection, index) => hl('p', selectionText(index))),
        hl(
          'p',
          ko
            ? '검색에서 반환하지 않은 응수가 약하다는 뜻은 아닙니다. 관측이 완결되어도 최선 응수나 강제 수순이 확정되지는 않습니다.'
            : 'Replies absent from the returned sample are not thereby weak. Complete observations do not establish a best reply or a forced continuation.',
        ),
      ]),
    inquiry.answer &&
      hl(
        'p.move-review__response-scope',
        ko
          ? '평가는 이 수를 둔 뒤의 두 응수만 비교합니다. 원래 착수의 평가는 이 결과만으로 바뀌지 않습니다.'
          : 'The evaluation compares these two replies after the move. This result alone does not change the original move assessment.',
      ),
    inquiry.answer &&
      hl(
        'p',
        inquiry.answer.evaluation_relation === 'equal'
          ? ko
            ? '이번 검색에서 두 응수의 평가값은 같습니다.'
            : 'The two replies received equal scores in this search.'
          : `${label(inquiry.alternative_move)}: ` +
              (inquiry.answer.evaluation_relation === 'better'
                ? ko
                  ? '응수하는 쪽의 검색 평가값이 더 높습니다.'
                  : 'The search score is higher for the responding side.'
                : ko
                  ? '응수하는 쪽의 검색 평가값이 더 낮습니다.'
                  : 'The search score is lower for the responding side.'),
      ),
    ...readings.map(({ candidate, detail }) =>
      renderTargetResponse(candidate, detail, inquiry, observations, props),
    ),
    inquiry.answer &&
      !hasAnswerReading &&
      hl('div', [
        ...renderComparisonPaths(inquiry.answer, observations, props, inquiry.answer.ply_offset + 1),
        hl(
          'p',
          ko
            ? '이 평가의 정확한 두 수순에 연결된 대상·지원 관측은 아직 없습니다.'
            : 'No target-and-support reading is linked to these exact evaluation lines yet.',
        ),
      ]),
    !inquiry.answer &&
      hl(
        'p',
        inquiry.state === 'observed'
          ? ko
            ? '기존 관측을 재사용했습니다. 두 관측의 대상·지원 정보와 응수의 평가 비교는 별개입니다.'
            : 'Existing observations were reused. Target-and-support observations are separate from comparing reply evaluations.'
          : inquiry.state === 'deferred'
            ? ko
              ? '응수의 평가 비교가 남아 있습니다. 추가 계산이 예약되었다는 뜻은 아닙니다.'
              : 'The response evaluation remains unresolved. This does not mean another search is scheduled.'
            : ko
              ? '이 응수들의 평가 결과를 아직 확보하지 못했습니다.'
              : 'The evaluation of these replies is not available yet.',
      ),
    inquiry.stop_reason && hl('p', investigationStopText(inquiry.stop_reason, props)),
  ]);
}

function targetResponseAnswer(detail: CommentaryTargetResponseDetail, inquiry: CommentaryResponseInquiry) {
  const answer = inquiry.answer;
  return answer &&
    [answer.reference_line_id, answer.alternative_line_id].every(id =>
      detail.branches.some(branch => branch.line_id === id),
    )
    ? answer
    : undefined;
}

function renderTargetResponse(
  candidate: CommentaryCandidate,
  detail: CommentaryTargetResponseDetail,
  inquiry: CommentaryResponseInquiry,
  observations: MoveReviewObservations,
  props: MoveReviewPanelProps,
): VNode {
  const ko = props.locale === 'ko-KR';
  const answer = targetResponseAnswer(detail, inquiry);
  const tracked = (piece: CommentaryTrackedPiece) => trackedPieceText(piece, props);
  return hl('section.move-review__target-response-reading', { key: candidate.id }, [
    continuationLinks(candidate.detail.continuation_candidate_ids, props),
    continuationLinks(candidate.detail.continuation_comparison_ids, props),
    hl('h5', ko ? '응수 뒤 대상과 지원의 관측' : 'Observed target and support after each reply'),
    ...detail.branches.map(branch => {
      const path = observations.paths.find(path => path.id === branch.line_id)!;
      const frame = lineOccurrenceFrame(path, branch.reply_occurrence_id);
      const evaluation =
        answer &&
        (branch.line_id === answer.reference_line_id
          ? answer.reference_evaluation
          : branch.line_id === answer.alternative_line_id
            ? answer.alternative_evaluation
            : undefined);
      const capture = branch.observed_recapture;
      const recaptureFrame: MoveReviewFrameSelection | null = capture.occurrence_id
        ? lineOccurrenceFrame(path, capture.occurrence_id)
        : null;
      const targetText =
        branch.target_state === 'present'
          ? `${ko ? '원래 대상이 남아 있음' : 'The original target remains'}: ${pieceText(branch.target, props.copy)}.`
          : branch.target_state === 'captured'
            ? `${ko ? '원래 대상이 잡힘' : 'The original target was captured'}: ${pieceText(detail.original_target, props.copy)}.`
            : ko
              ? '원래 대상의 상태를 확인할 관측이 부족합니다.'
              : 'The original target’s state is not established.';
      return hl('div.move-review__response-branch', { key: branch.line_id }, [
        hl(
          'h6',
          path.moves
            .slice(0, frame.ply)
            .map(move => move.label)
            .join(' '),
        ),
        evaluation && hl('p', comparisonEvaluation(evaluation, props)),
        hl('p', targetText),
        branch.capturer &&
          hl(
            'p',
            `${ko ? '대상을 잡은 기물' : 'The capturing piece'}: ${pieceText(branch.capturer, props.copy)}.`,
          ),
        hl(
          'button.button.button-thin',
          {
            attrs: { type: 'button' },
            on: {
              click: () =>
                showObservationFrame(props, {
                  ...frame,
                  inspectSquare: branch.target?.square ?? branch.capturer?.square ?? inquiry.target.square,
                }),
            },
          },
          ko ? '응수 이후 보드 보기' : 'Inspect the board after this reply',
        ),
        branch.providers.state !== 'not_applicable' &&
          hl(
            'p',
            branch.providers.state === 'unknown'
              ? ko
                ? '대상의 칸을 지원하는 기물의 목록은 미확정입니다.'
                : 'The target’s geometric supporters are not established.'
              : branch.providers.pieces.length
                ? `${ko ? '대상의 칸을 기하적으로 지원하는 기물' : 'Geometric supporters of the target’s square'}: ${branch.providers.pieces.map(tracked).join('; ')}.`
                : ko
                  ? '이 위치에서 대상의 칸을 기하적으로 지원하는 같은 편 기물은 없습니다.'
                  : 'No friendly piece geometrically supports the target’s square in this position.',
          ),
        branch.recaptures.state !== 'not_applicable' &&
          hl(
            'p',
            branch.recaptures.state === 'unknown'
              ? ko
                ? '즉시 재포획할 합법수를 확인하지 못했습니다.'
                : 'Legal immediate recaptures are not established.'
              : branch.recaptures.moves.length
                ? `${ko ? '합법적인 즉시 재포획' : 'Legal immediate recaptures'}: ${branch.recaptures.moves.map(move => `${pieceText(move.actor, props.copy)} (${move.move_uci})`).join('; ')}.`
                : ko
                  ? '이 위치에서 그 포획 기물을 즉시 재포획할 합법수는 없습니다.'
                  : 'There is no legal immediate recapture of that capturing piece in this position.',
          ),
        (capture.state !== 'not_observed' || branch.target_state === 'captured') &&
          hl(
            'p',
            capture.state === 'complete'
              ? `${ko ? '수순에서 실제로 기록된 즉시 재포획 기물' : 'Immediate recapturer recorded in the line'}: ${tracked(capture.actor!)}. ` +
                  `${ko ? '회수 뒤 기록된 지원 대상' : 'Recorded support after recapture'}: ${recordedSupportText(capture, props)}.`
              : capture.state === 'unknown'
                ? ko
                  ? '회수 뒤 기물의 지원 관계는 미확정입니다.'
                  : 'Support after the recapture remains unresolved.'
                : ko
                  ? '이 수순에는 즉시 재포획이 기록되지 않았습니다. 회수 뒤 지원과 지연 회수는 미확정입니다.'
                  : 'No immediate recapture is recorded in this line. Support after a recapture and later recovery remain unresolved.',
          ),
        recaptureFrame &&
          hl(
            'button.button.button-thin',
            {
              attrs: { type: 'button' },
              on: {
                click: () =>
                  showObservationFrame(props, {
                    ...recaptureFrame,
                    inspectSquare: capture.actor?.current.square,
                  }),
              },
            },
            ko ? '기록된 재포획 이후 보기' : 'Inspect after the recorded recapture',
          ),
        renderLineSummary(path, path.lineSummary, {
          ...props,
          actions: { ...props.actions, pinFrame: frame => showObservationFrame(props, frame) },
        }),
      ]);
    }),
    hl(
      'p',
      ko
        ? '지원은 칸의 기하적 통제입니다. 이 유한 수순의 관측만으로 강제 수순이나 평가 차이의 모든 원인을 확정하지 않습니다.'
        : 'Support means geometric control of a square. These finite observations do not establish a forced line or all causes of an evaluation difference.',
    ),
  ]);
}

function investigationStopText(reason: string, props: MoveReviewPanelProps): string {
  const ko = props.locale === 'ko-KR';
  return reason === 'reference_changed'
    ? ko
      ? '비교 기준 수가 바뀌어 이전 기준의 질문을 중단했습니다.'
      : 'The comparison reference changed, so the question about the earlier reference was stopped.'
    : reason === 'investigation_limit'
      ? ko
        ? '이번 검토의 추가 비교 한도를 사용하여 이 질문은 남겨 두었습니다.'
        : 'This question remains because this review used its allowance for additional comparisons.'
      : reason in props.copy.stopReasons
        ? props.copy.stopReasons[reason as keyof MoveReviewCopy['stopReasons']]
        : ko
          ? '이번 조사에서 필요한 비교를 마치지 못했습니다.'
          : 'The required comparison was not completed in this investigation.';
}
function renderInquiryObservations(
  observations: MoveReviewObservations,
  occurrenceIds: string[],
  props: MoveReviewPanelProps,
): VNode[] {
  const ko = props.locale === 'ko-KR';
  return [...new Set(occurrenceIds)].flatMap(occurrenceId => {
    const path = observations.paths.find(
      path => path.kind !== 'history' && path.occurrenceIds.includes(occurrenceId),
    );
    if (!path) return [];
    const index = path.occurrenceIds.indexOf(occurrenceId);
    const statements = observations.statements.filter(
      statement =>
        statement.occurrenceId === occurrenceId &&
        (statement.kind !== 'structural_change' || statement.changes.length > 0),
    );
    return [
      hl('details.move-review__inquiry-observations', { key: occurrenceId }, [
        hl(
          'summary',
          `${path.moves[index].label} · ${ko ? '이 수에서 관측한 변화' : 'Observations for this move'}`,
        ),
        ...statements.map(statement => hl('p', observationText(statement, props.copy))),
        !statements.length &&
          hl(
            'p',
            ko
              ? '이 수에 연결된 변화 관측이 아직 없습니다.'
              : 'No change observations are linked to this move yet.',
          ),
        hl(
          'div',
          (['before', 'after'] as const).map(phase =>
            hl(
              'button.button.button-thin',
              {
                attrs: { type: 'button' },
                on: { click: () => showObservationFrame(props, { proofId: path.id, ply: index + 1, phase }) },
              },
              props.copy.frameLabels[phase],
            ),
          ),
        ),
      ]),
    ];
  });
}

function showObservationFrame(props: MoveReviewPanelProps, frame: MoveReviewFrameSelection): void {
  if (props.view.expandedProofId !== frame.proofId) props.actions.toggleProof(frame.proofId);
  props.actions.pinFrame(frame);
}

function renderCommentarySelection(props: MoveReviewPanelProps): VNode | undefined {
  const observations = moveReviewObservations(props.job);
  const selection = observations?.selection;
  const record = props.job.recordReview;
  if (!selection?.scenes.length && !record) return;
  const ko = props.locale === 'ko-KR';
  const subject =
    props.job.kind === 'completed' || props.job.kind === 'position-action'
      ? props.job.snapshot.subject
      : props.job.kind === 'idle'
        ? undefined
        : props.job.subject;
  const ready = record?.scenes.filter(scene => scene.state === 'ready') ?? [];
  const selectedPly = subject?.after.path.length ?? 0;
  const anchor =
    props.job.observationFrame !== undefined
      ? props.job.observationFrame
      : observations && subject
        ? moveReviewFrameForSubject(observations, subject, subject)
        : undefined;
  const occurrence =
    anchor && observations?.paths.find(path => path.id === anchor.proofId)?.occurrenceIds[anchor.ply - 1];
  const sceneButtons = (direction: -1 | 1): VNode => {
    const candidate =
      direction > 0
        ? ready.find(scene => scene.path.length > selectedPly)
        : [...ready].reverse().find(scene => scene.path.length < selectedPly);
    return hl(
      'button.button.button-thin',
      {
        attrs: { type: 'button', disabled: !candidate || !props.actions.jumpToScene },
        on: { click: () => candidate && props.actions.jumpToScene?.(candidate.path) },
      },
      direction < 0 ? (ko ? '이전 장면' : 'Previous moment') : ko ? '다음 장면' : 'Next moment',
    );
  };
  return hl('section.move-review__selection', [
    record &&
      hl('div.move-review__review-progress', [
        hl(
          'p',
          ko
            ? `${record.total}수 중 ${record.assessed}수 평가 도착 · ${record.completed}수 분석 완료${record.stopped ? ` · ${record.stopped}수 중단` : ''}`
            : `${record.assessed} of ${record.total} moves assessed · ${record.completed} analyses completed${record.stopped ? ` · ${record.stopped} stopped` : ''}`,
        ),
        record.status === 'finished' &&
          hl('p.move-review__notice', ko ? '기보 처리가 종료되었습니다.' : 'Game processing has finished.'),
        record.status === 'paused' &&
          hl('p.move-review__notice', ko ? '기보 검토를 멈췄습니다.' : 'Game review is paused.'),
        record.status === 'blocked' &&
          hl(
            'p.move-review__notice',
            ko
              ? '분석을 계속할 수 없어 기보 검토를 멈췄습니다. 수를 선택해 다시 시도할 수 있습니다.'
              : 'Game review stopped because analysis could not continue. Select a move to retry.',
          ),
        ready.length > 0 &&
          hl('nav', { attrs: { 'aria-label': ko ? '설명할 장면' : 'Commentary moments' } }, [
            sceneButtons(-1),
            sceneButtons(1),
          ]),
        record.scenes.some(scene => scene.state !== 'ready') &&
          hl(
            'p.move-review__notice',
            ko
              ? '설명 근거가 아직 부족한 비교도 남아 있습니다.'
              : 'Some comparisons still need explanatory evidence.',
          ),
      ]),
    ...(!observations || !selection
      ? []
      : selection.scenes
          .filter(scene => scene.rootOccurrenceId === occurrence)
          .map(scene => {
            const main = selection.candidates.find(
              candidate => scene.candidateIds.includes(candidate.id) && candidate.kind === 'comparison',
            );
            if (!main) return undefined;
            if (
              pvReadings(observations, occurrence || undefined).some(
                ({ candidate, detail }) => matchingPrimary(observations, candidate, detail)?.id === main.id,
              )
            )
              return undefined;
            const frame = commentarySceneFrame(observations, scene);
            const question = questionComparisonDetail(main);
            const ply = question ? question.prefix_moves_uci.length + 1 : 1;
            return hl(
              'article.move-review__scene',
              {
                key: `${observations.owner?.jobId ?? ''}:${scene.id}`,
                attrs: { 'data-evidence-state': scene.state },
              },
              [
                hl(
                  'h3',
                  scene.state === 'ready'
                    ? question
                      ? ko
                        ? '두 선택의 비교'
                        : 'Comparison of the two choices'
                      : ko
                        ? '수순별 교환 결과'
                        : 'Exchanges in these lines'
                    : ko
                      ? '더 살펴볼 비교'
                      : 'A comparison to examine',
                ),
                scene.state === 'ready' && typeof main.detail.text === 'string'
                  ? hl('p', main.detail.text)
                  : hl(
                      'p.move-review__notice',
                      ko
                        ? main.detail.material_relation === 'equal'
                          ? '두 수순에서 잃는 기물의 종류별 수와 승격 수는 같습니다. 배치와 수순 순서까지 같은 뜻은 아니며, 평가 차이의 설명은 더 필요합니다.'
                          : '평가 비교는 있지만, 이어지는 수순의 설명 근거가 아직 부족합니다.'
                        : main.detail.material_relation === 'equal'
                          ? 'Both lines lose the same numbers of each piece type and have the same promotion counts. Their positions and move order may differ; the evaluation still needs explanation.'
                          : 'The evaluation comparison is available, but its continuation evidence is incomplete.',
                    ),
                hl('div.move-review__observation-frames', [
                  frame &&
                    hl(
                      'button',
                      { attrs: { type: 'button' }, on: { click: () => showObservationFrame(props, frame) } },
                      ko ? '선택한 위치 보기' : 'Inspect this position',
                    ),
                  ...main.lineIds.map(lineId => {
                    const path = observations.paths.find(path => path.id === lineId);
                    if (!path?.moves[ply - 1]) return undefined;
                    return hl(
                      'button',
                      {
                        key: lineId,
                        attrs: { type: 'button' },
                        on: {
                          click: () =>
                            showObservationFrame(props, { proofId: lineId, ply, phase: 'after' }),
                        },
                      },
                      `${path.moves[ply - 1].label} · ${ko ? '수순 보기' : 'Inspect line'}`,
                    );
                  }),
                ]),
              ],
            );
          })),
  ]);
}

export function renderMoveReviewNotationBadge(
  symbol: MoveReviewVerdictSymbol,
  accessibleLabel: string,
): VNode | undefined {
  if (symbol === 'none') return;
  const tone = moveReviewVerdictTone(symbol);
  return hl(
    `span.move-review__notation-badge.move-review__notation-badge--${tone}`,
    {
      attrs: {
        title: accessibleLabel,
        'aria-label': accessibleLabel,
      },
    },
    symbol,
  );
}

function moveReviewVerdictTone(symbol: MoveReviewVerdictSymbol): MoveReviewVerdictTone {
  switch (symbol) {
    case 'none':
      return 'neutral';
    case '?!':
      return 'inaccuracy';
    case '?':
      return 'mistake';
    case '??':
      return 'blunder';
  }
}

function moveReviewStateLabel(job: MoveReviewJobState, copy: MoveReviewCopy): string {
  switch (job.kind) {
    case 'idle':
      return '';
    case 'loading':
      return copy.analysing;
    case 'unsearched':
      return copy.gradeNotRequested;
    case 'completed':
    case 'position-action':
      return copy.completed;
    case 'abstained':
      return copy.analysisStopped;
    case 'fault':
      return '';
  }
}

function renderPanelContent(props: MoveReviewPanelProps): VNode {
  const { job, copy } = props;
  switch (job.kind) {
    case 'idle':
      return renderState('idle', copy.idleTitle, copy.idleBody);
    case 'loading':
      return renderLoadingState(copy.analysing);
    case 'unsearched':
      return hl('div.move-review__state.move-review__state--unsearched', [
        job.observationsPending && hl('p', copy.observationsLoading),
        !job.observationsPending && !job.observations && hl('p', copy.observationsMissing),
        renderGradeRequest(props),
      ]);
    case 'completed':
      return renderReviewBody(props, job.snapshot.evidence);
    case 'position-action':
      return renderPositionAction(job.snapshot.action, props);
    case 'abstained':
      if (job.stopCondition === 'engine_unavailable' || job.stopCondition === 'browser_unsupported')
        return renderFailureState(
          'unsupported',
          copy.unsupported,
          copy.stopReasons[job.stopCondition],
          job.stopCondition === 'engine_unavailable',
          props,
        );
      return hl('div.move-review__state.move-review__state--abstained', [
        hl('p', copy.stopReasons[job.stopCondition]),
        renderGradeRequest(props),
      ]);
    case 'fault':
      return renderFailureState('error', copy.unavailable, job.message, job.retryable, props);
  }
}

function renderGradeRequest(props: MoveReviewPanelProps): VNode {
  return hl(
    'button.button.button-thin.move-review__request-grade',
    {
      attrs: { type: 'button' },
      on: { click: props.actions.retry },
    },
    props.copy.requestGrade,
  );
}

function renderPositionAction(
  action: Extract<MoveReviewJobState, { kind: 'position-action' }>['snapshot']['action'],
  props: MoveReviewPanelProps,
): VNode {
  if (action.kind === 'automatic-terminal') {
    const terminal = action.terminal;
    const detail =
      terminal.kind === 'checkmate'
        ? `${props.copy.terminalLabels.checkmate} · ${props.copy.colorLabels[terminal.winner]}`
        : props.copy.terminalLabels[terminal.kind];
    return renderState('completed', props.copy.terminalResult, detail);
  }
  return renderState('completed', props.copy.drawClaimAvailable, drawClaimSummary(action.claims, props.copy));
}

function drawClaimSummary(claims: MoveReviewDrawClaim[], copy: MoveReviewCopy): string {
  return claims
    .map(claim => `${copy.drawRuleLabels[claim.rule]} · ${copy.drawAvailabilityLabels[claim.availability]}`)
    .join(' · ');
}

function renderState(kind: string, title: string, body: string): VNode {
  return hl(`div.move-review__state.move-review__state--${kind}`, [hl('h3', title), hl('p', body)]);
}

function renderLoadingState(label: string): VNode {
  return hl('div.move-review__state.move-review__state--loading', [
    hl('h3', label),
    hl('div.move-review__loading-line', { attrs: { 'aria-hidden': 'true' } }),
  ]);
}

function renderFailureState(
  kind: 'error' | 'unsupported',
  title: string,
  body: string,
  retryable: boolean,
  props: MoveReviewPanelProps,
): VNode {
  return hl(
    `div.move-review__state.move-review__state--${kind}`,
    { attrs: { role: kind === 'error' ? 'alert' : 'status', 'aria-atomic': 'true' } },
    [
      hl('h3', title),
      hl('p', body),
      retryable &&
        hl('div.move-review__state-actions', [
          hl(
            'button.button.button-thin.move-review__retry',
            {
              attrs: { type: 'button' },
              on: { click: props.actions.retry },
            },
            props.copy.retry,
          ),
        ]),
    ],
  );
}

function renderReviewBody(props: MoveReviewPanelProps, evidence: MoveReviewEvidence): VNode {
  const played = evidence.candidates.find(candidate => candidate.roles.includes('played'));
  if (!played)
    return hl('div.move-review__body', [
      hl('p', props.copy.comparisonUnavailable),
      evidence.drawClaims && hl('p', drawClaimSummary(evidence.drawClaims, props.copy)),
    ]);
  const best = evidence.candidates.find(candidate => candidate.roles.includes('best'))!;
  const review = played.review;
  const core = review.kind === 'move-verdict' ? review.core : undefined;
  const verdictLabel = core
    ? core.kind === 'best-choice'
      ? props.copy.selectedReference
      : moveReviewVerdictCodeLabel(core.verdictCode, props.copy)
    : review.kind === 'single-candidate-insight'
      ? props.copy.lineInsight
      : review.kind === 'forced-single-move'
        ? props.copy.forcedSingleMove
        : props.copy.unavailable;
  const hasBadge = !!core && core.verdictSymbol !== 'none';
  const tone = core ? moveReviewVerdictTone(core.verdictSymbol) : 'neutral';
  return hl('div.move-review__body', [
    hl(
      'section.move-review__summary',
      {
        class: {
          'has-no-badge': !hasBadge,
        },
        attrs: { 'aria-label': `${verdictLabel}: ${played.label}` },
      },
      [
        hasBadge &&
          hl(
            `span.move-review__verdict-badge.move-review__verdict-badge--${tone}`,
            { attrs: { 'aria-hidden': 'true' } },
            core!.verdictSymbol,
          ),
        hl('div.move-review__summary-copy', [
          hl('span.move-review__eyebrow', played.label),
          hl('h3', verdictLabel),
          hl('p.move-review__core-comparison', renderCoreComparison(played, best, core, props.copy)),
          review.kind === 'abstained' ? hl('p', props.copy.candidateUnavailable) : undefined,
        ]),
      ],
    ),
    core?.winChance && renderMetrics(core, core.winChance, props),
    core && (core.referenceTerminal || core.reviewedTerminal) && renderTerminalOutcomes(core, props),
    evidence.drawClaims &&
      renderState(
        'completed',
        props.copy.drawClaimAvailable,
        drawClaimSummary(evidence.drawClaims, props.copy),
      ),
    renderEvidenceDisclosure(props, evidence),
  ]);
}

function renderCoreComparison(
  played: MoveReviewCandidate,
  best: MoveReviewCandidate,
  core: MoveReviewCore | undefined,
  copy: MoveReviewCopy,
): string {
  if (core?.kind === 'best-choice')
    return `${copy.best} ${played.label} · ${copy.runnerUp} ${core.bestChoice.runnerUpUci}: ${moveReviewVerdictCodeLabel(
      core.bestChoice.runnerUpVerdictCode,
      copy,
    )}`;
  return played.uci === best.uci
    ? `${copy.played} + ${copy.best} · ${played.label}`
    : `${copy.played} ${played.label} · ${copy.best} ${best.label}`;
}

function renderMetrics(
  core: MoveReviewCore,
  winChance: NonNullable<MoveReviewCore['winChance']>,
  props: MoveReviewPanelProps,
): VNode {
  const referenceMove = core.kind === 'best-choice' ? core.bestChoice.runnerUpUci : core.bestUci;
  return hl('dl.move-review__metrics', [
    hl('div', [
      hl('dt.move-review__metric-label', `${props.copy.winChance}: ${referenceMove} → ${core.playedUci}`),
      hl(
        'dd',
        `${formatMoveReviewPercent(winChance.referencePercent, props.locale)} → ${formatMoveReviewPercent(
          winChance.playedPercent,
          props.locale,
        )}`,
      ),
    ]),
    hl('div', [
      hl('dt.move-review__metric-label', props.copy.winChanceChange),
      hl('dd', formatMoveReviewPercentagePointChange(winChance.changePercentagePoints, props.locale)),
    ]),
  ]);
}

function renderTerminalOutcomes(core: MoveReviewCore, props: MoveReviewPanelProps): VNode {
  const label = (terminal: NonNullable<MoveReviewCore['referenceTerminal']>): string =>
    terminal.kind === 'checkmate'
      ? `${props.copy.terminalLabels.checkmate} · ${props.copy.colorLabels[terminal.winner]}`
      : props.copy.terminalLabels[terminal.kind];
  return hl('dl.move-review__outcomes', [
    core.referenceTerminal &&
      hl('div', [
        hl('dt', core.kind === 'best-choice' ? props.copy.runnerUp : props.copy.best),
        hl('dd', label(core.referenceTerminal)),
      ]),
    core.reviewedTerminal &&
      hl('div', [
        hl('dt', core.kind === 'best-choice' ? props.copy.best : props.copy.played),
        hl('dd', label(core.reviewedTerminal)),
      ]),
  ]);
}

function renderEvidenceDisclosure(props: MoveReviewPanelProps, evidence: MoveReviewEvidence): VNode {
  const expanded = props.view.evidenceExpanded;
  const contentId = 'move-review-evidence';
  return hl('div.move-review__evidence-disclosure', [
    hl(
      'button.button.button-thin.move-review__evidence-toggle',
      {
        attrs: {
          type: 'button',
          'aria-expanded': expanded ? 'true' : 'false',
          'aria-controls': contentId,
        },
        on: { click: props.actions.toggleEvidence },
      },
      expanded ? props.copy.hideEvidence : props.copy.showEvidence,
    ),
    hl(
      `div#${contentId}.move-review__evidence`,
      { attrs: { hidden: !expanded } },
      expanded ? renderEvidence(props, evidence) : [],
    ),
  ]);
}

function renderEvidence(props: MoveReviewPanelProps, evidence: MoveReviewEvidence): VNode[] {
  const selected = selectedMoveReviewCandidate(evidence, props.view.selectedCandidateUci)!;
  return [renderCandidates(evidence.candidates, selected, props), renderCandidateReview(selected, props)];
}

function renderCandidateReview(candidate: MoveReviewCandidate, props: MoveReviewPanelProps): VNode {
  const review = candidate.review;
  if (review.kind === 'abstained') return renderCandidateMessage(candidate, props.copy.candidateUnavailable);
  if (review.kind === 'forced-single-move') return renderForcedSingleMove(candidate, review, props);
  if (review.kind === 'single-candidate-insight') return renderInsight(candidate, review.proof, props);
  return renderStructuredReview(candidate, review, props);
}

function renderForcedSingleMove(
  candidate: MoveReviewCandidate,
  review: Extract<MoveReviewCandidate['review'], { kind: 'forced-single-move' }>,
  props: MoveReviewPanelProps,
): VNode {
  const terminal = review.terminal;
  const terminalLabel = terminal
    ? terminal.kind === 'checkmate'
      ? `${props.copy.terminalLabels.checkmate} · ${props.copy.colorLabels[terminal.winner]}`
      : props.copy.terminalLabels[terminal.kind]
    : undefined;
  return hl(
    'section#move-review-selected-candidate.move-review__section',
    {
      attrs: {
        role: 'tabpanel',
        'aria-labelledby': `move-review-candidate-${candidate.uci}`,
      },
    },
    [
      hl('p.move-review__candidate-message', props.copy.forcedSingleMove),
      hl('p', [hl('code', review.lineUcis.join(' '))]),
      terminalLabel && hl('p', `${props.copy.terminalResult}: ${terminalLabel}`),
    ],
  );
}

function renderCandidateMessage(candidate: MoveReviewCandidate, message: string): VNode {
  return hl(
    'section#move-review-selected-candidate.move-review__section',
    {
      attrs: {
        role: 'tabpanel',
        'aria-labelledby': `move-review-candidate-${candidate.uci}`,
      },
    },
    [hl('p.move-review__candidate-message', message)],
  );
}

function renderInsight(
  candidate: MoveReviewCandidate,
  proof: MoveReviewProof,
  props: MoveReviewPanelProps,
): VNode {
  const expanded = props.view.expandedProofId === proof.id;
  const contentId = `move-review-proof-${proof.id}`;
  return hl(
    'section#move-review-selected-candidate.move-review__section',
    {
      attrs: {
        role: 'tabpanel',
        'aria-labelledby': `move-review-candidate-${candidate.uci}`,
      },
    },
    [
      hl('article.move-review__proof-entry', [
        hl(
          'button.move-review__proof-entry-button',
          {
            attrs: {
              type: 'button',
              'aria-expanded': expanded ? 'true' : 'false',
              ...(expanded ? { 'aria-controls': contentId } : {}),
            },
            on: { click: () => props.actions.toggleProof(proof.id) },
          },
          [
            hl('span.move-review__proof-entry-copy', [
              hl('strong', props.copy.lineInsight),
              hl('span', candidate.label),
            ]),
            hl('span.move-review__proof-entry-chevron', { attrs: { 'aria-hidden': 'true' } }, '⌄'),
          ],
        ),
        expanded && renderProof(proof, contentId, props),
      ]),
    ],
  );
}

function renderCandidates(
  candidates: MoveReviewCandidate[],
  selected: MoveReviewCandidate,
  props: MoveReviewPanelProps,
): VNode {
  const panelId = 'move-review-selected-candidate';
  return hl('section.move-review__section', [
    hl(
      'div.move-review__candidates',
      { attrs: { role: 'tablist', 'aria-label': props.copy.candidateMoves } },
      candidates.map((candidate, index) => {
        const active = candidate.uci === selected.uci;
        return hl(
          'button.move-review__candidate',
          {
            key: candidate.uci,
            attrs: {
              type: 'button',
              role: 'tab',
              id: `move-review-candidate-${candidate.uci}`,
              'aria-selected': active ? 'true' : 'false',
              'aria-controls': panelId,
              tabindex: active ? '0' : '-1',
            },
            on: {
              click: () => props.actions.selectCandidate(candidate.uci),
              keydown: event => moveCandidateSelection(event, index, candidates, props.actions),
            },
          },
          [
            hl('span.move-review__candidate-role', candidateRoleLabel(candidate.roles, props.copy)),
            hl('span.move-review__candidate-san', candidate.label),
            candidate.winPercent !== undefined &&
              hl(
                'span.move-review__candidate-win',
                formatMoveReviewPercent(candidate.winPercent, props.locale),
              ),
          ],
        );
      }),
    ),
  ]);
}

function moveCandidateSelection(
  event: KeyboardEvent,
  index: number,
  candidates: MoveReviewCandidate[],
  actions: MoveReviewPanelActions,
): void {
  let target: number | undefined;
  if (event.key === 'ArrowLeft' || event.key === 'ArrowUp')
    target = (index - 1 + candidates.length) % candidates.length;
  else if (event.key === 'ArrowRight' || event.key === 'ArrowDown') target = (index + 1) % candidates.length;
  else if (event.key === 'Home') target = 0;
  else if (event.key === 'End') target = candidates.length - 1;
  if (target === undefined) return;
  event.preventDefault();
  const candidate = candidates[target];
  const tabs = (event.currentTarget as HTMLElement).parentElement?.querySelectorAll<HTMLElement>(
    '[role="tab"]',
  );
  tabs?.[target]?.focus();
  actions.selectCandidate(candidate.uci);
}

function candidateRoleLabel(roles: MoveReviewCandidateRole[], copy: MoveReviewCopy): string {
  return roles.map(role => copy[role]).join(' + ');
}

type MoveReviewVerdictReview = Extract<MoveReviewCandidateReview, { kind: 'move-verdict' }>;

function renderStructuredReview(
  candidate: MoveReviewCandidate,
  review: MoveReviewVerdictReview,
  props: MoveReviewPanelProps,
): VNode {
  const panelId = 'move-review-selected-candidate';
  return hl(
    `section#${panelId}.move-review__section`,
    {
      attrs: {
        role: 'tabpanel',
        'aria-labelledby': `move-review-candidate-${candidate.uci}`,
      },
    },
    review.comparisonProofs.map(proof =>
      renderAssessmentProof(
        proof,
        proof.moves[0]?.uci === candidate.uci
          ? candidate.label
          : `${props.copy.runnerUp} ${proof.moves[0]?.label}`,
        props,
      ),
    ),
  );
}

function renderAssessmentProof(proof: MoveReviewProof, label: string, props: MoveReviewPanelProps): VNode {
  const expanded = props.view.expandedProofId === proof.id;
  const contentId = `move-review-proof-${proof.id}`;
  return hl('article.move-review__proof-entry', { key: proof.id }, [
    hl(
      'button.move-review__proof-entry-button',
      {
        attrs: {
          type: 'button',
          'aria-expanded': expanded ? 'true' : 'false',
          ...(expanded ? { 'aria-controls': contentId } : {}),
        },
        on: { click: () => props.actions.toggleProof(proof.id) },
      },
      [
        hl('span.move-review__proof-entry-copy', [
          hl('strong', props.copy.lineInsight),
          hl('span.move-review__proof-entry-summary', [hl('code', label)]),
        ]),
        hl('span.move-review__proof-entry-chevron', { attrs: { 'aria-hidden': 'true' } }, '⌄'),
      ],
    ),
    expanded && renderProof(proof, contentId, props),
  ]);
}

function renderObservations(props: MoveReviewPanelProps): VNode | undefined {
  const observations = moveReviewObservations(props.job);
  if (!observations?.paths.length) return;
  const selected =
    observations.paths.find(path => path.id === props.view.expandedProofId) ??
    observations.paths.find(path => path.id === props.job.observationFrame?.proofId) ??
    (props.job.observationFrame !== null
      ? (observations.paths.find(path => path.kind === 'played') ?? observations.paths[0])
      : undefined);
  return hl('section.move-review__observations', [
    hl('h3', props.copy.observationsTitle),
    ...(observations.inputIssues ?? []).map(issue =>
      hl(
        'p.move-review__notice',
        props.copy.inputIssueText[issue.reason]
          .replace('{line}', String(issue.inputLineIndex + 1))
          .replace('{ply}', String(issue.acceptedPlyCount)),
      ),
    ),
    hl(
      'div.move-review__observation-paths',
      observations.paths.map((path, index) =>
        hl(
          'button',
          {
            key: path.id,
            attrs: {
              type: 'button',
              'aria-pressed': path.id === selected?.id ? 'true' : 'false',
              title: path.moves.map(move => move.label).join(' '),
            },
            on: {
              click: () => {
                props.actions.clearPin();
                props.actions.clearPreview();
                props.actions.toggleProof(path.id);
              },
            },
          },
          `${path.sources?.map(source => props.copy.lineSourceLabels[source]).join(' · ') ?? props.copy.pathLabels[path.kind]} ${index + 1}: ${path.moves
            .slice(0, 6)
            .map(move => move.label)
            .join(' ')}${path.moves.length > 6 ? ' …' : ''}`,
        ),
      ),
    ),
    selected
      ? renderProof(
          selected,
          undefined,
          { ...props, canWrite: props.canWrite && selected.kind !== 'history' },
          observations,
        )
      : hl('p.move-review__notice', props.copy.observationsMissing),
  ]);
}

function observationText(statement: MoveReviewObservation, copy: MoveReviewCopy): string {
  if (statement.kind === 'piece_control') {
    return (['before', 'after'] as const)
      .map(phase => {
        const point = statement[phase];
        return `${copy.frameLabels[phase]}: ${pieceText(point.controller, copy)} → ${pieceText(point.target, copy)} · ${copy.controlRelationLabels[statement.relation]}: ${copy.controlStateLabels[point.state]}.`;
      })
      .join(' ');
  }
  const fields = copy.observationFields;
  const list = (values: string[]): string => (values.length ? values.join('; ') : fields.none);
  const pieces = (values: Pick<ObservedPiece, 'role' | 'square'>[]): string =>
    list(values.map(piece => pieceText(piece, copy)));
  const responseText = (
    response: Extract<MoveReviewObservation, { kind: 'check_response' }>['response'],
  ): string =>
    `${legalResourceText(response.resource, copy)} (${response.modes.map(mode => copy.checkModeLabels[mode]).join(' + ')})`;
  if (statement.kind === 'recapture_resources')
    return [
      `${fields.captured}: ${pieceText(statement.captured, copy)}`,
      `${fields.geometric}: ${pieces(statement.geometric_recapturers)}`,
      `${fields.legal}: ${list(statement.legal_recaptures.map(resource => legalResourceText(resource, copy)))}`,
      `${fields.restricted}: ${list(
        statement.restricted_recaptures.map(
          restriction =>
            copy.illegalRecaptureText
              .replace('{piece}', pieceText(restriction.piece, copy))
              .replace('{square}', restriction.resource.destination)
              .replace('{controllers}', pieces(restriction.post_move_controllers))
              .replace('{king}', restriction.king_square) +
            ` (${movementResourceText(restriction.resource, copy)}); ${fields.pins}: ${list(
              restriction.absolute_pin_paths.map(
                path =>
                  `${pieceText(path.pinner, copy)} → ${pieceText(path.pinned, copy)} → ${fields.king} ${path.king_square} (${path.geometry.squares.join(' → ')})`,
              ),
            )}`,
        ),
      )}`,
    ].join('. ');
  if (statement.kind === 'check_responses')
    return [
      observationSummary(statement, copy),
      `${fields.legal}: ${list(statement.responses.map(responseText))}`,
      `${fields.controlledDestinations}: ${list(
        statement.controlled_king_destinations.map(
          destination =>
            `${movementResourceText(destination.resource, copy)} ← ${pieces(destination.controllers)}`,
        ),
      )}`,
    ].join('. ');
  if (statement.kind === 'check_response')
    return [
      `${copy.colorLabels[statement.responding_side]} ${fields.king} ${statement.king_square}`,
      `${fields.checkers}: ${pieces(statement.checkers)}`,
      responseText(statement.response),
    ].join('. ');
  if (statement.kind === 'pawn_structure')
    return [
      statement.changed_facets.map(facet => copy.pawnFacetLabels[facet]).join(', '),
      ...(['before', 'after'] as const).map(
        phase => `${copy.frameLabels[phase]}: ${pawnStateText(statement[phase], copy)}`,
      ),
    ].join('. ');
  if (statement.kind === 'structural_change')
    return statement.changes
      .map(
        change =>
          `${copy.structuralChangeLabels[change.kind]}: ${list(change.subjects.map(subject => structuralSubjectText(subject, copy)))}` +
          (change.targets.length
            ? `; ${fields.relatedTargets}: ${list(change.targets.map(target => structuralSubjectText(target, copy)))}`
            : ''),
      )
      .join('. ');
  if (statement.kind === 'stalemate')
    return copy.stalemateText
      .replace('{side}', copy.colorLabels[statement.stalled_side])
      .replace('{square}', statement.king_square);
  if (statement.kind === 'castling')
    return `${copy.observationLabels.castling} · ${copy.colorLabels[statement.side]} ${copy.roleLabels.king} ${statement.from} → ${statement.to}; ${copy.roleLabels.rook} ${statement.rook.from} → ${statement.rook.to}.`;
  if (statement.kind === 'promotion')
    return `${copy.observationLabels.promotion} · ${copy.colorLabels[statement.side]} ${copy.roleLabels.pawn} ${statement.from} → ${copy.roleLabels[statement.promoted_role]} ${statement.to}.`;
  const values: Record<string, string> = {
    side: copy.colorLabels[statement.side],
    role: copy.roleLabels[statement.role],
    from: statement.from,
    to: statement.to,
  };
  let text: string;
  if (statement.kind === 'capture') {
    values.square = statement.capturedSquare;
    values.captured = copy.roleLabels[statement.capturedRole];
    text = copy.captureText;
    if (statement.recapture === 'proven') text += ` ${copy.recaptureText}`;
    if (statement.recapture === 'unknown') text += ` ${copy.recaptureUnknown}`;
  } else if (statement.kind === 'development') {
    text = copy.developmentText;
    if (statement.centralSquares.length) {
      values.squares = statement.centralSquares.join(', ');
      text += ` ${copy.centralControlText}`;
    }
  } else {
    const unhandled: never = statement;
    return unhandled;
  }
  return text.replace(/\{(\w+)\}/g, (_, key: string) => values[key] ?? '');
}

function pieceText(
  piece: (Pick<ObservedPiece, 'role' | 'square'> & { side?: Color }) | null,
  copy: MoveReviewCopy,
): string {
  return piece
    ? `${piece.side ? `${copy.colorLabels[piece.side]} ` : ''}${copy.roleLabels[piece.role]} ${piece.square}`
    : copy.removedPiece;
}

function legalResourceText(resource: LegalResource, copy: MoveReviewCopy): string {
  const m = resource.movement;
  return (
    `${copy.colorLabels[m.side]} ${copy.roleLabels[m.before_role]} ${m.from} → ${copy.roleLabels[m.after_role]} ${m.to} (${resource.move_uci})` +
    (resource.capture ? ` · ${copy.observationFields.captured}: ${pieceText(resource.capture, copy)}` : '')
  );
}

function movementResourceText(resource: MovementResource, copy: MoveReviewCopy): string {
  return (
    `${resource.destination} · ${copy.movementModeLabels[resource.mode]} · ${copy.observationFields[resource.target.kind]}` +
    (resource.target.kind === 'empty' ? '' : ` ${copy.roleLabels[resource.target.role]}`) +
    (resource.mode === 'en_passant_capture'
      ? ` · ${copy.observationFields.captured}: ${pieceText(resource.captured_pawn, copy)}`
      : '')
  );
}

function pawnStateText(pawn: PawnState | null, copy: MoveReviewCopy): string {
  if (!pawn) return copy.removedPiece;
  const fields = copy.observationFields;
  return [
    `${copy.colorLabels[pawn.side]} ${copy.roleLabels.pawn} ${pawn.square}`,
    ...(['doubled', 'isolated', 'passed', 'geometrically_protected_passer'] as const).map(
      flag => `${copy.pawnFacetLabels[flag]}: ${pawn[flag] ? fields.yes : fields.no}`,
    ),
    `${fields.frontSquare}: ${pawn.front_square ?? fields.none}`,
    `${fields.frontOccupant}: ${pawn.front_occupant ? pieceText(pawn.front_occupant, copy) : fields.empty}`,
    `${fields.component}: ${pawn.component_pawns.join(', ') || fields.none}`,
    `${fields.connections}: ${pawn.connections.map(connection => `${copy.pawnConnectionLabels[connection.direction]} ${connection.peer}`).join('; ') || fields.none}`,
    `${fields.enemyPawnContacts}: ${pawn.enemy_pawn_contacts.join(', ') || fields.none}`,
  ].join(' · ');
}

function structuralSubjectText(subject: StructuralSubject, copy: MoveReviewCopy): string {
  return (
    `${'side' in subject ? `${copy.colorLabels[subject.side]} ` : ''}${copy.structuralSubjectLabels[subject.kind]} ` +
    ('file' in subject
      ? subject.file
      : 'square' in subject
        ? subject.square
        : `${subject.from} → ${subject.to}`) +
    ('relative_rank' in subject ? ` (${copy.observationFields.relativeRank}: ${subject.relative_rank})` : '')
  );
}

function observationSummary(statement: MoveReviewObservation, copy: MoveReviewCopy): string | undefined {
  if (statement.kind === 'piece_control') {
    const before = statement.before.state;
    const after = statement.after.state;
    const change =
      before === 'absent' && after === 'present'
        ? 'created'
        : before === 'present' && after === 'present'
          ? 'retained'
          : before === 'present' && after === 'absent'
            ? 'lost'
            : before === 'present' && after === 'removed'
              ? 'removed'
              : undefined;
    const status = change
      ? copy.controlChangeLabels[change]
      : `${copy.frameLabels.before}: ${copy.controlStateLabels[before]}; ${copy.frameLabels.after}: ${copy.controlStateLabels[after]}`;
    // A capture can move the controller onto the target's former square. Never
    // combine that after-controller with the removed target's before-location.
    const point = after === 'removed' ? statement.before : statement.after;
    return `${pieceText(point.controller, copy)} → ${pieceText(point.target, copy)} · ${copy.controlRelationLabels[statement.relation]} ${status}.`;
  }
  if (statement.kind === 'recapture_resources')
    return `${copy.observationLabels[statement.kind]} · ${copy.observationFields.legal}: ${statement.legal_recaptures.length} · ${copy.observationFields.geometric}: ${statement.geometric_recapturers.length} · ${copy.observationFields.restricted}: ${statement.restricted_recaptures.length}`;
  if (statement.kind === 'check_responses')
    return `${copy.colorLabels[statement.checked_side]} ${copy.observationFields.king} ${statement.king_square} · ${copy.observationFields.checkers}: ${statement.checkers.map(piece => pieceText(piece, copy)).join(', ')} · ${statement.terminal === 'checkmate' ? copy.terminalLabels.checkmate : `${copy.observationFields.ongoing} · ${copy.observationFields.legal}: ${statement.responses.length}`}`;
  if (statement.kind === 'check_response')
    return `${copy.observationLabels[statement.kind]} · ${statement.response.resource.move_uci} · ${statement.response.modes.map(mode => copy.checkModeLabels[mode]).join(' + ')}`;
  return;
}

function observationPhases(statement: MoveReviewObservation): ('before' | 'after')[] {
  return 'phase' in statement && statement.phase !== 'transition' ? [statement.phase] : ['before', 'after'];
}

function renderLineSummary(
  proof: MoveReviewProof,
  summary: ObservedLineSummary | undefined,
  props: MoveReviewPanelProps,
): VNode {
  const labels = props.copy.lineLabels;
  if (!summary)
    return hl('details.move-review__line-summary', { key: proof.id }, [
      hl('summary', labels.title),
      hl('p', labels.missingSummary),
    ]);
  const material = summary.material;
  const amount = (cp: number): string =>
    `${cp > 0 ? '+' : ''}${(cp / 100).toLocaleString(props.locale, { maximumFractionDigits: 2 })}`;
  const eventTexts = new Map<number, string[]>();
  for (const event of summary.events) {
    const template = event.kind === 'capture' ? props.copy.captureText : props.copy.lineEventText[event.kind];
    const values: Record<string, string | undefined> = {
      side: event.side && props.copy.colorLabels[event.side],
      role: event.piece_role && props.copy.roleLabels[event.piece_role],
      captured: event.target_role && props.copy.roleLabels[event.target_role],
      square: event.square,
    };
    let missing = false;
    const sentence = template.replace(/\{(\w+)\}/g, (_, key: string) => {
      if (values[key] === undefined) missing = true;
      return values[key] ?? '';
    });
    const text = missing ? `${props.copy.lineEventLabels[event.kind]}.` : sentence;
    const entries = eventTexts.get(event.ply_offset) ?? [];
    entries.push(text);
    eventTexts.set(event.ply_offset, entries);
  }
  const materialEvents = new Map(material?.events.map(event => [event.ply_offset, event]));
  const offsets = [...new Set([...eventTexts.keys(), ...materialEvents.keys()])].sort((a, b) => a - b);
  const eventButton = (offset: number, text: string): VNode => {
    const selection = { proofId: proof.id, ply: offset + 1 };
    return hl(
      'button.move-review__line-event',
      {
        attrs: { type: 'button' },
        on: {
          mouseenter: () => props.actions.previewFrame(selection),
          focus: () => props.actions.previewFrame(selection),
          blur: props.actions.clearPreview,
          click: () => props.actions.pinFrame(selection),
        },
      },
      `${proofFrameLabel(proof, offset + 1, props.copy)} · ${text}`,
    );
  };
  return hl('details.move-review__line-summary', { key: proof.id }, [
    hl(
      'summary',
      `${labels.title}${material ? ` · ${props.copy.colorLabels[summary.perspective]} ${labels.material}: ${amount(material.net_cp)}` : ''}`,
    ),
    hl('p', labels.units.replace('{side}', props.copy.colorLabels[summary.perspective])),
    material
      ? hl('div', [
          hl(
            'p',
            `${labels.net}: ${amount(material.net_cp)} · ${labels.minimum}: ${amount(material.max_loss_cp)} · ${labels.maximum}: ${amount(material.max_gain_cp)}`,
          ),
          material.recovered_after_loss && hl('p', labels.recovered),
          material.events.length === 0 && hl('p', labels.noMaterialEvents),
        ])
      : hl('p', labels.missingMaterial),
    hl(
      'ol',
      { on: { mouseleave: props.actions.clearPreview } },
      offsets.map(offset => {
        const eventText = eventTexts.get(offset)?.join(' ');
        const event = materialEvents.get(offset);
        return hl('li', { key: offset }, [
          eventButton(offset, eventText || labels.material),
          event &&
            hl(
              'p',
              `${props.copy.colorLabels[event.side]} ${props.copy.roleLabels[event.before_role]} ${event.from} → ${props.copy.roleLabels[event.after_role]} ${event.to} · ${labels.delta}: ${amount(event.delta_cp)} · ${labels.balance}: ${amount(event.balance_cp)}`,
            ),
          event?.capture &&
            hl(
              'p',
              `${props.copy.observationFields.captured}: ${props.copy.roleLabels[event.capture.role]} ${event.capture.square} · ${labels.value}: ${amount(event.capture.value_cp)} · ${props.copy.lineEventLabels.recapture}: ${event.capture.recapture === 'unknown' ? props.copy.controlStateLabels.unknown : props.copy.observationFields[event.capture.recapture === 'proven' ? 'yes' : 'no']}`,
            ),
          event &&
            event.promotion_gain_cp > 0 &&
            hl('p', `${labels.promotionGain}: ${amount(event.promotion_gain_cp)}`),
        ]);
      }),
    ),
    eventButton(summary.ending.ply_offset, labels[summary.ending.kind]),
    hl('p', labels.limit),
  ]);
}

function renderPositionResources(
  resources: PositionResources | undefined,
  frame: MoveReviewFrameSelection,
  props: MoveReviewPanelProps,
): VNode {
  const labels = props.copy.resourceLabels;
  const selected = frame.inspectSquare;
  const setup = resources && selected ? parseFen(resources.fen).unwrap() : undefined;
  const selectedPiece = selected && setup?.board.get(parseSquare(selected)!);
  const legalMoves = (from: Key, destination?: Key) =>
    resources!.legal_moves.moves.filter(
      move => move.uci.slice(0, 2) === from && (!destination || move.destination === destination),
    );
  const legality = (side: Color, moves: unknown[]): 'legal' | 'illegal' | 'not_turn' | 'unknown' =>
    moves.length
      ? 'legal'
      : side !== setup!.turn
        ? 'not_turn'
        : resources!.legal_moves.complete
          ? 'illegal'
          : 'unknown';
  const selectedMoves = selected && resources ? legalMoves(selected) : [];
  const controls = resources?.controls.pieces.filter(entry => entry.squares.includes(selected!));
  const outgoing = resources?.controls.pieces.find(entry => entry.piece.square === selected);
  const pawn = resources?.pawn_states.pawns.find(entry => entry.square === selected);
  const pawnFile = resources?.pawn_files.files.find(entry => entry.file === selected?.[0]);
  const rays = resources?.rays.lines.filter(
    entry => entry.piece.square === selected || entry.squares.includes(selected!),
  );
  const advances = resources?.pawn_advances.paths.filter(
    entry => entry.piece.square === selected || entry.traversed_squares.includes(selected!),
  );
  const section = (title: string, complete: boolean, rows: string[]): VNode =>
    hl('section', [
      hl('h4', title),
      rows.length
        ? hl(
            'ul',
            rows.map(text => hl('li', text)),
          )
        : complete && hl('p', labels.none),
      !complete && hl('p.move-review__resource-unknown', labels.partial),
    ]);
  return hl('div.move-review__position-resources', [
    hl('label', [
      labels.square,
      hl(
        'select',
        {
          attrs: { 'aria-label': labels.square },
          on: {
            change: event =>
              props.actions.pinFrame({
                ...frame,
                observationKey: undefined,
                highlightKey: undefined,
                inspectSquare: ((event.target as HTMLSelectElement).value as Key) || undefined,
              }),
          },
        },
        [
          hl('option', { attrs: { value: '', selected: !selected } }, labels.choose),
          ...Array.from({ length: 64 }, (_, i) => `${'abcdefgh'[i % 8]}${Math.floor(i / 8) + 1}` as Key).map(
            square => hl('option', { attrs: { value: square, selected: square === selected } }, square),
          ),
        ],
      ),
    ]),
    hl(
      'div.move-review__resource-frames',
      { on: { mouseleave: props.actions.clearPreview } },
      (['before', 'after'] as const).map(phase => {
        const selection = { ...frame, phase, observationKey: undefined, highlightKey: undefined };
        return hl(
          'button',
          {
            attrs: { type: 'button', 'aria-pressed': (frame.phase ?? 'after') === phase ? 'true' : 'false' },
            on: {
              mouseenter: () => props.actions.previewFrame(selection),
              focus: () => props.actions.previewFrame(selection),
              blur: props.actions.clearPreview,
              click: () => props.actions.pinFrame(selection),
            },
          },
          props.copy.frameLabels[phase],
        );
      }),
    ),
    selected &&
      (resources
        ? hl('div', { attrs: { 'data-position-id': resources.id } }, [
            ...(selectedPiece
              ? [
                  section(
                    labels.friendlySupport,
                    resources.controls.complete,
                    controls!
                      .filter(entry => entry.piece.side === selectedPiece.color)
                      .map(entry => pieceText(entry.piece, props.copy)),
                  ),
                  section(
                    labels.opposingControl,
                    resources.controls.complete,
                    controls!
                      .filter(entry => entry.piece.side !== selectedPiece.color)
                      .map(entry => pieceText(entry.piece, props.copy)),
                  ),
                ]
              : [
                  section(
                    labels.controls,
                    resources.controls.complete,
                    controls!.map(entry => pieceText(entry.piece, props.copy)),
                  ),
                ]),
            selectedPiece?.role === 'pawn' &&
              section(
                labels.pawnState,
                !!pawn && [pawn.passed, pawn.isolated, pawn.doubled].every(value => value !== null),
                pawn
                  ? (['passed', 'isolated', 'doubled'] as const).map(
                      facet =>
                        `${props.copy.pawnFacetLabels[facet]}: ${pawn[facet] === null ? labels.unconfirmed : props.copy.observationFields[pawn[facet] ? 'yes' : 'no']}`,
                    )
                  : [],
              ),
            selectedPiece?.role === 'rook' &&
              section(
                labels.rookFile,
                !!pawnFile,
                pawnFile
                  ? [
                      `${pawnFile.file} · ${
                        pawnFile.occupancy === 'open'
                          ? labels.openFile
                          : pawnFile.occupancy ===
                              (selectedPiece.color === 'white' ? 'black_pawns_only' : 'white_pawns_only')
                            ? labels.semiOpenFile
                            : labels.ownPawnFile
                      }`,
                    ]
                  : [],
              ),
            section(
              labels.rays,
              resources.rays.complete,
              rays!.map(
                entry =>
                  `${pieceText(entry.piece, props.copy)} · ${labels.path}: ${entry.squares.join(' – ')}. ` +
                  `${labels.first}: ${entry.occupants[0] ? pieceText(entry.occupants[0], props.copy) : labels.none}. ` +
                  `${labels.behind}: ${
                    entry.occupants
                      .slice(1)
                      .map(piece => pieceText(piece, props.copy))
                      .join(', ') || labels.none
                  }.`,
              ),
            ),
            section(
              labels.advances,
              resources.pawn_advances.complete,
              advances!.map(entry => {
                const moves = legalMoves(entry.piece.square, entry.destination);
                return (
                  `${pieceText(entry.piece, props.copy)} · ${labels.path}: ${entry.piece.square} – ${entry.traversed_squares.join(' – ')} · ${labels[legality(entry.piece.side, moves)]}` +
                  (moves.length ? ` (${moves.map(move => move.uci).join(', ')})` : '')
                );
              }),
            ),
            selectedPiece &&
              section(
                labels.outgoingControls,
                resources.controls.complete,
                outgoing?.squares.length ? [outgoing.squares.join(', ')] : [],
              ),
            selectedPiece &&
              hl('section', [
                hl('h4', labels.legalMoves),
                selectedMoves.length
                  ? hl(
                      'ul',
                      selectedMoves.map(move =>
                        hl(
                          'li',
                          `${move.destination} (${move.uci})` +
                            (move.captured_square ? ` · ${labels.capturedOn}: ${move.captured_square}` : ''),
                        ),
                      ),
                    )
                  : hl(
                      'p',
                      resources.legal_moves.complete && selectedPiece.color === setup!.turn
                        ? labels.noLegalMoves
                        : labels[legality(selectedPiece.color, selectedMoves)],
                    ),
                selectedPiece.color === setup!.turn &&
                  !resources.legal_moves.complete &&
                  hl('p.move-review__resource-unknown', labels.partial),
              ]),
          ])
        : hl('p.move-review__resource-unknown', labels.unavailable)),
  ]);
}

function renderProof(
  proof: MoveReviewProof,
  contentId: string | undefined,
  props: MoveReviewPanelProps,
  observations?: MoveReviewObservations,
): VNode {
  const frame = activeFrame(proof, props.view, observations, props.job.observationFrame);
  const pinned = props.view.pinnedFrame;
  const path = observations?.paths.find(path => path.id === proof.id);
  const occurrenceId = path?.occurrenceIds[frame.ply - 1];
  const statements = observations?.statements.flatMap((statement, index) =>
    statement.occurrenceId === occurrenceId ? [{ statement, key: observationKey(statement, index) }] : [],
  );
  const selectedStatement = statements?.find(item => item.key === frame.observationKey)?.statement;
  const highlights = observations?.highlights?.filter(item => item.occurrenceId === occurrenceId) ?? [];
  const board = proofBoard(proof, frame, selectedStatement);
  const highlightFrame = highlights
    .find(item => JSON.stringify(item) === frame.highlightKey)
    ?.frames?.find(item => item.phase === frame.phase);
  const positionId = path?.positionIds?.[frame.ply - (frame.phase === 'before' ? 1 : 0)];
  const resources = observations?.positions?.find(
    position => position.id === positionId && position.fen === board.fen,
  );
  if (highlightFrame) {
    const sources = new Set(highlightFrame.sourceIds);
    board.shapes = [
      ...highlightFrame.squares.map(orig => ({ orig, brush: 'blue' })),
      ...(resources?.controls.pieces ?? []).flatMap(entry =>
        entry.squares.flatMap((dest, index) =>
          sources.has(entry.source_ids[index]) ? [{ orig: entry.piece.square, dest, brush: 'blue' }] : [],
        ),
      ),
    ];
  }
  if (frame.inspectSquare) {
    board.shapes = [
      { orig: frame.inspectSquare, brush: 'blue' },
      ...(resources?.controls.pieces.filter(entry => entry.squares.includes(frame.inspectSquare!)) ?? []).map(
        entry => ({ orig: entry.piece.square, dest: frame.inspectSquare, brush: 'blue' }),
      ),
    ];
  }
  const grouped = (statement: MoveReviewObservation): boolean =>
    statement.kind === 'piece_control' ||
    statement.kind === 'pawn_structure' ||
    statement.kind === 'structural_change';
  const visible = (statements ?? []).filter(
    item => item.statement.kind !== 'structural_change' || item.statement.changes.length > 0,
  );
  const details = visible.filter(item => grouped(item.statement) && item.statement.kind !== 'piece_control');
  const controllers = new Map<string, { label: string; items: typeof visible }>();
  for (const item of visible) {
    if (item.statement.kind !== 'piece_control') continue;
    const { before, after } = item.statement;
    const controller = before.controller ?? after.controller!;
    const key = `${controller.side}:${controller.square}:${controller.role}`;
    const group = controllers.get(key) ?? { label: pieceText(controller, props.copy), items: [] };
    group.items.push(item);
    controllers.set(key, group);
  }
  const rows = (items: NonNullable<typeof statements>): VNode[] =>
    items.map(({ statement, key }) => {
      const text = observationText(statement, props.copy);
      const summary =
        statement.kind === 'piece_control' || !grouped(statement)
          ? observationSummary(statement, props.copy)
          : undefined;
      return hl(`${summary ? 'details' : 'div'}.move-review__observation`, { key }, [
        summary && hl('summary', summary),
        hl('p', text),
        hl(
          'div.move-review__observation-frames',
          observationPhases(statement).map(phase => {
            const selection = { proofId: proof.id, ply: frame.ply, phase, observationKey: key };
            const isActive = frame.observationKey === key && frame.phase === phase;
            const isPinned =
              pinned?.proofId === proof.id &&
              pinned.ply === frame.ply &&
              pinned.observationKey === key &&
              pinned.phase === phase;
            return hl(
              'button.move-review__observation-frame',
              {
                key: phase,
                class: { 'is-previewed': isActive && !isPinned },
                attrs: {
                  type: 'button',
                  'aria-pressed': isPinned ? 'true' : 'false',
                  'aria-label': `${props.copy.frameLabels[phase]}: ${text}`,
                },
                on: {
                  mouseenter: () => props.actions.previewFrame(selection),
                  focus: () => props.actions.previewFrame(selection),
                  blur: props.actions.clearPreview,
                  click: () => props.actions.pinFrame(selection),
                },
              },
              props.copy.frameLabels[phase],
            );
          }),
        ),
      ]);
    });
  const added = props.addedProofId === proof.id;
  return hl(
    'div.move-review__proof',
    {
      attrs: { ...(contentId ? { id: contentId } : {}), 'aria-keyshortcuts': 'Escape' },
      on: {
        keydown: event => {
          if (event.key !== 'Escape') return;
          event.preventDefault();
          event.stopPropagation();
          props.actions.clearPreview();
          props.actions.clearPin();
        },
      },
    },
    [
      hl('div.move-review__proof-copy', [
        highlights.length > 0 &&
          hl('section.move-review__work', { on: { mouseleave: props.actions.clearPreview } }, [
            hl('h4', props.copy.workTitle),
            ...highlights.map(item => {
              const highlightKey = item.frames ? JSON.stringify(item) : undefined;
              const phase = item.frames ? (frame.phase ?? 'after') : 'after';
              const squares = item.frames
                ? (item.frames.find(evidence => evidence.phase === phase)?.squares ?? [])
                : item.squares;
              const buttons: { phase: 'before' | 'after'; inspectSquare?: Key }[] = [
                ...(item.frames?.map(({ phase }) => ({ phase })) ?? []),
                ...squares.map(inspectSquare => ({ phase, inspectSquare })),
              ];
              return hl('div.move-review__observation', { key: item.text }, [
                hl('p', item.text),
                hl(
                  'div.move-review__observation-frames',
                  buttons.map(({ phase, inspectSquare }) => {
                    const selection = {
                      proofId: proof.id,
                      ply: frame.ply,
                      phase,
                      ...(highlightKey ? { highlightKey } : {}),
                      ...(inspectSquare ? { inspectSquare } : {}),
                    };
                    const label = `${props.copy.frameLabels[phase]}${inspectSquare ? ` · ${inspectSquare}` : ''}`;
                    return hl(
                      inspectSquare ? 'button' : 'button.move-review__highlight-frame',
                      {
                        class: { 'move-review__observation-frame': true },
                        attrs: {
                          type: 'button',
                          'aria-label': `${label}: ${item.text}`,
                          'aria-pressed':
                            pinned?.proofId === proof.id &&
                            pinned.ply === frame.ply &&
                            pinned.phase === phase &&
                            pinned.highlightKey === highlightKey &&
                            pinned.inspectSquare === inspectSquare
                              ? 'true'
                              : 'false',
                        },
                        on: {
                          mouseenter: () => props.actions.previewFrame(selection),
                          focus: () => props.actions.previewFrame(selection),
                          blur: props.actions.clearPreview,
                          click: () => props.actions.pinFrame(selection),
                        },
                      },
                      inspectSquare ?? props.copy.frameLabels[phase],
                    );
                  }),
                ),
              ]);
            }),
          ]),
        statements &&
          hl('div.move-review__observation-statements', { on: { mouseleave: props.actions.clearPreview } }, [
            ...rows(visible.filter(item => !grouped(item.statement))),
            ...Array.from(controllers.entries())
              .sort(([a], [b]) => a.localeCompare(b))
              .map(([key, group]) =>
                hl('details.move-review__controller', { key }, [
                  hl(
                    'summary',
                    `${group.label} · ${props.copy.resourceLabels.relationships} (${group.items.length})`,
                  ),
                  ...rows(group.items),
                ]),
              ),
            details.length > 0 &&
              hl('details.move-review__observation-details', { key: 'details' }, [
                hl('summary', props.copy.observationDetails.replace('{count}', String(details.length))),
                ...rows(details),
              ]),
          ]),
        hl(
          'div.move-review__proof-moves',
          { on: { mouseleave: props.actions.clearPreview } },
          renderProofMoves(proof, frame, pinned, props),
        ),
        path && renderLineSummary(proof, path.lineSummary, props),
        props.canWrite &&
          !added &&
          hl('div.move-review__proof-actions', [
            hl(
              'button.button.button-thin.move-review__add',
              {
                attrs: { type: 'button' },
                on: { click: () => props.actions.addProof(proof.id) },
              },
              props.copy.addToStudy,
            ),
          ]),
        added &&
          hl('div.move-review__added', { attrs: { role: 'status', 'aria-live': 'polite' } }, [
            props.copy.addedToStudy,
            hl('div.move-review__added-actions', [
              hl(
                'button.button.button-thin.move-review__view-added',
                {
                  attrs: { type: 'button' },
                  on: { click: () => props.actions.viewAddedLine(proof.id) },
                },
                props.copy.viewAddedLine,
              ),
            ]),
          ]),
      ]),
      hl('figure.move-review__proof-visual', [
        hl(
          'figcaption',
          `${props.copy.proofBoard}: ${proofFrameLabel(proof, frame.ply, props.copy)} · ${props.copy.frameLabels[frame.phase ?? 'after']}`,
        ),
        renderBoardPreview(board, props.orientation, 'div.move-review__proof-board'),
      ]),
      observations && renderPositionResources(resources, frame, props),
    ],
  );
}

function renderProofMoves(
  proof: MoveReviewProof,
  active: MoveReviewFrameSelection,
  pinned: MoveReviewFrameSelection | undefined,
  props: MoveReviewPanelProps,
): VNode[] {
  return proof.moves.map((move, index) => {
    const ply = index + 1;
    const frame = { proofId: proof.id, ply };
    const isActive =
      active.ply === ply && active.phase !== 'before' && !active.observationKey && !active.highlightKey;
    const isPinned =
      pinned?.proofId === proof.id &&
      pinned.ply === ply &&
      pinned.phase !== 'before' &&
      !pinned.observationKey &&
      !pinned.highlightKey;
    const step = `${props.copy.proofStep} ${ply}`;
    return hl('span.move-review__proof-move', [
      hl('span.move-review__proof-index', String(ply)),
      hl(
        'button.move-review__proof-san',
        {
          key: `${proof.id}:${ply}`,
          class: { 'is-previewed': isActive && !isPinned },
          attrs: {
            type: 'button',
            'aria-pressed': isPinned ? 'true' : 'false',
            'aria-label': `${step}: ${move.label}`,
          },
          on: {
            mouseenter: () => props.actions.previewFrame(frame),
            focus: () => props.actions.previewFrame(frame),
            blur: props.actions.clearPreview,
            click: () => props.actions.pinFrame(frame),
          },
        },
        move.label,
      ),
    ]);
  });
}

function observationKey(statement: MoveReviewObservation, index: number): string {
  if (statement.kind === 'piece_control')
    return JSON.stringify([
      statement.occurrenceId,
      statement.kind,
      statement.relation,
      statement.before.controller ?? statement.after.controller,
      statement.before.target ?? statement.after.target,
      index,
    ]);
  // Keep repeated kinds (and even repeated statements) distinct; changed payloads invalidate old selections.
  return JSON.stringify([statement, index]);
}

function activeFrame(
  proof: MoveReviewProof,
  view: MoveReviewViewState,
  observations?: MoveReviewObservations,
  selected?: MoveReviewFrameSelection | null,
): MoveReviewFrameSelection {
  const path = observations?.paths.find(path => path.id === proof.id);
  const occurrenceIds = path?.occurrenceIds;
  const valid = (frame?: MoveReviewFrameSelection): frame is MoveReviewFrameSelection =>
    frame?.proofId === proof.id &&
    Number.isInteger(frame.ply) &&
    frame.ply >= 1 &&
    frame.ply <= proof.moves.length &&
    (frame.highlightKey === undefined ||
      !!observations?.highlights?.some(
        item =>
          item.occurrenceId === occurrenceIds?.[frame.ply - 1] &&
          JSON.stringify(item) === frame.highlightKey &&
          item.frames?.some(
            evidence =>
              evidence.phase === frame.phase &&
              evidence.positionId ===
                path?.positionIds?.[frame.ply - (evidence.phase === 'before' ? 1 : 0)] &&
              (frame.inspectSquare === undefined || evidence.squares.includes(frame.inspectSquare)),
          ),
      )) &&
    (frame.observationKey === undefined ||
      !!observations?.statements.some(
        (statement, index) =>
          statement.occurrenceId === occurrenceIds?.[frame.ply - 1] &&
          observationPhases(statement).includes(frame.phase ?? 'after') &&
          observationKey(statement, index) === frame.observationKey,
      ));
  if (valid(view.hoveredFrame)) return view.hoveredFrame;
  if (valid(view.pinnedFrame)) return view.pinnedFrame;
  if (selected && valid(selected)) return selected;
  return { proofId: proof.id, ply: 1 };
}

function proofBoard(
  proof: MoveReviewProof,
  frame: MoveReviewFrameSelection,
  statement?: MoveReviewObservation,
): { fen: FEN; uci?: Uci; shapes: DrawShape[] } {
  const before = frame.phase === 'before';
  const move = proof.moves[frame.ply - 1]!;
  const shapes: DrawShape[] = [];
  if (statement?.kind === 'piece_control') {
    const point = statement[before ? 'before' : 'after'];
    if (point.state === 'present' && point.controller && point.target) {
      const brush = statement.relation === 'support' ? 'green' : 'red';
      shapes.push({ orig: point.controller.square, dest: point.target.square, brush });
      shapes.push({ orig: point.target.square, brush });
    }
  } else if (statement?.kind === 'recapture_resources') {
    for (const piece of statement.geometric_recapturers)
      shapes.push({ orig: piece.square, dest: statement.to as Key, brush: 'blue' });
    for (const resource of statement.legal_recaptures)
      shapes.push({ orig: resource.movement.from, dest: resource.movement.to, brush: 'green' });
    for (const restricted of statement.restricted_recaptures)
      shapes.push({ orig: restricted.piece.square, brush: 'red' });
  } else if (statement?.kind === 'check_responses' || statement?.kind === 'check_response') {
    shapes.push({ orig: statement.king_square, brush: 'red' });
    for (const checker of statement.checkers)
      shapes.push({ orig: checker.square, dest: statement.king_square, brush: 'red' });
    if (statement.kind === 'check_response')
      shapes.push({
        orig: statement.response.resource.movement.from,
        dest: statement.response.resource.movement.to,
        brush: 'green',
      });
  } else if (statement?.kind === 'pawn_structure') {
    const pawn = statement[before ? 'before' : 'after'];
    if (pawn) {
      shapes.push({ orig: pawn.square, brush: 'blue' });
      for (const connection of pawn.connections) {
        if (connection.kind === 'phalanx') shapes.push({ orig: connection.peer, brush: 'blue' });
        else
          shapes.push({
            orig: connection.direction === 'supports' ? pawn.square : connection.peer,
            dest: connection.direction === 'supports' ? connection.peer : pawn.square,
            brush: 'green',
          });
      }
    }
  } else if (statement?.kind === 'stalemate') {
    shapes.push({ orig: statement.king_square, brush: 'blue' });
  } else if (statement?.kind === 'structural_change') {
    // Transition subjects do not all denote pieces on either individual board.
  } else if (statement) {
    shapes.push({ orig: (before ? statement.from : statement.to) as Key, brush: 'blue' });
    if (before && statement.kind === 'capture')
      shapes.push({ orig: statement.capturedSquare as Key, brush: 'red' });
    if (!before && statement.kind === 'development')
      for (const square of statement.centralSquares)
        shapes.push({ orig: statement.to as Key, dest: square as Key, brush: 'blue' });
    if (statement.kind === 'castling')
      shapes.push({ orig: before ? statement.rook.from : statement.rook.to, brush: 'blue' });
  }
  return before
    ? { fen: frame.ply === 1 ? proof.startFen : proof.moves[frame.ply - 2]!.fenAfter, shapes }
    : { fen: move.fenAfter, uci: move.uci, shapes };
}

function proofFrameLabel(proof: MoveReviewProof, ply: number, copy: MoveReviewCopy): string {
  return `${copy.proofStep} ${ply}: ${proof.moves[ply - 1]!.label}`;
}
