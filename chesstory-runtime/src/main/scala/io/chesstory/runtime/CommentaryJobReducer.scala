package io.chesstory.runtime

import java.util.Locale

import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.assembly.{ MoveReviewInputNormalizer, MoveReviewJudgmentOrchestrator, PreparedMoveReview, PreparedMoveReviewInventory, PreparedPositionCommentary }
import lila.chessjudgment.model.{ ProbeRequest, ProbeResolution, ProbeResult }
import lila.chessjudgment.model.judgment.{ EvidenceBackedJudgmentPacket, PlayerFacingPositionAction }
import lila.chessjudgment.model.line.{
  AutomaticTerminal,
  CandidateLineEvaluation,
  CanonicalPositionHistory,
  CanonicalPositionHistoryFailure,
  DeclaredDrawClaim,
  FivefoldRepetitionKnowledge,
  PositionRuleAssessment,
  ThreefoldClaimKnowledge
}
import lila.chessjudgment.model.strategic.EngineLine

final case class CommentaryJobPolicy(
    rootSearchDepth: Int,
    maximumIssuedEngineWorkCount: Int,
    deadlineEpochMs: Long,
    engineProfile: CommentaryEngineProfile
)

enum CommentaryJobStartRejection:
  case PositionHistoryInvalid(reason: CanonicalPositionHistoryFailure)
  case RootSearchDepthInvalid
  case MaximumIssuedEngineWorkCountInvalid

enum CommentaryJobStopCondition:
  case DeadlineExceeded
  case BudgetExhausted
  case Cancelled
  case EngineExecutionFailed
  case InvalidEngineWorkReport
  case ReviewConstructionFailed
  case MoveReviewPreparationFailed
  case RepetitionHistoryUnavailable

enum EngineWorkReportRejection:
  case WorkNotOutstanding

/** Browser ceval scores are always expressed from White's perspective. */
enum ReportedWhiteEngineScore:
  case Centipawns(value: Int)
  case Mate(value: Int)

final case class ReportedEngineLineSuffix(
    moves: List[String],
    whiteScore: ReportedWhiteEngineScore,
    depth: Int
)

enum EngineWorkReport:
  case Completed(
      engineProfile: CommentaryEngineProfile,
      workId: String,
      lineSuffixes: List[ReportedEngineLineSuffix]
  )
  case ExecutorFailed(engineProfile: CommentaryEngineProfile, workId: String)

  def reportedWorkId: String = this match
    case EngineWorkReport.Completed(_, value, _) => value
    case EngineWorkReport.ExecutorFailed(_, value) => value

  def reportedEngineProfile: CommentaryEngineProfile = this match
    case EngineWorkReport.Completed(value, _, _) => value
    case EngineWorkReport.ExecutorFailed(value, _) => value

/** Exact server-issued work. The browser searches searchFen and returns only suffixes. */
final case class IssuedEngineWork(
    engineProfile: CommentaryEngineProfile,
    workId: String,
    enginePositionInitialFen: String,
    enginePositionMovesUci: List[String],
    searchFen: String,
    commentaryLineOriginFen: String,
    commentaryLinePrefixUci: List[String],
    requiredDepth: Int,
    multiPv: Int
)

/** The sole admission boundary for engine-line limits and browser suffix binding. */
private[runtime] object EngineLineAdmission:
  private val MaximumFullLineMoves = 80
  private val MaximumDepth = 100
  private val MaximumCentipawns = 100000L
  private val MaximumMateDistance = 1000L
  private val MateScoreCpCarrier = 10000
  private val CanonicalUci = "[a-h][1-8][a-h][1-8][nbrq]?".r

  def acceptsRuntimeInputUci(value: String): Boolean =
    Option(value).exists(raw => CanonicalUci.matches(raw.trim.toLowerCase(Locale.ROOT)))

  def acceptsCanonicalLowercaseUci(value: String): Boolean =
    Option(value).exists(CanonicalUci.matches)

  def acceptsRuntimeInputDepth(depth: Int): Boolean =
    depth >= 0 && depth <= MaximumDepth

  def acceptsRequiredDepth(depth: Int): Boolean =
    depth >= 1 && depth <= MaximumDepth

  def acceptsMultiPv(multiPv: Int): Boolean =
    multiPv >= 1 && multiPv <= 3

  def acceptsIssuedWork(requiredDepth: Int, multiPv: Int): Boolean =
    acceptsRequiredDepth(requiredDepth) && acceptsMultiPv(multiPv)

  def acceptsRuntimeInputLine(line: EngineLine): Boolean =
    line.moves.nonEmpty && line.moves.size <= MaximumFullLineMoves &&
      line.moves.forall(acceptsRuntimeInputUci) &&
      acceptsCentipawnScore(line.scoreCp) &&
      line.mate.forall(acceptsMateScore) &&
      acceptsRuntimeInputDepth(line.depth)

  def acceptsReportedLineSuffixValues(
      lineSuffix: ReportedEngineLineSuffix,
      requiredDepth: Int
  ): Boolean =
      lineSuffix.moves.nonEmpty &&
      lineSuffix.moves.forall(acceptsCanonicalLowercaseUci) &&
      acceptsReportedWhiteEngineScore(lineSuffix.whiteScore) &&
      acceptsRequiredDepth(lineSuffix.depth) &&
      lineSuffix.depth >= requiredDepth

  def bindReportedLineSuffixes(
      issuedWork: IssuedEngineWork,
      lineSuffixes: List[ReportedEngineLineSuffix]
  ): Option[List[CandidateLineEvaluation]] =
    if !acceptsIssuedWork(issuedWork.requiredDepth, issuedWork.multiPv) ||
      lineSuffixes.size != issuedWork.multiPv
    then None
    else if !lineSuffixes.forall { lineSuffix =>
      acceptsReportedLineSuffixValues(lineSuffix, issuedWork.requiredDepth) &&
        issuedWork.commentaryLinePrefixUci.size + lineSuffix.moves.size <= MaximumFullLineMoves
    } then None
    else if issuedWork.multiPv > 1 &&
        lineSuffixes.map(_.moves.head).distinct.size != lineSuffixes.size
    then None
    else
      CanonicalPositionHistory
        .from(
          issuedWork.enginePositionInitialFen,
          issuedWork.enginePositionMovesUci,
          issuedWork.searchFen
        )
        .toOption
        .flatMap { engineHistory =>
          Option
            .when(PositionRuleAssessment.automaticTerminal(engineHistory.currentPosition).isEmpty)(engineHistory)
            .flatMap { nonterminalSearchHistory =>
              lineSuffixes.foldLeft(Option(List.empty[CandidateLineEvaluation])) { (bound, lineSuffix) =>
                bound.flatMap { evaluations =>
                    nonterminalSearchHistory
                      .inspectAutomaticTerminalBoundary(lineSuffix.moves)
                      .toOption
                      .flatMap { terminalBoundary =>
                        val logicalSuffix = terminalBoundary.map(_._1).getOrElse(lineSuffix.moves)
                        commentaryLineHistory(
                          issuedWork,
                          issuedWork.commentaryLinePrefixUci ++ logicalSuffix
                        ).flatMap(history =>
                          serverEngineLine(issuedWork, history.movePrefixUci, lineSuffix)
                            .map(CandidateLineEvaluation.EngineSearch.apply)
                        )
                      }
                      .map(evaluations :+ _)
                }
              }
            }
        }

  private def commentaryLineHistory(
      issuedWork: IssuedEngineWork,
      moves: List[String]
  ): Option[CanonicalPositionHistory] =
    CanonicalPositionHistory
      .from(
        issuedWork.commentaryLineOriginFen,
        Nil,
        issuedWork.commentaryLineOriginFen
      )
      .flatMap(_.extend(moves))
      .toOption

  private def acceptsReportedWhiteEngineScore(whiteScore: ReportedWhiteEngineScore): Boolean = whiteScore match
    case ReportedWhiteEngineScore.Centipawns(value) => acceptsCentipawnScore(value)
    case ReportedWhiteEngineScore.Mate(value) => acceptsMateScore(value)

  private def acceptsCentipawnScore(value: Int): Boolean =
    value.toLong.abs <= MaximumCentipawns

  private def acceptsMateScore(value: Int): Boolean =
    value != 0 && value.toLong.abs <= MaximumMateDistance

  private def serverEngineLine(
      issuedWork: IssuedEngineWork,
      fullMoves: List[String],
      lineSuffix: ReportedEngineLineSuffix
  ): Option[EngineLine] =
    lineSuffix.whiteScore match
      case ReportedWhiteEngineScore.Centipawns(value) =>
        Some(EngineLine(fullMoves, value, None, lineSuffix.depth))
      case ReportedWhiteEngineScore.Mate(value) =>
        normalizeMateDistanceFromInitialPosition(issuedWork, value).map { initialPositionMate =>
          EngineLine(
            fullMoves,
            Integer.signum(initialPositionMate) * MateScoreCpCarrier,
            Some(initialPositionMate),
            lineSuffix.depth
          )
        }

  /**
    * Browser mate scores are measured from searchFen. EngineLine begins at
    * initialFen, so account for each mating-side move already present in the
    * server-issued canonical prefix before admitting the line.
    */
  private def normalizeMateDistanceFromInitialPosition(
      issuedWork: IssuedEngineWork,
      searchPositionMate: Int
  ): Option[Int] =
    Fen.read(Standard, Fen.Full(issuedWork.commentaryLineOriginFen)).flatMap { initialPosition =>
      val matingSideIsWhite = searchPositionMate > 0
      val matingSideMovesInPrefix =
        issuedWork.commentaryLinePrefixUci.indices.count { prefixIndex =>
          val moverIsWhite =
            if initialPosition.color.white then prefixIndex % 2 == 0
            else prefixIndex % 2 != 0
          moverIsWhite == matingSideIsWhite
        }
      val initialPositionMateDistance = searchPositionMate.toLong.abs + matingSideMovesInPrefix.toLong
      Option.when(initialPositionMateDistance <= MaximumMateDistance)(
        Integer.signum(searchPositionMate) * initialPositionMateDistance.toInt
      )
    }

final case class CommentaryJobProgress(
    legalMoveCount: Int,
    rootLinesCollected: Int,
    moveCommentariesCompleted: Int,
    engineWorkIssued: Int,
    engineWorkReportsAccepted: Int
)

sealed trait CommentaryJobState:
  def progress: CommentaryJobProgress
  def engineProfile: CommentaryEngineProfile
sealed trait AwaitingEngineWork extends CommentaryJobState:
  def issuedWork: IssuedEngineWork
sealed trait CompletedMoveReview:
  def legalMoveIndex: Int
  def moveUci: String
  private[runtime] def judgmentPacket: EvidenceBackedJudgmentPacket
sealed trait CompletedCommentaryJob extends CommentaryJobState:
  def currentFen: String
  def result: CompletedPositionCommentary
  def progress: CommentaryJobProgress
enum CompletedPositionCommentary:
  case PositionAction(action: PlayerFacingPositionAction)
  case MoveChoices(
      drawClaims: List[lila.chessjudgment.model.line.DrawClaimAction],
      moveReviews: List[CompletedMoveReview]
  )
final case class StoppedCommentaryJob(
    engineProfile: CommentaryEngineProfile,
    stopCondition: CommentaryJobStopCondition,
    progress: CommentaryJobProgress
) extends CommentaryJobState
/** Pure single-job state reducer: no registry, persistence, transport, or projection. */
object CommentaryJobReducer:
  def startJob(
      initialFen: String,
      movePrefixUci: List[String],
      currentFen: String,
      policy: CommentaryJobPolicy,
      nowEpochMs: Long
  ): Either[CommentaryJobStartRejection, CommentaryJobState] =
    if !EngineLineAdmission.acceptsRequiredDepth(policy.rootSearchDepth) then
      Left(CommentaryJobStartRejection.RootSearchDepthInvalid)
    else if policy.maximumIssuedEngineWorkCount < 0 then Left(CommentaryJobStartRejection.MaximumIssuedEngineWorkCountInvalid)
    else CanonicalPositionHistory
      .from(initialFen, movePrefixUci, currentFen)
      .left.map(CommentaryJobStartRejection.PositionHistoryInvalid.apply)
      .map { positionHistory =>
      val rootRuleAssessment = PositionRuleAssessment.assess(positionHistory)
      val orderedLegalMoves = positionHistory.currentPosition.legalMoves.map(_.toUci.uci).sorted
      if nowEpochMs >= policy.deadlineEpochMs then
        stop(
          policy.engineProfile,
          progress(orderedLegalMoves, 0, 0, EngineWorkLedger.empty),
          CommentaryJobStopCondition.DeadlineExceeded
        )
      else rootRuleAssessment match
        case PositionRuleAssessment.Terminal(terminal) =>
          CompletedCommentaryJobRecord(
            policy.engineProfile,
            positionHistory.currentFen,
            CompletedPositionCommentary.PositionAction(
              PlayerFacingPositionAction.AutomaticTerminal(terminal)
            ),
            progress(orderedLegalMoves, 0, 0, EngineWorkLedger.empty)
          )
        case PositionRuleAssessment.Nonterminal(
              FivefoldRepetitionKnowledge.HistoryUnavailable,
              _,
              _
            ) =>
          stop(
            policy.engineProfile,
            progress(orderedLegalMoves, 0, 0, EngineWorkLedger.empty),
            CommentaryJobStopCondition.RepetitionHistoryUnavailable
          )
        case PositionRuleAssessment.Nonterminal(
              _,
              ThreefoldClaimKnowledge.HistoryUnavailable,
              _
            ) =>
          stop(
            policy.engineProfile,
            progress(orderedLegalMoves, 0, 0, EngineWorkLedger.empty),
            CommentaryJobStopCondition.RepetitionHistoryUnavailable
          )
        case PositionRuleAssessment.Nonterminal(
              FivefoldRepetitionKnowledge.CertifiedNonterminal,
              ThreefoldClaimKnowledge.Known(_),
              _
            ) =>
          val rootRequests = orderedLegalMoves.map(RootLineRequest.apply)
          advanceRootCollection(
            positionHistory,
            rootRuleAssessment,
            orderedLegalMoves,
            policy,
            rootRequests,
            Nil,
            Set.empty,
            EngineWorkLedger.empty
          )
    }
  /** Applies the deadline to an active job without issuing or accepting work. */
  def expireJob(state: CommentaryJobState, nowEpochMs: Long): CommentaryJobState =
    state match
      case active: ActiveCommentaryJob if nowEpochMs >= active.policy.deadlineEpochMs =>
        stop(active.engineProfile, active.progress, CommentaryJobStopCondition.DeadlineExceeded)
      case current => current
  def submitIssuedWorkReport(
      state: CommentaryJobState,
      report: EngineWorkReport,
      nowEpochMs: Long
  ): Either[EngineWorkReportRejection, CommentaryJobState] =
    state match
      case active: ActiveCommentaryJob =>
        if report.reportedWorkId != active.issuedWork.workId then
          Left(EngineWorkReportRejection.WorkNotOutstanding)
        else expireJob(active, nowEpochMs) match
          case stopped: StoppedCommentaryJob => Right(stopped)
          case live: ActiveCommentaryJob =>
            report match
              case EngineWorkReport.ExecutorFailed(_, _) =>
                Right(
                  stop(
                    live.engineProfile,
                    live.progressFor(live.ledger.acceptedPhysicalReport),
                    CommentaryJobStopCondition.EngineExecutionFailed
                  )
                )
              case EngineWorkReport.Completed(_, _, lineSuffixes) =>
                live.pending match
                  case root: RootPending  => submitRootReport(live, root, lineSuffixes)
                  case probe: ProbePending => submitProbeReport(live, probe, lineSuffixes)
          case _ => Right(stop(active.engineProfile, active.progress, CommentaryJobStopCondition.ReviewConstructionFailed))
      case _ => Left(EngineWorkReportRejection.WorkNotOutstanding)
  def cancelJob(state: CommentaryJobState): CommentaryJobState =
    state match
      case active: ActiveCommentaryJob =>
        stop(active.engineProfile, active.progress, CommentaryJobStopCondition.Cancelled)
      case terminal                    => terminal
  private final case class CompletedMoveReviewRecord(legalMoveIndex: Int, moveUci: String, private[runtime] val judgmentPacket: EvidenceBackedJudgmentPacket) extends CompletedMoveReview
  private final case class CompletedCommentaryJobRecord(
      engineProfile: CommentaryEngineProfile,
      currentFen: String,
      result: CompletedPositionCommentary,
      progress: CommentaryJobProgress
  ) extends CompletedCommentaryJob
  private final case class RootLineRequest(playedMoveUci: String)
  private sealed trait PendingWork:
    def work: IssuedEngineWork
  private final case class RootPending(
      request: RootLineRequest,
      declaredDrawClaims: Set[DeclaredDrawClaim],
      work: IssuedEngineWork
  ) extends PendingWork
  private final case class ProbePending(
      request: ProbeRequest,
      work: IssuedEngineWork,
      identity: ProbeSearchIdentity
  )
      extends PendingWork
  private final case class ProbeSearchIdentity(
      engineProfile: CommentaryEngineProfile,
      enginePositionInitialFen: String,
      enginePositionMovesUci: List[String],
      searchFen: String,
      commentaryLineOriginFen: String,
      commentaryLinePrefixUci: List[String],
      requiredDepth: Int,
      multiPv: Int,
      requestFen: String,
      requestMoves: List[String],
      purpose: Option[lila.chessjudgment.model.ProbePurpose],
      objective: Option[String],
      canonicalRequiredSignals: List[String],
      horizon: Option[String],
      candidateMove: Option[String],
      opponentResourceMove: Option[String],
      requestDepthFloor: Option[Int]
  )
  /** Single-job immutable engine-work accounting and admitted probe-search cache. */
  private final case class EngineWorkLedger(
      engineWorkIssued: Int,
      engineWorkReportsAccepted: Int,
      admittedProbeEngineSearches: Map[ProbeSearchIdentity, ProbeResolution.EngineSearch]
  ):
    def issuedWorkId: String = s"work:$engineWorkIssued"
    def afterIssuing: EngineWorkLedger = copy(engineWorkIssued = engineWorkIssued + 1)
    def acceptedPhysicalReport: EngineWorkLedger =
      copy(engineWorkReportsAccepted = engineWorkReportsAccepted + 1)
  private object EngineWorkLedger:
    val empty: EngineWorkLedger = EngineWorkLedger(0, 0, Map.empty)
  private sealed trait JobPhase
  private final case class CollectingRootLines(
      remaining: List[RootLineRequest],
      acceptedEvaluations: List[CandidateLineEvaluation],
      declaredDrawClaims: Set[DeclaredDrawClaim]
  ) extends JobPhase
  private final case class ReviewingRootLine(
      inventory: PreparedMoveReviewInventory,
      record: PreparedMoveReview,
      remaining: List[PreparedMoveReview],
      fulfilledProbeResults: List[(ProbeRequest, ProbeResult)],
      completedMoveReviews: List[CompletedMoveReview]
  ) extends JobPhase
  private final case class ActiveCommentaryJob(
      positionHistory: CanonicalPositionHistory,
      private[runtime] val positionRuleAssessment: PositionRuleAssessment,
      orderedLegalMoves: List[String],
      phase: JobPhase,
      pending: PendingWork,
      ledger: EngineWorkLedger,
      policy: CommentaryJobPolicy
  ) extends AwaitingEngineWork:
    def engineProfile: CommentaryEngineProfile = policy.engineProfile
    def issuedWork: IssuedEngineWork = pending.work
    def progress: CommentaryJobProgress =
      CommentaryJobReducer.progressForPhase(orderedLegalMoves, phase, ledger)
    def progressFor(value: EngineWorkLedger): CommentaryJobProgress =
      CommentaryJobReducer.progressForPhase(orderedLegalMoves, phase, value)
  private def submitRootReport(
      active: ActiveCommentaryJob,
      pending: RootPending,
      lineSuffixes: List[ReportedEngineLineSuffix]
  ): Either[EngineWorkReportRejection, CommentaryJobState] =
    EngineLineAdmission.bindReportedLineSuffixes(pending.work, lineSuffixes) match
      case Some(evaluation :: Nil) =>
        active.phase match
          case roots: CollectingRootLines =>
            Right(
              advanceRootCollection(
                active.positionHistory,
                active.positionRuleAssessment,
                active.orderedLegalMoves,
                active.policy,
                roots.remaining,
                roots.acceptedEvaluations :+ evaluation,
                roots.declaredDrawClaims ++ pending.declaredDrawClaims,
                active.ledger.acceptedPhysicalReport
              )
            )
          case _ =>
            Right(stop(active.engineProfile, active.progress, CommentaryJobStopCondition.ReviewConstructionFailed))
      case _ =>
        Right(stop(active.engineProfile, active.progress, CommentaryJobStopCondition.InvalidEngineWorkReport))
  private def submitProbeReport(
      active: ActiveCommentaryJob,
      pending: ProbePending,
      lineSuffixes: List[ReportedEngineLineSuffix]
  ): Either[EngineWorkReportRejection, CommentaryJobState] =
    EngineLineAdmission.bindReportedLineSuffixes(pending.work, lineSuffixes) match
      case Some(evaluations) =>
        active.phase match
          case review: ReviewingRootLine =>
            val resolution: ProbeResolution.EngineSearch = ProbeResolution.EngineSearch(
              evaluations,
              lineSuffixes.map(_.depth).min
            )
            Right(
              acceptProbeResolution(
                active.positionHistory,
                active.positionRuleAssessment,
                active.orderedLegalMoves,
                active.policy,
                review,
                pending.request,
                buildCanonicalProbeResult(
                  pending.request,
                  resolution
                ),
                active.ledger.acceptedPhysicalReport,
                Some(
                  pending.identity -> resolution
                )
              )
            )
          case _ =>
            Right(stop(active.engineProfile, active.progress, CommentaryJobStopCondition.ReviewConstructionFailed))
      case _ =>
        Right(stop(active.engineProfile, active.progress, CommentaryJobStopCondition.InvalidEngineWorkReport))
  private def advanceRootCollection(
      positionHistory: CanonicalPositionHistory,
      rootRuleAssessment: PositionRuleAssessment,
      orderedLegalMoves: List[String],
      policy: CommentaryJobPolicy,
      remaining: List[RootLineRequest],
      acceptedEvaluations: List[CandidateLineEvaluation],
      declaredDrawClaims: Set[DeclaredDrawClaim],
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    rootRuleAssessment match
      case PositionRuleAssessment.Nonterminal(
            FivefoldRepetitionKnowledge.CertifiedNonterminal,
            ThreefoldClaimKnowledge.Known(_),
            _
          ) =>
        remaining match
          case Nil =>
            MoveReviewInputNormalizer.prepareMoveReviewInventory(
              positionHistory,
              orderedLegalMoves,
              rootRuleAssessment,
              acceptedEvaluations,
              declaredDrawClaims
            ) match
              case None =>
                stop(
                  policy.engineProfile,
                  progress(orderedLegalMoves, acceptedEvaluations.size, 0, ledger),
                  CommentaryJobStopCondition.MoveReviewPreparationFailed
                )
              case Some(PreparedPositionCommentary.PositionAction(action)) =>
                CompletedCommentaryJobRecord(
                  policy.engineProfile,
                  positionHistory.currentFen,
                  CompletedPositionCommentary.PositionAction(action),
                  progress(
                    orderedLegalMoves,
                    acceptedEvaluations.size,
                    0,
                    ledger
                  )
                )
              case Some(PreparedPositionCommentary.MoveChoices(inventory)) =>
                beginReview(
                  positionHistory,
                  rootRuleAssessment,
                  orderedLegalMoves,
                  policy,
                  inventory,
                  inventory.reviewRecords,
                  Nil,
                  ledger
                )
          case request :: tail =>
            val prefix = List(request.playedMoveUci)
            positionHistory.extend(prefix) match
              case Right(afterRootHistory) =>
                PositionRuleAssessment.assess(afterRootHistory) match
                  case PositionRuleAssessment.Terminal(terminal) =>
                    advanceRootCollection(
                      positionHistory,
                      rootRuleAssessment,
                      orderedLegalMoves,
                      policy,
                      tail,
                      acceptedEvaluations :+ CandidateLineEvaluation.ExactAutomaticTerminal(
                        List(request.playedMoveUci),
                        terminal
                      ),
                      declaredDrawClaims,
                      ledger
                    )
                  case PositionRuleAssessment.Nonterminal(
                        FivefoldRepetitionKnowledge.HistoryUnavailable,
                        _,
                        _
                      ) =>
                    stop(
                      policy.engineProfile,
                      progress(orderedLegalMoves, acceptedEvaluations.size, 0, ledger),
                      CommentaryJobStopCondition.RepetitionHistoryUnavailable
                    )
                  case PositionRuleAssessment.Nonterminal(
                        _,
                        ThreefoldClaimKnowledge.HistoryUnavailable,
                        _
                      ) =>
                    stop(
                      policy.engineProfile,
                      progress(orderedLegalMoves, acceptedEvaluations.size, 0, ledger),
                      CommentaryJobStopCondition.RepetitionHistoryUnavailable
                    )
                  case successorRuleAssessment @ PositionRuleAssessment.Nonterminal(
                        FivefoldRepetitionKnowledge.CertifiedNonterminal,
                        ThreefoldClaimKnowledge.Known(_),
                        _
                      ) =>
                    val moveClaims = PositionRuleAssessment.declaredDrawClaims(
                      request.playedMoveUci,
                      rootRuleAssessment,
                      successorRuleAssessment
                    )
                    val pending = RootPending(
                      request,
                      moveClaims,
                      IssuedEngineWork(
                        policy.engineProfile,
                        ledger.issuedWorkId,
                        positionHistory.initialFen,
                        afterRootHistory.movePrefixUci,
                        afterRootHistory.currentFen,
                        positionHistory.currentFen,
                        prefix,
                        policy.rootSearchDepth,
                        multiPv = 1
                      )
                    )
                    issueNextWork(
                      positionHistory,
                      rootRuleAssessment,
                      orderedLegalMoves,
                      policy,
                      CollectingRootLines(tail, acceptedEvaluations, declaredDrawClaims),
                      pending,
                      ledger
                    )
              case Left(_) =>
                stop(
                  policy.engineProfile,
                  progress(orderedLegalMoves, acceptedEvaluations.size, 0, ledger),
                  CommentaryJobStopCondition.ReviewConstructionFailed
                )
      case PositionRuleAssessment.Terminal(_) =>
        stop(
          policy.engineProfile,
          progress(orderedLegalMoves, acceptedEvaluations.size, 0, ledger),
          CommentaryJobStopCondition.ReviewConstructionFailed
        )
      case PositionRuleAssessment.Nonterminal(
            FivefoldRepetitionKnowledge.HistoryUnavailable,
            _,
            _
          ) =>
        stop(
          policy.engineProfile,
          progress(orderedLegalMoves, acceptedEvaluations.size, 0, ledger),
          CommentaryJobStopCondition.RepetitionHistoryUnavailable
        )
      case PositionRuleAssessment.Nonterminal(
            _,
            ThreefoldClaimKnowledge.HistoryUnavailable,
            _
          ) =>
        stop(
          policy.engineProfile,
          progress(orderedLegalMoves, acceptedEvaluations.size, 0, ledger),
          CommentaryJobStopCondition.RepetitionHistoryUnavailable
        )

  private def beginReview(
      positionHistory: CanonicalPositionHistory,
      rootRuleAssessment: PositionRuleAssessment,
      orderedLegalMoves: List[String],
      policy: CommentaryJobPolicy,
      inventory: PreparedMoveReviewInventory,
      records: List[PreparedMoveReview],
      completedMoveReviews: List[CompletedMoveReview],
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    records match
      case Nil if completedMoveReviews.size == inventory.reviewRecords.size && completedMoveReviews.forall(_.judgmentPacket.probeRequests.isEmpty) =>
        CompletedCommentaryJobRecord(
          policy.engineProfile,
          positionHistory.currentFen,
          CompletedPositionCommentary.MoveChoices(inventory.drawClaims, completedMoveReviews),
          progress(orderedLegalMoves, orderedLegalMoves.size, completedMoveReviews.size, ledger)
        )
      case Nil =>
        stop(
          policy.engineProfile,
          progress(orderedLegalMoves, orderedLegalMoves.size, completedMoveReviews.size, ledger),
          CommentaryJobStopCondition.ReviewConstructionFailed
        )
      case record :: remaining =>
        MoveReviewJudgmentOrchestrator.executePreparedReview(record.preparedInput, Nil) match
          case Some(execution) if execution.admittedProbeResultIds.isEmpty && execution.packet.probeRequests.isEmpty =>
            beginReview(
              positionHistory,
              rootRuleAssessment,
              orderedLegalMoves,
              policy,
              inventory,
              remaining,
              completedMoveReviews :+ CompletedMoveReviewRecord(record.legalMoveIndex, record.playedMoveUci, execution.packet),
              ledger
            )
          case Some(execution) if execution.admittedProbeResultIds.isEmpty =>
            issueProbeWork(
              positionHistory,
              rootRuleAssessment,
              orderedLegalMoves,
              policy,
              ReviewingRootLine(inventory, record, remaining, Nil, completedMoveReviews),
              execution.packet.probeRequests.head,
              ledger
            )
          case _ =>
            stop(
              policy.engineProfile,
              progress(orderedLegalMoves, orderedLegalMoves.size, completedMoveReviews.size, ledger),
              CommentaryJobStopCondition.ReviewConstructionFailed
            )

  private def issueProbeWork(
      positionHistory: CanonicalPositionHistory,
      rootRuleAssessment: PositionRuleAssessment,
      orderedLegalMoves: List[String],
      policy: CommentaryJobPolicy,
      review: ReviewingRootLine,
      request: ProbeRequest,
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    probeResolution(positionHistory, request, ledger, policy) match
      case Right(Right(pending)) =>
        ledger.admittedProbeEngineSearches.get(pending.identity) match
          case Some(cached) =>
            acceptProbeResolution(
              positionHistory,
              rootRuleAssessment,
              orderedLegalMoves,
              policy,
              review,
              request,
              buildCanonicalProbeResult(
                request,
                cached
              ),
              ledger,
              None
            )
          case None =>
            issueNextWork(
              positionHistory,
              rootRuleAssessment,
              orderedLegalMoves,
              policy,
              review,
              pending,
              ledger
            )
      case Right(Left(result)) =>
        acceptProbeResolution(
          positionHistory,
          rootRuleAssessment,
          orderedLegalMoves,
          policy,
          review,
          request,
          result,
          ledger,
          None
        )
      case Left(stopCondition) =>
        stop(
          policy.engineProfile,
          progress(orderedLegalMoves, orderedLegalMoves.size, review.completedMoveReviews.size, ledger),
          stopCondition
        )

  private def issueNextWork(
      positionHistory: CanonicalPositionHistory,
      rootRuleAssessment: PositionRuleAssessment,
      orderedLegalMoves: List[String],
      policy: CommentaryJobPolicy,
      phase: JobPhase,
      pending: PendingWork,
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    val current = progressForPhase(orderedLegalMoves, phase, ledger)
    if ledger.engineWorkIssued >= policy.maximumIssuedEngineWorkCount then
      stop(policy.engineProfile, current, CommentaryJobStopCondition.BudgetExhausted)
    else
      ActiveCommentaryJob(
        positionHistory,
        rootRuleAssessment,
        orderedLegalMoves,
        phase,
        pending,
        ledger.afterIssuing,
        policy
      )

  private def acceptProbeResolution(
      positionHistory: CanonicalPositionHistory,
      rootRuleAssessment: PositionRuleAssessment,
      orderedLegalMoves: List[String],
      policy: CommentaryJobPolicy,
      review: ReviewingRootLine,
      request: ProbeRequest,
      result: ProbeResult,
      ledger: EngineWorkLedger,
      prospectiveCacheEntry: Option[(ProbeSearchIdentity, ProbeResolution.EngineSearch)]
  ): CommentaryJobState =
    val fulfilled = review.fulfilledProbeResults :+ (request -> result)
    MoveReviewJudgmentOrchestrator.executePreparedReview(review.record.preparedInput, fulfilled) match
      case Some(execution)
          if execution.admittedProbeResultIds.contains(result.id) &&
            !execution.packet.probeRequests.exists(_.id == result.id) =>
        val admittedLedger = prospectiveCacheEntry.fold(ledger) { case (identity, cached) =>
          ledger.copy(
            admittedProbeEngineSearches = ledger.admittedProbeEngineSearches.updated(identity, cached)
          )
        }
        if execution.packet.probeRequests.isEmpty then
          beginReview(
            positionHistory,
            rootRuleAssessment,
            orderedLegalMoves,
            policy,
            review.inventory,
            review.remaining,
            review.completedMoveReviews :+ CompletedMoveReviewRecord(
              review.record.legalMoveIndex,
              review.record.playedMoveUci,
              execution.packet
            ),
            admittedLedger
          )
        else
          issueProbeWork(
            positionHistory,
            rootRuleAssessment,
            orderedLegalMoves,
            policy,
            review.copy(fulfilledProbeResults = fulfilled),
            execution.packet.probeRequests.head,
            admittedLedger
          )
      case Some(_) =>
        stop(
          policy.engineProfile,
          progress(orderedLegalMoves, orderedLegalMoves.size, review.completedMoveReviews.size, ledger),
          CommentaryJobStopCondition.InvalidEngineWorkReport
        )
      case None =>
        stop(
          policy.engineProfile,
          progress(orderedLegalMoves, orderedLegalMoves.size, review.completedMoveReviews.size, ledger),
          CommentaryJobStopCondition.ReviewConstructionFailed
        )

  private def probeResolution(
      positionHistory: CanonicalPositionHistory,
      request: ProbeRequest,
      ledger: EngineWorkLedger,
      policy: CommentaryJobPolicy
  ): Either[CommentaryJobStopCondition, Either[ProbeResult, ProbePending]] =
    request.multiPv
      .filter(multiPv => EngineLineAdmission.acceptsIssuedWork(request.depth, multiPv))
      .toRight(CommentaryJobStopCondition.ReviewConstructionFailed)
      .flatMap { multiPv =>
        request.candidateMove
          .toRight(CommentaryJobStopCondition.ReviewConstructionFailed)
          .flatMap { candidateMove =>
            positionHistory
              .extend(List(candidateMove))
              .left.map(_ => CommentaryJobStopCondition.ReviewConstructionFailed)
              .flatMap { branchHistory =>
                PositionRuleAssessment.assess(branchHistory) match
                  case PositionRuleAssessment.Nonterminal(
                        FivefoldRepetitionKnowledge.CertifiedNonterminal,
                        ThreefoldClaimKnowledge.Known(_),
                        _
                      ) =>
                    branchHistory
                      .extend(request.moves)
                      .left.map(_ => CommentaryJobStopCondition.ReviewConstructionFailed)
                      .flatMap { searchHistory =>
                        PositionRuleAssessment.assess(searchHistory) match
                          case PositionRuleAssessment.Nonterminal(
                                FivefoldRepetitionKnowledge.CertifiedNonterminal,
                                ThreefoldClaimKnowledge.Known(_),
                                _
                              ) =>
                            val issuedWork = IssuedEngineWork(
                              policy.engineProfile,
                              ledger.issuedWorkId,
                              positionHistory.initialFen,
                              searchHistory.movePrefixUci,
                              searchHistory.currentFen,
                              request.fen,
                              request.moves,
                              request.depth,
                              multiPv
                            )
                            Right(Right(ProbePending(
                              request,
                              issuedWork,
                              probeSearchIdentity(request, issuedWork)
                            )))
                          case PositionRuleAssessment.Terminal(terminal) =>
                            Right(Left(buildCanonicalProbeResult(
                              request,
                              ProbeResolution.ExactAutomaticTerminal(
                                CandidateLineEvaluation.ExactAutomaticTerminal(request.moves, terminal)
                              )
                            )))
                          case PositionRuleAssessment.Nonterminal(
                                FivefoldRepetitionKnowledge.HistoryUnavailable,
                                _,
                                _
                              ) =>
                            Left(CommentaryJobStopCondition.RepetitionHistoryUnavailable)
                          case PositionRuleAssessment.Nonterminal(
                                _,
                                ThreefoldClaimKnowledge.HistoryUnavailable,
                                _
                              ) =>
                            Left(CommentaryJobStopCondition.RepetitionHistoryUnavailable)
                      }
                  case PositionRuleAssessment.Terminal(terminal) =>
                    Right(Left(buildCanonicalProbeResult(
                      request,
                      ProbeResolution.ExactAutomaticTerminal(
                        CandidateLineEvaluation.ExactAutomaticTerminal(Nil, terminal)
                      )
                    )))
                  case PositionRuleAssessment.Nonterminal(
                        FivefoldRepetitionKnowledge.HistoryUnavailable,
                        _,
                        _
                      ) =>
                    Left(CommentaryJobStopCondition.RepetitionHistoryUnavailable)
                  case PositionRuleAssessment.Nonterminal(
                        _,
                        ThreefoldClaimKnowledge.HistoryUnavailable,
                        _
                      ) =>
                    Left(CommentaryJobStopCondition.RepetitionHistoryUnavailable)
              }
          }
      }

  private def probeSearchIdentity(
      request: ProbeRequest,
      issuedWork: IssuedEngineWork
  ): ProbeSearchIdentity =
    ProbeSearchIdentity(
      issuedWork.engineProfile,
      issuedWork.enginePositionInitialFen,
      issuedWork.enginePositionMovesUci,
      issuedWork.searchFen,
      issuedWork.commentaryLineOriginFen,
      issuedWork.commentaryLinePrefixUci,
      issuedWork.requiredDepth,
      issuedWork.multiPv,
      request.fen,
      request.moves,
      request.purpose,
      request.objective,
      request.requiredSignals.map(_.trim).filter(_.nonEmpty).distinct.sorted,
      request.horizon,
      request.candidateMove,
      request.opponentResourceMove,
      request.depthFloor
    )

  private def buildCanonicalProbeResult(request: ProbeRequest, resolution: ProbeResolution): ProbeResult =
    ProbeResult(
      id = request.id,
      fen = Some(request.fen),
      resolution = resolution,
      purpose = request.purpose,
      probedMove = request.candidateMove,
      objective = request.objective,
      requiredSignals = request.requiredSignals,
      horizon = request.horizon,
      candidateMove = request.candidateMove,
      opponentResourceMove = request.opponentResourceMove,
      variationHash = request.variationHash
    )

  private def progressForPhase(
      orderedLegalMoves: List[String],
      phase: JobPhase,
      ledger: EngineWorkLedger
  ): CommentaryJobProgress =
    phase match
      case roots: CollectingRootLines =>
        progress(orderedLegalMoves, roots.acceptedEvaluations.size, 0, ledger)
      case review: ReviewingRootLine =>
        progress(orderedLegalMoves, orderedLegalMoves.size, review.completedMoveReviews.size, ledger)

  private def progress(
      orderedLegalMoves: List[String],
      rootLinesCollected: Int,
      moveCommentariesCompleted: Int,
      ledger: EngineWorkLedger
  ): CommentaryJobProgress =
    CommentaryJobProgress(
      orderedLegalMoves.size,
      rootLinesCollected,
      moveCommentariesCompleted,
      ledger.engineWorkIssued,
      ledger.engineWorkReportsAccepted
    )

  private def stop(
      engineProfile: CommentaryEngineProfile,
      progress: CommentaryJobProgress,
      condition: CommentaryJobStopCondition
  ): StoppedCommentaryJob =
    StoppedCommentaryJob(engineProfile, condition, progress)
