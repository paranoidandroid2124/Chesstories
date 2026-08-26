package io.chesstory.runtime

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Locale

import lila.chessjudgment.analysis.assembly.{
  MoveReviewInputNormalizer,
  MoveReviewJudgmentOrchestrator,
  PreparedFocusedMoveReview,
  PreparedFocusedPositionCommentary
}
import lila.chessjudgment.model.{
  ProbeContractValidator,
  ProbeRequest,
  ProbeResolution,
  ProbeResult
}
import lila.chessjudgment.model.judgment.{
  EvidenceBackedJudgmentPacket,
  EvidenceRef,
  NormalizedMoveReviewInput,
  PlayerFacingPositionAction
}
import lila.chessjudgment.model.line.{
  AutomaticTerminal,
  CandidateLineEvaluation,
  CanonicalPositionHistory,
  CanonicalPositionHistoryFailure,
  DrawClaimAction,
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
  case FocusedMoveInvalid
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

enum CommentaryJobPhase(val wireId: String):
  case RootSearch extends CommentaryJobPhase("root_search")
  case FocusComparison extends CommentaryJobPhase("focus_comparison")
  case CausalProbe extends CommentaryJobPhase("causal_probe")
  case Completed extends CommentaryJobPhase("completed")
  case Stopped extends CommentaryJobPhase("stopped")

enum EngineWorkReportRejection:
  case WorkNotOutstanding

enum EngineWorkPurpose(val wireId: String):
  case RootSearch extends EngineWorkPurpose("root_search")
  case FocusComparison extends EngineWorkPurpose("focus_comparison")
  case CausalProbe extends EngineWorkPurpose("causal_probe")

enum EngineRootRestriction:
  case Unrestricted
  case Restricted(movesUci: List[String])

final case class EngineSearchLimits(
    depth: Int,
    nodes: Int,
    movetimeMs: Int,
    multiPv: Int
)

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
      executionKeySha256: String,
      lineSuffixes: List[ReportedEngineLineSuffix]
  )
  case ExecutorFailed(
      engineProfile: CommentaryEngineProfile,
      workId: String,
      executionKeySha256: String
  )

  def reportedWorkId: String = this match
    case EngineWorkReport.Completed(_, value, _, _)    => value
    case EngineWorkReport.ExecutorFailed(_, value, _) => value

  def reportedEngineProfile: CommentaryEngineProfile = this match
    case EngineWorkReport.Completed(value, _, _, _)    => value
    case EngineWorkReport.ExecutorFailed(value, _, _) => value

  def reportedExecutionKeySha256: String = this match
    case EngineWorkReport.Completed(_, _, value, _)    => value
    case EngineWorkReport.ExecutorFailed(_, _, value) => value

/** Exact work searched by the browser. No server-side engine implementation exists. */
final case class IssuedEngineWork(
    engineProfile: CommentaryEngineProfile,
    workId: String,
    purpose: EngineWorkPurpose,
    executionKeySha256: String,
    enginePositionInitialFen: String,
    enginePositionMovesUci: List[String],
    searchFen: String,
    rootRestriction: EngineRootRestriction,
    commentaryLineOriginFen: String,
    commentaryLinePrefixUci: List[String],
    searchLimits: EngineSearchLimits,
    maxSearchElapsedMs: Int
):
  def requiredDepth: Int = searchLimits.depth
  def multiPv: Int = searchLimits.multiPv

private[runtime] object IssuedEngineWork:
  def create(
      engineProfile: CommentaryEngineProfile,
      workId: String,
      purpose: EngineWorkPurpose,
      enginePositionInitialFen: String,
      enginePositionMovesUci: List[String],
      searchFen: String,
      rootRestriction: EngineRootRestriction,
      commentaryLineOriginFen: String,
      commentaryLinePrefixUci: List[String],
      searchLimits: EngineSearchLimits,
      maxSearchElapsedMs: Int
  ): IssuedEngineWork =
    val executionKey = digest(
      List(
        engineProfile.wireId,
        workId,
        purpose.wireId,
        enginePositionInitialFen,
        enginePositionMovesUci.mkString(","),
        searchFen,
        restrictionIdentity(rootRestriction),
        searchLimits.depth.toString,
        searchLimits.nodes.toString,
        searchLimits.movetimeMs.toString,
        searchLimits.multiPv.toString,
        maxSearchElapsedMs.toString
      )
    )
    IssuedEngineWork(
      engineProfile,
      workId,
      purpose,
      executionKey,
      enginePositionInitialFen,
      enginePositionMovesUci,
      searchFen,
      rootRestriction,
      commentaryLineOriginFen,
      commentaryLinePrefixUci,
      searchLimits,
      maxSearchElapsedMs
    )

  private def restrictionIdentity(value: EngineRootRestriction): String = value match
    case EngineRootRestriction.Unrestricted      => "unrestricted"
    case EngineRootRestriction.Restricted(moves) => s"restricted:${moves.mkString(",")}"

  private def digest(fields: List[String]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(fields.mkString("\u0000").getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

/** The sole admission boundary for browser-returned engine lines. */
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

  def acceptsRuntimeInputDepth(depth: Int): Boolean = depth >= 0 && depth <= MaximumDepth

  def acceptsRequiredDepth(depth: Int): Boolean = depth >= 1 && depth <= MaximumDepth

  def acceptsMultiPv(multiPv: Int): Boolean = multiPv >= 1 && multiPv <= 3

  def acceptsIssuedWork(limits: EngineSearchLimits): Boolean =
    acceptsRequiredDepth(limits.depth) &&
      acceptsMultiPv(limits.multiPv) &&
      limits.nodes > 0 &&
      limits.movetimeMs > 0

  def acceptsRuntimeInputLine(line: EngineLine): Boolean =
    line.moves.nonEmpty && line.moves.size <= MaximumFullLineMoves &&
      line.moves.forall(acceptsRuntimeInputUci) &&
      acceptsCentipawnScore(line.scoreCp) &&
      line.mate.forall(acceptsMateScore) &&
      acceptsRuntimeInputDepth(line.depth)

  def bindReportedLineSuffixes(
      issuedWork: IssuedEngineWork,
      lineSuffixes: List[ReportedEngineLineSuffix]
  ): Option[List[CandidateLineEvaluation]] =
    val restrictedRootsValid = issuedWork.rootRestriction match
      case EngineRootRestriction.Unrestricted => true
      case EngineRootRestriction.Restricted(moves) =>
        moves.size == issuedWork.multiPv &&
          moves.distinct.size == moves.size &&
          lineSuffixes.flatMap(_.moves.headOption).toSet == moves.toSet
    if !acceptsIssuedWork(issuedWork.searchLimits) ||
      lineSuffixes.size != issuedWork.multiPv ||
      !restrictedRootsValid
    then None
    else if !lineSuffixes.forall { lineSuffix =>
      lineSuffix.moves.nonEmpty &&
        lineSuffix.moves.forall(acceptsCanonicalLowercaseUci) &&
        acceptsReportedWhiteEngineScore(lineSuffix.whiteScore) &&
        acceptsRequiredDepth(lineSuffix.depth) &&
        lineSuffix.depth >= issuedWork.requiredDepth &&
        issuedWork.commentaryLinePrefixUci.size + lineSuffix.moves.size <= MaximumFullLineMoves
    } then None
    else if issuedWork.multiPv > 1 &&
        lineSuffixes.flatMap(_.moves.headOption).distinct.size != lineSuffixes.size
    then None
    else
      for
        engineHistory <- CanonicalPositionHistory
          .from(
          issuedWork.enginePositionInitialFen,
          issuedWork.enginePositionMovesUci,
          issuedWork.searchFen
          )
          .toOption
        if PositionRuleAssessment.automaticTerminal(engineHistory.currentPosition).isEmpty
        commentaryOrigin <- CanonicalPositionHistory
          .from(issuedWork.commentaryLineOriginFen, Nil, issuedWork.commentaryLineOriginFen)
          .toOption
        commentaryPrefix <- commentaryOrigin.extend(issuedWork.commentaryLinePrefixUci).toOption
        whitePrefixMoves = issuedWork.commentaryLinePrefixUci.indices.count { prefixIndex =>
          if commentaryOrigin.currentPosition.color.white then prefixIndex % 2 == 0 else prefixIndex % 2 != 0
        }
        blackPrefixMoves = issuedWork.commentaryLinePrefixUci.size - whitePrefixMoves
        evaluations <- lineSuffixes.foldLeft(Option(List.empty[CandidateLineEvaluation])) { (bound, suffix) =>
          bound.flatMap { accepted =>
            engineHistory
              .inspectAutomaticTerminalBoundary(suffix.moves)
              .toOption
              .flatMap { terminalBoundary =>
                val logicalSuffix = terminalBoundary.map(_._1).getOrElse(suffix.moves)
                commentaryPrefix.extend(logicalSuffix).toOption.flatMap { history =>
                  terminalBoundary match
                    case Some((_, terminal)) =>
                      Option.when(reportedTerminalScoreConsistent(suffix.whiteScore, terminal))(
                        CandidateLineEvaluation.ExactAutomaticTerminal(history.movePrefixUci, terminal)
                      )
                    case None =>
                      serverEngineLine(
                        history.movePrefixUci,
                        suffix,
                        whitePrefixMoves,
                        blackPrefixMoves
                      ).map(CandidateLineEvaluation.EngineSearch.apply)
                }
              }
              .map(accepted :+ _)
          }
        }
      yield evaluations

  private def acceptsReportedWhiteEngineScore(value: ReportedWhiteEngineScore): Boolean = value match
    case ReportedWhiteEngineScore.Centipawns(score) => acceptsCentipawnScore(score)
    case ReportedWhiteEngineScore.Mate(distance)    => acceptsMateScore(distance)

  private def acceptsCentipawnScore(value: Int): Boolean = value.toLong.abs <= MaximumCentipawns

  private def acceptsMateScore(value: Int): Boolean =
    value != 0 && value.toLong.abs <= MaximumMateDistance

  private def reportedTerminalScoreConsistent(
      score: ReportedWhiteEngineScore,
      terminal: AutomaticTerminal
  ): Boolean = terminal match
    case AutomaticTerminal.Checkmate(winner) => score match
      case ReportedWhiteEngineScore.Mate(distance) => (distance > 0) == winner.white
      case _                                        => false
    case AutomaticTerminal.Stalemate |
        AutomaticTerminal.InsufficientMaterial |
        AutomaticTerminal.FivefoldRepetition |
        AutomaticTerminal.SeventyFiveMoveRule => score match
      case ReportedWhiteEngineScore.Centipawns(value) => value.abs <= 1
      case _                                           => false

  private def serverEngineLine(
      fullMoves: List[String],
      suffix: ReportedEngineLineSuffix,
      whitePrefixMoves: Int,
      blackPrefixMoves: Int
  ): Option[EngineLine] = suffix.whiteScore match
    case ReportedWhiteEngineScore.Centipawns(value) =>
      Some(EngineLine(fullMoves, value, None, suffix.depth))
    case ReportedWhiteEngineScore.Mate(value) =>
      normalizeMateDistanceFromInitialPosition(value, whitePrefixMoves, blackPrefixMoves).map { initialPositionMate =>
        EngineLine(
          fullMoves,
          Integer.signum(initialPositionMate) * MateScoreCpCarrier,
          Some(initialPositionMate),
          suffix.depth
        )
      }

  private def normalizeMateDistanceFromInitialPosition(
      searchPositionMate: Int,
      whitePrefixMoves: Int,
      blackPrefixMoves: Int
  ): Option[Int] =
    val prefixMoves = if searchPositionMate > 0 then whitePrefixMoves else blackPrefixMoves
    val distance = searchPositionMate.toLong.abs + prefixMoves.toLong
    Option.when(distance <= MaximumMateDistance)(Integer.signum(searchPositionMate) * distance.toInt)

final case class CommentaryJobProgress(
    phase: CommentaryJobPhase,
    legalMoveCount: Int,
    rootCandidateLinesAdmitted: Int,
    selectedCommentariesCompleted: Int,
    physicalWorksIssued: Int,
    physicalReportsAccepted: Int,
    causalWavesCompleted: Int
)

sealed trait CommentaryJobState:
  def progress: CommentaryJobProgress
  def engineProfile: CommentaryEngineProfile
  def playedMoveUci: String
  def resultingFen: String

sealed trait AwaitingEngineWork extends CommentaryJobState:
  def issuedWork: IssuedEngineWork

sealed trait CompletedMoveReview:
  def legalMoveIndex: Int
  def moveUci: String
  def bestMoveUci: String
  def bestLegalMoveIndex: Int
  def playedRootRank: Option[Int]
  private[runtime] def judgmentPacket: EvidenceBackedJudgmentPacket

sealed trait CompletedCommentaryJob extends CommentaryJobState:
  def currentFen: String
  def playedMoveUci: String
  def resultingFen: String
  def result: CompletedPositionCommentary

enum CompletedPositionCommentary:
  case PositionAction(action: PlayerFacingPositionAction)
  case FocusedMoveReview(
      drawClaims: List[DrawClaimAction],
      review: CompletedMoveReview
  )

final case class StoppedCommentaryJob(
    engineProfile: CommentaryEngineProfile,
    playedMoveUci: String,
    resultingFen: String,
    stopCondition: CommentaryJobStopCondition,
    progress: CommentaryJobProgress
) extends CommentaryJobState

/** Pure single-job reducer. Engine search is always issued to the browser. */
object CommentaryJobReducer:
  private val RootNodes = 5_000_000
  private val RootMovetimeMs = 5_000
  private val RootElapsedMs = 6_000
  private val FocusNodes = 2_000_000
  private val FocusMovetimeMs = 2_500
  private val FocusElapsedMs = 3_500
  private val ProbeNodes = 2_000_000
  private val ProbeMovetimeMs = 2_500
  private val ProbeElapsedMs = 3_500

  def startJob(
      initialFen: String,
      movePrefixUci: List[String],
      currentFen: String,
      playedMoveUci: String,
      resultingFen: String,
      policy: CommentaryJobPolicy,
      nowEpochMs: Long
  ): Either[CommentaryJobStartRejection, CommentaryJobState] =
    if !EngineLineAdmission.acceptsRequiredDepth(policy.rootSearchDepth) then
      Left(CommentaryJobStartRejection.RootSearchDepthInvalid)
    else if policy.maximumIssuedEngineWorkCount < 1 then
      Left(CommentaryJobStartRejection.MaximumIssuedEngineWorkCountInvalid)
    else
      CanonicalPositionHistory
        .from(initialFen, movePrefixUci, currentFen)
        .left.map(CommentaryJobStartRejection.PositionHistoryInvalid.apply)
        .flatMap { history =>
          val playedMove = EvidenceRef.normalizeMove(playedMoveUci)
          history
            .extend(List(playedMove))
            .toOption
            .filter(_.currentFen == CanonicalPositionHistory.normalizeFen(resultingFen))
            .toRight(CommentaryJobStartRejection.FocusedMoveInvalid)
            .map(_ => startValidated(history, playedMove, CanonicalPositionHistory.normalizeFen(resultingFen), policy, nowEpochMs))
        }

  def expireJob(state: CommentaryJobState, nowEpochMs: Long): CommentaryJobState = state match
    case active: ActiveCommentaryJob if nowEpochMs >= active.policy.deadlineEpochMs =>
      stop(active, CommentaryJobStopCondition.DeadlineExceeded)
    case current => current

  def submitIssuedWorkReport(
      state: CommentaryJobState,
      report: EngineWorkReport,
      nowEpochMs: Long
  ): Either[EngineWorkReportRejection, CommentaryJobState] = state match
    case active: ActiveCommentaryJob
        if report.reportedWorkId == active.issuedWork.workId &&
          report.reportedEngineProfile == active.engineProfile &&
          report.reportedExecutionKeySha256 == active.issuedWork.executionKeySha256 =>
      expireJob(active, nowEpochMs) match
        case stopped: StoppedCommentaryJob => Right(stopped)
        case live: ActiveCommentaryJob =>
          report match
            case EngineWorkReport.ExecutorFailed(_, _, _) =>
              Right(
                stop(
                  live.copy(ledger = live.ledger.afterAcceptedReport),
                  CommentaryJobStopCondition.EngineExecutionFailed
                )
              )
            case EngineWorkReport.Completed(_, _, _, suffixes) =>
              submitCompletedReport(live, suffixes)
        case _ => Right(stop(active, CommentaryJobStopCondition.ReviewConstructionFailed))
    case _: ActiveCommentaryJob => Left(EngineWorkReportRejection.WorkNotOutstanding)
    case _                      => Left(EngineWorkReportRejection.WorkNotOutstanding)

  def cancelJob(state: CommentaryJobState): CommentaryJobState = state match
    case active: ActiveCommentaryJob => stop(active, CommentaryJobStopCondition.Cancelled)
    case terminal                    => terminal

  private final case class CompletedMoveReviewRecord(
      legalMoveIndex: Int,
      moveUci: String,
      bestMoveUci: String,
      bestLegalMoveIndex: Int,
      playedRootRank: Option[Int],
      private[runtime] val judgmentPacket: EvidenceBackedJudgmentPacket
  ) extends CompletedMoveReview

  private final case class CompletedCommentaryJobRecord(
      engineProfile: CommentaryEngineProfile,
      currentFen: String,
      playedMoveUci: String,
      resultingFen: String,
      result: CompletedPositionCommentary,
      progress: CommentaryJobProgress
  ) extends CompletedCommentaryJob

  private sealed trait PendingWork:
    def work: IssuedEngineWork

  private final case class RootPending(work: IssuedEngineWork) extends PendingWork
  private final case class FocusPending(work: IssuedEngineWork) extends PendingWork
  private final case class ProbePending(
      request: ProbeRequest,
      identity: PhysicalSearchIdentity,
      work: IssuedEngineWork
  ) extends PendingWork

  private sealed trait JobPhase
  private case object CollectingRoot extends JobPhase
  private final case class CollectingFocus(
      rootEvaluations: List[CandidateLineEvaluation]
  ) extends JobPhase

  private final case class ReviewContext(
      review: PreparedFocusedMoveReview,
      preparedInput: NormalizedMoveReviewInput,
      rootEvaluations: List[CandidateLineEvaluation],
      focusEvaluations: List[CandidateLineEvaluation],
      fulfilled: List[(ProbeRequest, ProbeResult)],
      wavesCompleted: Int
  )

  private final case class CollectingProbeWave(
      context: ReviewContext,
      remaining: List[ProbeRequest]
  ) extends JobPhase

  private final case class PhysicalSearchIdentity(
      engineProfile: CommentaryEngineProfile,
      enginePositionInitialFen: String,
      enginePositionMovesUci: List[String],
      searchFen: String,
      rootRestriction: EngineRootRestriction,
      searchLimits: EngineSearchLimits,
      maxSearchElapsedMs: Int
  )

  private final case class EngineWorkLedger(
      physicalWorksIssued: Int,
      physicalReportsAccepted: Int,
      validatedPhysicalSearches: Map[PhysicalSearchIdentity, List[ReportedEngineLineSuffix]]
  ):
    def issuedWorkId: String = s"work:$physicalWorksIssued"
    def afterIssuing: EngineWorkLedger = copy(physicalWorksIssued = physicalWorksIssued + 1)
    def afterAcceptedReport: EngineWorkLedger =
      copy(physicalReportsAccepted = physicalReportsAccepted + 1)

  private object EngineWorkLedger:
    val empty: EngineWorkLedger = EngineWorkLedger(0, 0, Map.empty)

  private final case class ActiveCommentaryJob(
      positionHistory: CanonicalPositionHistory,
      rootRuleAssessment: PositionRuleAssessment,
      playedMoveUci: String,
      resultingFen: String,
      legalMoves: List[String],
      phase: JobPhase,
      pending: PendingWork,
      ledger: EngineWorkLedger,
      policy: CommentaryJobPolicy
  ) extends AwaitingEngineWork:
    def engineProfile: CommentaryEngineProfile = policy.engineProfile
    def issuedWork: IssuedEngineWork = pending.work
    def progress: CommentaryJobProgress = progressFor(this, phase, ledger)

  private def startValidated(
      history: CanonicalPositionHistory,
      playedMove: String,
      resultingFen: String,
      policy: CommentaryJobPolicy,
      nowEpochMs: Long
  ): CommentaryJobState =
    val rootRuleAssessment = PositionRuleAssessment.assess(history)
    val legalMoves = history.currentLegalMoveUcis
    if nowEpochMs >= policy.deadlineEpochMs then
      stopped(
        policy.engineProfile,
        playedMove,
        resultingFen,
        initialProgress(legalMoves.size, CommentaryJobPhase.Stopped),
        CommentaryJobStopCondition.DeadlineExceeded
      )
    else rootRuleAssessment match
      case PositionRuleAssessment.Nonterminal(
            FivefoldRepetitionKnowledge.CertifiedNonterminal,
            ThreefoldClaimKnowledge.Known(_),
            _
          ) =>
        val limits = EngineSearchLimits(
          policy.rootSearchDepth,
          RootNodes,
          RootMovetimeMs,
          legalMoves.size.min(3)
        )
        val work = issuedWork(
          policy.engineProfile,
          EngineWorkLedger.empty.issuedWorkId,
          EngineWorkPurpose.RootSearch,
          history,
          EngineRootRestriction.Unrestricted,
          history.currentFen,
          Nil,
          limits,
          RootElapsedMs
        )
        ActiveCommentaryJob(
          history,
          rootRuleAssessment,
          playedMove,
          resultingFen,
          legalMoves,
          CollectingRoot,
          RootPending(work),
          EngineWorkLedger.empty.afterIssuing,
          policy
        )
      case PositionRuleAssessment.Nonterminal(_, _, _) =>
        stopped(
          policy.engineProfile,
          playedMove,
          resultingFen,
          initialProgress(legalMoves.size, CommentaryJobPhase.Stopped),
          CommentaryJobStopCondition.RepetitionHistoryUnavailable
        )
      case PositionRuleAssessment.Terminal(_) =>
        stopped(
          policy.engineProfile,
          playedMove,
          resultingFen,
          initialProgress(legalMoves.size, CommentaryJobPhase.Stopped),
          CommentaryJobStopCondition.ReviewConstructionFailed
        )

  private def submitCompletedReport(
      active: ActiveCommentaryJob,
      suffixes: List[ReportedEngineLineSuffix]
  ): Either[EngineWorkReportRejection, CommentaryJobState] =
    EngineLineAdmission.bindReportedLineSuffixes(active.issuedWork, suffixes) match
      case None => Right(stop(active, CommentaryJobStopCondition.InvalidEngineWorkReport))
      case Some(evaluations) =>
        val acceptedLedger = active.ledger.afterAcceptedReport
        val acceptedActive = active.copy(ledger = acceptedLedger)
        active.pending match
          case _: RootPending =>
            active.phase match
              case CollectingRoot => Right(afterRootReport(acceptedActive, evaluations, acceptedLedger))
              case _ => Right(stop(acceptedActive, CommentaryJobStopCondition.ReviewConstructionFailed))
          case _: FocusPending =>
            active.phase match
              case focus: CollectingFocus =>
                Right(beginPreparedReview(acceptedActive, focus.rootEvaluations, evaluations, acceptedLedger))
              case _ => Right(stop(acceptedActive, CommentaryJobStopCondition.ReviewConstructionFailed))
          case probe: ProbePending =>
            active.phase match
              case wave: CollectingProbeWave =>
                val resolution = ProbeResolution.EngineSearch(evaluations, suffixes.map(_.depth).min)
                val cachedLedger = acceptedLedger.copy(
                  validatedPhysicalSearches = acceptedLedger.validatedPhysicalSearches.updated(
                    probe.identity,
                    suffixes
                  )
                )
                Right(
                  continueProbeWave(
                    acceptedActive,
                    wave.context.copy(
                      fulfilled = wave.context.fulfilled :+ (
                        probe.request -> canonicalProbeResult(probe.request, resolution)
                      )
                    ),
                    wave.remaining,
                    cachedLedger
                  )
                )
              case _ => Right(stop(acceptedActive, CommentaryJobStopCondition.ReviewConstructionFailed))

  private def afterRootReport(
      active: ActiveCommentaryJob,
      rootEvaluations: List[CandidateLineEvaluation],
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    val ranked = rootEvaluations.zipWithIndex.sortBy { case (evaluation, index) =>
      (-evaluation.rankingScoreForMover(active.positionHistory.currentPosition.color), index)
    }.map(_._1)
    ranked.headOption.flatMap(_.moves.headOption) match
      case None => stop(active, CommentaryJobStopCondition.InvalidEngineWorkReport)
      case Some(bestMove) =>
        val playedCovered = ranked.exists(_.moves.headOption.exists(EvidenceRef.sameMove(_, active.playedMoveUci)))
        if playedCovered then beginPreparedReview(active, ranked, Nil, ledger)
        else
          val limits = EngineSearchLimits(
            active.policy.rootSearchDepth,
            FocusNodes,
            FocusMovetimeMs,
            multiPv = 2
          )
          val work = issuedWork(
            active.engineProfile,
            ledger.issuedWorkId,
            EngineWorkPurpose.FocusComparison,
            active.positionHistory,
            EngineRootRestriction.Restricted(List(EvidenceRef.normalizeMove(bestMove), active.playedMoveUci)),
            active.positionHistory.currentFen,
            Nil,
            limits,
            FocusElapsedMs
          )
          issue(active, CollectingFocus(ranked), FocusPending(work), ledger)

  private def beginPreparedReview(
      active: ActiveCommentaryJob,
      rootEvaluations: List[CandidateLineEvaluation],
      focusEvaluations: List[CandidateLineEvaluation],
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    MoveReviewInputNormalizer.prepareFocusedMoveReview(
      active.positionHistory,
      active.playedMoveUci,
      active.resultingFen,
      active.rootRuleAssessment,
      rootEvaluations,
      focusEvaluations
    ) match
      case None => stop(active, CommentaryJobStopCondition.MoveReviewPreparationFailed)
      case Some(PreparedFocusedPositionCommentary.PositionAction(action)) =>
        completePositionAction(active, action, rootEvaluations.size, ledger)
      case Some(PreparedFocusedPositionCommentary.MoveReview(review)) =>
        runReview(
          active,
          ReviewContext(review, review.preparedInput, rootEvaluations, focusEvaluations, Nil, 0),
          ledger
        )

  private def runReview(
      active: ActiveCommentaryJob,
      context: ReviewContext,
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    val alreadyAdmittedIds = context.preparedInput.threatBranches.map(_.sourceProbeId).toSet
    val newlyFulfilled = context.fulfilled.filterNot { case (request, _) =>
      alreadyAdmittedIds(request.id)
    }
    MoveReviewJudgmentOrchestrator.executePreparedReview(
      context.preparedInput,
      newlyFulfilled
    ) match
      case None => stop(active, CommentaryJobStopCondition.ReviewConstructionFailed)
      case Some(execution)
          if execution.admittedProbeResultIds.toSet != context.fulfilled.map(_._1.id).toSet =>
        stop(active, CommentaryJobStopCondition.InvalidEngineWorkReport)
      case Some(execution) if execution.packet.probeRequests.isEmpty =>
        completeReview(
          active,
          context.copy(preparedInput = execution.preparedInput),
          execution.packet,
          ledger
        )
      case Some(execution) =>
        beginProbeWave(
          active,
          context.copy(preparedInput = execution.preparedInput),
          execution.packet.probeRequests,
          ledger
        )

  private def beginProbeWave(
      active: ActiveCommentaryJob,
      context: ReviewContext,
      requests: List[ProbeRequest],
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    val objectives = requests.map(_.objective)
    val requestIds = requests.map(_.id)
    if requests.isEmpty ||
      objectives.distinct.size != 1 ||
      requests.exists(request => !ProbeContractValidator.validateRequest(request).isValid) ||
      requestIds.exists(_.trim.isEmpty) ||
      requestIds.distinct.size != requestIds.size ||
      requestIds.exists(context.fulfilled.map(_._1.id).toSet)
    then stop(active, CommentaryJobStopCondition.ReviewConstructionFailed)
    else continueProbeWave(active, context, requests, ledger)

  private def continueProbeWave(
      active: ActiveCommentaryJob,
      context: ReviewContext,
      remaining: List[ProbeRequest],
      ledger: EngineWorkLedger
  ): CommentaryJobState = remaining match
    case Nil =>
      runReview(
        active,
        context.copy(
          wavesCompleted = context.wavesCompleted + 1
        ),
        ledger
      )
    case request :: tail =>
      resolveProbe(active, context, request, ledger) match
        case Left(condition) => stop(active, condition)
        case Right(Left(result)) =>
          continueProbeWave(
            active,
            context.copy(fulfilled = context.fulfilled :+ (request -> result)),
            tail,
            ledger
          )
        case Right(Right((pending, identity))) =>
          ledger.validatedPhysicalSearches.get(identity) match
            case Some(cachedSuffixes) =>
              EngineLineAdmission.bindReportedLineSuffixes(pending.work, cachedSuffixes) match
                case Some(evaluations) =>
                  continueProbeWave(
                    active,
                    context.copy(
                      fulfilled = context.fulfilled :+ (
                        request -> canonicalProbeResult(
                          request,
                          ProbeResolution.EngineSearch(evaluations, cachedSuffixes.map(_.depth).min)
                        )
                      )
                    ),
                    tail,
                    ledger
                  )
                case None => stop(active, CommentaryJobStopCondition.ReviewConstructionFailed)
            case None =>
              issue(
                active,
                CollectingProbeWave(context, tail),
                pending,
                ledger
              )

  private def resolveProbe(
      active: ActiveCommentaryJob,
      context: ReviewContext,
      request: ProbeRequest,
      ledger: EngineWorkLedger
  ): Either[
    CommentaryJobStopCondition,
    Either[ProbeResult, (ProbePending, PhysicalSearchIdentity)]
  ] =
    for
      _ <- Either.cond(
        ProbeContractValidator.validateRequest(request).isValid &&
          EngineLineAdmission.acceptsRequiredDepth(request.depth) &&
          EngineLineAdmission.acceptsMultiPv(request.multiPv),
        (),
        CommentaryJobStopCondition.ReviewConstructionFailed
      )
      candidateMove = EvidenceRef.normalizeMove(request.candidateMove)
      branchHistory <- active.positionHistory
        .extend(List(candidateMove))
        .left.map(_ => CommentaryJobStopCondition.ReviewConstructionFailed)
      _ <- Either.cond(
        CanonicalPositionHistory.sameBoardState(branchHistory.currentFen, request.fen),
        (),
        CommentaryJobStopCondition.ReviewConstructionFailed
      )
      resolution <- PositionRuleAssessment.assess(branchHistory) match
        case PositionRuleAssessment.Terminal(terminal) =>
          Right(
            Left(
              canonicalProbeResult(
                request,
                ProbeResolution.ExactAutomaticTerminal(
                  CandidateLineEvaluation.ExactAutomaticTerminal(Nil, terminal)
                )
              )
            )
          )
        case PositionRuleAssessment.Nonterminal(
              FivefoldRepetitionKnowledge.CertifiedNonterminal,
              ThreefoldClaimKnowledge.Known(_),
              _
            ) =>
          branchHistory.extend(request.moves.map(EvidenceRef.normalizeMove))
            .left.map(_ => CommentaryJobStopCondition.ReviewConstructionFailed)
            .flatMap { searchHistory =>
              PositionRuleAssessment.assess(searchHistory) match
                case PositionRuleAssessment.Terminal(terminal) =>
                  Right(
                    Left(
                      canonicalProbeResult(
                        request,
                        ProbeResolution.ExactAutomaticTerminal(
                          CandidateLineEvaluation.ExactAutomaticTerminal(
                            request.moves.map(EvidenceRef.normalizeMove),
                            terminal
                          )
                        )
                      )
                    )
                  )
                case PositionRuleAssessment.Nonterminal(
                      FivefoldRepetitionKnowledge.CertifiedNonterminal,
                      ThreefoldClaimKnowledge.Known(_),
                      _
                    ) =>
                  reuseCoreEvidence(context, request, request.multiPv).map(result => Right(Left(result))).getOrElse {
                    val limits = EngineSearchLimits(request.depth, ProbeNodes, ProbeMovetimeMs, request.multiPv)
                    val work = issuedWork(
                      active.engineProfile,
                      ledger.issuedWorkId,
                      EngineWorkPurpose.CausalProbe,
                      searchHistory,
                      EngineRootRestriction.Unrestricted,
                      branchHistory.currentFen,
                      request.moves.map(EvidenceRef.normalizeMove),
                      limits,
                      ProbeElapsedMs
                    )
                    val identity = physicalIdentity(work)
                    Right(Right(ProbePending(request, identity, work) -> identity))
                  }
                case PositionRuleAssessment.Nonterminal(_, _, _) =>
                  Left(CommentaryJobStopCondition.RepetitionHistoryUnavailable)
            }
        case PositionRuleAssessment.Nonterminal(_, _, _) =>
          Left(CommentaryJobStopCondition.RepetitionHistoryUnavailable)
    yield resolution

  private def reuseCoreEvidence(
      context: ReviewContext,
      request: ProbeRequest,
      multiPv: Int
  ): Option[ProbeResult] =
    val candidateMove = EvidenceRef.normalizeMove(request.candidateMove)
    val forcedPrefix = request.moves.map(EvidenceRef.normalizeMove)
    val matching = (context.focusEvaluations ++ context.rootEvaluations)
      .flatMap(_.engineLine)
      .filter(line =>
        line.depth >= request.depth &&
          line.moves.headOption.exists(EvidenceRef.sameMove(_, candidateMove)) &&
          line.moves.tail.take(forcedPrefix.size).zip(forcedPrefix).forall(EvidenceRef.sameMove) &&
          line.moves.tail.size > forcedPrefix.size
      )
      .map(line => line.copy(moves = line.moves.tail.map(EvidenceRef.normalizeMove)))
    val grouped = matching.groupBy(line => line.moves.drop(forcedPrefix.size).headOption)
    val conflicting = grouped.values.exists(_.distinct.size > 1)
    val candidates = matching.distinctBy(line => line.moves.drop(forcedPrefix.size).headOption).take(multiPv)
    Option
      .when(!conflicting && candidates.size == multiPv)(
        canonicalProbeResult(
          request,
          ProbeResolution.EngineSearch(
            candidates.map(line => CandidateLineEvaluation.EngineSearch(line)),
            candidates.map(_.depth).min
          )
        )
      )

  private def completePositionAction(
      active: ActiveCommentaryJob,
      action: PlayerFacingPositionAction,
      rootLineCount: Int,
      ledger: EngineWorkLedger
  ): CompletedCommentaryJob =
    CompletedCommentaryJobRecord(
      active.engineProfile,
      active.positionHistory.currentFen,
      active.playedMoveUci,
      active.resultingFen,
      CompletedPositionCommentary.PositionAction(action),
      progress(
        CommentaryJobPhase.Completed,
        active.legalMoves.size,
        rootLineCount,
        0,
        ledger,
        0
      )
    )

  private def completeReview(
      active: ActiveCommentaryJob,
      context: ReviewContext,
      packet: EvidenceBackedJudgmentPacket,
      ledger: EngineWorkLedger
  ): CompletedCommentaryJob =
    val bestLegalMoveIndex = active.legalMoves.indexOf(context.review.bestMoveUci)
    if bestLegalMoveIndex < 0 then
      throw IllegalStateException("prepared reference move is not legal at the reviewed root")
    val record = CompletedMoveReviewRecord(
      context.review.legalMoveIndex,
      context.review.playedMoveUci,
      context.review.bestMoveUci,
      bestLegalMoveIndex,
      context.review.playedRootRank,
      packet
    )
    CompletedCommentaryJobRecord(
      active.engineProfile,
      active.positionHistory.currentFen,
      active.playedMoveUci,
      active.resultingFen,
      CompletedPositionCommentary.FocusedMoveReview(context.review.drawClaims, record),
      progress(
        CommentaryJobPhase.Completed,
        active.legalMoves.size,
        context.rootEvaluations.size,
        1,
        ledger,
        context.wavesCompleted
      )
    )

  private def issue(
      active: ActiveCommentaryJob,
      phase: JobPhase,
      pending: PendingWork,
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    if ledger.physicalWorksIssued >= active.policy.maximumIssuedEngineWorkCount then
      stop(active.copy(phase = phase, ledger = ledger), CommentaryJobStopCondition.BudgetExhausted)
    else active.copy(phase = phase, pending = pending, ledger = ledger.afterIssuing)

  private def issuedWork(
      engineProfile: CommentaryEngineProfile,
      workId: String,
      purpose: EngineWorkPurpose,
      engineHistory: CanonicalPositionHistory,
      rootRestriction: EngineRootRestriction,
      commentaryLineOriginFen: String,
      commentaryLinePrefixUci: List[String],
      limits: EngineSearchLimits,
      maxSearchElapsedMs: Int
  ): IssuedEngineWork =
    IssuedEngineWork.create(
      engineProfile,
      workId,
      purpose,
      engineHistory.initialFen,
      engineHistory.movePrefixUci,
      engineHistory.currentFen,
      rootRestriction,
      commentaryLineOriginFen,
      commentaryLinePrefixUci,
      limits,
      maxSearchElapsedMs
    )

  private def physicalIdentity(work: IssuedEngineWork): PhysicalSearchIdentity =
    PhysicalSearchIdentity(
      work.engineProfile,
      work.enginePositionInitialFen,
      work.enginePositionMovesUci,
      work.searchFen,
      work.rootRestriction,
      work.searchLimits,
      work.maxSearchElapsedMs
    )

  private def canonicalProbeResult(request: ProbeRequest, resolution: ProbeResolution): ProbeResult =
    ProbeResult(
      id = request.id,
      resolution = resolution
    )

  private def progressFor(
      active: ActiveCommentaryJob,
      phase: JobPhase,
      ledger: EngineWorkLedger
  ): CommentaryJobProgress = phase match
    case CollectingRoot =>
      progress(CommentaryJobPhase.RootSearch, active.legalMoves.size, 0, 0, ledger, 0)
    case focus: CollectingFocus =>
      progress(
        CommentaryJobPhase.FocusComparison,
        active.legalMoves.size,
        focus.rootEvaluations.size,
        0,
        ledger,
        0
      )
    case wave: CollectingProbeWave =>
      progress(
        CommentaryJobPhase.CausalProbe,
        active.legalMoves.size,
        wave.context.rootEvaluations.size,
        0,
        ledger,
        wave.context.wavesCompleted
      )

  private def progress(
      phase: CommentaryJobPhase,
      legalMoveCount: Int,
      rootLineCount: Int,
      selectedCompleted: Int,
      ledger: EngineWorkLedger,
      wavesCompleted: Int
  ): CommentaryJobProgress =
    CommentaryJobProgress(
      phase,
      legalMoveCount,
      rootLineCount,
      selectedCompleted,
      ledger.physicalWorksIssued,
      ledger.physicalReportsAccepted,
      wavesCompleted
    )

  private def initialProgress(
      legalMoveCount: Int,
      phase: CommentaryJobPhase
  ): CommentaryJobProgress =
    progress(phase, legalMoveCount, 0, 0, EngineWorkLedger.empty, 0)

  private def stop(
      active: ActiveCommentaryJob,
      condition: CommentaryJobStopCondition
  ): StoppedCommentaryJob =
    stopped(
      active.engineProfile,
      active.playedMoveUci,
      active.resultingFen,
      active.progress.copy(phase = CommentaryJobPhase.Stopped),
      condition
    )

  private def stopped(
      engineProfile: CommentaryEngineProfile,
      playedMoveUci: String,
      resultingFen: String,
      progress: CommentaryJobProgress,
      condition: CommentaryJobStopCondition
  ): StoppedCommentaryJob =
    StoppedCommentaryJob(engineProfile, playedMoveUci, resultingFen, condition, progress)
