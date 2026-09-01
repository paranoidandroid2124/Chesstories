package io.chesstory.runtime

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Locale

import lila.chessjudgment.analysis.assembly.{
  ExplanationRequest,
  MoveReviewInputAdmission,
  MoveReviewJudgmentOrchestrator,
  PreparedFocusedMoveReview,
  PreparedFocusedPositionCommentary
}
import lila.chessjudgment.model.judgment.{
  EvidenceBackedJudgmentPacket,
  EvidenceRef,
  AdmittedMoveReviewInput,
  PlayerFacingPositionAction
}
import lila.chessjudgment.model.line.{
  AdmittedCandidateLine,
  AutomaticTerminal,
  CandidateLineEvaluation,
  CanonicalHistoryContinuation,
  CanonicalPositionHistory,
  CanonicalPositionHistoryFailure,
  DrawClaimAction,
  FivefoldRepetitionKnowledge,
  PositionRuleAssessment,
  ThreefoldClaimKnowledge
}
import lila.chessjudgment.model.line.EngineLine

final case class CommentaryJobPolicy(
    rootSearchDepth: Int,
    deadlineEpochMs: Long,
    engineProfile: CommentaryEngineProfile
)

enum CommentaryJobStartRejection:
  case PositionHistoryInvalid(reason: CanonicalPositionHistoryFailure)
  case FocusedMoveInvalid
  case RootSearchDepthInvalid

enum CommentaryJobStopCondition:
  case DeadlineExceeded
  case Cancelled
  case EngineExecutionFailed
  case InvalidEngineWorkReport
  case ReviewConstructionFailed
  case MoveReviewPreparationFailed
  case RepetitionHistoryUnavailable

enum CommentaryJobPhase(val wireId: String):
  case RootSearch extends CommentaryJobPhase("root_search")
  case FocusComparison extends CommentaryJobPhase("focus_comparison")
  case Completed extends CommentaryJobPhase("completed")
  case Stopped extends CommentaryJobPhase("stopped")

enum EngineWorkReportRejection:
  case WorkNotOutstanding

enum EngineWorkPurpose(val wireId: String):
  case RootSearch extends EngineWorkPurpose("root_search")
  case FocusComparison extends EngineWorkPurpose("focus_comparison")

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

private[runtime] final case class AdmittedPhysicalEngineLine(
    reported: ReportedEngineLineSuffix,
    continuation: CanonicalHistoryContinuation
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

  def acceptsRuntimeInputMoves(moves: List[String]): Boolean =
    moves.nonEmpty && moves.size <= MaximumFullLineMoves && moves.forall(acceptsRuntimeInputUci)

  def acceptsRuntimeInputLine(line: EngineLine): Boolean =
    acceptsRuntimeInputMoves(line.moves) &&
      acceptsCentipawnScore(line.scoreCp) &&
      line.mate.forall(acceptsMateScore) &&
      acceptsRuntimeInputDepth(line.depth)

  def admitReportedLineSuffixes(
      issuedWork: IssuedEngineWork,
      lineSuffixes: List[ReportedEngineLineSuffix]
  ): Option[List[AdmittedPhysicalEngineLine]] =
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
        admitted <- lineSuffixes.foldLeft(Option(List.empty[AdmittedPhysicalEngineLine])) { (bound, suffix) =>
          bound.flatMap { accepted =>
            engineHistory
              .admitToFirstAutomaticTerminal(suffix.moves)
              .toOption
              .map(continuation => AdmittedPhysicalEngineLine(suffix, continuation))
              .map(accepted :+ _)
          }
        }
      yield admitted

  def bindAdmittedLineSuffixes(
      issuedWork: IssuedEngineWork,
      admittedLines: List[AdmittedPhysicalEngineLine]
  ): Option[List[AdmittedCandidateLine]] =
    for
      _ <- Option.when(admittedLines.size == issuedWork.multiPv)(())
      commentaryOrigin <- CanonicalPositionHistory
        .from(issuedWork.commentaryLineOriginFen, Nil, issuedWork.commentaryLineOriginFen)
        .toOption
      if admittedLines.forall(line =>
        line.continuation.startingFen == CanonicalPositionHistory.normalizeFen(issuedWork.searchFen)
      )
      whitePrefixMoves = issuedWork.commentaryLinePrefixUci.indices.count { prefixIndex =>
        if commentaryOrigin.currentPosition.color.white then prefixIndex % 2 == 0 else prefixIndex % 2 != 0
      }
      blackPrefixMoves = issuedWork.commentaryLinePrefixUci.size - whitePrefixMoves
      bound <- admittedLines.foldLeft(Option(List.empty[AdmittedCandidateLine])) { (accepted, admitted) =>
        accepted.flatMap { lines =>
          val fullMoves = issuedWork.commentaryLinePrefixUci ++ admitted.continuation.moves
          val evaluation = admitted.continuation.automaticTerminal match
            case Some(terminal) =>
              Option.when(reportedTerminalScoreConsistent(admitted.reported.whiteScore, terminal))(
                CandidateLineEvaluation.ExactAutomaticTerminal(fullMoves, terminal)
              )
            case None =>
              serverEngineLine(
                fullMoves,
                admitted.reported,
                whitePrefixMoves,
                blackPrefixMoves
              ).map(CandidateLineEvaluation.EngineSearch.apply)
          evaluation
            .flatMap(
              admitted.continuation.bindFromOccurrence(
                commentaryOrigin.currentFen,
                issuedWork.commentaryLinePrefixUci,
                _
              )
            )
            .map(lines :+ _)
        }
      }
    yield bound

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
    physicalReportsAccepted: Int
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

  private sealed trait JobPhase
  private case object CollectingRoot extends JobPhase
  private final case class CollectingFocus(
      rootEvaluations: List[AdmittedCandidateLine]
  ) extends JobPhase

  private final case class ReviewContext(
      review: PreparedFocusedMoveReview,
      preparedInput: AdmittedMoveReviewInput,
      rootEvaluations: List[AdmittedCandidateLine]
  )

  private final case class EngineWorkLedger(
      physicalWorksIssued: Int,
      physicalReportsAccepted: Int
  ):
    def issuedWorkId: String = s"work:$physicalWorksIssued"
    def afterIssuing: EngineWorkLedger = copy(physicalWorksIssued = physicalWorksIssued + 1)
    def afterAcceptedReport: EngineWorkLedger =
      copy(physicalReportsAccepted = physicalReportsAccepted + 1)

  private object EngineWorkLedger:
    val empty: EngineWorkLedger = EngineWorkLedger(0, 0)

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
    val admittedReport = for
      physical <- EngineLineAdmission.admitReportedLineSuffixes(active.issuedWork, suffixes)
      evaluations <- EngineLineAdmission.bindAdmittedLineSuffixes(active.issuedWork, physical)
    yield physical -> evaluations
    admittedReport match
      case None => Right(stop(active, CommentaryJobStopCondition.InvalidEngineWorkReport))
      case Some((_, evaluations)) =>
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

  private def afterRootReport(
      active: ActiveCommentaryJob,
      rootEvaluations: List[AdmittedCandidateLine],
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    val ranked = rootEvaluations.zipWithIndex.sortBy { case (evaluation, index) =>
      (-evaluation.evaluation.rankingScoreForMover(active.positionHistory.currentPosition.color), index)
    }.map(_._1)
    ranked.headOption.flatMap(_.evaluation.moves.headOption) match
      case None => stop(active, CommentaryJobStopCondition.InvalidEngineWorkReport)
      case Some(bestMove) =>
        val playedCovered = ranked.exists(
          _.evaluation.moves.headOption.exists(EvidenceRef.sameMove(_, active.playedMoveUci))
        )
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
      rootEvaluations: List[AdmittedCandidateLine],
      focusEvaluations: List[AdmittedCandidateLine],
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    MoveReviewInputAdmission.prepareFocusedMoveReview(
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
          ReviewContext(review, review.preparedInput, rootEvaluations),
          ledger
        )

  private def runReview(
      active: ActiveCommentaryJob,
      context: ReviewContext,
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    MoveReviewJudgmentOrchestrator.executePreparedReview(
      context.preparedInput,
      Some(ExplanationRequest.forObservedMove(context.preparedInput))
    ) match
      case None => stop(active, CommentaryJobStopCondition.ReviewConstructionFailed)
      case Some(execution) =>
        completeReview(
          active,
          context.copy(preparedInput = execution.preparedInput),
          execution.packet,
          ledger
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
        ledger
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
        ledger
      )
    )

  private def issue(
      active: ActiveCommentaryJob,
      phase: JobPhase,
      pending: PendingWork,
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    active.copy(phase = phase, pending = pending, ledger = ledger.afterIssuing)

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

  private def progressFor(
      active: ActiveCommentaryJob,
      phase: JobPhase,
      ledger: EngineWorkLedger
  ): CommentaryJobProgress = phase match
    case CollectingRoot =>
      progress(CommentaryJobPhase.RootSearch, active.legalMoves.size, 0, 0, ledger)
    case focus: CollectingFocus =>
      progress(
        CommentaryJobPhase.FocusComparison,
        active.legalMoves.size,
        focus.rootEvaluations.size,
        0,
        ledger
      )

  private def progress(
      phase: CommentaryJobPhase,
      legalMoveCount: Int,
      rootLineCount: Int,
      selectedCompleted: Int,
      ledger: EngineWorkLedger
  ): CommentaryJobProgress =
    CommentaryJobProgress(
      phase,
      legalMoveCount,
      rootLineCount,
      selectedCompleted,
      ledger.physicalWorksIssued,
      ledger.physicalReportsAccepted
    )

  private def initialProgress(
      legalMoveCount: Int,
      phase: CommentaryJobPhase
  ): CommentaryJobProgress =
    progress(phase, legalMoveCount, 0, 0, EngineWorkLedger.empty)

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
