package io.chesstory.runtime

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Locale

import scala.util.Try

import lila.chessjudgment.analysis.assembly.{
  MoveReviewAssembler,
  MoveReviewInputAdmission,
  OccurrenceResources,
  PreparedFocusedMoveReview,
  PreparedFocusedPositionCommentary
}
import lila.chessjudgment.model.judgment.{
  EvalComparison,
  EvidenceBackedJudgmentPacket,
  EvidenceRef,
  EvaluationSource,
  MoveChoiceVerdict,
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
    engineProfile: CommentaryEngineProfile,
    investigateChoices: Boolean = false,
    maximumLocalExpansions: Int = CommentaryProposal.MaximumLocalExpansions,
    previewDepth: Option[Int] = None,
    probeThreats: Boolean = false,
    shortJob: Boolean = false
)

/** Search completion is separate from assessment freshness and never certifies chess sufficiency. */
final case class CommentaryVerification(
    targetDepth: Int,
    state: String = "pending",
    reason: String = "initial_verification",
    triggerWorkIds: List[String] = Nil,
    completedDepth: Option[Int] = None,
    stopReason: Option[String] = None
)

enum CommentaryJobStartRejection:
  case PositionHistoryInvalid(reason: CanonicalPositionHistoryFailure)
  case FocusedMoveInvalid
  case RootSearchDepthInvalid
  case InvalidProposals
  case LocalExpansionLimitInvalid

enum CommentaryJobStopCondition:
  case EngineUnavailable
  case BrowserUnsupported
  case DeadlineExceeded
  case Cancelled
  case SearchBudgetExhausted
  case EngineExecutionFailed
  case InvalidEngineWorkReport
  case ReviewConstructionFailed
  case MoveReviewPreparationFailed
  case RepetitionHistoryUnavailable

enum CommentaryJobPhase(val wireId: String):
  case RootSearch extends CommentaryJobPhase("root_search")
  case FocusComparison extends CommentaryJobPhase("focus_comparison")
  case InvestigationComparison extends CommentaryJobPhase("investigation_comparison")
  case ResponseComparison extends CommentaryJobPhase("response_comparison")
  case ThreatProbe extends CommentaryJobPhase("threat_probe")
  case Completed extends CommentaryJobPhase("completed")
  case Stopped extends CommentaryJobPhase("stopped")

enum EngineWorkReportRejection:
  case WorkNotOutstanding

enum EngineWorkPurpose(val wireId: String):
  case RootSearch extends EngineWorkPurpose("root_search")
  case FocusComparison extends EngineWorkPurpose("focus_comparison")
  case InvestigationComparison extends EngineWorkPurpose("investigation_comparison")
  case ResponseComparison extends EngineWorkPurpose("response_comparison")
  case ThreatProbe extends EngineWorkPurpose("threat_probe")

/** Why issued work yielded no admissible result; never a chess proposition state. */
enum EngineWorkFailure:
  case EngineUnavailable
  case BrowserUnsupported
  case Cancelled
  case BudgetExhausted
  case ExecutionFailed

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
      executionKeySha256: String,
      failure: EngineWorkFailure
  )

  def reportedWorkId: String = this match
    case EngineWorkReport.Completed(_, value, _, _)    => value
    case EngineWorkReport.ExecutorFailed(_, value, _, _) => value

  def reportedEngineProfile: CommentaryEngineProfile = this match
    case EngineWorkReport.Completed(value, _, _, _)    => value
    case EngineWorkReport.ExecutorFailed(value, _, _, _) => value

  def reportedExecutionKeySha256: String = this match
    case EngineWorkReport.Completed(_, _, value, _)    => value
    case EngineWorkReport.ExecutorFailed(_, _, value, _) => value

/** Exact work requested from the browser. No server-side engine implementation exists. */
final case class EngineSearchAnchor(rootMove: String, rootOccurrenceId: String, positionId: String,
    prefixMovesUci: List[String] = Nil):
  def prefixMoves: List[String] = if prefixMovesUci.isEmpty then List(rootMove) else prefixMovesUci
  private[runtime] def scopeKey: (String, String, String, List[String]) =
    (rootMove, rootOccurrenceId, positionId, prefixMoves)
  def plyOffset: Int = prefixMoves.size

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
    maxSearchElapsedMs: Int,
    searchAnchor: Option[EngineSearchAnchor] = None,
    // Issuance ownership survives when a deeper question retires this work's answer.
    proposalRefs: List[(String, String)] = Nil,
    threatOriginPrefixUci: Option[List[String]] = None
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
      maxSearchElapsedMs: Int,
      searchAnchor: Option[EngineSearchAnchor] = None,
      threatOriginPrefixUci: Option[List[String]] = None
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
      ) ++ searchAnchor.toList.flatMap(anchor => List(anchor.rootMove, anchor.rootOccurrenceId, anchor.positionId) ++
        Option.when(anchor.plyOffset > 1)(anchor.prefixMoves.mkString(","))) ++
        threatOriginPrefixUci.toList.flatMap(prefix => List(commentaryLineOriginFen, prefix.mkString(",")))
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
      maxSearchElapsedMs,
      searchAnchor,
      threatOriginPrefixUci = threatOriginPrefixUci
    )

  private def restrictionIdentity(value: EngineRootRestriction): String = value match
    case EngineRootRestriction.Unrestricted      => "unrestricted"
    case EngineRootRestriction.Restricted(moves) => s"restricted:${moves.mkString(",")}"

  private def digest(fields: List[String]): String =
    java.util.HexFormat.of().formatHex(MessageDigest
      .getInstance("SHA-256")
      .digest(fields.mkString("\u0000").getBytes(StandardCharsets.UTF_8)))

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

  /** Search restrictions describe the allowed pool; MultiPV describes returned lines. */
  def acceptsReturnedRoots(work: IssuedEngineWork, moves: List[String]): Boolean =
    moves.size == work.multiPv && moves.distinct.size == moves.size && moves.forall(acceptsCanonicalLowercaseUci) &&
      (work.rootRestriction match
        case EngineRootRestriction.Unrestricted => true
        case EngineRootRestriction.Restricted(pool) =>
          pool.distinct.size == pool.size && pool.forall(acceptsCanonicalLowercaseUci) &&
            pool.size >= work.multiPv &&
            (work.purpose == EngineWorkPurpose.ResponseComparison ||
              work.purpose == EngineWorkPurpose.InvestigationComparison || pool.size == work.multiPv) &&
            moves.forall(pool.contains))

  def admitReportedLineSuffixes(
      issuedWork: IssuedEngineWork,
      lineSuffixes: List[ReportedEngineLineSuffix],
      existingHistory: Option[CanonicalPositionHistory] = None
  ): Option[List[AdmittedPhysicalEngineLine]] =
    if !acceptsIssuedWork(issuedWork.searchLimits) ||
      !acceptsReturnedRoots(issuedWork, lineSuffixes.flatMap(_.moves.headOption))
    then None
    else if !lineSuffixes.forall { lineSuffix =>
      lineSuffix.moves.nonEmpty &&
        lineSuffix.moves.forall(acceptsCanonicalLowercaseUci) &&
        acceptsReportedWhiteEngineScore(lineSuffix.whiteScore) &&
        acceptsRequiredDepth(lineSuffix.depth) &&
        lineSuffix.depth >= issuedWork.requiredDepth &&
        issuedWork.commentaryLinePrefixUci.size + issuedWork.searchAnchor.fold(0)(_.plyOffset) +
          lineSuffix.moves.size <= MaximumFullLineMoves
    } then None
    else
      for
        engineHistory <- existingHistory match
          case Some(history) => Option.when(history.initialFen == issuedWork.enginePositionInitialFen &&
            history.movePrefixUci == issuedWork.enginePositionMovesUci && history.currentFen == issuedWork.searchFen)(history)
          case None => CanonicalPositionHistory.from(issuedWork.enginePositionInitialFen,
            issuedWork.enginePositionMovesUci, issuedWork.searchFen).toOption
        if issuedWork.rootRestriction match
          case EngineRootRestriction.Unrestricted => issuedWork.multiPv <= engineHistory.currentLegalMoveUcis.size
          case EngineRootRestriction.Restricted(moves) => moves.forall(engineHistory.currentLegalMoveUcis.contains)
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
    if issuedWork.purpose == EngineWorkPurpose.ThreatProbe then return None
    for
      _ <- Option.when(admittedLines.size == issuedWork.multiPv)(())
      commentaryOrigin <- CanonicalPositionHistory
        .from(issuedWork.commentaryLineOriginFen, Nil, issuedWork.commentaryLineOriginFen)
        .toOption
      if admittedLines.forall(line =>
        line.continuation.startingFen == CanonicalPositionHistory.normalizeFen(issuedWork.searchFen)
      )
      // Validate the search suffix before adding the commentary prefix, while retaining its legal replay.
      if admittedLines.forall(line =>
        serverEngineLine(line.continuation.moves, line.reported, 0, 0)
          .flatMap(CandidateLineEvaluation.fromEngineLine(_, line.continuation.automaticTerminal)).nonEmpty
      )
      whitePrefixMoves = issuedWork.commentaryLinePrefixUci.indices.count { prefixIndex =>
        if commentaryOrigin.currentPosition.color.white then prefixIndex % 2 == 0 else prefixIndex % 2 != 0
      }
      blackPrefixMoves = issuedWork.commentaryLinePrefixUci.size - whitePrefixMoves
      bound <- admittedLines.foldLeft(Option(List.empty[AdmittedCandidateLine])) { (accepted, admitted) =>
        accepted.flatMap { lines =>
          val fullMoves = issuedWork.commentaryLinePrefixUci ++ admitted.continuation.moves
          val evaluation = serverEngineLine(
            fullMoves,
            admitted.reported,
            whitePrefixMoves,
            blackPrefixMoves
          ).flatMap(CandidateLineEvaluation.fromEngineLine(_, admitted.continuation.automaticTerminal))
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

  private def serverEngineLine(
      fullMoves: List[String],
      suffix: ReportedEngineLineSuffix,
      whitePrefixMoves: Int,
      blackPrefixMoves: Int
  ): Option[EngineLine] = suffix.whiteScore match
    case ReportedWhiteEngineScore.Centipawns(value) =>
      Some(EngineLine(fullMoves, value, None, suffix.depth))
    case ReportedWhiteEngineScore.Mate(value) =>
      normalizeMateDistance(value, whitePrefixMoves, blackPrefixMoves).map { initialPositionMate =>
        EngineLine(
          fullMoves,
          Integer.signum(initialPositionMate) * MateScoreCpCarrier,
          Some(initialPositionMate),
          suffix.depth
        )
      }

  private def normalizeMateDistance(
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
  def verification: Option[CommentaryVerification] = None
  def observationsPacket: Option[EvidenceBackedJudgmentPacket] = None
  private[runtime] def deferObservationDelivery: Boolean = false
  private[runtime] def proposalEntries: List[CommentaryProposal.Entry] = Nil
  private[runtime] def localExpansionsUsed: Int = 0
  private[runtime] def discoveryState: CommentaryDiscovery.State = CommentaryDiscovery.State()
  private[runtime] def inquiries: List[CommentaryInquiry] = Nil
  private[runtime] def rootSearches: List[CommentaryRootSearch] = Nil
  private[runtime] def pvComparisons: List[PrincipalVariationComparison.Reading] = Nil
  private[runtime] def responseInquiries: List[CommentaryResponseInquiry] = Nil
  private[runtime] def responseSearches: List[TargetResponseInvestigation.Search] = Nil
  private[runtime] def continuationReadings: List[LineContinuationInvestigation.Reading] = Nil
  private[runtime] def continuationInquiries: List[CommentaryContinuationInquiry] = Nil
  private[runtime] def preparedPaths: Option[ObservedLineProtocol.PreparedPaths] = None
  private[runtime] def assessment: Option[CompletedCommentaryJob] = None
  private[runtime] def assessmentNeedsRefresh: Boolean = false
  private[runtime] def assessmentScope: String = "root_search"
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
  def bestRootRank: Option[Int] = Some(1)
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
    progress: CommentaryJobProgress,
    override val observationsPacket: Option[EvidenceBackedJudgmentPacket] = None,
    override private[runtime] val rootSearches: List[CommentaryRootSearch] = Nil,
    override private[runtime] val proposalEntries: List[CommentaryProposal.Entry] = Nil,
    override private[runtime] val localExpansionsUsed: Int = 0,
    override private[runtime] val discoveryState: CommentaryDiscovery.State = CommentaryDiscovery.State(),
    override val verification: Option[CommentaryVerification] = None
) extends CommentaryJobState

/** Issued root work and its original receipt, shared by every included inquiry. */
private[runtime] final case class CommentaryRootSearch(
    work: IssuedEngineWork,
    receipt: Option[CommentaryInvestigation.EngineCandidates] = None,
    stopReason: Option[CommentaryJobStopCondition] = None):
  def state: String =
    if stopReason.nonEmpty then "stopped" else if receipt.nonEmpty then "completed" else "awaiting_engine"

/** Question state belongs to this reviewed occurrence and its issued work ledger. */
private[runtime] final case class CommentaryInquiry(
    question: CommentaryInvestigation.Question,
    state: String = "deferred",
    answer: Option[CommentaryInvestigation.Answer] = None,
    stopReason: Option[String] = None,
    assessmentEffect: Option[String] = None
)

private[runtime] final case class CommentaryResponseAnswer(
    pair: CommentaryInvestigation.Answer, searchPositionId: String, plyOffset: Int = 1
):
  def json: play.api.libs.json.JsObject = CommentaryInvestigation.answerJson(pair) ++
    play.api.libs.json.Json.obj("search_position_id" -> searchPositionId, "ply_offset" -> plyOffset)

private[runtime] final case class CommentaryResponseInquiry(
    question: TargetResponseInvestigation.Question, state: String = "deferred",
    answer: Option[CommentaryResponseAnswer] = None, stopReason: Option[String] = None,
    reading: Option[TargetResponseInvestigation.Reading] = None,
    readingOwner: Option[EvidenceBackedJudgmentPacket] = None,
    retainedReadings: List[TargetResponseInvestigation.Reading] = Nil
)

/** Keep a discovered relation intact when several questions share one search. */
private[runtime] final case class CommentaryContinuationPremise(
    origin: LineContinuationInvestigation.ChoicePremise, sourceLineIds: List[String]):
  def id(questionId: String): String = CommentarySelection.stableId("continuation-premise",
    List(questionId, origin.kind, origin.requirementId.getOrElse("")) ++ origin.triggerIds.sorted ++
      origin.comparisonSources.map(_.json.toString).sorted)

private[runtime] final case class CommentaryPremiseObservation(
    premise: CommentaryContinuationPremise, observationState: String,
    witnessLineIds: List[String], unresolvedSourceLineIds: List[String])

/** An observation-derived question about a choice at an exact continuation prefix. */
private[runtime] final case class CommentaryContinuationQuestion(
    id: String, rootOccurrenceId: String, rootPositionId: String, positionId: String,
    prefixMoves: List[String], prefixOccurrenceIds: List[String],
    observedMoves: List[String], sourceLineIds: List[String],
    legalMoves: List[String], moveSourceIds: Map[String, String], premises: List[CommentaryContinuationPremise]
):
  def triggerOccurrenceIds: List[String] = premises.flatMap(_.origin.triggerIds).distinct.sorted
  def kinds: List[String] = premises.map(_.origin.kind).distinct.sorted
  def comparisonSources: List[LineContinuationInvestigation.ComparisonSource] =
    premises.flatMap(_.origin.comparisonSources).distinct
  def anchor: EngineSearchAnchor = EngineSearchAnchor(prefixMoves.head, prefixOccurrenceIds.head,
    positionId, if prefixMoves.size == 1 then Nil else prefixMoves)

private[runtime] final case class CommentaryContinuationInquiry(
    question: CommentaryContinuationQuestion, state: String = "deferred",
    stopReason: Option[String] = None, searchWorkIds: List[String] = Nil,
    premiseObservations: List[CommentaryPremiseObservation] = Nil, followupQuestionIds: List[String] = Nil,
    routeChallenge: Option[CommentaryRouteChallenge] = None
):
  def unresolvedSourceLineIds: List[String] = premiseObservations.flatMap(_.unresolvedSourceLineIds).distinct.sorted

private[runtime] final case class CommentaryRouteChallengeComparison(
    workId: String, referenceLineId: String, challengeLineId: String)

private[runtime] final case class CommentaryRouteChallengeClass(
    movesUci: List[String], premiseOccurrenceIds: List[String], sourceLineIds: List[String],
    sampledWorkIds: List[String], comparison: Option[CommentaryRouteChallengeComparison], kind: String = "recovery_route")

private[runtime] final case class CommentaryRouteChallenge(
    referenceWorkId: String, referenceMove: String, referenceLineId: String,
    moveClasses: List[CommentaryRouteChallengeClass], state: String, stopReason: Option[String] = None):
  def missingMoves: List[String] = moveClasses.filter(_.sampledWorkIds.isEmpty).flatMap(_.movesUci).distinct.sorted

/** Pure single-job reducer. Engine search is always issued to the browser. */
object CommentaryJobReducer:
  // An unresolved explanation is not permission for unbounded search. Keep
  // normal commentary work within the user's chosen explanatory horizon.
  private[runtime] val MaximumAnalysisDepth = 18
  private val RootNodes = 5_000_000
  private val RootMovetimeMs = 5_000
  // A selected pair can need more nodes than the three-root survey. Keep its
  // existing wall-clock ceiling; a fast engine must not stop after one second
  // merely because the unresolved pair reached the survey's node allowance.
  private val SelectedInquiryNodes = 20_000_000
  // This comparison is the prerequisite for the already selected root idea.
  // Both arms must finish at the admitted depth. Keep the overall deadline;
  // reserve eight seconds here instead of ending the job on a partial pair.
  private val SelectedReplyNodes = 40_000_000
  private val SelectedReplyMovetimeMs = 8_000
  private val SelectedReplyElapsedMs = 9_000
  private val CompletionRootNodes = 250_000_000
  private val CompletionRootMovetimeMs = 120_000
  private val CompletionRootElapsedMs = 121_000
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
      nowEpochMs: Long,
      inputLines: List[lila.chessjudgment.model.judgment.SuppliedCommentaryLine] = Nil,
      proposals: List[CommentaryProposal.Request] = Nil
  ): Either[CommentaryJobStartRejection, CommentaryJobState] =
    if !EngineLineAdmission.acceptsRequiredDepth(policy.rootSearchDepth) ||
        policy.rootSearchDepth > MaximumAnalysisDepth ||
        policy.previewDepth.exists(depth => depth != 8 || depth >= policy.rootSearchDepth) then
      Left(CommentaryJobStartRejection.RootSearchDepthInvalid)
    else if !CommentaryProposal.validRequests(proposals) then Left(CommentaryJobStartRejection.InvalidProposals)
    else if policy.maximumLocalExpansions < 0 || policy.maximumLocalExpansions > CommentaryProposal.MaximumLocalExpansions then
      Left(CommentaryJobStartRejection.LocalExpansionLimitInvalid)
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
            .map(_ => startValidated(history, playedMove, CanonicalPositionHistory.normalizeFen(resultingFen), policy, nowEpochMs, inputLines, proposals))
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
          val remaining = math.min(live.remainingBudgetMs, live.policy.deadlineEpochMs - nowEpochMs)
          report match
            case EngineWorkReport.ExecutorFailed(_, _, _, failure) =>
              // Complete the selected comparison or finite fact inquiry once
              // before abandoning it for a short-search timeout. The retry
              // retains the same position, roots and owners and uses the
              // existing selected-reply allowance for the mandatory focus
              // when it fits, within the original job deadline.
              val accepted = live.copy(ledger = live.ledger.afterAcceptedReport, remainingBudgetMs = remaining,
                failedProposalWorks = if failure == EngineWorkFailure.BudgetExhausted &&
                    live.phase.isInstanceOf[InvestigatingProposals] then
                  (live.failedProposalWorks :+ live.issuedWork).distinct else live.failedProposalWorks)
              val responseInquiry = live.phase match
                case phase: InvestigatingResponses =>
                  live.continuationInquiries.exists(inquiry => inquiry.question.id == phase.selectedId &&
                    inquiry.state == "awaiting_engine") || live.responseInquiries.exists(inquiry =>
                    inquiry.question.id == phase.selectedId && inquiry.state == "awaiting_engine")
                case _ => false
              val selectedInquiry = live.phase.isInstanceOf[CollectingFocus] ||
                live.phase.isInstanceOf[InvestigatingProposals] || live.phase.isInstanceOf[InvestigatingFacts] || responseInquiry
              val compactFocus = live.phase.isInstanceOf[CollectingFocus] && remaining < RootElapsedMs + 2_000
              val initialRoot = live.phase.isInstanceOf[CollectingRoot] &&
                live.verification.exists(value => value.state == "pending" && value.reason == "initial_verification" &&
                  value.targetDepth == live.issuedWork.requiredDepth) &&
                live.issuedWork.rootRestriction == EngineRootRestriction.Unrestricted &&
                live.issuedWork.searchLimits.nodes == RootNodes &&
                live.issuedWork.searchLimits.movetimeMs == RootMovetimeMs
              if failure == EngineWorkFailure.BudgetExhausted && initialRoot &&
                  remaining >= SelectedReplyElapsedMs + 2_000 then
                val retained = accepted.copy(rootSearches = accepted.rootSearches.map(search =>
                  if search.work == live.issuedWork then search.copy(stopReason =
                    Some(CommentaryJobStopCondition.SearchBudgetExhausted)) else search))
                val work = issuedWork(live.engineProfile, retained.ledger.issuedWorkId,
                  EngineWorkPurpose.RootSearch, live.positionHistory, EngineRootRestriction.Unrestricted,
                  live.positionHistory.currentFen, Nil, EngineSearchLimits(live.issuedWork.requiredDepth,
                    SelectedReplyNodes, SelectedReplyMovetimeMs, live.issuedWork.multiPv), SelectedReplyElapsedMs)
                Right(issue(retained, live.phase, work, retained.ledger))
              else if failure == EngineWorkFailure.BudgetExhausted && selectedInquiry &&
                  live.issuedWork.requiredDepth >= 16 && live.issuedWork.searchLimits.movetimeMs == FocusMovetimeMs &&
                  live.issuedWork.searchLimits.nodes == FocusNodes &&
                  remaining >= (if responseInquiry then 12_000 else
                    (if compactFocus then FocusElapsedMs else RootElapsedMs) + 2_000) then
                val retained = accepted.copy(rootSearches = accepted.rootSearches.map(search =>
                  if search.work == live.issuedWork then search.copy(stopReason =
                    Some(CommentaryJobStopCondition.SearchBudgetExhausted)) else search))
                val history = live.searchHistory.getOrElse(live.positionHistory)
                val finishFocus = live.phase.isInstanceOf[CollectingFocus] &&
                  remaining >= SelectedReplyElapsedMs + 2_000
                val work = issuedWork(live.engineProfile, retained.ledger.issuedWorkId,
                  live.issuedWork.purpose, history, live.issuedWork.rootRestriction,
                  history.currentFen, Nil,
                  EngineSearchLimits(live.issuedWork.requiredDepth,
                    if finishFocus then SelectedReplyNodes else SelectedInquiryNodes,
                    if finishFocus then SelectedReplyMovetimeMs else if compactFocus then FocusMovetimeMs
                      else RootMovetimeMs, live.issuedWork.multiPv),
                  if finishFocus then SelectedReplyElapsedMs else if compactFocus then FocusElapsedMs else RootElapsedMs,
                  live.issuedWork.searchAnchor).copy(proposalRefs = live.issuedWork.proposalRefs)
                val retry = issue(retained, live.phase, work, retained.ledger, live.searchHistory)
                Right(retry match
                  case active: ActiveCommentaryJob if !live.phase.isInstanceOf[CollectingFocus] => refreshProposalReadings(active)
                  case other => other)
              else if failure == EngineWorkFailure.BudgetExhausted &&
                  live.phase.isInstanceOf[InvestigatingProposals] &&
                  live.issuedWork.rootRestriction == EngineRootRestriction.Unrestricted &&
                  live.issuedWork.searchAnchor.exists(anchor => anchor.prefixMoves.size == 1 &&
                    live.firstReplySurveysIssued(anchor.prefixMoves)) then
                val phase = live.phase.asInstanceOf[InvestigatingProposals]
                // Failure to rank all replies does not exhaust an exact pair.
                // Keep the survey attempt, then finish the named dependency
                // once without manufacturing a completed search receipt.
                val entries = accepted.proposalEntries.map(entry => entry.copy(readings = entry.readings.map(reading =>
                  if reading.state == "awaiting_engine" && reading.workId.contains(live.issuedWork.workId) then
                    reading.copy(state = "deferred", stopReason = Some("survey_budget_exhausted"), workId = None)
                  else reading)))
                Right(beginResponses(accepted.copy(proposalEntries = entries), phase.baseline, phase.questions))
              else if failure == EngineWorkFailure.BudgetExhausted &&
                  live.phase.isInstanceOf[InvestigatingProposals] &&
                  !live.phase.asInstanceOf[InvestigatingProposals].verifyAfter then
                val phase = live.phase.asInstanceOf[InvestigatingProposals]
                // An unfinished reply is unknown. It neither refutes its parent
                // nor stops other independent questions in the same review.
                val entries = exhaustedProposalEntries(accepted.proposalEntries, phase.selected,
                  live.issuedWork, live.issuedWork)
                Right(beginResponses(accepted.copy(proposalEntries = entries), phase.baseline, phase.questions))
              else Right(
                stop(
                  accepted,
                  failure match
                    case EngineWorkFailure.Cancelled       => CommentaryJobStopCondition.Cancelled
                    case EngineWorkFailure.BudgetExhausted => CommentaryJobStopCondition.SearchBudgetExhausted
                    case EngineWorkFailure.ExecutionFailed => CommentaryJobStopCondition.EngineExecutionFailed
                    case EngineWorkFailure.EngineUnavailable => CommentaryJobStopCondition.EngineUnavailable
                    case EngineWorkFailure.BrowserUnsupported => CommentaryJobStopCondition.BrowserUnsupported
                )
              )
            case EngineWorkReport.Completed(_, _, _, suffixes) =>
              // Finish the required comparison before spending the remaining time
              // on discovery. Explicit proposals retain their existing obligations.
              // A ready selected comparison can fit the shorter allowance.
              // Keep two seconds for its reading and delivery; a five-second
              // comparison is no reason to suppress every shorter question.
              val budgeted = live.copy(remainingBudgetMs = remaining, inheritedPrepared = live.preparedPaths,
                policy = if remaining < 2_500 then
                  live.policy.copy(investigateChoices = false) else live.policy)
              submitCompletedReport(budgeted, suffixes)
        case completed: CompletedCommentaryJob => Right(completed)
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
      private[runtime] val judgmentPacket: EvidenceBackedJudgmentPacket,
      override val bestRootRank: Option[Int] = Some(1)
  ) extends CompletedMoveReview

  private final case class CompletedCommentaryJobRecord(
      engineProfile: CommentaryEngineProfile,
      currentFen: String,
      playedMoveUci: String,
      resultingFen: String,
      result: CompletedPositionCommentary,
      progress: CommentaryJobProgress,
      override val observationsPacket: Option[EvidenceBackedJudgmentPacket],
      override private[runtime] val inquiries: List[CommentaryInquiry] = Nil,
      override private[runtime] val rootSearches: List[CommentaryRootSearch] = Nil,
      override private[runtime] val assessmentNeedsRefresh: Boolean = false,
      override private[runtime] val assessmentScope: String = "root_search",
      override private[runtime] val pvComparisons: List[PrincipalVariationComparison.Reading] = Nil,
      override private[runtime] val responseInquiries: List[CommentaryResponseInquiry] = Nil,
      override private[runtime] val preparedPaths: Option[ObservedLineProtocol.PreparedPaths] = None,
      override private[runtime] val responseSearches: List[TargetResponseInvestigation.Search] = Nil,
      override private[runtime] val continuationReadings: List[LineContinuationInvestigation.Reading] = Nil,
      override private[runtime] val continuationInquiries: List[CommentaryContinuationInquiry] = Nil,
      override private[runtime] val proposalEntries: List[CommentaryProposal.Entry] = Nil,
      override private[runtime] val localExpansionsUsed: Int = 0,
      override private[runtime] val discoveryState: CommentaryDiscovery.State = CommentaryDiscovery.State(),
      override val verification: Option[CommentaryVerification] = None
  ) extends CompletedCommentaryJob

  private sealed trait JobPhase
  private final case class CollectingRoot(baseline: Option[CompletedCommentaryJobRecord] = None) extends JobPhase
  private final case class CollectingFocus(
      rootEvaluations: List[AdmittedCandidateLine],
      baseline: Option[CompletedCommentaryJobRecord] = None
  ) extends JobPhase

  private sealed trait InvestigationPhase extends JobPhase:
    def baseline: CompletedCommentaryJobRecord
    def questions: List[CommentaryInquiry]
    def responses: List[CommentaryResponseInquiry]
    def refreshing: Boolean = false

  private final case class Investigating(
      baseline: CompletedCommentaryJobRecord,
      questions: List[CommentaryInquiry],
      selected: CommentaryInvestigation.Question,
      responses: List[CommentaryResponseInquiry] = Nil
  ) extends InvestigationPhase

  private final case class RefreshingAssessment(
      baseline: CompletedCommentaryJobRecord,
      questions: List[CommentaryInquiry],
      responses: List[CommentaryResponseInquiry],
      referenceMove: String,
      selectedQuestionId: Option[String],
      requiredDepth: Int,
      override val refreshing: Boolean = true
  ) extends InvestigationPhase

  private final case class InvestigatingResponses(
      baseline: CompletedCommentaryJobRecord,
      questions: List[CommentaryInquiry],
      responses: List[CommentaryResponseInquiry],
      selectedId: String
  ) extends InvestigationPhase

  private final case class InvestigatingProposals(
      baseline: CompletedCommentaryJobRecord,
      questions: List[CommentaryInquiry],
      responses: List[CommentaryResponseInquiry],
      selected: CommentaryProposal.CompareMoves,
      verifyAfter: Boolean = false
  ) extends InvestigationPhase

  private final case class InvestigatingFacts(
      baseline: CompletedCommentaryJobRecord,
      questions: List[CommentaryInquiry],
      responses: List[CommentaryResponseInquiry],
      prefixMoves: List[String]
  ) extends InvestigationPhase

  private final case class ProbingThreat(
      baseline: CompletedCommentaryJobRecord,
      originPrefix: List[String],
      questions: List[CommentaryInquiry] = Nil,
      responses: List[CommentaryResponseInquiry] = Nil
  ) extends InvestigationPhase

  private final case class ReviewContext(
      review: PreparedFocusedMoveReview,
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
      issuedWork: IssuedEngineWork,
      ledger: EngineWorkLedger,
      policy: CommentaryJobPolicy,
      packet: EvidenceBackedJudgmentPacket,
      remainingBudgetMs: Long = Long.MaxValue,
      receipts: List[CommentaryInvestigation.EngineCandidates] = Nil,
      override private[runtime] val rootSearches: List[CommentaryRootSearch] = Nil,
      override private[runtime] val pvComparisons: List[PrincipalVariationComparison.Reading] = Nil,
      responseReceipts: List[CommentaryInvestigation.EngineCandidates] = Nil,
      explorationsIssued: Int = 0,
      firstReplySurveysIssued: Set[List[String]] = Set.empty,
      failedProposalWorks: List[IssuedEngineWork] = Nil,
      inheritedPrepared: Option[ObservedLineProtocol.PreparedPaths] = None,
      searchHistory: Option[CanonicalPositionHistory] = None,
      responseHistories: Map[String, Option[CanonicalPositionHistory]] = Map.empty,
      exploredPrefixes: Set[List[String]] = Set.empty,
      override private[runtime] val responseSearches: List[TargetResponseInvestigation.Search] = Nil,
      override private[runtime] val continuationReadings: List[LineContinuationInvestigation.Reading] = Nil,
      override private[runtime] val continuationInquiries: List[CommentaryContinuationInquiry] = Nil,
      override private[runtime] val proposalEntries: List[CommentaryProposal.Entry] = Nil,
      override private[runtime] val localExpansionsUsed: Int = 0,
      override private[runtime] val discoveryState: CommentaryDiscovery.State = CommentaryDiscovery.State(),
      override val verification: Option[CommentaryVerification] = None,
      childRechecksIssued: Int = 0,
      threatOriginsTried: Set[List[String]] = Set.empty
  ) extends AwaitingEngineWork:
    override def observationsPacket = Some(packet)
    override private[runtime] def deferObservationDelivery = assessment.isEmpty
    override private[runtime] lazy val preparedPaths = Some(inheritedPrepared.filter(_.belongsTo(packet))
      .getOrElse(ObservedLineProtocol.prepare(packet)))
    private[runtime] lazy val comparisonConflicts =
      CommentaryComparisonConflict.read(packet, responseSearches, investigationDepth(this))
    private val choiceTasks = scala.collection.mutable.Map.empty[String, ChoiceTasks]
    def currentChoices(reference: String): ChoiceTasks =
      choiceTasks.getOrElseUpdate(reference, new ChoiceTasks(this, reference))
    override private[runtime] def inquiries = phase match
      case investigation: InvestigationPhase => investigation.questions
      case root: CollectingRoot => root.baseline.toList.flatMap(_.inquiries)
      case focus: CollectingFocus => focus.baseline.toList.flatMap(_.inquiries)
    // Every packet replacement invalidates the old readings' owner, including intermediate
    // assessment repairs. Rebind once before any consumer or projection can see this state.
    override private[runtime] lazy val responseInquiries = (phase match
      case investigation: InvestigationPhase => investigation.responses
      case root: CollectingRoot => root.baseline.toList.flatMap(_.responseInquiries)
      case focus: CollectingFocus => focus.baseline.toList.flatMap(_.responseInquiries)
    ).map(bindResponseReading(this, _))
    override private[runtime] def assessment = phase match
      case investigation: InvestigationPhase => Some(investigation.baseline)
      case root: CollectingRoot => root.baseline
      case focus: CollectingFocus => focus.baseline
    override private[runtime] def assessmentNeedsRefresh = phase match
      case investigation: InvestigationPhase => investigation.refreshing || verification.exists(value =>
        value.state != "complete" && value.reason == "child_challenge")
      case _ => assessment.nonEmpty && verification.exists(value =>
        value.state != "complete" && (value.reason == "child_challenge" ||
          receipts.exists(_.work.requiredDepth >= value.targetDepth)))
    override private[runtime] def assessmentScope = assessment.fold("root_search")(_.assessmentScope)
    def engineProfile: CommentaryEngineProfile = policy.engineProfile
    def progress: CommentaryJobProgress = progressFor(this, phase, ledger)

  private def startValidated(
      history: CanonicalPositionHistory,
      playedMove: String,
      resultingFen: String,
      policy: CommentaryJobPolicy,
      nowEpochMs: Long,
      inputLines: List[lila.chessjudgment.model.judgment.SuppliedCommentaryLine],
      proposals: List[CommentaryProposal.Request]
  ): CommentaryJobState =
    val rootRuleAssessment = PositionRuleAssessment.assess(history)
    val legalMoves = history.currentLegalMoveUcis
    val packet = MoveReviewInputAdmission.admitIndependent(history, playedMove, suppliedLines = inputLines)
      .map(input => MoveReviewAssembler.assembleOccurrences(input))
      .flatMap(EvidenceBackedJudgmentPacket.fromAssembly)
    if nowEpochMs >= policy.deadlineEpochMs then
      stopped(
        policy.engineProfile,
        playedMove,
        resultingFen,
        initialProgress(legalMoves.size, CommentaryJobPhase.Stopped),
        CommentaryJobStopCondition.DeadlineExceeded,
        packet,
        proposalEntries = CommentaryProposal.stop(CommentaryProposal.entries(proposals), "deadline_exceeded")
      )
    else rootRuleAssessment match
      case PositionRuleAssessment.Nonterminal(
            FivefoldRepetitionKnowledge.CertifiedNonterminal,
            ThreefoldClaimKnowledge.Known(_),
            _
          ) =>
        val limits = EngineSearchLimits(
          policy.previewDepth.getOrElse(policy.rootSearchDepth),
          if policy.previewDepth.nonEmpty then FocusNodes else RootNodes,
          if policy.previewDepth.nonEmpty then FocusMovetimeMs else RootMovetimeMs,
          legalMoves.size.min(if policy.shortJob then 2 else 3)
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
          if policy.previewDepth.nonEmpty then FocusElapsedMs else RootElapsedMs
        )
        ActiveCommentaryJob(
          history,
          rootRuleAssessment,
          playedMove,
          resultingFen,
          legalMoves,
          CollectingRoot(),
          work,
          EngineWorkLedger.empty.afterIssuing,
          policy,
          packet.getOrElse(throw IllegalStateException("validated nonterminal move lost its occurrence")),
          remainingBudgetMs = policy.deadlineEpochMs - nowEpochMs,
          rootSearches = List(CommentaryRootSearch(work)),
          proposalEntries = CommentaryProposal.entries(proposals),
          verification = policy.previewDepth.map(_ => CommentaryVerification(policy.rootSearchDepth))
        )
      case PositionRuleAssessment.Nonterminal(_, _, _) =>
        stopped(
          policy.engineProfile,
          playedMove,
          resultingFen,
          initialProgress(legalMoves.size, CommentaryJobPhase.Stopped),
          CommentaryJobStopCondition.RepetitionHistoryUnavailable,
          packet,
          proposalEntries = CommentaryProposal.stop(CommentaryProposal.entries(proposals), "repetition_history_unavailable")
        )
      case PositionRuleAssessment.Terminal(_) =>
        stopped(
          policy.engineProfile,
          playedMove,
          resultingFen,
          initialProgress(legalMoves.size, CommentaryJobPhase.Stopped),
          CommentaryJobStopCondition.ReviewConstructionFailed,
          packet,
          proposalEntries = CommentaryProposal.stop(CommentaryProposal.entries(proposals), "review_construction_failed")
        )

  private def submitCompletedReport(
      active: ActiveCommentaryJob,
      suffixes: List[ReportedEngineLineSuffix]
  ): Either[EngineWorkReportRejection, CommentaryJobState] =
    active.phase match
      case phase: ProbingThreat =>
        val physical = EngineLineAdmission.admitReportedLineSuffixes(active.issuedWork, suffixes, active.searchHistory)
        if physical.isEmpty then return Right(stop(active, CommentaryJobStopCondition.InvalidEngineWorkReport))
        val candidates = physical.get.flatMap { line =>
          line.continuation.resultingHistory.segmentReplaySteps.headOption.filter(step =>
            step.capturedRole.nonEmpty || step.after.check.yes).map(_.uci)
        }
        val proposed = CommentaryThreat.proposals(active.packet, phase.originPrefix, candidates, active.issuedWork.workId)
          .filterNot(entry => active.proposalEntries.exists(_.request.id == entry.request.id))
        val room = (CommentaryProposal.MaximumRequirements - active.proposalEntries.map(_.request.requirements.size).sum).max(0)
        val entries = active.proposalEntries ++ proposed.take(room / 2)
        require(CommentaryProposal.validRequests(entries.map(_.request)))
        // Discard the counterfactual score and PV. The real packet and assessment retain their owners.
        return Right(beginInvestigation(active.copy(ledger = active.ledger.afterAcceptedReport,
          proposalEntries = entries), phase.baseline))
      case _ => ()
    EngineLineAdmission.admitReportedLineSuffixes(active.issuedWork, suffixes,
      active.searchHistory.orElse(Some(active.positionHistory))) match
      case None => Right(stop(active, CommentaryJobStopCondition.InvalidEngineWorkReport))
      case Some(physical) =>
        EngineLineAdmission.bindAdmittedLineSuffixes(active.issuedWork, physical) match
          case None =>
            val legalInput = MoveReviewInputAdmission.retainLegalContinuations(
              active.packet.assembly.input, physical.map(_.continuation))
            Right(assembleRetainedPacket(legalInput, active.packet) match
              case Some((packet, failure)) =>
                stop(active.copy(packet = packet), failure.getOrElse(CommentaryJobStopCondition.InvalidEngineWorkReport))
              case None => stop(active, CommentaryJobStopCondition.ReviewConstructionFailed))
          case Some(evaluations) =>
            val acceptedLedger = active.ledger.afterAcceptedReport
            val source = active.issuedWork.purpose match
              case EngineWorkPurpose.RootSearch => EvaluationSource.RootSearch
              case EngineWorkPurpose.FocusComparison => EvaluationSource.FocusComparison
              case EngineWorkPurpose.InvestigationComparison => EvaluationSource.InvestigationComparison
              case EngineWorkPurpose.ResponseComparison => EvaluationSource.ResponseComparison
              case EngineWorkPurpose.ThreatProbe => return Right(stop(active, CommentaryJobStopCondition.InvalidEngineWorkReport))
            val response = source == EvaluationSource.ResponseComparison
            val baseInput = active.phase match
              case focus: CollectingFocus => MoveReviewInputAdmission.retainAdmittedLines(
                active.packet.assembly.input, focus.rootEvaluations, EvaluationSource.RootSearch)
              case _ => active.packet.assembly.input
            val retainedInput =
              if response then MoveReviewInputAdmission.retainLegalContinuations(
                baseInput, physical.map(_.continuation))
              else MoveReviewInputAdmission.retainAdmittedLines(baseInput, evaluations, source)
            val receipt = CommentaryInvestigation.EngineCandidates(active.issuedWork, evaluations)
            val acceptedActive = active.copy(ledger = acceptedLedger,
              receipts = if response then active.receipts else active.receipts :+ receipt,
              rootSearches = if response then active.rootSearches else active.rootSearches.map(search =>
                if search.work == active.issuedWork then search.copy(receipt = Some(receipt)) else search),
              responseReceipts = if response then active.responseReceipts :+ receipt else active.responseReceipts)
            // An admitted root challenge remains a refresh obligation even when its new paths fail assembly.
            val pendingAssessment = active.phase match
              case phase: InvestigationPhase if source == EvaluationSource.InvestigationComparison && !phase.refreshing =>
                rootAssessmentRefresh(acceptedActive, phase, evaluations).fold(acceptedActive)(refresh =>
                  acceptedActive.copy(phase = refresh))
              case _ => acceptedActive
            // The first root survey cannot assess an omitted played move yet.
            // Keep its exact admitted receipt and assemble its physical paths
            // together with the required focus reply, rather than twice.
            if active.phase.isInstanceOf[CollectingRoot] && active.assessment.isEmpty &&
                evaluations.nonEmpty && evaluations.flatMap(_.evaluation.engineLine.map(_.depth)).distinct.size == 1 &&
                !evaluations.exists(_.evaluation.moves.headOption.contains(active.playedMoveUci)) then
              return Right(afterRootReport(pendingAssessment, evaluations, acceptedLedger))
            assembleRetainedPacket(retainedInput, active.packet) match
              case None => Right(stop(pendingAssessment, CommentaryJobStopCondition.ReviewConstructionFailed))
              case Some((packet, Some(failure))) => Right(stop(pendingAssessment.copy(packet = packet), failure))
              case Some((packet, None)) =>
                val retained = pendingAssessment.copy(packet = packet)
                active.phase match
                  case _: CollectingRoot | _: CollectingFocus
                      if evaluations.flatMap(_.evaluation.engineLine.map(_.depth)).distinct.size > 1 =>
                    Right(stop(retained, CommentaryJobStopCondition.InvalidEngineWorkReport))
                  case _: CollectingRoot => Right(afterRootReport(retained, evaluations, acceptedLedger))
                  case focus: CollectingFocus =>
                    Right(beginPreparedReview(retained, focus.rootEvaluations, evaluations, acceptedLedger))
                  case investigation: Investigating =>
                    Right(afterInvestigationReport(retained, investigation, evaluations))
                  case refresh: RefreshingAssessment =>
                    Right(refreshAssessment(retained, refresh, evaluations))
                  case investigation: InvestigatingResponses =>
                    Right(afterResponseReport(retained, investigation, evaluations))
                  case investigation: InvestigatingProposals =>
                    val read = readPrincipalVariations(retained,
                      reviewReference(investigation.baseline).getOrElse(retained.playedMoveUci))
                    if !CommentaryInvestigation.candidatesReady(read.issuedWork, evaluations) ||
                        (read.issuedWork.searchAnchor.nonEmpty && !read.responseSearches.exists(_.work == read.issuedWork)) then
                      Right(stop(read, CommentaryJobStopCondition.InvalidEngineWorkReport))
                    else if investigation.verifyAfter then
                      Right(issueVerification(refreshProposalReadings(read), investigation.baseline, read.verification.get))
                    else if read.issuedWork.searchAnchor.isEmpty then
                      Right(reconcileRootAssessment(read, investigation, evaluations))
                    else Right(afterConditionalReport(read, investigation.baseline, investigation.questions))
                  case investigation: InvestigatingFacts =>
                    val read = readPrincipalVariations(retained,
                      reviewReference(investigation.baseline).getOrElse(retained.playedMoveUci))
                    if !read.issuedWork.searchAnchor.exists(_.prefixMoves == investigation.prefixMoves) ||
                        !read.responseSearches.exists(_.work == read.issuedWork) then
                      Right(stop(read, CommentaryJobStopCondition.InvalidEngineWorkReport))
                    else Right(afterConditionalReport(read, investigation.baseline, investigation.questions))
                  case _: ProbingThreat => Right(stop(retained, CommentaryJobStopCondition.InvalidEngineWorkReport))

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
        val playedRootLineAdmitted = ranked.exists(
          _.evaluation.moves.headOption.exists(EvidenceRef.sameMove(_, active.playedMoveUci))
        )
        if playedRootLineAdmitted then beginPreparedReview(active, ranked, Nil, ledger)
        else
          val limits = EngineSearchLimits(
            active.issuedWork.requiredDepth,
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
          val baseline = active.phase match
            case root: CollectingRoot => root.baseline
            case _ => None
          // A deeper root can omit the played move after child work has already completed.
          // Publish the new observations with readers bound to this packet while the old
          // assessment waits for its same-depth focus comparison.
          val read = baseline.fold(active)(previous => refreshProposalReadings(
            readPrincipalVariations(active, reviewReference(previous).getOrElse(bestMove))))
          val rebound = baseline.map(previous => previous.copy(
            responseInquiries = previous.responseInquiries.map(bindResponseReading(read, _))))
          issue(read, CollectingFocus(ranked, rebound), work, ledger)

  private def beginPreparedReview(
      active: ActiveCommentaryJob,
      rootEvaluations: List[AdmittedCandidateLine],
      focusEvaluations: List[AdmittedCandidateLine],
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    Try(MoveReviewInputAdmission.prepareFocusedMoveReview(
      active.positionHistory,
      active.playedMoveUci,
      active.resultingFen,
      active.rootRuleAssessment,
      rootEvaluations,
      focusEvaluations
    )).toOption.flatten match
      case None => stop(active, CommentaryJobStopCondition.MoveReviewPreparationFailed)
      case Some(PreparedFocusedPositionCommentary.PositionAction(action)) =>
        val preliminary = completePositionAction(withCompletedVerification(active), action, rootEvaluations.size, ledger)
        if preliminary.verification.exists(_.state == "pending") then
          return issueVerification(active, preliminary, preliminary.verification.get)
        prepareProposalInputs(active) match
          case Left((retained, failure)) => stop(retained, failure)
          case Right(retained) =>
            val read = refreshProposalReadings(readPrincipalVariations(retained, active.playedMoveUci))
            val assessed = withCompletedVerification(read)
            val completed = completePositionAction(assessed, action, rootEvaluations.size, ledger)
            if completed.verification.exists(_.state == "pending") then
              issueVerification(assessed, completed, completed.verification.get)
            else completed.copy(proposalEntries = CommentaryProposal.stop(completed.proposalEntries, "position_action"))
      case Some(PreparedFocusedPositionCommentary.MoveReview(review)) =>
        runReview(
          active,
          ReviewContext(review, rootEvaluations),
          ledger
        )

  private def runReview(
      active: ActiveCommentaryJob,
      context: ReviewContext,
      ledger: EngineWorkLedger
  ): CommentaryJobState =
    val prepared = context.review.preparedInput
    assembleRetainedPacket(prepared, active.packet) match
      case None => stop(active, CommentaryJobStopCondition.ReviewConstructionFailed)
      case Some((packet, Some(failure))) => stop(active.copy(packet = packet), failure)
      case Some((packet, None)) =>
        val verified = withCompletedVerification(active)
        val completed = completeReview(
          verified,
          context,
          packet,
          ledger
        )
        if verified.verification.exists(_.state == "pending") then
          issueVerification(verified.copy(packet = packet), completed, verified.verification.get)
        else if active.policy.investigateChoices || active.proposalEntries.nonEmpty then
          beginInvestigation(verified.copy(packet = packet), completed)
        else completed

  private def withCompletedVerification(active: ActiveCommentaryJob): ActiveCommentaryJob =
    active.verification match
      case Some(value) if active.issuedWork.requiredDepth >= value.targetDepth =>
        active.copy(verification = Some(value.copy(state = "complete",
          completedDepth = Some(active.issuedWork.requiredDepth), stopReason = None)))
      case _ => active

  /** Return this work with the existing assessment before running discovery or another engine search. */
  private def issueVerification(active: ActiveCommentaryJob, baseline: CompletedCommentaryJobRecord,
      verification: CommentaryVerification): CommentaryJobState =
    if verification.targetDepth > MaximumAnalysisDepth then
      val bounded = active.copy(verification = Some(verification.copy(state = "stopped",
        stopReason = Some("depth_limit"))))
      return finishInvestigation(bounded, baseline, baseline.inquiries,
        baseline.responseInquiries, needsRefresh = true)
    // The child comparison has already changed the assessment's required
    // depth. Complete that root obligation with the existing larger profile
    // when the request still has room; do not give it another small root pass.
    val completion = verification.reason == "child_challenge" && active.remainingBudgetMs >= 9000
    val extended = completion && active.remainingBudgetMs >= CompletionRootElapsedMs
    val work = issuedWork(active.engineProfile, active.ledger.issuedWorkId, EngineWorkPurpose.RootSearch,
      active.positionHistory, EngineRootRestriction.Unrestricted, active.positionHistory.currentFen, Nil,
      EngineSearchLimits(verification.targetDepth,
        if extended then CompletionRootNodes else if completion then SelectedReplyNodes else RootNodes,
        if extended then CompletionRootMovetimeMs else if completion then SelectedReplyMovetimeMs else RootMovetimeMs,
        active.legalMoves.size.min(3)),
      if extended then CompletionRootElapsedMs else if completion then SelectedReplyElapsedMs else RootElapsedMs)
    issue(active.copy(verification = Some(verification)), CollectingRoot(Some(baseline)), work, active.ledger)

  private[runtime] def assembleRetainedPacket(
      prepared: lila.chessjudgment.model.judgment.AdmittedMoveReviewInput,
      retained: EvidenceBackedJudgmentPacket
  ): Option[(EvidenceBackedJudgmentPacket, Option[CommentaryJobStopCondition])] =
    Try(MoveReviewAssembler.assembleOccurrences(prepared, Some(retained.assembly))).toOption
      .flatMap(assessOccurrences)

  /** None means lower assembly failed; a returned failure retains the new lower packet. */
  private[runtime] def assessOccurrences(
      occurrences: lila.chessjudgment.model.judgment.JudgmentAssemblyContext
  ): Option[(EvidenceBackedJudgmentPacket, Option[CommentaryJobStopCondition])] =
    Try(EvidenceBackedJudgmentPacket.fromAssembly(occurrences)).toOption.flatten.map { lower =>
      Try(EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.withAssessment(occurrences)))
        .toOption.flatten match
          case Some(packet) => packet -> None
          case None => lower -> Some(CommentaryJobStopCondition.ReviewConstructionFailed)
    }

  private def completePositionAction(
      active: ActiveCommentaryJob,
      action: PlayerFacingPositionAction,
      rootLineCount: Int,
      ledger: EngineWorkLedger
  ): CompletedCommentaryJobRecord =
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
      ),
      active.observationsPacket,
      rootSearches = active.rootSearches,
      proposalEntries = active.proposalEntries,
      localExpansionsUsed = active.localExpansionsUsed,
      discoveryState = active.discoveryState,
      verification = active.verification.map(value => action match
        case _: PlayerFacingPositionAction.ForcedSingleMove => value.copy(state = "not_required", stopReason = None)
        case _ => value)
    )

  private def completeReview(
      active: ActiveCommentaryJob,
      context: ReviewContext,
      packet: EvidenceBackedJudgmentPacket,
      ledger: EngineWorkLedger
  ): CompletedCommentaryJobRecord =
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
      ),
      Some(packet),
      rootSearches = active.rootSearches,
      proposalEntries = active.proposalEntries,
      localExpansionsUsed = active.localExpansionsUsed,
      discoveryState = active.discoveryState,
      verification = active.verification
    )

  private def issue(
      active: ActiveCommentaryJob,
      phase: JobPhase,
      work: IssuedEngineWork,
      ledger: EngineWorkLedger,
      searchHistory: Option[CanonicalPositionHistory] = None
  ): CommentaryJobState =
    releaseUnusedCalculations(active.copy(phase = phase, issuedWork = work, ledger = ledger.afterIssuing,
      rootSearches = if Set(EngineWorkPurpose.ResponseComparison, EngineWorkPurpose.ThreatProbe)(work.purpose) then active.rootSearches
        else active.rootSearches :+ CommentaryRootSearch(work),
      searchHistory = searchHistory.orElse(Some(active.positionHistory))))

  /** Root discovery and response investigation share two exploratory slots.
    * A discovered root winner may require a separate actual/winner refresh.
    * None replenishes the request deadline.
    */
  private def beginInvestigation(active: ActiveCommentaryJob,
      baseline: CompletedCommentaryJobRecord): CommentaryJobState =
    val reference = baseline.result match
      case CompletedPositionCommentary.FocusedMoveReview(_, review) => review.bestMoveUci
      case _ => return baseline
    if active.policy.investigateChoices && active.policy.probeThreats then
      // Two bounded origins cover an existing danger and a threat created by the reviewed move.
      // They do not replenish the two legal investigation slots or the request deadline.
      val next = List(Nil, List(active.playedMoveUci)).filterNot(active.threatOriginsTried).flatMap { prefix =>
        active.positionHistory.extend(prefix).toOption.flatMap(CommentaryThreat.hypothesis).map(prefix -> _)
      }.headOption
      next match
       case Some((prefix, hypothesis)) =>
        val work = IssuedEngineWork.create(active.engineProfile, active.ledger.issuedWorkId, EngineWorkPurpose.ThreatProbe,
          hypothesis.initialFen, Nil, hypothesis.currentFen, EngineRootRestriction.Unrestricted,
          active.positionHistory.currentFen, Nil, EngineSearchLimits(8, 250000, 750,
            hypothesis.currentLegalMoveUcis.size.min(2)), 1250, threatOriginPrefixUci = Some(prefix))
        return issue(active.copy(threatOriginsTried = active.threatOriginsTried + prefix),
          ProbingThreat(baseline, prefix), work, active.ledger, Some(hypothesis))
       case None => ()
    val retained = prepareProposalInputs(active) match
      case Left((retained, failure)) => return stop(retained, failure, Some(baseline))
      case Right(retained) => retained
    val read = readPrincipalVariations(retained, reference)
    val questions = (if active.policy.investigateChoices then CommentaryInvestigation.discover(read.packet, reference,
      currentRootReceipt(read), read.pvComparisons, read.preparedPaths)
      .map(question => reuseInquiry(read, question)) else Nil)
    val retainedQuestions = questions ++ active.inquiries.filterNot(old => questions.exists(_.question.id == old.question.id))
      .map(reconsiderInquiry(read, _))
    beginResponses(read, baseline, retainedQuestions)

  private def currentRootReceipt(active: ActiveCommentaryJob): Option[CommentaryInvestigation.EngineCandidates] =
    active.receipts.reverse.filter(_.work.purpose == EngineWorkPurpose.RootSearch)
      .sortBy(receipt => -receipt.lines.flatMap(_.evaluation.engineLine.map(_.depth)).minOption.getOrElse(receipt.work.requiredDepth))
      .headOption

  private def investigationDepth(active: ActiveCommentaryJob): Int =
    (active.policy.rootSearchDepth :: active.verification.flatMap(_.completedDepth).toList ++
      active.verification.filter(_.reason == "child_challenge").map(_.targetDepth).toList ++
      (active.phase match
        case refresh: RefreshingAssessment => List(refresh.requiredDepth)
        case _ => Nil) ++
      active.packet.assembly.input.assessmentCandidates.flatMap(_.evaluation.engineLine.map(_.depth))).max

  private def currentAnswer(active: ActiveCommentaryJob, answer: CommentaryInvestigation.Answer): Boolean =
    List(answer.referenceDepth, answer.alternativeDepth).flatten.forall(_ >= investigationDepth(active))

  private def explanationReceipts(active: ActiveCommentaryJob): List[CommentaryInvestigation.EngineCandidates] =
    active.receipts.filter(receipt => receipt.lines.flatMap(_.evaluation.engineLine.map(_.depth))
      .forall(_ >= investigationDepth(active)))

  private def historyAt(active: ActiveCommentaryJob, prefix: List[String]): Option[CanonicalPositionHistory] =
    if prefix.isEmpty then Some(active.positionHistory)
    else active.responseHistories.get(prefix.mkString(" ")).getOrElse {
      Option.when(active.exploredPrefixes(prefix) ||
        CommentaryProposal.observedPrefix(active.packet, active.preparedPaths.get, prefix).nonEmpty)(())
        .flatMap(_ => active.positionHistory.extend(prefix).toOption)
    }

  /** The public packet owns its evidence. Replay caches survive only while a current or pending
    * consumer needs them; the small exact-prefix ledger survives eviction and prevents recharging.
    */
  private def releaseUnusedCalculations(active: ActiveCommentaryJob): ActiveCommentaryJob =
    val proposalPrefixes = CommentaryIdea.currentQuestions(active.proposalEntries).flatMap { entry =>
      val needed = CommentaryProposal.neededRequirementIds(entry)
      entry.request.requirements.filter(value => needed(value.id)).map(CommentaryProposal.prefix) ++
        entry.readings.filter(value => needed(value.requirementId)).flatMap(_.result.toList.flatMap(_.needs.map(_.prefixMoves)))
    }
    val live = (active.issuedWork.searchAnchor.toList.map(_.prefixMoves) ++
      active.responseReceipts.flatMap(_.work.searchAnchor.map(_.prefixMoves)) ++ proposalPrefixes ++
      active.continuationInquiries.filter(value => Set("deferred", "forced")(value.state) ||
        value.routeChallenge.exists(_.state == "not_returned")).map(_.question.prefixMoves) ++
      active.responseInquiries.filter(_.answer.isEmpty).map(value => List(value.question.rootMove)) ++
      active.discoveryState.scans.flatMap(_.pendingReplayPrefixes)).map(_.mkString(" ")).toSet
    active.copy(responseHistories = active.responseHistories.filter((key, value) => value.isEmpty || live(key)),
      inheritedPrepared = active.preparedPaths)

  /** Local candidate replay uses the same exact history and legal-line admission as returned PVs.
    * The quota belongs to the reviewed root, including every newly materialized prefix edge.
    */
  private final class LocalReplayBatch(active: ActiveCommentaryJob):
    private var histories = active.responseHistories.updated("", Some(active.positionHistory))
    private val known = scala.collection.mutable.Set.empty[List[String]]
    known ++= active.exploredPrefixes
    known += Nil
    active.packet.assembly.input.legalLines.foreach(line => line.moves.inits.foreach(known.add))
    histories.foreach { (key, value) =>
      if value.nonEmpty then known += key.split(" ").filter(_.nonEmpty).toList
    }
    private var used = active.localExpansionsUsed
    private val requested = scala.collection.mutable.LinkedHashMap.empty[List[String], CanonicalPositionHistory]
    private val discoveryOnlyRequests = scala.collection.mutable.Map.empty[List[String], Boolean]
    private val exactRequests = scala.collection.mutable.Set.empty[List[String]]
    private val existingLines = active.packet.assembly.input.legalLines.map(line => line.moves -> line).toMap
    private def unpublished = requested.toList.filter { (moves, _) =>
      !existingLines.valuesIterator.exists(line =>
        (if exactRequests(moves) then line.moves == moves else line.moves.startsWith(moves)) &&
        (discoveryOnlyRequests(moves) || !line.discoveryOnly))
    }
    def hasAdditions: Boolean = unpublished.nonEmpty
    def probePaths(previous: List[OccurrenceResources.ProbePath]): List[OccurrenceResources.ProbePath] =
      OccurrenceResources.probePaths(active.packet, requested.values.toList, previous)
    def tracked(entries: List[CommentaryProposal.Entry]): ActiveCommentaryJob =
      active.copy(responseHistories = histories, exploredPrefixes = known.toSet,
        localExpansionsUsed = used, proposalEntries = entries)
    def ensurePath(moves: List[String], discoveryOnly: Boolean = false,
        exact: Boolean = false): Either[String, CanonicalPositionHistory] =
      var prefix = List.empty[String]
      val cached = Option.when(known(moves))(histories.get(moves.mkString(" ")).flatten).flatten
      var history = cached.getOrElse(active.positionHistory)
      var failure = Option.empty[String]
      val iterator = if cached.nonEmpty then Iterator.empty else moves.iterator
      while iterator.hasNext && failure.isEmpty do
        val move = iterator.next()
        val next = prefix :+ move
        val key = next.mkString(" ")
        if known(next) then
          histories.get(key).flatten.orElse(history.extend(List(move)).toOption) match
            case Some(value) =>
              history = value
              histories = histories.updated(key, Some(value))
            case None => failure = Some("invalid_continuation")
        else if !PositionRuleAssessment.assess(history).isInstanceOf[PositionRuleAssessment.Nonterminal] then
          failure = Some("automatic_terminal")
        else if !history.currentLegalMoveUcis.contains(move) then failure = Some("illegal_move")
        else if used >= active.policy.maximumLocalExpansions then failure = Some("local_expansion_limit")
        else
          // Reserve once before preparing a new edge; failures never replenish the job's quota.
          used += 1
          history.admitToFirstAutomaticTerminal(List(move)).toOption match
            case None => failure = Some("invalid_continuation")
            case Some(value) =>
              history = value.resultingHistory
              known += next
              histories = histories.updated(key, Some(history))
        prefix = next
      // Preserve a legal partial prefix too. Knowing an edge and publishing a path are separate facts.
      val accepted = history.movePrefixUci.drop(active.positionHistory.movePrefixUci.size)
      if accepted.nonEmpty then
        requested.update(accepted, history)
        discoveryOnlyRequests.update(accepted, discoveryOnlyRequests.getOrElse(accepted, true) && discoveryOnly)
        if exact && accepted == moves then exactRequests += accepted
      failure.toLeft(history)
    def retain(entries: List[CommentaryProposal.Entry]): Either[(ActiveCommentaryJob, CommentaryJobStopCondition), ActiveCommentaryJob] =
      val retained = tracked(entries)
      if !hasAdditions then Right(retained)
      else
        val paths = OccurrenceResources.probePaths(active.packet, unpublished.map(_._2))
        val input = paths.groupBy(path => discoveryOnlyRequests(path.moves)).toList
          .foldLeft(Option(active.packet.assembly.input)) { case (input, (discoveryOnly, selected)) =>
            input.flatMap(MoveReviewInputAdmission.retainProbePaths(_, selected, discoveryOnly))
          }
        input.flatMap(assembleRetainedPacket(_, active.packet)) match
          case Some((packet, None)) => Right(retained.copy(packet = packet))
          case Some((packet, Some(failure))) => Left(retained.copy(packet = packet) -> failure)
          case None => Left(retained -> CommentaryJobStopCondition.ReviewConstructionFailed)

  /** Discovery may inspect only the root and positions actually selected for a returned search.
    * Locally replayed candidates never register more discovery anchors or source plans.
    */
  private def discoverQuestions(active: ActiveCommentaryJob, reference: String)
      : Either[(ActiveCommentaryJob, CommentaryJobStopCondition), ActiveCommentaryJob] =
    if !active.policy.investigateChoices then return Right(active)
    import CommentaryDiscovery.{ Anchor, SourcePath, State }
    val prepared = active.preparedPaths.get
    val anchors = Anchor(Nil, reference) :: active.responseSearches.reverse
      .distinctBy(_.anchor.prefixMoves).flatMap(search => search.lines.headOption.map(line =>
        Anchor(search.anchor.prefixMoves, line.evaluation.moves.head)))
    val currentReceipts = CommentaryInvestigation.preferredReceipts(explanationReceipts(active))
      .distinctBy(receipt => receipt.work.rootRestriction)
    val rootSources = currentReceipts.flatMap(receipt => receipt.lines.flatMap(line =>
      prepared.certifiedPath(active.packet, line.evaluation.moves).map(path => SourcePath(line.evaluation.moves,
        CommentaryProposal.Source("generator", receipt.work.purpose.wireId,
          sourceRefs = List(receipt.work.workId, path.line.ref.id))))))
    val responseSources = active.responseSearches.flatMap(search => search.lines.zipWithIndex.map { (line, rank) =>
      val path = search.pathsByMove(line.evaluation.moves.head)
      SourcePath(path.moves, CommentaryProposal.Source("generator", search.work.purpose.wireId,
        sourceRefs = List(search.work.workId, path.line.ref.id)), Some(search.anchor.prefixMoves -> rank))
    })
    val suppliedSources = active.packet.assembly.input.suppliedLines.flatMap(line =>
      Option.when(line.acceptedMoves.nonEmpty)(line.acceptedMoves).flatMap(moves =>
        CommentaryProposal.observedPrefix(active.packet, prepared, moves).map(path => SourcePath(moves,
          CommentaryProposal.Source("user", "supplied_" + line.input.source.wireId,
            sourceRefs = List(path.line.ref.id))))))
    val sources = (rootSources ++ responseSources ++ suppliedSources).distinct
      .sortBy(source => (source.moves.mkString(" "), source.source.toString))
    val inputKey = CommentarySelection.stableId("discovery-input", List(CommentaryDiscovery.GeneratorVersion) ++
      anchors.map(anchor => anchor.prefixMoves.mkString(" ") + ":" + anchor.referenceMove) ++
      sources.map(source => source.moves.mkString(" ") + ":" + source.source.toString + ":" + source.choiceRank) ++
      // An answered preparation can advance the frontier even when the returned PV is unchanged.
      active.proposalEntries.filter(entry => entry.admission == "generated" &&
        (entry.request.requirements.exists(_.id == "reference_action") ||
          entry.request.origin.question.producer == "C-tactical-counter")).sortBy(_.request.id).map(entry =>
        entry.request.id + ":" + entry.generatedArms.flatMap(_.headRequirementIds)
          .distinct.sorted.map(id => id + ":" + entry.readings.find(_.requirementId == id)
            .map(reading => CommentaryProposal.isResolved(reading).toString).getOrElse("missing")).mkString(",")))
    def inventoryKey(job: ActiveCommentaryJob): String = CommentarySelection.stableId("discovery-inventory",
      job.packet.resources.map(view => view.occurrence.id + ":" + view.completeKinds.toList.map(_.toString).sorted.mkString(",") +
        ":" + view.legalRows.map(_.map(_.move).mkString(",")).getOrElse("unknown")).sorted)
    val previous = active.discoveryState
    val pendingBecameAvailable = previous.scans.flatMap(_.pendingReplayPrefixes).exists(moves =>
      CommentaryProposal.observedPrefix(active.packet, prepared, moves).nonEmpty)
    val incompleteInventoryChanged = previous.scans.exists(_.reason.exists(_.startsWith("incomplete_"))) &&
      previous.inventoryKey != inventoryKey(active)
    if previous.inputKey == inputKey && !pendingBecameAvailable && !incompleteInventoryChanged then return Right(active)

    var retained = active
    // Intermediate replay waves have not been published. Admit their final candidate union once,
    // so readily available contact facts cannot fill the budget before reply witnesses arrive.
    val publishedEntries = active.proposalEntries
    var probes = List.empty[OccurrenceResources.ProbePath]
    var wave = 0
    var continue = true
    while continue do
      val batch = CommentaryDiscovery.discover(retained.packet, retained.preparedPaths.get,
        anchors, sources, publishedEntries, probes,
        pvGuided = active.policy.shortJob || active.policy.previewDepth.nonEmpty || active.remainingBudgetMs <= 10000)
      val existing = publishedEntries.map(entry => entry.request.id -> entry).toMap
      val additions = batch.additions.filter(entry =>
        existing.get(entry.request.id).forall(_.admission == "generated")).map(_.copy(admission = "generated"))
      val replacements = additions.map(entry => entry.request.id -> entry).toMap
      val entries = publishedEntries.map(entry => replacements.getOrElse(entry.request.id, entry)) ++
        additions.filterNot(entry => existing.contains(entry.request.id))
      if !CommentaryProposal.validRequests(entries.map(_.request)) then
        return Left(retained -> CommentaryJobStopCondition.ReviewConstructionFailed)
      retained = retained.copy(proposalEntries = entries,
        discoveryState = State(inputKey, batch.scans,
          if batch.scans.exists(_.reason.exists(_.startsWith("incomplete_"))) then inventoryKey(retained) else ""))
      def pause(reason: String): Unit =
        retained = retained.copy(discoveryState = retained.discoveryState.copy(scans =
          retained.discoveryState.scans.map(scan =>
            if scan.state == "partial" && scan.reason.exists(pendingReason =>
                Set("missing_reference_path", "candidate_replay_pending", "response_probe_pending")(pendingReason)) then scan.copy(reason = Some(reason))
            else scan)))
        continue = false
      if batch.replayNeeds.isEmpty then continue = false
      else if wave >= 2 && (wave >= 5 || batch.functionalReplayNeeds.isEmpty) then pause(
        if retained.localExpansionsUsed >= retained.policy.maximumLocalExpansions then "local_expansion_limit"
        else "discovery_wave_limit")
      else
        val replay = new LocalReplayBatch(retained)
        var failure: Option[String] = None
        val replayNeeds = if wave < 2 then (batch.functionalReplayNeeds ++ batch.replayNeeds).distinct
          else batch.functionalReplayNeeds.distinct
        replayNeeds.foreach(moves => replay.ensurePath(moves, discoveryOnly = true) match
          case Left(reason) => if failure.isEmpty || reason != "local_expansion_limit" then failure = Some(reason)
          case Right(_) => ())
        val previousProbeMoves = probes.map(_.moves).toSet
        probes = replay.probePaths(probes)
        retained = replay.tracked(entries)
        wave += 1
        if probes.forall(path => previousProbeMoves(path.moves)) then pause(failure.getOrElse("no_new_evidence"))
    // Use the same temporal interpretation before building public evidence. An event-before
    // sample needs its event edge; an absent event needs the whole inspected scope. Unknown
    // local predicates still request promotion instead of being discarded as unimportant.
    // Promote the concrete witnesses referenced by admitted question readings.
    val generated = retained.proposalEntries.filter(_.admission == "generated")
    val inspection = CommentaryFactReader.inspect(retained.packet, retained.preparedPaths.get,
      generated, probes, retained.continuationInquiries)
    val required = generated.flatMap(entry => entry.request.requirements.flatMap(requirement =>
      inspection.get(entry.request.id).flatMap(_.get(requirement.id)).toList.flatMap(_.promotionNeeds)))
      .filter(_.nonEmpty).distinct
    val histories = required.flatMap(prefix => retained.responseHistories.get(prefix.mkString(" ")).flatten)
    if histories.size != required.size then return Left(retained -> CommentaryJobStopCondition.ReviewConstructionFailed)
    val selected = if required.isEmpty then Nil else
      val byMoves = OccurrenceResources.probePaths(retained.packet, histories, probes).map(path => path.moves -> path).toMap
      required.map(byMoves)
    if selected.isEmpty then Right(retained)
    else MoveReviewInputAdmission.retainProbePaths(retained.packet.assembly.input, selected)
      .flatMap(input => assembleRetainedPacket(input, retained.packet)) match
        case Some((packet, None)) => Right(retained.copy(packet = packet))
        case Some((packet, Some(failure))) => Left(retained.copy(packet = packet) -> failure)
        case None => Left(retained -> CommentaryJobStopCondition.ReviewConstructionFailed)

  private def prepareProposalInputs(active: ActiveCommentaryJob, onlyProducer: Option[String] = None)
      : Either[(ActiveCommentaryJob, CommentaryJobStopCondition), ActiveCommentaryJob] =
    if active.proposalEntries.isEmpty then return Right(active)
    val replay = new LocalReplayBatch(active)
    val entries = active.proposalEntries.map { entry =>
      val needed = if onlyProducer.exists(_ != entry.request.origin.question.producer) then Set.empty[String]
        else CommentaryProposal.neededRequirementIds(entry)
      def ensure(moves: List[String]) = replay.ensurePath(moves, discoveryOnly = entry.admission == "generated")
      entry.copy(readings = entry.request.requirements.map { requirement =>
        val previous = entry.readings.find(_.requirementId == requirement.id).get
        val result = if !needed(requirement.id) then Right(()) else requirement match
          case value: CommentaryProposal.ObserveMove => ensure(value.prefixMovesUci :+ value.moveUci).map(_ => ())
          case value: CommentaryProposal.CompareMoves => ensure(value.prefixMovesUci).flatMap { history =>
            if !PositionRuleAssessment.assess(history).isInstanceOf[PositionRuleAssessment.Nonterminal] then Left("automatic_terminal")
            else if !List(value.referenceMove, value.alternativeMove).forall(history.currentLegalMoveUcis.contains) then Left("illegal_move")
            else Right(())
          }
          case _: CommentaryProposal.Uninterpreted => Right(())
          case value: CommentaryProposal.Fact => value.path.source match
            case CommentaryFact.Declared(moves) => ensure(moves).map(_ => ())
            case _ => Right(())
          case _: CommentaryProposal.FactComparison | _: CommentaryProposal.FactGroup => Right(())
        result.fold(reason => previous.copy(state = if reason == "local_expansion_limit" then "deferred" else "invalid_input",
          stopReason = Some(reason)), _ =>
          if previous.state == "deferred" && previous.stopReason.contains("local_expansion_limit") then
            previous.copy(stopReason = None)
          else previous)
      })
    }
    replay.retain(entries)

  /** Re-reading is separate from replay: partial legal reports and cancellation can add witnesses too. */
  private def refreshProposalReadings(active: ActiveCommentaryJob): ActiveCommentaryJob =
    if active.proposalEntries.isEmpty then return active
    val prepared = active.preparedPaths.get
    // Several heads can ask the same oriented comparison. Resolve its owned/current
    // answer once in this refresh; each head still applies its own expected relation.
    val comparisons = scala.collection.mutable.Map.empty[
      (List[String], String, String, Boolean), (Option[CommentaryInvestigation.Answer], Boolean, Boolean)]
    def readComparisons(input: List[CommentaryProposal.Entry]) = input.map { entry =>
      entry.copy(readings = entry.request.requirements.map { requirement =>
        val prior = entry.readings.find(_.requirementId == requirement.id).get
        requirement match
          case _: CommentaryProposal.Uninterpreted => prior
          case _: CommentaryProposal.Fact | _: CommentaryProposal.FactComparison | _: CommentaryProposal.FactGroup => prior
          case value: CommentaryProposal.ObserveMove =>
            CommentaryProposal.observedPrefix(active.packet, prepared, value.prefixMovesUci :+ value.moveUci)
              .flatMap(path => for
                occurrence <- path.ids.lift(value.prefixMovesUci.size)
                position <- path.positions.lift(value.prefixMovesUci.size + 1).flatMap(_.id)
              yield CommentaryProposal.Reading(value.id, "observed", lineId = Some(path.line.ref.id),
                occurrenceId = Some(occurrence), positionId = Some(position)))
              .getOrElse(prior)
          case value: CommentaryProposal.CompareMoves =>
            val functionalValue = value.id.startsWith(FunctionalChoiceReading.ValuePrefix) ||
              value.id.startsWith(AnchoredChoiceReading.ValuePrefix)
            val (answer, outdated, choiceConflict) = comparisons.getOrElseUpdate(
              (value.prefixMovesUci, value.referenceMove, value.alternativeMove, functionalValue), {
                val choice = Option.when(functionalValue)(CommentaryChoiceValue.resolve(active.packet,
                  prepared, value, active.responseSearches, active.engineProfile.wireId, investigationDepth(active)))
                val answers = if functionalValue then choice.iterator.flatMap(_.toOption.flatten).map(_.answer)
                else if value.prefixMovesUci.isEmpty then CommentaryInvestigation.preferredReceipts(active.receipts).iterator.flatMap(receipt =>
                  CommentaryInvestigation.pairedAnswer(active.packet, value.referenceMove, value.alternativeMove,
                    receipt.work, receipt.lines, Some(prepared)))
                else active.responseSearches.reverseIterator.filter(_.anchor.prefixMoves == value.prefixMovesUci)
                  .flatMap(_.pairAnswer(active.packet, value.referenceMove, value.alternativeMove))
                var outdated = false
                val answer = answers.find { answer =>
                  val current = currentAnswer(active, answer)
                  outdated ||= !current
                  current
                }
                (answer, outdated, choice.exists(_.isLeft))
              })
            val conflict = answer.exists(answer => active.comparisonConflicts.exists(_.affects(value, answer))) ||
              choiceConflict
            val awaiting = active.ledger.physicalReportsAccepted < active.ledger.physicalWorksIssued &&
              proposalWorkMatches(value, active.issuedWork)
            // A new owned revalidation is pending even when the historical answer
            // disagrees. Keep its issued owner, but never publish the old answer.
            if awaiting && (conflict || answer.isEmpty) && !Set("invalid_input", "stopped")(prior.state) then prior.copy(state = "awaiting_engine",
              stopReason = None, answer = None, workId = Some(active.issuedWork.workId))
            else if conflict then CommentaryProposal.Reading(value.id,
              if prior.state == "stopped" then "stopped" else "deferred",
              if prior.state == "stopped" then prior.stopReason else Some("comparison_conflict"))
            else answer.map(CommentaryProposal.pairReading(value, _)).getOrElse {
              if outdated || prior.answer.nonEmpty then prior.copy(state = "deferred", answer = None, workId = None,
                stopReason = Some("comparison_not_current"))
              else if prior.state == "awaiting_engine" then prior.copy(state = "deferred", workId = None,
                stopReason = Some("not_returned"))
              else prior
            }
      })
    }
    val entries = readComparisons(active.proposalEntries)
    val facts = CommentaryFactReader.read(active.packet, prepared, entries, active.continuationInquiries)
    val reference = active.phase match
      case phase: InvestigationPhase => reviewReference(phase.baseline).getOrElse(active.playedMoveUci)
      case _ => active.playedMoveUci
    val functional = FunctionalChoiceReading.withQuestions(active.packet, prepared, facts,
      active.responseSearches, Set(active.playedMoveUci, reference),
      active.engineProfile.wireId, investigationDepth(active))
    val realizationResources = FunctionalChoiceReading.task(active.packet, prepared, functional,
      active.responseSearches, Set(active.playedMoveUci, reference),
      active.engineProfile.wireId, investigationDepth(active)).toList.map(_.resource)
    val realizing = FunctionalRealizationReading.withQuestions(active.packet, prepared, functional, realizationResources)
    val augmented = AnchoredChoiceReading.withQuestions(active.packet, prepared, realizing,
      active.responseSearches, Set(active.playedMoveUci, reference),
      active.engineProfile.wireId, investigationDepth(active))
    val current = if augmented == facts then facts else CommentaryFactReader.read(active.packet,
      prepared, readComparisons(augmented), active.continuationInquiries)
    val outstanding = active.ledger.physicalReportsAccepted < active.ledger.physicalWorksIssued
    active.copy(proposalEntries = current.map(entry => entry.copy(readings = entry.readings.map { reading =>
      val awaiting = outstanding && reading.result.toList.flatMap(_.needs).exists { need =>
        (need.kind == "continuation" && active.issuedWork.purpose == EngineWorkPurpose.ResponseComparison &&
          active.issuedWork.searchAnchor.exists(_.prefixMoves == need.prefixMoves)) ||
        (need.kind == "comparison" && need.requirementId.exists(id => entry.readings.exists(source =>
          source.requirementId == id && source.state == "awaiting_engine" && source.workId.contains(active.issuedWork.workId))))
      }
      if awaiting && !CommentaryProposal.isResolved(reading) && !Set("invalid_input", "stopped")(reading.state) then
        reading.copy(state = "awaiting_engine", stopReason = None, workId = Some(active.issuedWork.workId))
      else reading
    })))

  /** A final returned comparison can first reveal a later realization. Admit
    * only its bounded local ObserveMove traces before final publication;
    * otherwise the last report would leave a correct reader permanently unused.
    * Existing local expansion and evidence admission limits still own this work.
    */
  private def prepareFunctionalRealizations(active: ActiveCommentaryJob, reference: String)
      : Either[(ActiveCommentaryJob, CommentaryJobStopCondition), ActiveCommentaryJob] =
    var current = active
    var wave = 0
    while wave < FunctionalRealizationReading.MaximumLocalWaves do
      val needed = current.proposalEntries.filter(_.request.origin.question.producer ==
        FunctionalRealizationReading.Producer).exists(entry =>
          CommentaryProposal.neededRequirementIds(entry).nonEmpty)
      if !needed then return Right(current)
      val before = current
      prepareProposalInputs(current, Some(FunctionalRealizationReading.Producer)) match
        case Left(failure) => return Left(failure)
        case Right(retained) =>
          val rebound = if retained.packet eq current.packet then retained
            else readPrincipalVariations(retained, reference)
          current = refreshProposalReadings(rebound)
      wave += 1
      if (before.packet eq current.packet) && before.proposalEntries == current.proposalEntries then
        return Right(current)
    Right(current)

  /** Needs carry their finite horizon; a topic name or an empty root is not a child frontier. */
  private case class FactContinuation(need: CommentaryFact.Need, owners: Set[(String, String)])

  private def pendingFactContinuations(active: ActiveCommentaryJob,
      generated: Boolean = false): List[FactContinuation] =
    CommentaryIdea.currentQuestions(active.proposalEntries).filter(entry => (entry.admission == "generated") == generated)
      .flatMap { entry =>
        val needed = CommentaryProposal.neededRequirementIds(entry)
        entry.readings.filter(reading => needed(reading.requirementId) &&
          !CommentaryProposal.isResolved(reading) && !Set("invalid_input", "stopped")(reading.state))
          .flatMap(reading => reading.result.toList.flatMap(_.needs).filter(need =>
            need.kind == "continuation" && need.prefixMoves.nonEmpty && need.prefixMoves.size < 80 &&
              need.horizon.exists(horizon => horizon > need.prefixMoves.size && horizon <= 80))
            .map(need => FactContinuation(need, Set(entry.request.id -> reading.requirementId))))
      }.groupBy(_.need.prefixMoves).values.map { values =>
        FactContinuation(values.maxBy(_.need.horizon.get).need, values.flatMap(_.owners).toSet)
      }.toList.sortBy(value => (value.need.prefixMoves.size, value.need.prefixMoves.mkString(" ")))

  private def factContinuationContext(active: ActiveCommentaryJob, prefix: List[String])
      : Option[(CanonicalPositionHistory, EngineSearchAnchor)] =
    for
      path <- CommentaryProposal.observedPrefix(active.packet, active.preparedPaths.get, prefix)
      first <- path.ids.headOption
      positionId <- path.positions.lift(prefix.size).flatMap(_.id)
      history <- active.responseHistories.get(prefix.mkString(" ")).flatten.orElse(
        active.positionHistory.extend(prefix).toOption)
      if PositionRuleAssessment.assess(history).isInstanceOf[PositionRuleAssessment.Nonterminal]
    yield history -> EngineSearchAnchor(prefix.head, first, positionId, prefix)

  private def issueFactContinuation(active: ActiveCommentaryJob, baseline: CompletedCommentaryJobRecord,
      questions: List[CommentaryInquiry], responses: List[CommentaryResponseInquiry],
      need: CommentaryFact.Need): Option[CommentaryJobState] =
    if active.explorationsIssued >= 2 then None
    else factContinuationContext(active, need.prefixMoves).filter(_._1.currentLegalMoveUcis.size > 1).map { (child, anchor) =>
        val next = active.copy(explorationsIssued = active.explorationsIssued + 1,
          responseHistories = active.responseHistories.updated(need.prefixMoves.mkString(" "), Some(child)))
        val work = issuedWork(active.engineProfile, active.ledger.issuedWorkId, EngineWorkPurpose.ResponseComparison,
          child, EngineRootRestriction.Unrestricted, child.currentFen, Nil,
          EngineSearchLimits(investigationDepth(active), FocusNodes, FocusMovetimeMs,
            math.min(3, child.currentLegalMoveUcis.size)), FocusElapsedMs, Some(anchor))
        refreshProposalReadings(issue(next, InvestigatingFacts(baseline, questions, responses, need.prefixMoves),
          work, next.ledger, Some(child)).asInstanceOf[ActiveCommentaryJob])
    }

  private def proposalWorkMatches(value: CommentaryProposal.CompareMoves, work: IssuedEngineWork): Boolean =
    val sameAnchor = if value.prefixMovesUci.isEmpty then work.searchAnchor.isEmpty
      else work.searchAnchor.exists(_.prefixMoves == value.prefixMovesUci)
    sameAnchor && (work.rootRestriction match
      case EngineRootRestriction.Unrestricted => true
      case EngineRootRestriction.Restricted(moves) => List(value.referenceMove, value.alternativeMove).forall(moves.contains))

  private def pendingProposalComparisons(entries: List[CommentaryProposal.Entry])
      : List[(CommentaryProposal.Entry, CommentaryProposal.CompareMoves)] =
    entries.flatMap { entry =>
      val needed = CommentaryProposal.neededRequirementIds(entry)
      entry.request.requirements.collect {
      case value: CommentaryProposal.CompareMoves if entry.readings.exists(reading =>
          reading.requirementId == value.id && needed(value.id) && reading.state == "deferred" &&
            !reading.stopReason.contains("local_expansion_limit")) => entry -> value
    }}

  private case class ProposalChoice(value: CommentaryProposal.CompareMoves, submitted: Boolean,
      decision: (Int, Int), essential: Boolean, governsBranch: Boolean, rootDependency: Boolean = false)

  /** Derived only from this immutable job revision; copy never carries these readings forward. */
  private final class ChoiceTasks(active: ActiveCommentaryJob, reference: String):
    private val roots = Set(active.playedMoveUci, reference)
    private lazy val functional = active.preparedPaths.flatMap(paths => FunctionalChoiceReading.task(
      active.packet, paths, active.proposalEntries, active.responseSearches, roots,
      active.engineProfile.wireId, investigationDepth(active)))
    private lazy val family = active.preparedPaths.flatMap(paths => AnchoredChoiceReading.family(
      active.packet, paths, active.proposalEntries, active.responseSearches, roots,
      active.engineProfile.wireId, investigationDepth(active)))
    private lazy val anchored = Option.when(family.isEmpty)(active.preparedPaths.flatMap(paths =>
      AnchoredChoiceReading.task(active.packet, paths, active.proposalEntries, active.responseSearches,
        roots, active.engineProfile.wireId, investigationDepth(active)))).flatten
    lazy val obligations = functional.toList.flatMap(_.obligations)
      .map(value => value.comparison -> value.phase) ++
      family.map(_.obligations).getOrElse(anchored.toList.flatMap(_.obligations))
        .map(value => value.comparison -> value.phase)
    lazy val incomplete = functional.exists(!_.complete) || family.fold(anchored.exists(!_.complete))(!_.complete)
    lazy val conflicts = obligations.sortBy((value, phase) =>
      (phase, value.prefixMovesUci.size, value.prefixMovesUci.mkString(" "), value.id))
      .flatMap { (value, _) => active.preparedPaths.toList.flatMap(paths =>
        CommentaryChoiceValue.conflict(active.packet, paths, value, active.responseSearches,
          active.engineProfile.wireId, investigationDepth(active)).map(value -> _)) }
      .distinctBy((value, conflict) => (CommentaryIdea.pair(value), conflict.workIds))

  private def pendingChoiceTask(active: ActiveCommentaryJob, reference: String): Boolean =
    val roots = Set(active.playedMoveUci, reference)
    // A root refresh may temporarily withdraw the family's certificate. Its
    // already selected unanswered duties still precede speculative discovery.
    val pendingHeads = active.proposalEntries.exists(entry =>
      entry.request.requirements.exists {
        case pair: CommentaryProposal.CompareMoves => pair.prefixMovesUci.isEmpty &&
          Set(pair.referenceMove, pair.alternativeMove) == roots
        case _ => false
      } && entry.readings.exists(reading =>
        (reading.requirementId.startsWith(FunctionalChoiceReading.ValuePrefix) ||
          reading.requirementId.startsWith(AnchoredChoiceReading.ValuePrefix)) &&
          Set("deferred", "awaiting_engine")(reading.state)))
    pendingHeads || active.currentChoices(reference).incomplete

  /** Register a current dependency before optional discovery can consume the
    * remaining proposition slots. Old questions and their receipts stay intact.
    */
  private def registerComparisonConflicts(active: ActiveCommentaryJob, reference: String): ActiveCommentaryJob =
    val required = active.currentChoices(reference).conflicts.map(_._2)
    val candidates = (required ++ active.comparisonConflicts).distinct
      .flatMap(_.entry).filter(entry => CommentaryProposal.validRequests(List(entry.request)))
      .filterNot(entry => active.proposalEntries.exists(_.request.id == entry.request.id))
      .distinctBy(_.request.id)
    val room = (CommentaryProposal.MaximumRequirements -
      active.proposalEntries.map(_.request.requirements.size).sum).max(0)
    val additions = candidates.take(room)
    if additions.isEmpty then active else refreshProposalReadings(
      active.copy(proposalEntries = active.proposalEntries ++ additions))

  /** A local contradictory value first tests its concrete divergent reply,
    * then renews the parent selection. Neither child score changes the root
    * assessment or supplies a missing parent value.
    */
  private def revalidateChoice(active: ActiveCommentaryJob, baseline: CompletedCommentaryJobRecord,
      questions: List[CommentaryInquiry], responses: List[CommentaryResponseInquiry], reference: String)
      : Option[CommentaryJobState] =
    active.currentChoices(reference).conflicts.headOption.map { (choice, conflict) =>
      val entry = conflict.entry.flatMap(value =>
        active.proposalEntries.find(_.request.id == value.request.id))
      def receiptOrder(id: String) = active.responseReceipts.indexWhere(_.work.workId == id)
      val lastConflict = conflict.workIds.map(receiptOrder).max
      val childAnswer = for
        pair <- conflict.comparison
        owner <- entry
        answer <- owner.readings.find(_.requirementId == pair.id).flatMap(_.answer)
        if answer.referenceDepth == answer.alternativeDepth && answer.referenceDepth.exists(_ >= conflict.depth)
        if receiptOrder(answer.workId) > lastConflict
        search <- active.responseSearches.find(search => search.belongsTo(active.packet) &&
          search.work.workId == answer.workId && search.work.executionKeySha256 == answer.executionKeySha256)
        if search.anchor.prefixMoves == pair.prefixMovesUci
        if search.pairAnswer(active.packet, pair.referenceMove, pair.alternativeMove).contains(answer)
      yield answer
      val missingEntry = conflict.comparison.nonEmpty && entry.isEmpty
      val stoppedChild = entry.exists(_.readings.exists(reading =>
        conflict.comparison.exists(_.id == reading.requirementId) && reading.state == "stopped"))
      val renewParent = conflict.comparison.isEmpty || childAnswer.nonEmpty
      val depthLimit = conflict.depth > MaximumAnalysisDepth ||
        (renewParent && conflict.depth >= MaximumAnalysisDepth)
      val issued = if missingEntry || stoppedChild || depthLimit then None
        else conflict.comparison.filter(_ => childAnswer.isEmpty) match
          case Some(pair) =>
            // A matching old child receipt may answer this generic pair at a
            // lower depth or before the current contradiction. Its immutable
            // observation remains, but this exact current conflict owns a new
            // unanswered duty at conflict.depth.
            val pendingChild = active.copy(proposalEntries = active.proposalEntries.map { value =>
              if !entry.exists(_.request.id == value.request.id) then value else
                value.copy(readings = value.readings.map { reading =>
                  if reading.requirementId != pair.id then reading else
                    reading.copy(state = "deferred", answer = None, workId = None, stopReason = None)
                })
            })
            issueProposalComparison(pendingChild, baseline, questions, responses, pair,
              rootDependency = true, minimumDepth = conflict.depth)
          // A verified child cannot settle the parent's inconsistent values.
          // Repeat its full selection at a higher level, not the same horizon.
          case None if !depthLimit => issueProposalComparison(active, baseline, questions, responses, choice,
            rootDependency = true, surveyFirstReply = true, minimumDepth = conflict.depth + 1)
          case None => None
      issued.getOrElse {
        val reason = if missingEntry then "requirement_limit" else if stoppedChild then "search_budget_exhausted"
          else if depthLimit then "depth_limit"
          else "comparison_revalidation_incomplete"
        val pair = CommentaryIdea.pair(choice)
        val entries = active.proposalEntries.map(entry => entry.copy(readings = entry.readings.map { reading =>
          if entry.request.requirements.exists {
            case value: CommentaryProposal.CompareMoves => value.id == reading.requirementId &&
              CommentaryIdea.pair(value) == pair
            case _ => false
          } then reading.copy(state = "stopped", stopReason = Some(reason), answer = None, workId = None)
          else reading
        }))
        finishInvestigation(active.copy(proposalEntries = entries), baseline, questions, responses)
      }
    }

  /** A ready counter to an observed first reply belongs to the current root
    * comparison. Its named changed condition must own that exact reply pair;
    * an unrelated historical effect or a merely legal alternative is not enough.
    */
  private def currentFirstReplyDependencies(active: ActiveCommentaryJob, reference: String)
      : Set[(List[String], Set[String])] =
    active.preparedPaths.toList.flatMap { paths =>
      val owners = explanationReceipts(active).flatMap(receipt =>
        CommentaryInvestigation.pairedAnswer(active.packet, active.playedMoveUci, reference,
          receipt.work, receipt.lines, Some(paths))).filter(currentAnswer(active, _))
      pendingProposalComparisons(CommentaryIdea.currentQuestions(active.proposalEntries)).collect {
        case (entry, value) if value.prefixMovesUci.size == 1 && value.expectedRelation.nonEmpty &&
            owners.exists(answer => List(answer.referenceLineId, answer.alternativeLineId).exists(id =>
              paths.paths.exists(path => path.line.ref.id == id && path.certified.nonEmpty &&
                path.ids == path.declared && List(value.referenceMove, value.alternativeMove).exists(move =>
                  path.moves.startsWith(value.prefixMovesUci :+ move))))) &&
            CommentaryProposal.comparisonContexts(entry, value, readyOnly = true).exists { context =>
              val facts = context.map(fact => fact.id -> fact).toMap
              entry.request.requirements.exists {
                case contrast: CommentaryProposal.FactComparison if contrast.relation == "different" &&
                    entry.readings.exists(reading => reading.requirementId == contrast.id &&
                      Set("supported", "observed")(reading.state)) =>
                  (facts.get(contrast.leftRequirementId), facts.get(contrast.rightRequirementId)) match
                    case (Some(left), Some(right)) =>
                      val prefixes = List(left, right).flatMap(fact => fact.path.source match
                        case CommentaryFact.Declared(moves) => Some(moves)
                        case _ => None)
                      left.predicate == right.predicate && left.at == right.at &&
                        left.at == CommentaryFact.Ply(value.prefixMovesUci.size + 1) &&
                        prefixes.toSet == Set(value.prefixMovesUci :+ value.referenceMove,
                          value.prefixMovesUci :+ value.alternativeMove)
                    case _ => false
                case _ => false
              }
            } => CommentaryIdea.pair(value)
      }
    }.toSet

  private def firstReplyOwners(active: ActiveCommentaryJob, prefix: List[String], reference: String)
      : List[(String, String)] =
    val dependencies = currentFirstReplyDependencies(active, reference)
    pendingProposalComparisons(CommentaryIdea.currentQuestions(active.proposalEntries)).collect {
      case (entry, value) if value.prefixMovesUci == prefix && dependencies(CommentaryIdea.pair(value)) =>
        entry.request.id -> value.id
    }.distinct.sorted

  private def awaitingTargetPool(responses: List[CommentaryResponseInquiry], anchor: EngineSearchAnchor,
      selectedId: Option[String] = None)
      : List[CommentaryResponseInquiry] = responses.map { inquiry =>
    if anchor.plyOffset == 1 && inquiry.question.rootMove == anchor.rootMove &&
        inquiry.question.rootMoveOccurrenceId == anchor.rootOccurrenceId &&
        inquiry.question.childPositionId == anchor.positionId &&
        (inquiry.answer.isEmpty || selectedId.contains(inquiry.question.id)) then
      inquiry.copy(state = "awaiting_engine", answer = None, stopReason = None)
    else inquiry
  }

  private def issueResponseComparison(active: ActiveCommentaryJob, baseline: CompletedCommentaryJobRecord,
      pending: List[CommentaryInquiry], responses: List[CommentaryResponseInquiry], anchor: EngineSearchAnchor,
      restriction: EngineRootRestriction, selectedId: String, requiredTarget: Boolean = false)
      : Option[CommentaryJobState] =
    val compact = active.remainingBudgetMs <= 10_000
    val longReply = requiredTarget && restriction != EngineRootRestriction.Unrestricted &&
      active.remainingBudgetMs >= SelectedReplyElapsedMs + 2_000
    val historyKey = anchor.prefixMoves.mkString(" ")
    val child = active.responseHistories.get(historyKey).flatten.orElse(
      active.positionHistory.extend(anchor.prefixMoves).toOption.filter(source =>
        PositionRuleAssessment.assess(source).isInstanceOf[PositionRuleAssessment.Nonterminal]))
    val shortReplyMs = math.min(FocusElapsedMs.toLong, active.remainingBudgetMs - 1_500).toInt
    child.filter(_ => shortReplyMs >= 1_000).map { source =>
      // A selected recheck remains pending even when its survey supplied an
      // earlier answer. That receipt stays in responseSearches until completion.
      val scheduled = awaitingTargetPool(responses, anchor, Some(selectedId))
      val continued = active.copy(responseHistories = active.responseHistories.updated(historyKey, child),
        continuationInquiries = active.continuationInquiries.map(inquiry =>
          if inquiry.question.id == selectedId && inquiry.state == "surveyed" &&
              restriction != EngineRootRestriction.Unrestricted && inquiry.routeChallenge.exists(_.state == "not_returned") then
            inquiry.copy(routeChallenge = inquiry.routeChallenge.map(_.copy(state = "awaiting_engine", stopReason = None)))
          else if inquiry.question.positionId == anchor.positionId && inquiry.state == "deferred" then
            inquiry.copy(state = "awaiting_engine", stopReason = None) else inquiry))
      val phase = InvestigatingResponses(baseline, pending, scheduled, selectedId)
      val count = restriction match
        case EngineRootRestriction.Unrestricted => source.currentLegalMoveUcis.size
        case EngineRootRestriction.Restricted(moves) => moves.size
      val owners = if anchor.plyOffset != 1 then Nil else firstReplyOwners(active, anchor.prefixMoves,
        reviewReference(baseline).getOrElse(active.playedMoveUci))
      val work = issuedWork(active.engineProfile, active.ledger.issuedWorkId,
        EngineWorkPurpose.ResponseComparison, source, restriction, source.currentFen, Nil,
        EngineSearchLimits(investigationDepth(active),
          if longReply then SelectedReplyNodes else if requiredTarget then SelectedInquiryNodes else FocusNodes,
          if longReply then SelectedReplyMovetimeMs else if requiredTarget && !compact then RootMovetimeMs
          else math.min(FocusMovetimeMs, shortReplyMs - 250),
          math.min(3, count)),
        if longReply then SelectedReplyElapsedMs else if requiredTarget && !compact then RootElapsedMs else shortReplyMs,
        Some(anchor)).copy(proposalRefs = owners)
      refreshProposalReadings(issue(continued.copy(explorationsIssued = active.explorationsIssued + 1),
        phase, work, active.ledger, child).asInstanceOf[ActiveCommentaryJob])
    }

  private def proposalChoices(active: ActiveCommentaryJob, reference: String): List[ProposalChoice] =
    def key(value: CommentaryProposal.CompareMoves) = (value.prefixMovesUci, Set(value.referenceMove, value.alternativeMove))
    val functional = active.currentChoices(reference).obligations
      .map((value, phase) => key(value) -> phase).toMap
    val ideas = CommentaryIdea.read(active.proposalEntries)
    val withdrawn = ideas.filter(_.withdrawn).flatMap(_.members.map(_.entry.request.id)).toSet
    val failed = active.proposalEntries.flatMap(entry => entry.request.requirements.collect {
      case value: CommentaryProposal.CompareMoves if entry.readings.exists(reading =>
        reading.requirementId == value.id && reading.state == "stopped" &&
          reading.stopReason.contains("search_budget_exhausted")) => key(value)
    }).toSet
    val pending = pendingProposalComparisons(active.proposalEntries.filterNot(entry => withdrawn(entry.request.id)))
      .filter { (entry, value) =>
        // Bare source pairs can consume existing answers, but their own legal choices
        // alone do not justify new work. Filter owners before grouping shared pairs.
        (!failed(key(value)) || functional.contains(key(value))) && (entry.admission != "generated" || value.prefixMovesUci.nonEmpty ||
          entry.request.origin.question.producer != "C-opponent-choice" ||
          value.id != "contrast_opponent_choice" || entry.generatedArms.exists(arm =>
            arm.headRequirementIds.contains(value.id) && entry.request.requirements.exists {
              case fact: CommentaryProposal.Fact => arm.memberRequirementIds.contains(fact.id) &&
                fact.id != "choice_reference" && fact.id != "choice_alternative"
              case _ => false
            }))
      }
    val submitted = pending.collect { case (entry, value) if entry.admission != "generated" => key(value) }.toSet
    val currentMoves = Set(active.playedMoveUci, reference) ++
      CommentarySelection.primaryComparisonRecords(active.packet).flatMap { (_, comparison, _) =>
        List(comparison.referenceLine.rootMove, comparison.candidateLine.rootMove)
      }
    val generalFirstReplyDependencies = currentFirstReplyDependencies(active, reference)
    val knownReplyDependencies = CommentaryIdea.replySpaces(active.proposalEntries).filter(space =>
      Set(space.owner.comparison.referenceMove, space.owner.comparison.alternativeMove) ==
        Set(active.playedMoveUci, reference)).map(space => key(space.test)).toSet ++
      CommentaryIdea.reserveEntries(active.proposalEntries).filter(use =>
        use.owner.comparison.referenceMove == active.playedMoveUci && use.owner.comparison.alternativeMove == reference &&
          active.preparedPaths.exists(RootSubjectComparison.currentReserveEntry(_, use)))
        .flatMap(use => key(use.test) :: use.optionTest.filter(test =>
          active.preparedPaths.exists(paths => RootSubjectComparison.reserveEntryValue(paths, use).contains(test) &&
            RootSubjectComparison.strongestReserveCounter(active.packet, paths, use, active.responseSearches)))
          .map(key).toList)
    val holdDependencies =
      CommentaryIdea.reserveHolds(active.proposalEntries).filter(hold =>
        hold.owner.comparison.referenceMove == active.playedMoveUci && hold.owner.comparison.alternativeMove == reference &&
          active.preparedPaths.exists(RootSubjectComparison.currentReserveHold(_, hold)))
        .map(hold => key(hold.test)).toSet
    val directEntryDependencies = CommentaryIdea.directReserveEntries(active.proposalEntries).filter(entry =>
      entry.owner.comparison.referenceMove == active.playedMoveUci && entry.owner.comparison.alternativeMove == reference &&
        active.preparedPaths.exists(RootSubjectComparison.currentDirectReserveEntry(_, entry)))
      .map(entry => key(entry.test)).toSet
    val timingDependencies = CommentaryIdea.reserveTimings(active.proposalEntries).filter(timing =>
      timing.owner.comparison.referenceMove == active.playedMoveUci && timing.owner.comparison.alternativeMove == reference)
      .map(timing => key(timing.test)).toSet
    val dutyDependencies = CommentaryIdea.currentDuties(active.proposalEntries).filter(duty =>
      duty.owner.comparison.referenceMove == active.playedMoveUci && duty.owner.comparison.alternativeMove == reference)
      .flatMap(duty => List(key(duty.actual), key(duty.alternative))).toSet
    // Settle the same first reply and the first decision to retain the option
    // before assigning significance to a use found in a distant continuation.
    val firstReplyDependencies = if timingDependencies.nonEmpty then Set.empty
      else CommentaryIdea.reserveResponses(active.proposalEntries).filter(response =>
        response.owner.comparison.referenceMove == active.playedMoveUci && response.owner.comparison.alternativeMove == reference)
        .map(response => key(response.test)).toSet
    val witnessedReplyDependencies = knownReplyDependencies ++ firstReplyDependencies
    val replyDependencies = witnessedReplyDependencies ++ holdDependencies ++ directEntryDependencies ++ timingDependencies ++ dutyDependencies ++
      generalFirstReplyDependencies ++ functional.keySet
    val evaluations = active.packet.assembly.input.assessmentCandidates.map(_.evaluation.moves)
    val presentChoices = pending.collect { case (entry, value) if value.id == "contrast_opponent_choice" &&
      entry.request.requirements.exists(requirement =>
        List("opponent_reference_", "reply_", "tactic_", "defense_").exists(requirement.id.startsWith)) => key(value)
    }.toSet
    val tacticalCounters = pending.collect { case (_, value) if value.id == "tactical_strength" => key(value) }.toSet
    // A witnessed legal check/capture challenges the actual reply before its downstream
    // outcome is worth searching. The source's hypothetical score does not establish strength.
    val observedTacticalAlternatives = pending.collect { case (entry, value) if
      value.prefixMovesUci.nonEmpty && entry.request.requirements.exists {
        case move: CommentaryProposal.ObserveMove => move.prefixMovesUci == value.prefixMovesUci &&
          move.moveUci == value.alternativeMove && entry.readings.exists(reading =>
            reading.requirementId == move.id && reading.state == "observed")
        case _ => false
      } && historyAt(active, value.prefixMovesUci :+ value.alternativeMove).exists(history =>
        history.currentPosition.check.yes || history.segmentReplaySteps.lastOption.exists(_.capturedRole.nonEmpty)) => key(value)
    }.toSet
    val directCounters = pending.collect { case (entry, value) if value.id == "tactical_strength" &&
      entry.request.requirements.exists(_.id.startsWith("tactical_direct_")) => key(value) }.toSet
    val stale = pending.collect { case (entry, value) if entry.readings.exists(reading =>
      reading.requirementId == value.id && reading.stopReason.contains("comparison_not_current")) => key(value) }.toSet
    // A current root PV can already witness the premise while its comparison is
    // still unanswered. Requiring that comparison's own confirmation would defer
    // the very acceptance/immediate-action decision needed to interpret the branch.
    val governing = ideas.filter(idea => idea.contribution &&
      (idea.tracking.exists(_.currentConfirmed) || evaluations.exists(_.startsWith(idea.key.branch))))
      .flatMap(_.essential.collect {
        case (member, value, _) if member.role(value) == 0 && member.explains(value) => key(value)
      })
      .toSet ++ directCounters
    // Combine the live owners before choosing a representative. A submitted alias
    // must not hide the tactical owner's existing bounded three-move search.
    pending.groupBy((_, value) => key(value)).values.map(values =>
      values.map(_._2).sortBy(value => (if value.id == "tactical_strength" then 0 else 1,
        value.id, value.referenceMove, value.alternativeMove)).head).toList.filter { value =>
      val tried = !functional.contains(key(value)) && (if value.prefixMovesUci.isEmpty then active.receipts else active.responseReceipts).exists(receipt =>
        receipt.work.requiredDepth >= (if functional.contains(key(value)) then
          CommentaryChoiceValue.pairDepth(active.packet, value.prefixMovesUci, active.responseSearches,
            active.engineProfile.wireId, investigationDepth(active)) else investigationDepth(active)) && proposalWorkMatches(value, receipt.work) &&
          (receipt.work.rootRestriction match
          case EngineRootRestriction.Restricted(moves) => moves.toSet == Set(value.referenceMove, value.alternativeMove) ||
            (Set("tactical_strength", "reserve_hold", "reserve_counter")(value.id) && Set(value.referenceMove, value.alternativeMove).subsetOf(moves.toSet))
          case _ => false))
      !tried && (submitted(key(value)) || (if value.prefixMovesUci.isEmpty then
        Set(value.referenceMove, value.alternativeMove).exists(currentMoves)
      else value.prefixMovesUci.headOption.exists(currentMoves)))
    }.map { value =>
      val onAssessment = value.prefixMovesUci.nonEmpty && evaluations.exists(moves =>
        moves.startsWith(value.prefixMovesUci) && moves.lift(value.prefixMovesUci.size)
          .exists(Set(value.referenceMove, value.alternativeMove)))
      val ideaPriority = ideas.flatMap(_.priority(value)).minOption
      // A deeper assessment invalidated this very answer. Settle that known dependency
      // before investigating a new reason for adopting the same current continuation.
      val decision = if onAssessment && stale(key(value)) then (-1, 0)
        else if onAssessment && directCounters(key(value)) then (0, -2)
        else if functional.contains(key(value)) then (0, -1 + functional(key(value)))
        else if dutyDependencies(key(value)) then (0, -1)
        else if timingDependencies(key(value)) then (0, -1)
        // Complete the known entry's counter before speculating whether a
        // different king move would yield some as-yet unseen entry.
        else if witnessedReplyDependencies(key(value)) then (0, 0)
        else if holdDependencies(key(value)) then (0, 1)
        // A general ready counter must not evict an already owned option/use
        // obligation just because its UCI sorts earlier. Both retain completion
        // priority over an unrelated downstream observation.
        else if generalFirstReplyDependencies(key(value)) then (0, 2)
        else ideaPriority.getOrElse(
          if presentChoices(key(value)) then (0, 1)
          else if onAssessment && tacticalCounters(key(value)) then (1, 0)
          else (2, 4))
      ProposalChoice(value, submitted(key(value)), decision,
        replyDependencies(key(value)) || ideaPriority.exists((contribution, role) => contribution == 0 && role <= 1),
        governing(key(value)) || (onAssessment && observedTacticalAlternatives(key(value))), replyDependencies(key(value)))
    }.sortBy(choice => (choice.decision, if choice.submitted then 0 else 1,
      if choice.decision != (0, 2) then 0 else active.responseSearches.reverseIterator.find(search =>
        search.belongsTo(active.packet) && search.anchor.prefixMoves == choice.value.prefixMovesUci &&
          search.work.rootRestriction == EngineRootRestriction.Unrestricted)
        .flatMap(_.lines.headOption).fold(1)(line =>
          if Set(choice.value.referenceMove, choice.value.alternativeMove)(line.evaluation.moves.head) then 0 else 1),
      if choice.value.prefixMovesUci.headOption.contains(active.playedMoveUci) then 0 else 1, choice.value.prefixMovesUci.size,
      choice.value.prefixMovesUci.mkString(" "), choice.value.referenceMove, choice.value.alternativeMove))

  /** A new general first-reply question starts with all legal replies. A
    * restricted pair supplements a survey that did not return both named
    * branches; it cannot identify the opponent's strongest response.
    * Existing typed option/use obligations retain their earlier priority.
    */
  private def issueProposalChoice(active: ActiveCommentaryJob, baseline: CompletedCommentaryJobRecord,
      questions: List[CommentaryInquiry], responses: List[CommentaryResponseInquiry], choice: ProposalChoice)
      : Option[CommentaryJobState] =
    val functional = active.currentChoices(reviewReference(baseline).getOrElse(active.playedMoveUci)).obligations
      .exists((value, _) => CommentaryIdea.pair(value) == CommentaryIdea.pair(choice.value))
    val functionalSurvey = functional && (CommentaryChoiceValue.survey(active.packet,
      choice.value.prefixMovesUci, active.responseSearches, active.engineProfile.wireId,
      investigationDepth(active)).isEmpty || CommentaryChoiceValue.selectionDepth(active.packet,
      choice.value, active.responseSearches, active.engineProfile.wireId, investigationDepth(active)) >
        CommentaryChoiceValue.pairDepth(active.packet, choice.value.prefixMovesUci,
          active.responseSearches, active.engineProfile.wireId, investigationDepth(active)))
    val survey = functionalSurvey || (choice.decision == (0, 2) && choice.value.prefixMovesUci.size == 1 &&
      active.explorationsIssued < 3 && !active.firstReplySurveysIssued(choice.value.prefixMovesUci) &&
      !active.responseSearches.exists(search => search.belongsTo(active.packet) &&
        search.anchor.prefixMoves == choice.value.prefixMovesUci &&
        search.work.rootRestriction == EngineRootRestriction.Unrestricted))
    issueProposalComparison(active, baseline, questions, responses, choice.value,
      rootDependency = choice.rootDependency, surveyFirstReply = survey)

  /** Another named pair in an already failed pool cannot issue the same search.
    * Only issuance identity and aliases differ; history, owners of the position,
    * profile and the complete search allowance remain part of this guard. */
  private[runtime] def sameProposalSearch(left: IssuedEngineWork, right: IssuedEngineWork): Boolean =
    def search(work: IssuedEngineWork) = work.copy(workId = "", executionKeySha256 = "", proposalRefs = Nil,
      rootRestriction = work.rootRestriction match
        case EngineRootRestriction.Restricted(moves) => EngineRootRestriction.Restricted(moves.distinct.sorted)
        case value => value)
    search(left) == search(right)

  private[runtime] def exhaustedProposalEntries(entries: List[CommentaryProposal.Entry],
      selected: CommentaryProposal.CompareMoves, attempted: IssuedEngineWork, failed: IssuedEngineWork)
      : List[CommentaryProposal.Entry] =
    val pair = CommentaryIdea.pair(selected)
    val dependents = attempted.proposalRefs.toSet
    val issuedOwners = failed.proposalRefs.toSet
    entries.map(entry =>
      val ids = entry.request.requirements.collect {
        case value: CommentaryProposal.CompareMoves if CommentaryIdea.pair(value) == pair ||
            dependents(entry.request.id -> value.id) => value.id
      }.toSet
      entry.copy(readings = entry.readings.map(reading =>
        if ids(reading.requirementId) && !CommentaryProposal.isResolved(reading) then
          reading.copy(state = "stopped", stopReason = Some("search_budget_exhausted"), answer = None,
            // A new alias was never issued by the failed work. Retain the
            // failure's scope without giving that alias a false work owner.
            workId = Option.when(issuedOwners(entry.request.id -> reading.requirementId))(failed.workId))
        else reading)))

  private def issueProposalComparison(active: ActiveCommentaryJob, baseline: CompletedCommentaryJobRecord,
      questions: List[CommentaryInquiry], responses: List[CommentaryResponseInquiry],
      value: CommentaryProposal.CompareMoves, verifyAfter: Boolean = false,
      rootDependency: Boolean = false, surveyFirstReply: Boolean = false,
      minimumDepth: Int = 0): Option[CommentaryJobState] =
    val prefix = value.prefixMovesUci
    val history = historyAt(active, prefix)
    val anchor = if prefix.isEmpty then Some(None) else
      CommentaryProposal.observedPrefix(active.packet, active.preparedPaths.get, prefix).flatMap(path => for
        first <- path.ids.headOption
        position <- path.positions.lift(prefix.size).flatMap(_.id)
      yield Some(EngineSearchAnchor(prefix.head, first, position, prefix)))
    val firstReply = prefix.size == 1 && (rootDependency ||
      CommentaryIdea.reserveResponses(active.proposalEntries).exists(response =>
        response.test.prefixMovesUci == prefix &&
          Set(response.test.referenceMove, response.test.alternativeMove) == Set(value.referenceMove, value.alternativeMove)))
    val requiredComparison = firstReply || rootDependency || verifyAfter
    val functional = rootDependency && active.currentChoices(
      reviewReference(baseline).getOrElse(active.playedMoveUci)).obligations.exists((question, _) =>
        CommentaryIdea.pair(question) == CommentaryIdea.pair(value))
    val valueDepth = if functional then
      if surveyFirstReply then CommentaryChoiceValue.selectionDepth(active.packet, value,
        active.responseSearches, active.engineProfile.wireId, investigationDepth(active)) else
        CommentaryChoiceValue.pairDepth(active.packet, prefix, active.responseSearches,
          active.engineProfile.wireId, investigationDepth(active))
      else investigationDepth(active)
    val targetDepth = math.max(valueDepth, minimumDepth)
    // Generic aliases may share a child pair with an older answered conflict.
    // Keep the exact current conflict owner visible without reviving a stopped
    // historical parent or attaching an anonymous owner after failure.
    val conflictOwners = if !rootDependency then Nil else
      active.currentChoices(reviewReference(baseline).getOrElse(active.playedMoveUci)).conflicts
        .flatMap { (_, conflict) => for
          request <- conflict.entry.toList
          entry <- active.proposalEntries.filter(_.request.id == request.request.id)
          pair <- conflict.comparison.toList if pair == value
          if CommentaryProposal.neededRequirementIds(entry)(pair.id) && entry.readings.exists(reading =>
            reading.requirementId == pair.id && reading.state == "deferred")
        yield entry -> pair }
    val pendingOwners = (pendingProposalComparisons(CommentaryIdea.currentQuestions(active.proposalEntries)) ++
      conflictOwners).distinct
    val currentOwner = pendingOwners.exists { (_, other) =>
      if functional then CommentaryIdea.pair(other) == CommentaryIdea.pair(value) else other == value
    }
    // A failed/stopped choice may remain an unresolved value question, but it
    // cannot renew an anonymous engine job after losing its pending owner.
    val currentReplyOwner = surveyFirstReply && firstReplyOwners(active, prefix,
      reviewReference(baseline).getOrElse(active.playedMoveUci)).nonEmpty
    if !currentOwner && !currentReplyOwner then return None
    if targetDepth > MaximumAnalysisDepth then
      val key = CommentaryIdea.pair(value)
      val bounded = active.copy(proposalEntries = active.proposalEntries.map(entry =>
        entry.copy(readings = entry.readings.map { reading =>
          if Set("deferred", "awaiting_engine")(reading.state) && entry.request.requirements.exists {
            case pair: CommentaryProposal.CompareMoves => pair.id == reading.requirementId &&
              CommentaryIdea.pair(pair) == key
            case _ => false
          } then reading.copy(state = "stopped", stopReason = Some("depth_limit"),
            answer = None, workId = None)
          else reading
        })))
      return Some(finishInvestigation(bounded, baseline, questions, responses))
    val submitted = pendingProposalComparisons(active.proposalEntries).exists((entry, other) =>
      other == value && entry.admission != "generated")
    val compact = active.remainingBudgetMs < RootElapsedMs + 2_000
    val longReply = firstReply && !surveyFirstReply && active.remainingBudgetMs >= SelectedReplyElapsedMs + 2_000
    for
      child <- history
      searchAnchor <- anchor
      if PositionRuleAssessment.assess(child).isInstanceOf[PositionRuleAssessment.Nonterminal]
      if List(value.referenceMove, value.alternativeMove).forall(child.currentLegalMoveUcis.contains)
      if !compact || ((requiredComparison || submitted) && active.remainingBudgetMs >= FocusElapsedMs + 2_000)
    yield
      val pending = pendingOwners
      val direct = pending.collect { case (entry, other) if other.id == "tactical_strength" &&
          entry.request.requirements.exists(_.id.startsWith("tactical_direct_")) =>
          (other.prefixMovesUci, other.referenceMove, other.alternativeMove)
        }.toSet
      def directCounter(other: CommentaryProposal.CompareMoves) = direct((other.prefixMovesUci, other.referenceMove, other.alternativeMove))
      val alternatives = if value.id == "tactical_strength" then pending.map(_._2).filter(other =>
        other.id == "tactical_strength" && other.prefixMovesUci == prefix && other.referenceMove == value.referenceMove &&
          directCounter(other) == directCounter(value))
        .map(_.alternativeMove).filter(child.currentLegalMoveUcis.contains).sorted else Nil
      val kingClass = if value.id != "reserve_hold" then Nil else
        CommentaryProposal.observedPrefix(active.packet, active.preparedPaths.get, prefix).toList
          .flatMap(path => active.preparedPaths.get.positionView(path, prefix.size).toList)
          .flatMap(_.legalRows.toList.flatten.filter(_.actor.role.name == "king").map(_.move))
      val pawnClass = if value.id != "reserve_counter" then Nil else
        CommentaryProposal.observedPrefix(active.packet, active.preparedPaths.get, prefix).toList
          .flatMap(path => active.preparedPaths.get.positionView(path, prefix.size).toList)
          .flatMap(_.legalRows.toList.flatten.filter(_.actor.role.name == "pawn").map(_.move))
      val moves = if kingClass.nonEmpty then (value.alternativeMove :: kingClass.sorted).distinct
        else if pawnClass.nonEmpty then (value.referenceMove :: pawnClass.sorted).distinct
        else (value.referenceMove :: value.alternativeMove :: alternatives).distinct.take(3)
      val originalOwners = pending.collect { case (entry, other) if other.prefixMovesUci == prefix &&
        ((functional && CommentaryIdea.pair(other) == CommentaryIdea.pair(value)) ||
          other.referenceMove == value.referenceMove && moves.contains(other.alternativeMove) &&
          (other == value || (value.id == "reserve_counter" && other.id == "reserve_counter") ||
            (value.id == "tactical_strength" && other.id == "tactical_strength" &&
            directCounter(other) == directCounter(value)))) => entry.request.id -> other.id
      }.distinct.sorted
      val owners = (originalOwners ++ Option.when(surveyFirstReply)(
        firstReplyOwners(active, prefix, reviewReference(baseline).getOrElse(active.playedMoveUci))).toList.flatten)
        .distinct.sorted
      // A direct opposing reply can decide the explanation of the root move.
      // Give that same-position comparison the existing root allowance rather
      // than abandoning a 30-second job after a 2.5-second partial search.
      val work = issuedWork(active.engineProfile, active.ledger.issuedWorkId,
        if prefix.isEmpty then EngineWorkPurpose.InvestigationComparison else EngineWorkPurpose.ResponseComparison,
        child, if surveyFirstReply then EngineRootRestriction.Unrestricted else EngineRootRestriction.Restricted(moves), child.currentFen, Nil,
        EngineSearchLimits(targetDepth,
          if longReply then SelectedReplyNodes else if requiredComparison || compact then SelectedInquiryNodes else FocusNodes,
          if longReply then SelectedReplyMovetimeMs else if compact then FocusMovetimeMs
            else if requiredComparison then RootMovetimeMs else FocusMovetimeMs,
          (if surveyFirstReply then child.currentLegalMoveUcis.size else moves.size).min(3)),
        if longReply then SelectedReplyElapsedMs else if compact then FocusElapsedMs
          else if requiredComparison then RootElapsedMs else FocusElapsedMs, searchAnchor)
        .copy(proposalRefs = owners)
      active.failedProposalWorks.find(sameProposalSearch(_, work)) match
        case Some(failed) =>
          val entries = exhaustedProposalEntries(active.proposalEntries, value, work, failed)
          beginResponses(active.copy(proposalEntries = entries), baseline, questions)
        case None =>
          val scheduled = searchAnchor.fold(responses)(awaitingTargetPool(responses, _))
          val next = active.copy(explorationsIssued = active.explorationsIssued + (if verifyAfter then 0 else 1),
            firstReplySurveysIssued = active.firstReplySurveysIssued ++ Option.when(surveyFirstReply)(prefix))
          val issued = issue(next, InvestigatingProposals(baseline, questions, scheduled, value, verifyAfter), work, next.ledger, Some(child))
            .asInstanceOf[ActiveCommentaryJob]
          refreshProposalReadings(issued)

  /** Rebind observations once per changed packet, retaining IDs already used to order a question. */
  private def readPrincipalVariations(active: ActiveCommentaryJob, reference: String): ActiveCommentaryJob =
    val paths = active.preparedPaths.get
    // A short automatic purpose job only consumes its owned reply readings.
    // Do not prepare every unrelated PV explanation before issuing that work.
    val purposeOnly = (active.policy.shortJob || active.remainingBudgetMs <= 10000) && active.policy.investigateChoices &&
      active.inquiries.isEmpty && active.responseInquiries.isEmpty && active.proposalEntries.isEmpty &&
      active.continuationInquiries.isEmpty && (paths.moveIntent.nonEmpty || paths.movePurpose.nonEmpty)
    val retainedIds = active.pvComparisons.map(_.id).toSet
    val references = (reference :: active.inquiries.map(_.question.referenceMove)).distinct
    val readings = if purposeOnly then Nil else PrincipalVariationComparison.readAll(active.packet, references,
      explanationReceipts(active), retainedIds, active.preparedPaths)
    val searches = active.responseReceipts.flatMap { receipt =>
      val prior = active.responseSearches.find(search => search.work == receipt.work && search.belongsTo(active.packet))
      prior.orElse(receipt.work.searchAnchor.flatMap(anchor =>
        historyAt(active, anchor.prefixMoves).flatMap(child =>
          TargetResponseInvestigation.readSearch(active.packet, receipt, child, active.preparedPaths.get))))
    }
    active.copy(pvComparisons = readings, inheritedPrepared = active.preparedPaths,
      responseSearches = searches.sortBy(search => search.lines.flatMap(_.evaluation.engineLine.map(_.depth))
        .minOption.getOrElse(search.work.requiredDepth)),
      continuationReadings = if purposeOnly then Nil else paths.continuationReadings)

  private def reuseInquiry(active: ActiveCommentaryJob,
      question: CommentaryInvestigation.Question): CommentaryInquiry =
    val answer = CommentaryInvestigation.preferredReceipts(active.receipts).iterator.flatMap(receipt =>
      CommentaryInvestigation.answer(question, receipt.work, receipt.lines, active.packet, active.preparedPaths))
      .find(currentAnswer(active, _))
    CommentaryInquiry(question, if answer.nonEmpty then "answered" else "deferred", answer,
      assessmentEffect = answer.map(_ => "unchanged"))

  private def reconsiderInquiry(active: ActiveCommentaryJob, prior: CommentaryInquiry): CommentaryInquiry =
    val answer = reuseInquiry(active, prior.question).answer
    if answer == prior.answer then prior
    else if answer.nonEmpty then prior.copy(state = "answered", answer = answer, stopReason = None,
      assessmentEffect = Some("unchanged"))
    else if prior.answer.nonEmpty then prior.copy(state = "deferred", answer = None,
      stopReason = Some("comparison_not_current"), assessmentEffect = None)
    else prior

  private def issueInvestigation(active: ActiveCommentaryJob, phase: Investigating,
      moves: List[String]): CommentaryJobState =
    if investigationDepth(active) > MaximumAnalysisDepth then
      val verification = CommentaryVerification(investigationDepth(active), state = "stopped",
        reason = "explanation_depth_limit", stopReason = Some("depth_limit"))
      return finishInvestigation(active.copy(verification = Some(verification)),
        phase.baseline, phase.questions, phase.responses, needsRefresh = true)
    val roots = moves.distinct
    val work = issuedWork(active.engineProfile, active.ledger.issuedWorkId,
      EngineWorkPurpose.InvestigationComparison, active.positionHistory,
      EngineRootRestriction.Restricted(roots), active.positionHistory.currentFen, Nil,
      EngineSearchLimits(investigationDepth(active), FocusNodes, FocusMovetimeMs, math.min(3, roots.size)), FocusElapsedMs)
    issue(active.copy(explorationsIssued = active.explorationsIssued + 1),
      phase, work, active.ledger)

  private def afterInvestigationReport(admitted: ActiveCommentaryJob, phase: Investigating,
      evaluations: List[AdmittedCandidateLine]): CommentaryJobState =
    val active = readPrincipalVariations(admitted, phase.selected.referenceMove)
    if !CommentaryInvestigation.candidatesReady(active.issuedWork, evaluations) then
      stop(active, CommentaryJobStopCondition.InvalidEngineWorkReport)
    else
      val heads = evaluations.map(_.evaluation.moves.head).toSet
      val questions = phase.questions.map { inquiry =>
        val answer = Option.when(heads(inquiry.question.referenceMove) && heads(inquiry.question.alternativeMove))(())
          .flatMap(_ => CommentaryInvestigation.answer(inquiry.question, active.issuedWork, evaluations,
            active.packet, active.preparedPaths))
        answer.fold(if inquiry.state == "awaiting_engine" then inquiry.copy(state = "deferred",
          stopReason = Some("investigation_limit")) else inquiry)(value =>
          inquiry.copy(state = "answered", answer = Some(value), stopReason = None, assessmentEffect = Some("unchanged")))
      }
      reconcileRootAssessment(active, phase.copy(questions = questions), evaluations)

  private def materialScoreChange(mover: chess.Color, before: CandidateLineEvaluation,
      after: CandidateLineEvaluation): Boolean = List(before -> after, after -> before).exists { (left, right) =>
    EvalComparison.fromEvaluations(mover, left, right).exists(comparison =>
      Set(MoveChoiceVerdict.Inaccuracy, MoveChoiceVerdict.Mistake, MoveChoiceVerdict.Blunder)(comparison.verdict))
  }

  /** All root investigations enter the same assessment boundary. A different PV or
    * small score drift still answers its own question, but does not invalidate an
    * independently owned actual/reference comparison by itself.
    */
  private def rootAssessmentRefresh(active: ActiveCommentaryJob, phase: InvestigationPhase,
      evaluations: List[AdmittedCandidateLine]): Option[RefreshingAssessment] =
    if evaluations.isEmpty then return None
    val referenceMove = reviewReference(phase.baseline).getOrElse(active.playedMoveUci)
    val previous = phase.baseline.result match
      case CompletedPositionCommentary.FocusedMoveReview(_, review) =>
        review.judgmentPacket.assembly.input.assessmentCandidates.map(_.evaluation)
      case _ => Nil
    val relevantPrevious = previous.filter(line => Set(referenceMove, active.playedMoveUci)(line.moves.head))
    val minimumDepth = (active.policy.rootSearchDepth :: previous.flatMap(_.engineLine.map(_.depth))).max
    val requiredDepth = (minimumDepth :: evaluations.flatMap(_.evaluation.engineLine.map(_.depth))).max
    val reference = evaluations.find(_.evaluation.moves.head == referenceMove)
    val best = evaluations.sortBy(line => (-line.evaluation.rankingScoreForMover(active.positionHistory.currentPosition.color),
      if line.evaluation.moves.head == referenceMove then 0 else 1, line.evaluation.moves.head)).head
    val bestMove = best.evaluation.moves.head
    val heads = evaluations.map(_.evaluation.moves.head).toSet
    val changedReference = reference.exists(line => relevantPrevious.find(_.moves.head == referenceMove).forall { previous =>
      line.evaluation.engineLine.map(_.depth).getOrElse(0) > previous.engineLine.map(_.depth).getOrElse(0) ||
        line.evaluation.engineLine.flatMap(_.mate) != previous.engineLine.flatMap(_.mate) ||
        materialScoreChange(active.positionHistory.currentPosition.color, previous, line.evaluation)
    })
    val improved = reference.exists(line => CommentaryInvestigation.evaluationRelation(
      active.positionHistory.currentPosition.color, line.evaluation, best.evaluation) == "better")
    if reference.nonEmpty && !heads(active.playedMoveUci) && !changedReference && !improved then
      return None
    val selectedId = phase.questions.find(inquiry => inquiry.question.referenceMove == referenceMove &&
      inquiry.question.alternativeMove == bestMove).map(_.question.id).orElse(phase match
        case inquiry: Investigating => phase.questions.find(value =>
          value.question.id == inquiry.selected.id && value.answer.nonEmpty).map(_.question.id)
        case _ => None)
    val questions = phase.questions.map(inquiry => if selectedId.contains(inquiry.question.id) then
      inquiry.copy(state = if inquiry.answer.nonEmpty then "answered" else "awaiting_engine",
        stopReason = None, assessmentEffect = Some("needs_refresh")) else inquiry)
    val refresh = RefreshingAssessment(phase.baseline, questions, phase.responses, referenceMove, selectedId, requiredDepth)
    val reconsidered = active.copy(phase = refresh)
    Some(refresh.copy(questions = questions.map { prior =>
      val current = reconsiderInquiry(reconsidered, prior)
      // Rebinding an answer does not discharge the new root assessment obligation.
      if selectedId.contains(prior.question.id) then current.copy(assessmentEffect = Some("needs_refresh"))
      else current
    }))

  private def reconcileRootAssessment(active: ActiveCommentaryJob, phase: InvestigationPhase,
      evaluations: List[AdmittedCandidateLine]): CommentaryJobState =
    val refresh = rootAssessmentRefresh(active, phase, evaluations) match
      case Some(value) => value
      case None => return beginResponses(active.copy(phase = phase), phase.baseline, phase.questions)
    if refresh.requiredDepth > MaximumAnalysisDepth then
      val verification = CommentaryVerification(refresh.requiredDepth, state = "stopped",
        reason = "explanation_depth_limit", stopReason = Some("depth_limit"))
      return finishInvestigation(active.copy(verification = Some(verification)),
        phase.baseline, refresh.questions, refresh.responses, needsRefresh = true)
    val heads = evaluations.map(_.evaluation.moves.head).toSet
    if heads(refresh.referenceMove) && heads(active.playedMoveUci) &&
        evaluations.flatMap(_.evaluation.engineLine.map(_.depth)).forall(_ >= refresh.requiredDepth) then
      refreshAssessment(active.copy(phase = refresh), refresh, evaluations)
    else
      // Keep the old reference in the same new work too: earlier superiority need not survive deeper search.
      val rankedMoves = evaluations.sortBy(line => (-line.evaluation.rankingScoreForMover(active.positionHistory.currentPosition.color),
        if line.evaluation.moves.head == refresh.referenceMove then 0 else 1, line.evaluation.moves.head)).map(_.evaluation.moves.head)
      val selected = List(refresh.referenceMove, active.playedMoveUci, rankedMoves.head).distinct
      // BestChoice still needs a runner-up when actual and reference are the same move.
      val roots = if selected.size > 1 then selected else selected ++ rankedMoves.filterNot(selected.contains).take(1)
      val work = issuedWork(active.engineProfile, active.ledger.issuedWorkId,
        EngineWorkPurpose.InvestigationComparison, active.positionHistory,
        EngineRootRestriction.Restricted(roots), active.positionHistory.currentFen, Nil,
        EngineSearchLimits(refresh.requiredDepth, FocusNodes, FocusMovetimeMs, roots.size), FocusElapsedMs)
      issue(active, refresh, work, active.ledger)

  private def refreshAssessment(active: ActiveCommentaryJob, phase: RefreshingAssessment,
      evaluations: List[AdmittedCandidateLine]): CommentaryJobState =
    if evaluations.size < 2 || !CommentaryInvestigation.candidatesReady(active.issuedWork, evaluations) ||
        !List(active.playedMoveUci, phase.referenceMove).forall(move => evaluations.exists(_.evaluation.moves.head == move)) ||
        !evaluations.flatMap(_.evaluation.engineLine.map(_.depth)).forall(_ >= phase.requiredDepth) then
      return stop(active.copy(phase = phase), CommentaryJobStopCondition.InvalidEngineWorkReport)
    val verifiedQuestions = phase.questions.map(reconsiderInquiry(active, _))
    val ranked = evaluations.sortBy(line => (-line.evaluation.rankingScoreForMover(active.positionHistory.currentPosition.color),
      if line.evaluation.moves.head == phase.referenceMove then 0 else 1, line.evaluation.moves.head))
    val actual = evaluations.find(_.evaluation.moves.head == active.playedMoveUci).get
    val assessmentPair = if ranked.head.evaluation.moves.head == active.playedMoveUci then
      List(actual, ranked.find(_.evaluation.moves.head != active.playedMoveUci).get)
    else List(ranked.head, actual)
    // Only this newly admitted, same-work pair sets the new primary. Older
    // reports remain observations, not a silently merged ranking.
    val refreshed = Try(MoveReviewInputAdmission.prepareFocusedMoveReview(
      active.positionHistory, active.playedMoveUci, active.resultingFen, active.rootRuleAssessment,
      assessmentPair, Nil, rankingSource = EvaluationSource.InvestigationComparison)).toOption.flatten.flatMap {
      case PreparedFocusedPositionCommentary.PositionAction(action) =>
        Some(completePositionAction(active, action, phase.baseline.progress.rootCandidateLinesAdmitted, active.ledger))
      case PreparedFocusedPositionCommentary.MoveReview(review) =>
        assembleRetainedPacket(review.preparedInput, active.packet).collect { case (packet, None) =>
          val result = completeReview(active, ReviewContext(review, evaluations), packet, active.ledger)
          val ranked = result.result match
            case CompletedPositionCommentary.FocusedMoveReview(claims, record: CompletedMoveReviewRecord) =>
              CompletedPositionCommentary.FocusedMoveReview(claims,
                record.copy(playedRootRank = None, bestRootRank = None))
            case other => other
          result.copy(result = ranked,
            progress = result.progress.copy(rootCandidateLinesAdmitted = phase.baseline.progress.rootCandidateLinesAdmitted),
            assessmentScope = "investigation_pair")
        }
    }
    refreshed match
      case None => stop(active.copy(phase = phase), CommentaryJobStopCondition.MoveReviewPreparationFailed)
      case Some(completed) =>
        val questions = verifiedQuestions.map(inquiry =>
          if phase.selectedQuestionId.contains(inquiry.question.id) then inquiry.copy(assessmentEffect = Some("refreshed")) else inquiry)
        val packet = completed.observationsPacket.getOrElse(active.packet)
        val reference = completed.result match
          case CompletedPositionCommentary.FocusedMoveReview(_, record) => Some(record.bestMoveUci)
          case _ => None
        val read = readPrincipalVariations(active.copy(packet = packet, verification = completed.verification),
          reference.getOrElse(phase.referenceMove))
        val updated = reference.filter(_ != phase.referenceMove).filter(_ => active.policy.investigateChoices).fold(questions) { move =>
          val previous = questions.map(inquiry =>
            if inquiry.state == "deferred" then inquiry.copy(state = "stopped", stopReason = Some("reference_changed"))
            else inquiry)
          val knownPairs = previous.filter(_.answer.nonEmpty).map(inquiry =>
            Set(inquiry.question.referenceMove, inquiry.question.alternativeMove)).toSet
          previous ++ CommentaryInvestigation.discover(packet, move,
            currentRootReceipt(read),
            // Each reference's reader keeps the latest usable pair before retained older evidence.
            read.pvComparisons.filter(_.referenceMove == move).distinctBy(_.alternativeMove), read.preparedPaths).filterNot(question =>
            knownPairs(Set(question.referenceMove, question.alternativeMove))).map { question =>
            val inquiry = reuseInquiry(read, question)
            if inquiry.answer.isEmpty then inquiry.copy(stopReason = Some("investigation_limit")) else inquiry
          }
        }
        beginResponses(read.copy(phase = phase.copy(baseline = completed, questions = updated, refreshing = false)),
          completed, updated)

  private def finishInvestigation(before: ActiveCommentaryJob, completed: CompletedCommentaryJobRecord,
      questions: List[CommentaryInquiry], responses: List[CommentaryResponseInquiry],
      needsRefresh: Boolean = false): CompletedCommentaryJobRecord =
    // Early purpose completion can bypass the general agenda refresh. Bind every
    // existing question to its returned receipt before delivering a final snapshot.
    val observed = before.preparedPaths.filter(_ => before.continuationInquiries.nonEmpty).fold(before) { paths =>
      before.copy(continuationInquiries = refreshContinuationInquiries(before.packet, paths, Nil,
        before.continuationInquiries, before.responseSearches))
    }
    val active = refreshProposalReadings(observed)
    val result = completed.result match
      case CompletedPositionCommentary.FocusedMoveReview(claims, review: CompletedMoveReviewRecord) =>
        // A failed upper assembly may still retain new lower observations. Keep the
        // previously certified assessment separately instead of erasing it with that lower packet.
        CompletedPositionCommentary.FocusedMoveReview(claims, review.copy(judgmentPacket =
          if active.packet.primary.nonEmpty then active.packet else review.judgmentPacket))
      case other => other
    completed.copy(result = result, observationsPacket = Some(active.packet),
      inquiries = questions.map(reconsiderInquiry(active, _)),
      rootSearches = active.rootSearches,
      pvComparisons = active.pvComparisons,
      responseInquiries = responses.map(bindResponseReading(active, _)), preparedPaths = active.preparedPaths,
      responseSearches = active.responseSearches,
      continuationReadings = active.continuationReadings, continuationInquiries = active.continuationInquiries.map(inquiry =>
        if inquiry.state == "forced" && inquiry.question.observedMoves.isEmpty &&
            active.localExpansionsUsed >= active.policy.maximumLocalExpansions then
          inquiry.copy(stopReason = Some("local_expansion_limit"))
        else inquiry),
      proposalEntries = CommentaryProposal.finish(active.proposalEntries,
        if active.explorationsIssued >= 2 then "investigation_limit" else "no_executable_work"),
      localExpansionsUsed = active.localExpansionsUsed,
      discoveryState = active.discoveryState,
      verification = active.verification,
      assessmentNeedsRefresh = needsRefresh || active.assessmentNeedsRefresh,
      progress = completed.progress.copy(phase = CommentaryJobPhase.Completed,
        physicalWorksIssued = active.ledger.physicalWorksIssued,
        physicalReportsAccepted = active.ledger.physicalReportsAccepted))

  private def reviewReference(completed: CompletedCommentaryJobRecord): Option[String] = completed.result match
    case CompletedPositionCommentary.FocusedMoveReview(_, review) => Some(review.bestMoveUci)
    case _ => None

  private def bindResponseReading(active: ActiveCommentaryJob,
      inquiry: CommentaryResponseInquiry): CommentaryResponseInquiry =
    if inquiry.readingOwner.exists(_ eq active.packet) then inquiry
    else
      val reading = inquiry.answer match
        case Some(answer) => TargetResponseInvestigation.read(active.packet, inquiry.question,
          answer.pair.referenceLineId, answer.pair.alternativeLineId, active.preparedPaths.get)
        case None => TargetResponseInvestigation.inspectExisting(active.packet, inquiry.question, active.preparedPaths.get)
      // Replacing a representative must not erase an already observed, weaker counterexample.
      val retained = (inquiry.retainedReadings ++ inquiry.reading).distinctBy(_.id).flatMap { previous =>
        if previous.belongsTo(active.packet) then Some(previous)
        else previous.lineIds match
          case List(left, right) => TargetResponseInvestigation.read(active.packet, previous.question,
            left, right, active.preparedPaths.get)
          case _ => None
      }.filterNot(previous => reading.exists(_.id == previous.id))
      // Cache an unavailable reading too; serialization must not repeat the same proof search.
      inquiry.copy(reading = reading, readingOwner = Some(active.packet), retainedReadings = retained)

  private def continuationQuestions(active: ActiveCommentaryJob, reference: String): List[CommentaryContinuationInquiry] =
    val prepared = active.preparedPaths.get
    // Ending discovery does not end an issued question's ownership. A late
    // report must still update its retained inquiry and route comparison.
    if !active.policy.investigateChoices then
      return refreshContinuationInquiries(active.packet, prepared, Nil,
        active.continuationInquiries, active.responseSearches)
    val root = active.packet.assembly.playedTransition.get
    val views = prepared.resourceViews(active.packet)
    val investigationIds = prepared.investigationPaths.map(_.line.ref.id).toSet
    val localPoints = active.continuationReadings.filter(reading =>
      investigationIds(reading.path.line.ref.id) && Set(active.playedMoveUci, reference)(reading.path.moves.head)).flatMap { reading =>
      reading.choicePoints.filter(point => point.prefixMovesUci.nonEmpty && point.prefixMovesUci.size < 80 &&
        reading.path.moves.startsWith(point.prefixMovesUci) &&
        reading.path.positions.lift(point.prefixMovesUci.size).flatMap(_.id).contains(point.positionId))
        .map(point => (reading.path, point))
    }
    val searchedFrontiers = active.responseSearches.flatMap(search =>
      LineContinuationInvestigation.unobservedContinuations(active.packet, search, prepared)).filter { (reading, point) =>
        Set(active.playedMoveUci, reference)(reading.path.moves.head) && point.prefixMovesUci.size < 80
      }
    val searchesById = active.responseSearches.map(search => search.work.workId -> search).toMap
    def supportBranch(source: LineContinuationInvestigation.ComparisonSource): Option[(String, String)] =
      for
        _ <- Option.when(source.kind == "support_application")(())
        search <- searchesById.get(source.workId)
        peer <- prepared.path(active.packet, source.peerLineId)
        move <- peer.moves.lift(search.anchor.plyOffset)
      yield search.anchor.positionId -> move
    // One earliest returned frontier per recapture branch. Later reports may
    // answer it, but do not move the same question down an endless endpoint chain.
    val firstSupportFrontier = scala.collection.mutable.Map.empty[(String, String), List[String]]
    active.continuationInquiries.foreach(inquiry => inquiry.question.comparisonSources.flatMap(supportBranch)
      .foreach(key => firstSupportFrontier.getOrElseUpdate(key, inquiry.question.prefixMoves)))
    val frontierPoints = searchedFrontiers.filter { (_, point) =>
      point.comparisonSources.flatMap(supportBranch).forall(key =>
        firstSupportFrontier.getOrElseUpdate(key, point.prefixMovesUci) == point.prefixMovesUci)
      }.map((reading, point) => reading.path -> point)
    val checkPoints = LineContinuationInvestigation.checkResponsePoints(active.packet, prepared).filter { (path, point) =>
      Set(active.playedMoveUci, reference)(path.moves.head) && point.prefixMovesUci.size < 80
    }
    val points = localPoints ++ frontierPoints ++ checkPoints
    val discovered = points.groupBy { (path, point) =>
      (point.prefixMovesUci, path.ids.take(point.prefixMovesUci.size), point.positionId)
    }.toList.flatMap { case ((prefix, ids, position), related) =>
      views.get(position).flatMap(_.legalRows).filter(_.nonEmpty).map { legal =>
        val branches = prepared.paths.filter(path => path.certified.nonEmpty && path.ids == path.declared &&
          path.moves.startsWith(prefix) && path.moves.size > prefix.size && path.ids.startsWith(ids) &&
          path.positions.lift(prefix.size).flatMap(_.id).contains(position))
        CommentaryContinuationQuestion(CommentarySelection.stableId("line-continuation-question", root.evidence.id :: ids),
          root.evidence.id, root.from.id.get, position, prefix, ids,
          branches.map(_.moves(prefix.size)).distinct.sorted,
          (related.map(_._1.line.ref.id) ++ branches.map(_.line.ref.id)).distinct.sorted,
          legal.map(_.move), legal.map(row => row.move -> row.sourceId).toMap,
          mergeContinuationPremises(related.flatMap { (path, point) =>
            point.premises.map(premise => CommentaryContinuationPremise(premise, List(path.line.ref.id)))
          }))
      }
    }
    refreshContinuationInquiries(active.packet, prepared, discovered, active.continuationInquiries, active.responseSearches)

  private def mergeContinuationPremises(premises: List[CommentaryContinuationPremise]) =
    premises.map(premise => premise.copy(origin = premise.origin.copy(
      triggerIds = premise.origin.triggerIds.distinct.sorted,
      comparisonSources = premise.origin.comparisonSources.distinct.sortBy(_.json.toString))))
      .groupBy(_.origin).toList.map { (origin, values) =>
        CommentaryContinuationPremise(origin, values.flatMap(_.sourceLineIds).distinct.sorted)
      }.sortBy(_.id(""))

  private[runtime] def refreshContinuationInquiries(packet: EvidenceBackedJudgmentPacket,
      prepared: ObservedLineProtocol.PreparedPaths, discovered: List[CommentaryContinuationQuestion],
      previousInquiries: List[CommentaryContinuationInquiry], searchesAtPositions: List[TargetResponseInvestigation.Search])
      : List[CommentaryContinuationInquiry] =
    prepared.requireOwner(packet)
    // Rebuilding a question may add a newly observed branch; it never invalidates an older exact choice point.
    val merged = (discovered ++ previousInquiries.map(_.question)).groupBy(_.id).values.map { values =>
      values.head.copy(premises = mergeContinuationPremises(values.flatMap(_.premises).toList),
        observedMoves = values.flatMap(_.observedMoves).distinct.sorted,
        sourceLineIds = values.flatMap(_.sourceLineIds).distinct.sorted)
    }.toList
    val readings = prepared.continuationReadings.map(reading => reading.path.line.ref.id -> reading).toMap
    val certified = prepared.paths.filter(path => path.certified.nonEmpty && path.ids == path.declared)
    val certifiedById = certified.map(path => path.line.ref.id -> path).toMap
    val investigationPaths = prepared.investigationPaths.filter(path => certifiedById.contains(path.line.ref.id))
    val refreshed = merged.map { original =>
      val branches = prepared.paths.filter(path => path.certified.nonEmpty && path.ids == path.declared &&
        path.moves.startsWith(original.prefixMoves) && path.moves.size > original.prefixMoves.size &&
        path.ids.startsWith(original.prefixOccurrenceIds) &&
        path.positions.lift(original.prefixMoves.size).flatMap(_.id).contains(original.positionId))
      // Several obligations can share one comparison. Reconstruct it once and
      // select its individual requirement, retaining partial witnesses.
      val comparisonRequirements = original.comparisonSources.map { source =>
        source -> LineContinuationInvestigation.continuationRequirements(packet, original.prefixMoves,
          List(source), prepared)
      }.toMap
      val question = original.copy(
        observedMoves = (original.observedMoves ++ branches.map(_.moves(original.prefixMoves.size))).distinct.sorted,
        sourceLineIds = (original.sourceLineIds ++ branches.map(_.line.ref.id)).distinct.sorted)
      val searches = searchesAtPositions.filter(search => search.belongsTo(packet) && search.anchor.positionId == question.positionId &&
        search.anchor.prefixMoves == question.prefixMoves)
      val surveyed = searches.exists(search => search.work.rootRestriction match
        case EngineRootRestriction.Unrestricted => true
        case EngineRootRestriction.Restricted(moves) => question.legalMoves.forall(moves.contains))
      val returned = searches.flatMap(_.pathsByMove.values).distinct
      val roots = (returned ++ certified.filter(path => question.sourceLineIds.contains(path.line.ref.id) &&
        path.ids == question.prefixOccurrenceIds) ++ branches.filter(path =>
        question.legalMoves.size == 1 && question.observedMoves.nonEmpty)).distinct
      // A probe can witness an explicit fact without replacing a returned investigation endpoint.
      // Endpoint replacement and its continuation readings must use the same qualified paths.
      val descendants = investigationPaths.filter(path => roots.exists(root => path.ids.startsWith(root.ids)))
      val ends = descendants.filterNot(path => descendants.exists(other =>
        other.ids.size > path.ids.size && other.ids.startsWith(path.ids)))
      val openEndsByOrigin = ends.flatMap(path => readings.get(path.line.ref.id).toList.flatMap(reading =>
        reading.events.filter(event => reading.openLossEventIds(event.id)).flatMap(event =>
          reading.lossOrigins.getOrElse(event.occurrenceId, List(event.occurrenceId))
            .map(_ -> path.line.ref.id)))).groupMap(_._1)(_._2)
      val observations = question.premises.map { premise =>
        val origin = premise.origin
        val transferred = Set("continuation_not_observed", "support_application_not_observed")(origin.kind)
        val requirements = origin.comparisonSources.map { source =>
          source -> comparisonRequirements.getOrElse(source, None).map(_.filter(requirement =>
            origin.requirementId.forall(_ == requirement.id))).filter(_.nonEmpty)
        }
        val originAvailable = premise.sourceLineIds.flatMap(certifiedById.get).exists(path =>
          path.ids.startsWith(question.prefixOccurrenceIds) &&
            path.positions.lift(question.prefixMoves.size).flatMap(_.id).contains(question.positionId))
        val materialRequirements = Option.when(origin.kind == "material_followup" && originAvailable)(
          LineContinuationInvestigation.materialContinuationRequirements(packet, question.prefixMoves,
            origin.triggerIds, prepared)).flatten
        val witnesses = if !originAvailable then Nil else origin.kind match
          case "check_response_not_observed" => branches.map(_.line.ref.id)
          case "material_followup" =>
            materialRequirements.toList.flatten.flatMap(_.witnessLineIds)
          case _ if transferred => requirements.flatMap(_._2.toList.flatten.flatMap(_.witnessLineIds))
          case _ => Nil
        val observationState = origin.kind match
          case "check_response_not_observed" =>
            if !originAvailable then "unavailable" else if witnesses.nonEmpty then "observed" else "unobserved"
          case "material_followup" =>
            if materialRequirements.isEmpty then "unavailable"
            else if materialRequirements.exists(_.forall(_.witnessLineIds.nonEmpty)) then "observed" else "unobserved"
          case _ if transferred =>
            if !originAvailable || requirements.isEmpty || requirements.exists(_._2.isEmpty) then "unavailable"
            else if requirements.forall(_._2.exists(_.forall(_.witnessLineIds.nonEmpty))) then "observed"
            else "unobserved"
          // These premises ask for a choice comparison. A completed search or
          // an existing event is not proof that the route is tactically stable.
          case _ => "not_applicable"
        val missingPeers = requirements.collect {
          case (source, answer) if answer.isEmpty || answer.exists(_.exists(_.witnessLineIds.isEmpty)) => source.peerLineId
        }
        val unfinished = if origin.kind == "check_response_not_observed" then Nil else
          origin.triggerIds.flatMap(id => openEndsByOrigin.getOrElse(id, Nil))
        val unfinishedFunctions = if origin.kind == "replacement_function_choice" then
          LineContinuationInvestigation.unresolvedReplacementFunctions(packet, question.prefixMoves,
            origin.triggerIds, premise.sourceLineIds, ends, prepared, origin.requirementId)
        else Nil
        val unfinishedUses = if origin.kind == "support_application_not_observed" then
          LineContinuationInvestigation.unresolvedSupportContinuations(packet, question.prefixMoves,
            origin.comparisonSources, ends, prepared, origin.requirementId)
        else Nil
        val unfinishedExposures = if Set("capture_exposure_response", "exposure_prevention_choice")(origin.kind) then
          LineContinuationInvestigation.unresolvedExposures(packet, question.prefixMoves,
            origin.triggerIds, premise.sourceLineIds, ends, prepared)
        else Nil
        val unchecked = if origin.kind == "check_response_not_observed" && witnesses.isEmpty then
          premise.sourceLineIds.filter(id => certifiedById.get(id).exists(path =>
            path.ids == question.prefixOccurrenceIds && path.positions.last.id.contains(question.positionId)))
        else Nil
        CommentaryPremiseObservation(premise, observationState, witnesses.distinct.sorted,
          (missingPeers ++ unfinished ++ unfinishedFunctions ++ unfinishedUses ++ unfinishedExposures ++ unchecked)
            .distinct.sorted)
      }
      val previous = previousInquiries.find(_.question.id == question.id)
      val witnessed = observations.nonEmpty && observations.forall(_.observationState == "observed")
      val state = if surveyed then "surveyed" else if witnessed then "observed"
        else if question.legalMoves.size == 1 then "forced"
        else previous.filter(_.state == "stopped").fold("deferred")(_.state)
      CommentaryContinuationInquiry(question, state,
        if state == "deferred" then Some("investigation_limit") else if state == "stopped" then previous.flatMap(_.stopReason)
        else None, searches.map(_.work.workId), observations,
        routeChallenge = readRouteChallenge(packet, prepared, question, searches, previous.flatMap(_.routeChallenge)))
    }
    refreshed.map { inquiry =>
      val question = inquiry.question
      val lossOccurrences = question.triggerOccurrenceIds.filter(id => readings.valuesIterator.exists(reading =>
        reading.events.exists(event => event.occurrenceId == id && event.originalVictim.nonEmpty))).toSet
      val followups = refreshed.filter { next =>
        next.question.prefixOccurrenceIds.size > question.prefixOccurrenceIds.size &&
          next.question.prefixOccurrenceIds.startsWith(question.prefixOccurrenceIds) &&
          next.question.triggerOccurrenceIds.exists(lossOccurrences)
      }.map(_.question.id).distinct.sorted
      inquiry.copy(followupQuestionIds = followups)
    }.sortBy(inquiry => (inquiry.question.prefixMoves.size,
      if inquiry.question.prefixMoves.head == packet.assembly.input.playedMoveUci then 0 else 1,
      inquiry.question.prefixMoves.mkString(" ")))

  /** Retain each recovery's complete legal class. One returned member cannot stand in
    * for another class, and only one owned work may compare its representative with R.
    */
  private[runtime] def readRouteChallenge(packet: EvidenceBackedJudgmentPacket,
      prepared: ObservedLineProtocol.PreparedPaths, question: CommentaryContinuationQuestion,
      searches: List[TargetResponseInvestigation.Search], previous: Option[CommentaryRouteChallenge])
      : Option[CommentaryRouteChallenge] =
    prepared.requireOwner(packet)
    val owned = searches.filter(search => search.belongsTo(packet) && search.anchor.positionId == question.positionId &&
      search.anchor.prefixMoves == question.prefixMoves)
    def covers(search: TargetResponseInvestigation.Search, moves: List[String]): Boolean = search.work.rootRestriction match
      case EngineRootRestriction.Unrestricted => true
      case EngineRootRestriction.Restricted(restricted) => moves.forall(restricted.contains)
    val full = owned.filter(search => covers(search, question.legalMoves))
    val baseline = full.lastOption
    baseline.flatMap { survey =>
      val classes = LineContinuationInvestigation.refutingMoveClasses(packet, question.prefixMoves,
        question.sourceLineIds, question.triggerOccurrenceIds, prepared)
      survey.lines.headOption.filter(_ => classes.nonEmpty).map { leader =>
        val reference = leader.evaluation.moves.head
        val sampled = classes.map { group =>
          val receipts = owned.filter(search => covers(search, group.movesUci) &&
            search.lines.exists(line => group.movesUci.contains(line.evaluation.moves.head)))
          val comparison = receipts.reverseIterator.flatMap(search => search.representativePair(reference, group.movesUci)
            .map((left, right) => CommentaryRouteChallengeComparison(search.work.workId,
              left.line.ref.id, right.line.ref.id))).take(1).toList.headOption
          CommentaryRouteChallengeClass(group.movesUci, group.premiseOccurrenceIds, group.sourceLineIds,
            receipts.map(_.work.workId), comparison, group.kind)
        }
        val missing = sampled.exists(_.sampledWorkIds.isEmpty)
        val nonSelf = sampled.filterNot(_.movesUci.contains(reference))
        val stopped = previous.filter(prior => prior.state == "stopped" && missing)
        val state = stopped.fold(
          if missing then "not_returned"
          else if nonSelf.nonEmpty && nonSelf.forall(_.comparison.nonEmpty) then "compared"
          else "sampled")(_.state)
        CommentaryRouteChallenge(survey.work.workId, reference, survey.pathsByMove(reference).line.ref.id,
          sampled, state, stopped.flatMap(_.stopReason).orElse(
            Option.when(missing || nonSelf.exists(_.comparison.isEmpty))("investigation_limit")))
      }
    }

  /** A unique legal continuation is a board observation, not an engine task.
    * Replay uses the same exact-history owner as returned engine lines and adds no evaluation.
    */
  private def observeForcedContinuations(active: ActiveCommentaryJob, reference: String,
      baseline: CompletedCommentaryJobRecord, questions: List[CommentaryInquiry], includeGenerated: Boolean = true)
      : Either[CommentaryJobState, ActiveCommentaryJob] =
    val automatic = active.continuationInquiries.filter(inquiry => inquiry.state == "forced" &&
      inquiry.question.observedMoves.isEmpty && inquiry.question.prefixMoves.size < 80)
      .map(inquiry => inquiry.question.prefixMoves -> 80)
    val factScopes = List(false, true).flatMap(generated =>
      pendingFactContinuations(active, generated).map(generated -> _))
    def facts(generated: Boolean) = factScopes.collect {
      case (scope, value) if scope == generated && factContinuationContext(active, value.need.prefixMoves)
          .exists(_._1.currentLegalMoveUcis.size == 1) => value.need
    }.map(need => need.prefixMoves -> need.horizon.get)
    val capBlocked = scala.collection.mutable.Set.empty[(String, String, CommentaryFact.Need)]
    def recordCapFailure(initialPrefix: List[String], moves: List[String], horizon: Int,
        discoveryOnly: Boolean): Unit =
      factScopes.filter((generated, value) =>
        generated == discoveryOnly && value.need.prefixMoves == initialPrefix).foreach { (_, value) =>
        value.owners.foreach { (requestId, requirementId) =>
          active.proposalEntries.find(_.request.id == requestId).toList
            .flatMap(_.readings.filter(_.requirementId == requirementId))
            .flatMap(_.result.toList.flatMap(_.needs)).filter(need =>
              need.kind == "continuation" && need.prefixMoves == initialPrefix &&
                need.horizon.exists(at => at >= moves.size && at <= horizon))
            .foreach(need => capBlocked += ((requestId, requirementId, need)))
        }
      }
    def retainCapReasons(current: ActiveCommentaryJob): ActiveCommentaryJob =
      if capBlocked.isEmpty then current
      else
        val fresh = refreshProposalReadings(current)
        fresh.copy(proposalEntries = fresh.proposalEntries.map { entry =>
          val needed = CommentaryProposal.neededRequirementIds(entry)
          entry.copy(readings = entry.readings.map { reading =>
            val exactBlocked = reading.result.exists(_.needs.exists(need =>
              capBlocked((entry.request.id, reading.requirementId, need))))
            if needed(reading.requirementId) && !CommentaryProposal.isResolved(reading) &&
                !Set("invalid_input", "stopped")(reading.state) && exactBlocked then
              reading.copy(state = "stopped", stopReason = Some("local_expansion_limit"))
            else reading
          })
        })
    def grouped(points: List[(List[String], Int)]) = points.groupMap(_._1)(_._2).toList
      .map((prefix, horizons) => prefix -> horizons.max).sortBy((prefix, _) => (prefix.size, prefix.mkString(" ")))
    val existing = grouped(automatic ++ facts(generated = false))
    val existingHorizons = existing.toMap
    // Preserve existing forced observations before spending their replay capacity on generated horizons.
    // LocalReplayBatch reuses shared edges when a later generated horizon extends the same prefix.
    val forced = existing.map((prefix, horizon) => (prefix, horizon, false)) ++
      grouped(if includeGenerated then facts(generated = true) else Nil).filter((prefix, horizon) =>
        horizon > existingHorizons.getOrElse(prefix, 0)).map((prefix, horizon) => (prefix, horizon, true))
    if forced.isEmpty then Right(active)
    else
      val replay = new LocalReplayBatch(active)
      var invalidContinuation = false
      forced.foreach { (initialPrefix, horizon, discoveryOnly) =>
        var prefix = initialPrefix
        def prepare(moves: List[String]): Option[CanonicalPositionHistory] = replay.ensurePath(moves, discoveryOnly) match
          case Right(history) => Some(history)
          case Left(reason) =>
            if reason == "local_expansion_limit" then
              recordCapFailure(initialPrefix, moves, horizon, discoveryOnly)
            else invalidContinuation = true
            None
        var current = prepare(prefix)
        var continue = true
        while continue && prefix.size < horizon do
          val next = current.filter(history =>
            PositionRuleAssessment.assess(history).isInstanceOf[PositionRuleAssessment.Nonterminal] &&
              history.currentLegalMoveUcis.size == 1).map(history => prefix ++ history.currentLegalMoveUcis)
          next match
            case Some(moves) =>
              current = prepare(moves)
              prefix = moves
              continue = current.nonEmpty
            case None => continue = false
      }
      replay.retain(active.proposalEntries) match
          case Left((retained, failure)) => Left(stop(retained, failure, Some(baseline)))
          case Right(retained) if invalidContinuation =>
            Left(stop(retained, CommentaryJobStopCondition.ReviewConstructionFailed, Some(baseline)))
          case Right(retained) if replay.hasAdditions =>
            val read = retainCapReasons(readPrincipalVariations(retained, reference))
            Left(beginResponses(read, baseline, questions))
          case Right(retained) => Right(retainCapReasons(retained))

  private[runtime] def investigationPrefixRelation(state: AwaitingEngineWork,
      reference: String): List[String] => Int =
    val read = state match
      case value: ActiveCommentaryJob => value
    // Provenance is independent of the kind of chess premise. A witnessed but
    // conditional suffix does not displace a question on the current comparison.
    // 0: current selected path; 1: beyond its finite end; 2: another branch.
    val prepared = read.preparedPaths.get
    val latestRoot = read.receipts.reverseIterator.find(receipt =>
      receipt.work.purpose != EngineWorkPurpose.ResponseComparison &&
        receipt.lines.exists(_.evaluation.moves.headOption.contains(reference)))
    val selectedRootIds = read.pvComparisons.filter(value => value.referenceMove == reference &&
      latestRoot.exists(_.work.workId == value.answer.workId) &&
      (read.playedMoveUci == reference || value.alternativeMove == read.playedMoveUci))
      .flatMap(value => List(value.answer.referenceLineId, value.answer.alternativeLineId)).distinct
    val selectedRootPaths = selectedRootIds.flatMap(prepared.path(read.packet, _))
    val rootPaths = if selectedRootPaths.nonEmpty then selectedRootPaths else latestRoot.toList.flatMap(_.lines)
      .filter(line => line.evaluation.moves.headOption.exists(Set(read.playedMoveUci, reference)))
      .flatMap(line => prepared.certifiedPath(read.packet, line.evaluation.moves))
    val rootPrefixes = rootPaths.map(_.ids) ++ List(read.playedMoveUci, reference).distinct
      .filterNot(move => rootPaths.exists(_.moves.headOption.contains(move))).flatMap(move =>
        prepared.paths.find(_.moves.headOption.contains(move)).map(_.ids.take(1)))
    def relativeTo(prefix: List[String], paths: List[List[String]]): Int =
      paths.map(ids => if ids.startsWith(prefix) then 0 else if prefix.startsWith(ids) then 1 else 2)
        .minOption.getOrElse(2)
    // The closest exact search boundary supersedes an older short ancestor.
    // Otherwise that ancestor could mislabel a now-divergent branch as merely short.
    var selectedBoundaries = List.empty[(List[String], Int, List[List[String]])]
    def prefixRelation(prefix: List[String]): Int =
      selectedBoundaries.filter((anchor, _, _) => prefix.startsWith(anchor)).maxByOption(_._1.size)
        .fold(relativeTo(prefix, rootPrefixes)) { (_, inherited, paths) =>
          math.max(inherited, relativeTo(prefix, paths))
        }
    read.responseSearches.groupBy(search => (search.anchor.positionId, search.anchor.prefixMoves)).values.toList
      .sortBy(searches => (searches.head.anchor.plyOffset, searches.head.anchor.positionId,
        searches.head.anchor.prefixMoves.mkString(" "))).foreach { searches =>
        val first = searches.head
        val prefix = first.pathsByMove(first.lines.head.evaluation.moves.head).ids.take(first.anchor.plyOffset)
        val inherited = prefixRelation(prefix)
        val parentPaths = selectedBoundaries.filter((anchor, _, _) => prefix.startsWith(anchor))
          .maxByOption(_._1.size).fold(rootPrefixes)(_._3).filter(_.startsWith(prefix))
        def selected(search: TargetResponseInvestigation.Search): List[List[String]] =
          val pairs = search.selectedLinePairs.flatMap((left, right) => List(left.ids, right.ids))
          // A singleton class supplies a path observation, never an evaluation pair.
          if pairs.nonEmpty then pairs.distinct else search.lines.headOption.toList
            .map(line => search.pathsByMove(line.evaluation.moves.head).ids)
        val fullIndex = searches.lastIndexWhere(_.work.rootRestriction == EngineRootRestriction.Unrestricted)
        val full = if fullIndex >= 0 then selected(searches(fullIndex)) else parentPaths
        val comparisons = searches.drop(fullIndex + 1).flatMap(selected)
        selectedBoundaries = selectedBoundaries :+ ((prefix, inherited, (full ++ comparisons).distinct))
      }
    val ranks = scala.collection.mutable.Map.empty[List[String], Int]
    prefix => ranks.synchronized { ranks.getOrElseUpdate(prefix, prefixRelation(prefix)) }

  /** Repeating a single already observed immediate gain is still a question, but
    * it does not by itself justify spending the next slot ahead of an opponent's
    * unobserved response. Mixed premises and choices with a concrete challenge
    * retain their existing priority.
    */
  private def repeatsImmediateRecovery(question: CommentaryContinuationQuestion,
      readings: List[LineContinuationInvestigation.Reading]): Boolean =
    if question.kinds.toSet != Set("other_target_gain") || question.comparisonSources.nonEmpty ||
        question.observedMoves.distinct.size != 1 then false
    else
      val count = question.prefixMoves.size
      val triggers = question.triggerOccurrenceIds.toSet
      val gains = readings.filter(reading => question.sourceLineIds.contains(reading.path.line.ref.id) &&
        reading.path.ids.startsWith(question.prefixOccurrenceIds) &&
        reading.path.positions.lift(count).flatMap(_.id).contains(question.positionId)).flatMap { reading =>
        val events = reading.events.map(event => event.id -> event).toMap
        reading.otherTargetGains.flatMap { link =>
          for
            loss <- events.get(link.lossEventId)
            gain <- events.get(link.gainEventId)
            if triggers(loss.occurrenceId) && triggers(gain.occurrenceId)
            if count == loss.plyOffset + 1 || count == gain.plyOffset - 1
          yield count == loss.plyOffset + 1 && count == gain.plyOffset &&
            gain.originalActor.exists(actor => reading.path.positions(count).sideToMove.contains(actor.side)) &&
            reading.path.moves.lift(count).contains(question.observedMoves.head)
        }
      }
      gains.nonEmpty && gains.forall(identity)

  /** Reuse exact answers and refresh needed conditions before allocating one shared slot.
    * A comparison family has no blanket priority over an unresolved current PV outcome.
    */
  private def beginResponses(before: ActiveCommentaryJob, baseline: CompletedCommentaryJobRecord,
      questions: List[CommentaryInquiry]): CommentaryJobState =
    if investigationDepth(before) > MaximumAnalysisDepth then
      val verification = CommentaryVerification(investigationDepth(before), state = "stopped",
        reason = "explanation_depth_limit", stopReason = Some("depth_limit"))
      return finishInvestigation(before.copy(verification = Some(verification)),
        baseline, questions, before.responseInquiries, needsRefresh = true)
    val reference = reviewReference(baseline).getOrElse(before.playedMoveUci)
    // In a short automatic job the reviewed move's reply/function account is
    // the first question, not a late beneficiary of all generated questions.
    // Explicit submitted questions keep the existing dependency agenda.
    if (before.policy.shortJob || before.remainingBudgetMs <= 10000) &&
        (before.policy.investigateChoices || before.responseInquiries.nonEmpty) &&
        !before.proposalEntries.exists(_.admission == "submitted") then
      firstPurposeResponse(before, baseline, reference) match
        case Some(result) => return result
        case None => ()
    val retained = prepareProposalInputs(before) match
      case Left((retained, failure)) => return stop(retained, failure, Some(baseline))
      case Right(retained) => retained
    val initial = readPrincipalVariations(retained, reference)
    val forcedReady = refreshProposalReadings(initial.copy(continuationInquiries =
      continuationQuestions(initial, reference)))
    // An already required unique reply precedes speculative candidate replay in the same shared budget.
    val owned = observeForcedContinuations(forcedReady, reference, baseline, questions, includeGenerated = false) match
      case Left(result) => return result
      case Right(value) => value
    val required = registerComparisonConflicts(owned, reference)
    val expanded = (if pendingChoiceTask(required, reference) then Right(required)
      else discoverQuestions(required, reference)) match
      case Left((retained, failure)) => return stop(retained, failure, Some(baseline))
      case Right(retained) => retained
    val discoveredActive = refreshProposalReadings(
      if expanded.packet eq required.packet then expanded else readPrincipalVariations(expanded, reference))
    val realizedActive = prepareFunctionalRealizations(discoveredActive, reference) match
      case Left((retained, failure)) => return stop(retained, failure, Some(baseline))
      case Right(retained) => retained
    val active = registerComparisonConflicts(realizedActive, reference)
    val discovered = if active.policy.investigateChoices then
      TargetResponseInvestigation.discover(active.packet, reference, active.preparedPaths.get, active.responseSearches)
      else Nil
    // Current evidence reselects replies; old questions/readings remain as conditional observations.
    val candidates = (discovered ++ active.responseInquiries.map(_.question)).distinctBy(_.id)
    val histories = candidates.map(_.rootMove).distinct.foldLeft(active.responseHistories) { (known, move) =>
      if known.contains(move) then known
      else known.updated(move, active.positionHistory.extend(List(move)).toOption.filter(child =>
        PositionRuleAssessment.assess(child).isInstanceOf[PositionRuleAssessment.Nonterminal]))
    }
    val prepared = refreshProposalReadings(active.copy(responseHistories = histories, continuationInquiries =
      continuationQuestions(active, reference)))
    val read = observeForcedContinuations(prepared, reference, baseline, questions) match
      case Left(result) => return result
      case Right(retained) => retained
    val all = candidates.filter(question => histories.get(question.rootMove).flatten.nonEmpty)
    val responses = readResponseInquiries(read, all)
    val pending = questions.map { prior =>
      val inquiry = reconsiderInquiry(read, prior)
      if inquiry.state == "deferred" then inquiry.copy(stopReason = inquiry.stopReason.orElse(Some("investigation_limit"))) else inquiry
    }
    revalidateChoice(read, baseline, pending, responses, reference) match
      case Some(issued) => return issued
      case None => ()
    // A contradictory answer is a verification obligation, not a third speculative topic.
    // Spend the existing single child recheck on its concrete divergence, then re-evaluate
    // the root. The child comparison cannot replace either conflicting parent score.
    val currentPairs = CommentaryIdea.currentQuestions(read.proposalEntries).flatMap(_.request.requirements.collect {
      case value: CommentaryProposal.CompareMoves => value
    })
    val conflict = read.comparisonConflicts.find(value =>
      currentPairs.exists(pair => pair.prefixMovesUci == value.prefix &&
        Set(pair.referenceMove, pair.alternativeMove)(value.move)) &&
      baseline.result.isInstanceOf[CompletedPositionCommentary.FocusedMoveReview] &&
      read.packet.assembly.input.assessmentCandidates.exists(_.evaluation.moves.startsWith(value.prefix :+ value.move)))
      .orElse(CommentaryComparisonConflict.unansweredChallenge(read.packet, read.responseSearches, read.proposalEntries))
    conflict.flatMap(value => read.verification.filter(_.state == "complete").map(value -> _)) match
      case Some((value, previous)) =>
        val needed = value.depth + value.prefix.size
        val reason = if needed > MaximumAnalysisDepth then Some("depth_limit")
          else Option.when(read.childRechecksIssued >= 1)("verification_limit")
        val verification = previous.copy(targetDepth = math.max(previous.targetDepth, math.min(100, needed)),
          state = if reason.nonEmpty then "stopped" else "pending", reason = "child_challenge",
          triggerWorkIds = (previous.triggerWorkIds ++ value.workIds).distinct, stopReason = reason)
        val retained = read.copy(verification = Some(verification))
        if reason.nonEmpty then
          val halted = refreshProposalReadings(retained)
          return finishInvestigation(halted.copy(proposalEntries = CommentaryProposal.stop(halted.proposalEntries, reason.get)),
            baseline, pending, responses, needsRefresh = true)
        val checking = retained.copy(childRechecksIssued = retained.childRechecksIssued + 1)
        val probe = value.comparison.filter(pair => checking.proposalEntries.exists(_.request.requirements.contains(pair)))
          .flatMap(pair => issueProposalComparison(checking, baseline, pending, responses, pair, verifyAfter = true))
        return probe.getOrElse(issueVerification(refreshProposalReadings(checking), baseline, verification))
      case None => ()
    val current = responses.filter(inquiry => Set(read.playedMoveUci, reference)(inquiry.question.rootMove))
      // The requested move first is a product-scope tie break; target enumeration cannot allocate extra searches.
      .sortBy(inquiry => (if inquiry.question.rootMove == read.playedMoveUci then 0 else 1,
        inquiry.question.rootMove, inquiry.question.id))
    def searchesFor(question: TargetResponseInvestigation.Question) =
      read.responseSearches.filter(search => search.belongsTo(read.packet) &&
        search.anchor.rootMove == question.rootMove && search.anchor.prefixMoves == List(question.rootMove) &&
        search.anchor.rootOccurrenceId == question.rootMoveOccurrenceId && search.anchor.positionId == question.childPositionId)
    def covers(search: TargetResponseInvestigation.Search, moves: List[String]): Boolean =
      search.work.rootRestriction match
        case EngineRootRestriction.Unrestricted => true
        case EngineRootRestriction.Restricted(restricted) => moves.forall(restricted.contains)
    val surveys = current.distinctBy(_.question.childPositionId).filter { inquiry =>
      val legal = inquiry.question.captureMoves ++ inquiry.question.declineMoves
      !searchesFor(inquiry.question).exists(search => covers(search, legal))
    }.map(inquiry => inquiry -> EngineRootRestriction.Unrestricted)
    def missingClassesFor(values: List[CommentaryResponseInquiry]) =
      values.distinctBy(_.question.childPositionId).flatMap { atChild =>
        val missing = values.filter(_.question.childPositionId == atChild.question.childPositionId).flatMap { inquiry =>
          List(inquiry.question.captureMoves, inquiry.question.declineMoves).filter { moves =>
            // A returned top-N omission is not a proof that a legal class is weak or impossible.
            !searchesFor(inquiry.question).exists(search => covers(search, moves) &&
              search.lines.exists(line => moves.contains(line.evaluation.moves.head)))
          }.map(moves => inquiry -> moves)
        }
        missing.headOption.map { (inquiry, _) =>
          // Complete the missing class against the strongest returned whole-pool reply
          // in the same new work, rather than borrowing a score from another receipt.
          val leader = searchesFor(inquiry.question).reverseIterator
            .find(_.work.rootRestriction == EngineRootRestriction.Unrestricted)
            .flatMap(_.lines.headOption).map(_.evaluation.moves.head).toList
          inquiry -> EngineRootRestriction.Restricted((missing.flatMap(_._2) ++ leader).distinct.sorted)
        }
      }
    val missingClasses = missingClassesFor(current)
    val reviewed = current.filter(inquiry =>
      ResponsePurposeReading.required(read.packet, read.preparedPaths.get, inquiry.question))
    val reviewedWork = surveys.find((inquiry, _) => reviewed.exists(_.question.id == inquiry.question.id))
      .orElse(reviewed.headOption.flatMap(inquiry => surveys.find(_._1.question.childPositionId == inquiry.question.childPositionId)
        .map((_, restriction) => inquiry -> restriction)))
      .orElse(missingClassesFor(reviewed).headOption)
    if read.explorationsIssued >= 2 then
      // Finish the already discovered judgment of the actual mover before a
      // useful late option or an unrelated exchange can end the investigation.
      if read.explorationsIssued < 4 then
        reviewedWork.flatMap { (inquiry, restriction) =>
          val question = inquiry.question
          issueResponseComparison(read, baseline, pending, responses,
            EngineSearchAnchor(question.rootMove, question.rootMoveOccurrenceId, question.childPositionId),
            restriction, question.id, requiredTarget = true)
        } match
          case Some(issued) => return issued
          case None => ()
      val functional = read.currentChoices(reference).obligations
        .map((value, _) => CommentaryIdea.pair(value)).toSet
      // Finish the finite resource already owned by this root question. A
      // quiet root grade or a completed, different reserve is not its answer.
      proposalChoices(read, reference).find(choice => choice.rootDependency &&
        functional(CommentaryIdea.pair(choice.value)))
        .flatMap(choice => issueProposalChoice(read, baseline, pending, responses, choice)) match
          case Some(issued) => return issued
          case None => ()
      val completedReserve = CommentaryIdea.reserveEntries(read.proposalEntries).filter(entry =>
        entry.owner.comparison.referenceMove == read.playedMoveUci && entry.owner.comparison.alternativeMove == reference &&
          read.preparedPaths.exists(paths => RootSubjectComparison.strongestReserveCounter(read.packet, paths, entry, read.responseSearches)))
      if read.preparedPaths.exists(paths => RootSubjectComparison.reserveEntryReady(read.packet, paths, completedReserve)) then
        return finishInvestigation(read, baseline, pending, responses)
      // Two comparisons may establish that the option is useful only later.
      // Finish that existing, receipt-linked question within the original
      // deadline; this allowance cannot start another unrelated topic.
      val linked = currentFirstReplyDependencies(read, reference) ++
        CommentaryIdea.reserveHolds(read.proposalEntries).filter(hold =>
        (hold.source.exists(source => source.comparison.prefixMovesUci.size < hold.test.prefixMovesUci.size) ||
          read.responseSearches.exists(search => search.anchor.prefixMoves.size < hold.test.prefixMovesUci.size &&
            search.lines.headOption.exists(line =>
              search.pathsByMove(line.evaluation.moves.head).moves.startsWith(hold.branch)))) &&
          read.preparedPaths.exists(RootSubjectComparison.currentReserveHold(_, hold)))
        .map(hold => CommentaryIdea.pair(hold.test)).toSet ++
        CommentaryIdea.reserveEntries(read.proposalEntries).filter(entry =>
          entry.source.nonEmpty && read.preparedPaths.exists(RootSubjectComparison.currentReserveEntry(_, entry)))
          .flatMap(entry => entry.test :: entry.optionTest.toList).map(CommentaryIdea.pair) ++
        CommentaryIdea.directReserveEntries(read.proposalEntries).filter(entry =>
          read.preparedPaths.exists(RootSubjectComparison.currentDirectReserveEntry(_, entry)))
          .map(entry => CommentaryIdea.pair(entry.test))
      // Finishing an already selected option's value is a dependency of this
      // answer, not a fifth unrelated topic. Its exact pair is attempted once;
      // issueProposalComparison still enforces the total deadline and reserve.
      val continuation = proposalChoices(read, reference)
        .find(choice => choice.rootDependency && linked(CommentaryIdea.pair(choice.value)) &&
          (read.explorationsIssued < 4 || choice.value.id == "reserve_entry_value"))
        .flatMap(choice => issueProposalChoice(read, baseline, pending, responses, choice))
      return continuation.getOrElse(finishInvestigation(read, baseline, pending, responses))

    val prefixRelation = investigationPrefixRelation(read, reference)
    val moveRanks = scala.collection.mutable.Map.empty[List[String], Int]
    def moveRank(prefix: List[String]): Int = moveRanks.getOrElseUpdate(prefix,
      if prefix.isEmpty then 0 else CommentaryProposal.observedPrefix(read.packet, read.preparedPaths.get, prefix)
        .map(path => prefixRelation(path.ids.take(prefix.size))).getOrElse(2))
    val currentContinuations = read.continuationInquiries.filter(inquiry =>
      Set(read.playedMoveUci, reference)(inquiry.question.prefixMoves.head))
      .sortBy(inquiry => (if inquiry.question.prefixMoves.head == read.playedMoveUci then 0 else 1,
        inquiry.question.prefixMoves.mkString(" "), inquiry.question.id))
    val factContinuations = List(false, true).flatMap(generated => pendingFactContinuations(read, generated)
      .filterNot(value => read.responseReceipts.exists(receipt =>
        receipt.work.rootRestriction == EngineRootRestriction.Unrestricted &&
          receipt.work.searchAnchor.exists(_.prefixMoves == value.need.prefixMoves)))
      .map(generated -> _))
    def factId(generated: Boolean, value: FactContinuation) = s"fact:$generated:${value.need.prefixMoves.mkString(" ")}"
    val contexts = CommentaryIdea.currentQuestions(read.proposalEntries).filter(_.generatedArms.nonEmpty).flatMap { entry =>
      entry.request.requirements.collect {
        case value: CommentaryProposal.CompareMoves if entry.readings.exists(reading =>
            reading.requirementId == value.id && !CommentaryProposal.isResolved(reading) &&
              !Set("invalid_input", "stopped")(reading.state)) =>
          (CommentaryIdea.pair(value), CommentaryProposal.comparisonContext(entry, value).map(entry -> _),
            CommentaryProposal.comparisonContexts(entry, value, readyOnly = true).map(_.flatMap(fact =>
              entry.readings.find(_.requirementId == fact.id).flatMap(_.result).map(fact -> _))))
      }
    }.groupBy(_._1).view.mapValues(values => (values.flatMap(_._2), values.flatMap(_._3))).toMap
    val pendingOutcomes = currentContinuations.filter(inquiry => inquiry.state == "deferred" &&
      prefixRelation(inquiry.question.prefixOccurrenceIds) == 0)
    val outcomeTests = LineContinuationInvestigation.conditionTests(read.packet, read.preparedPaths.get,
      if contexts.values.exists(_._2.nonEmpty) then pendingOutcomes else Nil, read.responseSearches)
    // These links refer to existing owners; they are not additional proof or work.
    // In particular, the donor's witnessed recovery does not answer its short peer.
    val conditionLinks = contexts.map { case (pair, (conditions, readyConditions)) =>
      val owners = conditions.map((entry, fact) => entry.request.id -> fact.id).toSet
      val facts = factContinuations.collect {
        case (generated, value) if moveRank(value.need.prefixMoves) == 0 && value.owners.exists(owners) => factId(generated, value)
      }
      val outcomes = outcomeTests.collect {
        case (id, test) if readyConditions.exists(test) => id
      }
      // An unmet context calls for the fact first. Only a ready arm's witnessed
      // condition can make its comparison precede a downstream route inquiry.
      pair -> (facts.toSet, outcomes.toSet)
    }
    val linkedOutcomes = conditionLinks.filter { case ((prefix, moves), _) =>
      moves.exists(move => moveRank(prefix :+ move) == 0)
    }.values.flatMap((facts, outcomes) => facts ++ outcomes).toSet
    val currentOutcomeIds = pendingOutcomes.map(_.question.id).toSet ++ factContinuations.collect {
      case (generated, value) if moveRank(value.need.prefixMoves) == 0 => factId(generated, value)
    }
    def outcomesAt(prefix: List[String]): Set[String] =
      currentContinuations.filter(_.question.prefixMoves == prefix).map(_.question.id).toSet ++
        factContinuations.collect { case (generated, value) if value.need.prefixMoves == prefix => factId(generated, value) }
    // These are ephemeral choices, not issued work or another evidence store. Only
    // the winner's issue function builds histories, engine work and new readings.
    case class Choice(prefix: List[String], priority: (Int, Int, Int, Int), outcomes: Set[String],
        governing: Option[(CommentaryProposal.CompareMoves, Set[String])], issue: () => Option[CommentaryJobState])
    val comparisons = proposalChoices(read, reference).zipWithIndex.map { (choice, order) =>
      val value = choice.value
      // The actual next move must belong to the pair: sharing only its decision
      // position does not put two off-PV alternatives on the current path.
      val rank = if choice.rootDependency then 0 else List(value.referenceMove, value.alternativeMove)
        .map(move => moveRank(value.prefixMovesUci :+ move)).min
      val branchOutcomes = if !choice.governsBranch || value.prefixMovesUci.isEmpty then Set.empty[String]
        else (currentContinuations.map(inquiry => inquiry.question.prefixMoves -> inquiry.question.id) ++
          factContinuations.map((generated, fact) => fact.need.prefixMoves -> factId(generated, fact)))
          .collect { case (prefix, id) if prefix.startsWith(value.prefixMovesUci) &&
            prefix.lift(value.prefixMovesUci.size).exists(Set(value.referenceMove, value.alternativeMove)) => id }.toSet
      val links = conditionLinks.get(CommentaryIdea.pair(value)).fold(Set.empty[String])(_._2) ++ branchOutcomes
      val urgency = if choice.decision._1 < 0 || choice.decision._2 < 0 then -2
        // Its changed root condition is already verified. Settle the useful
        // first response before refining a more distant exchange history.
        // Current stale answers and direct tactical challenges sort first.
        // Its premises are ready. A linked downstream fact's tie break must
        // not consume the remaining slot before this selected comparison.
        else if choice.rootDependency then -1
        else if choice.governsBranch && rank == 0 then 1
        else if choice.essential && choice.decision._2 == 0 then
          if links.nonEmpty || currentOutcomeIds.isEmpty then 0 else 3
        else if choice.essential then 1 else 3
      Choice(value.prefixMovesUci, (if choice.submitted then 0 else 1, rank, urgency, order), links,
        Option.when(choice.governsBranch && rank == 0 && links.nonEmpty)(value -> links),
        () => issueProposalChoice(read, baseline, pending, responses, choice))
    }
    val routesMissing = currentContinuations.filter(inquiry => inquiry.state == "surveyed" &&
      inquiry.routeChallenge.exists(_.state == "not_returned"))
    def missingMoves(inquiry: CommentaryContinuationInquiry, exposure: Boolean): List[String] =
      inquiry.routeChallenge.toList.flatMap(_.moveClasses).filter(group => group.sampledWorkIds.isEmpty &&
        ((group.kind == "exposed_capture") == exposure)).flatMap(_.movesUci).distinct.sorted
    def routeWork(inquiry: CommentaryContinuationInquiry, moves: List[String]) =
      val route = inquiry.routeChallenge.get
      (inquiry.question.anchor, EngineRootRestriction.Restricted((moves :+ route.referenceMove).distinct.sorted),
        inquiry.question.id)
    val continuations = currentContinuations.filter(_.state == "deferred")
    val routeCounterexamples = continuations.filter { inquiry =>
      val question = inquiry.question
      LineContinuationInvestigation.refutingMoves(read.packet, question.prefixMoves, question.sourceLineIds,
        question.triggerOccurrenceIds, read.preparedPaths.get).nonEmpty
    }
    val counterexampleIds = routeCounterexamples.map(_.question.id).toSet
    val observedRecoveryChoices = continuations.filter { inquiry =>
      val question = inquiry.question
      question.kinds.exists(kind =>
        Set("delayed_capturer_removal", "other_target_gain", "replacement_function_choice")(kind)) &&
        (counterexampleIds(question.id) ||
          !repeatsImmediateRecovery(question, read.preparedPaths.get.continuationReadings))
    }
    val missingObservations = continuations.filter(_.question.comparisonSources.nonEmpty)
    val checkResponses = continuations.filter(inquiry =>
      inquiry.question.kinds.contains("check_response_not_observed") && inquiry.question.observedMoves.isEmpty)
    val materialChoices = continuations.filter(_.question.kinds.exists(kind =>
      Set("material_followup", "capturer_reply")(kind)))
    val exposures = continuations.filter(_.question.kinds.exists(kind =>
      Set("capture_exposure_response", "exposure_prevention_choice")(kind)))
    val fullSearches = read.responseSearches.reverse.filter(_.work.rootRestriction == EngineRootRestriction.Unrestricted)
      .distinctBy(search => (search.anchor.positionId, search.anchor.prefixMoves))
    def returnedOrder(search: TargetResponseInvestigation.Search, inquiry: CommentaryContinuationInquiry): Option[Int] =
      val question = inquiry.question
      for
        move <- question.prefixMoves.lift(search.anchor.plyOffset)
        path <- search.pathsByMove.get(move)
        if question.prefixMoves.startsWith(search.anchor.prefixMoves) &&
          (path.ids.startsWith(question.prefixOccurrenceIds) || question.prefixOccurrenceIds.startsWith(path.ids))
        order = search.lines.indexWhere(_.evaluation.moves.head == move)
        if order >= 0
      yield order
    def firstInBand(values: List[CommentaryContinuationInquiry], rank: Int): Option[CommentaryContinuationInquiry] =
      val current = values.filter(inquiry => prefixRelation(inquiry.question.prefixOccurrenceIds) == rank)
      val linked = current.filter(inquiry => linkedOutcomes(inquiry.question.id))
      val band = if linked.nonEmpty then linked else current
      band.headOption.map { first =>
        // Only break an existing within-band tie with one current full search.
        // Restricted samples, divergent supplied suffixes and other anchors do
        // not gain priority; Search already owns the child-mover ordering.
        if rank == 2 then first
        else fullSearches.find(search => returnedOrder(search, first).nonEmpty).flatMap { search =>
          band.flatMap(inquiry => returnedOrder(search, inquiry).map(order => inquiry -> order))
            .minByOption(_._2).map(_._1)
        }.getOrElse(first)
      }
    def continuationWork(inquiry: CommentaryContinuationInquiry) =
      (inquiry.question.anchor, EngineRootRestriction.Unrestricted, inquiry.question.id)
    def issueResponse(anchor: EngineSearchAnchor, restriction: EngineRootRestriction,
        selectedId: String): Option[CommentaryJobState] =
      issueResponseComparison(read, baseline, pending, responses, anchor, restriction, selectedId,
        requiredTarget = reviewed.exists(_.question.id == selectedId))
    // Known challenges, missing concrete outcomes, then new legal-resource surveys.
    // The same tiers and current-path relation also apply to proposal comparisons.
    val responseChoices = (0 to 2).toList.flatMap { rank =>
      def atRank(inquiry: CommentaryContinuationInquiry): Boolean =
        prefixRelation(inquiry.question.prefixOccurrenceIds) == rank
      val strongRouteProbe = routesMissing.find(inquiry => atRank(inquiry) &&
        missingMoves(inquiry, exposure = false).nonEmpty).map(inquiry =>
        routeWork(inquiry, missingMoves(inquiry, exposure = false)))
      val exposureRouteProbe = routesMissing.find(inquiry => atRank(inquiry) &&
        missingMoves(inquiry, exposure = true).nonEmpty && missingMoves(inquiry, exposure = false).isEmpty)
        .map(inquiry => routeWork(inquiry, missingMoves(inquiry, exposure = true)))
      val counterexample = firstInBand(routeCounterexamples, rank).map(continuationWork)
      val concrete = firstInBand(observedRecoveryChoices, rank)
        .orElse(firstInBand(missingObservations, rank)).map(continuationWork)
      def targetWork(next: CommentaryResponseInquiry, restriction: EngineRootRestriction) =
        (EngineSearchAnchor(next.question.rootMove, next.question.rootMoveOccurrenceId, next.question.childPositionId),
          restriction, next.question.id)
      val firstTarget = surveys.find((inquiry, _) => prefixRelation(List(inquiry.question.rootMoveOccurrenceId)) == rank)
        .map((inquiry, restriction) => targetWork(inquiry, restriction))
      val missingTarget = missingClasses.find((inquiry, _) => prefixRelation(List(inquiry.question.rootMoveOccurrenceId)) == rank)
        .map((inquiry, restriction) => targetWork(inquiry, restriction))
      val requiredTarget = reviewedWork.filter((inquiry, _) =>
        prefixRelation(List(inquiry.question.rootMoveOccurrenceId)) == rank)
        .map((inquiry, restriction) => targetWork(inquiry, restriction))
      List(
        // The reviewed move's first response governs every later use in that
        // branch. Resolve its pool/pair before extending a remote PV episode.
        0 -> requiredTarget.orElse(strongRouteProbe).orElse(counterexample),
        1 -> firstInBand(checkResponses, rank).map(continuationWork).orElse(concrete)
          .orElse(firstInBand(materialChoices, rank).map(continuationWork)),
        // Establish this root's opponent pool before extending an independent
        // exposure suffix in the same band. A returned PV is not that survey.
        2 -> firstTarget.orElse(firstInBand(exposures, rank).map(continuationWork)).orElse(exposureRouteProbe)
          .orElse(missingTarget).orElse(firstInBand(continuations, rank).map(continuationWork))
      ).flatMap((urgency, work) => work.map(urgency -> _)).distinctBy(_._2).map { case (urgency, (anchor, restriction, id)) =>
        Choice(anchor.prefixMoves, (1, rank, urgency, comparisons.size), outcomesAt(anchor.prefixMoves), None,
          () => issueResponse(anchor, restriction, id))
      }
    }
    val retainedOptionUses = CommentaryIdea.reservedOptions(read.proposalEntries).collect {
      case (entry, owner) if owner.comparison.referenceMove == read.playedMoveUci &&
          owner.comparison.alternativeMove == reference => entry.request.id -> "reserve_use"
    }.toSet
    val facts = factContinuations.zipWithIndex.map { case ((generated, value), order) =>
      val need = value.need
      val tie = if linkedOutcomes(factId(generated, value)) then -1 else comparisons.size + order
      val retainedOption = value.owners.exists(retainedOptionUses)
      Choice(need.prefixMoves, (if generated then 1 else 0, moveRank(need.prefixMoves), if retainedOption then 0 else 1, tie),
        outcomesAt(need.prefixMoves), None, () => issueFactContinuation(read, baseline, pending, responses, need))
    }

    // A restricted survey can cover a question without returning its two PVs.
    // Keep that question unanswered, but do not repeat the same current scope.
    val surveyedPools = explanationReceipts(read).filter(receipt => receipt.work.searchAnchor.isEmpty &&
      CommentaryInvestigation.ownsSearch(read.positionHistory, receipt.work, receipt.lines) &&
      CommentaryInvestigation.candidatesReady(receipt.work, receipt.lines)).flatMap { receipt =>
        receipt.work.rootRestriction match
          case EngineRootRestriction.Restricted(moves) => Some(moves.toSet)
          case EngineRootRestriction.Unrestricted => None
      }
    val unanswered = pending.filter(inquiry => inquiry.state == "deferred" && inquiry.answer.isEmpty &&
      inquiry.question.referenceMove == reference)
    val linked = unanswered.filter(_.question.orderingCandidateIds.nonEmpty)
    val eligible = if read.policy.previewDepth.nonEmpty then
      unanswered.filter(inquiry => CommentaryInvestigation.investigationTier(inquiry.question) < 2)
        .sortBy(inquiry => (CommentaryInvestigation.investigationTier(inquiry.question), inquiry.question.id))
    else if linked.nonEmpty then linked else unanswered.filter(inquiry =>
      inquiry.question.reasons.exists(_.kind != CommentaryInvestigation.Kind.EngineAlternative))
    val family = eligible.filterNot(inquiry => surveyedPools.exists(moves =>
      moves(inquiry.question.referenceMove) && moves(inquiry.question.alternativeMove)))
    val familyChoice = family.headOption.map { next =>
      val urgency = if family.exists(inquiry => CommentaryInvestigation.investigationTier(inquiry.question) < 2) then 2 else 3
      // Existing target/PV evidence gives this family a place in the shared agenda;
      // explicit questions and equally urgent current-PV responses still precede it.
      Choice(Nil, (1, 0, urgency, comparisons.size + 1), Set.empty, None, () =>
        val ids = family.map(_.question.id).toSet
        val scheduled = pending.map(inquiry => if ids(inquiry.question.id) then
          inquiry.copy(state = "awaiting_engine", stopReason = None) else inquiry)
        Some(issueInvestigation(read, Investigating(baseline, scheduled, next.question, responses = responses),
          reference :: family.map(_.question.alternativeMove).distinct.sorted)))
    }
    if read.remainingBudgetMs < 2_500 then
      return finishInvestigation(read, baseline, pending, responses)
    var remaining = (comparisons ++ responseChoices ++ facts ++ familyChoice).sortBy(_.priority)
    def prerequisite(choice: Choice): Choice =
      // Acceptance or a concrete reply challenge can invalidate this continuation.
      // A root value/immediate-action comparison alone does not erase the need to
      // explain the requested move, even when another root move is preferable.
      remaining.find(candidate => candidate.priority._1 <= choice.priority._1 && candidate.governing.exists { (value, links) =>
        links.exists(choice.outcomes) && value.prefixMovesUci.size < choice.prefix.size &&
          choice.prefix.startsWith(value.prefixMovesUci) &&
          choice.prefix.lift(value.prefixMovesUci.size).exists(Set(value.referenceMove, value.alternativeMove))
      })
        .fold(choice)(prerequisite)
    while remaining.nonEmpty do
      val selected = prerequisite(remaining.head)
      selected.issue() match
        case Some(issued) => return issued
        case None => remaining = remaining.filterNot(_ == selected)

    finishInvestigation(read, baseline, pending, responses)

  private def firstPurposeResponse(before: ActiveCommentaryJob, baseline: CompletedCommentaryJobRecord,
      reference: String): Option[CommentaryJobState] =
    val initial = readPrincipalVariations(before, reference)
    val exampleNeeds = initial.preparedPaths.toList.flatMap(paths =>
      MoveIntent.discover(initial.packet, paths, initial.responseSearches).toList.flatMap(_.examples)
      .filter(moves => paths.certifiedPath(initial.packet, moves).isEmpty))
    val active = if exampleNeeds.isEmpty then initial else
      val replay = new LocalReplayBatch(initial)
      // A plan witness is an exact finite path. Its occurrence as the prefix of
      // a longer PV does not provide that path's independent material boundary.
      exampleNeeds.foreach(moves => replay.ensurePath(moves, discoveryOnly = true, exact = true))
      replay.retain(initial.proposalEntries) match
        case Right(retained) => readPrincipalVariations(retained, reference)
        case Left((retained, failure)) => return Some(stop(retained, failure, Some(baseline)))
    val paths = active.preparedPaths.get
    val assessmentPair = ReferencePurposeReading.assessmentPair(active.packet, paths)
    val functionalReading = MovePurpose.read(active.packet, paths, active.responseSearches,
      MoveWorkProjection.select(active.packet, paths).local, explanationReceipts(active))
    val current = TargetResponseInvestigation.discover(active.packet, reference, paths, active.responseSearches)
      .filter(ResponsePurposeReading.required(active.packet, paths, _))
    val targetResponses = readResponseInquiries(active, current)
    val pendingTargetContrast = current.size == 1 &&
      TargetResponseInvestigation.firstTargets(active.packet, paths, targetResponses, active.responseSearches).isEmpty &&
      active.responseSearches.count(s => s.belongsTo(active.packet) &&
        s.anchor.prefixMoves == List(current.head.rootMove) &&
        s.anchor.rootOccurrenceId == current.head.rootMoveOccurrenceId) < 2
    val assessedAnswer = MoveWorkProjection.purposeAnswer(active.packet, paths, assessmentPair, functionalReading,
      FunctionalReplyReading.read(active.proposalEntries),
      TargetResponseInvestigation.firstTargets(active.packet, paths, targetResponses, active.responseSearches))
    val assessedPurpose = assessedAnswer.filter(MoveWorkProjection.completePurpose)
      .flatMap(RootQuestionSummary.realize(paths, assessmentPair, _))
    val actualRoots = assessmentPair.map(_.moves.head).toSet
    val focusedPair = explanationReceipts(active).exists(receipt =>
      receipt.work.purpose != EngineWorkPurpose.RootSearch && receipt.work.searchAnchor.isEmpty &&
        (receipt.work.rootRestriction match
          case EngineRootRestriction.Restricted(moves) => actualRoots.subsetOf(moves.toSet)
          case _ => false))
    // A broad root survey can stop before one actual choice has a useful role.
    // Finish an already owned reply question first, then spend at most one
    // bounded comparison on these same roots before retaining a partial account.
    def remainingPairWork: Option[CommentaryJobState] =
      if assessmentPair.size != 2 || assessedAnswer.isEmpty || assessedPurpose.nonEmpty || focusedPair ||
          pendingTargetContrast || active.responseSearches.isEmpty || active.remainingBudgetMs < 4000 then None
      else
        val question = explanationReceipts(active).iterator.filter(receipt =>
          receipt.work.searchAnchor.isEmpty && actualRoots.subsetOf(receipt.lines.map(_.evaluation.moves.head).toSet))
          .flatMap(receipt => CommentaryInvestigation.discover(active.packet, active.playedMoveUci,
            Some(receipt), prepared = Some(paths))).find(q =>
            Set(q.referenceMove, q.alternativeMove) == actualRoots)
        question.map { q =>
          val inquiry = CommentaryInquiry(q, state = "awaiting_engine")
          issueInvestigation(active, Investigating(baseline,
            inquiry :: active.inquiries.filterNot(_.question.id == q.id), q, targetResponses), actualRoots.toList.sorted)
        }
    def finishPurpose: CommentaryJobState =
      remainingPairWork.getOrElse(finishInvestigation(active, baseline, active.inquiries, targetResponses))
    // Discovery, work allocation and delivery use the same prepared idea.
    // Returning two arbitrary reply classes is not a certificate of its purpose.
    MoveIntent.read(active.packet, paths, active.responseSearches) match
      case Some(intent) =>
        val remaining = MoveIntent.probes(active.packet, paths, intent.question, active.responseSearches)
        if intent.complete && MoveIntent.realize(active.packet, paths, intent).nonEmpty then
          return Some(finishPurpose)
        val issued = remaining.headOption.filter(_ => active.remainingBudgetMs >= 2500).flatMap { (anchor, moves) =>
          issueResponseComparison(active, baseline, active.inquiries, targetResponses, anchor,
            EngineRootRestriction.Restricted(moves.sorted), intent.question.id)
        }
        if issued.nonEmpty then return issued
        return Some(finishPurpose)
      case None => ()
    val functional = functionalReading.map(_.question)
      .orElse(MovePurpose.question(active.packet, paths, active.responseSearches)).map { q =>
      val searches = MovePurpose.searches(active.packet, q, active.responseSearches)
      val answered = functionalReading.exists(r => r.explanation.answered && r.replyContrast)
      // A returned challenge class is not an explanation. A witnessed task can
      // still need one bounded comparison with the strongest independent reply.
      val returned = searches.flatMap(_.lines.map(_.evaluation.moves.head))
      val scopeOpen = q.use >= 0 || !searches.exists(_.coversAllLegalMoves) ||
        !returned.exists(q.challenges.contains) || !returned.exists(move => !q.challenges.contains(move))
      (q, searches, !answered && scopeOpen)
    }
    val pendingReplyContrast = active.remainingBudgetMs >= 2500 &&
      (functional.exists((_, searches, needsWork) => needsWork && searches.size < 2) || pendingTargetContrast)
    // A complete account of the actual assessment pair takes precedence over
    // an extra same-destination question. Its returned replies already belong
    // to the two assessed moves; no third root is needed to explain them.
    // A shorter comparison does not answer whether the opponent accepts or
    // avoids the offered exchange. Keep that already-discovered question.
    if assessedPurpose.nonEmpty && !pendingReplyContrast then
      return Some(finishInvestigation(active, baseline, Nil, targetResponses))
    val purposeChoice = PurposeChoice.inquiry(active.packet, paths).filter(question =>
      Set(question.referenceMove, question.alternativeMove) == assessmentPair.map(_.moves.head).toSet)
    if purposeChoice.nonEmpty then
      val question = purposeChoice.get
      val roots = Set(question.referenceMove, question.alternativeMove)
      val surveyed = active.rootSearches.exists(search => search.work.purpose == EngineWorkPurpose.InvestigationComparison &&
        (search.work.rootRestriction match
          case EngineRootRestriction.Restricted(moves) => roots.subsetOf(moves.toSet)
          case _ => false))
      val ready = explanationReceipts(active).exists(receipt => CommentaryInvestigation.answer(
        question, receipt.work, receipt.lines, active.packet, Some(paths)).nonEmpty)
      if !ready && !surveyed && active.remainingBudgetMs >= 4000 then
        val inquiry = CommentaryInquiry(question, state = "awaiting_engine")
        return Some(issueInvestigation(active, Investigating(baseline,
          inquiry :: active.inquiries.filterNot(_.question.id == question.id), question, targetResponses),
          (List(question.referenceMove, question.alternativeMove, reference) ++
            assessmentPair.flatMap(_.moves.headOption)).distinct))
    functional match
      case Some((q, searches, needsWork)) =>
        if !needsWork || searches.size >= 2 || active.remainingBudgetMs < 2500 then
          return Some(finishPurpose)
        val restriction = searches.reverse.find(_.work.rootRestriction == EngineRootRestriction.Unrestricted)
          .fold[EngineRootRestriction] {
            EngineRootRestriction.Unrestricted
          } { pool =>
            val present = pool.lines.map(_.evaluation.moves.head)
            val source = q.plan.map(_.path).getOrElse(q.path)
            val classesReturned = present.exists(q.challenges.contains) && present.exists(move => !q.challenges.contains(move))
            val witnessed = Option.when(q.use >= 0 && (assessmentPair.exists(_ eq source) || classesReturned))(
              source.moves(1))
            val contrast = for
              witness <- witnessed
              other <- present.find(_ != witness)
            yield List(witness, other).sorted
            val moves = contrast.getOrElse {
              val legal = paths.positionView(q.path, 1).flatMap(_.legalRows).get.map(_.move)
              val missing = if present.exists(q.challenges.contains) then legal.filterNot(q.challenges.contains) else q.challenges
              (missing ++ present.take(1)).distinct.sorted
            }
            EngineRootRestriction.Restricted(moves)
          }
        return issueResponseComparison(active, baseline, active.inquiries, targetResponses, q.anchor, restriction, q.id,
          requiredTarget = true).orElse(Some(finishPurpose))
      case None => ()
    if current.size != 1 then return remainingPairWork
    val responses = targetResponses
    val q = current.head
    val searches = active.responseSearches.filter(search => search.belongsTo(active.packet) &&
      search.anchor.prefixMoves == List(q.rootMove) && search.anchor.rootOccurrenceId == q.rootMoveOccurrenceId &&
      search.anchor.positionId == q.childPositionId)
    if TargetResponseInvestigation.firstTargets(active.packet, paths, responses, searches).nonEmpty then
      return Some(finishPurpose)
    val pool = searches.reverse.find(_.work.rootRestriction == EngineRootRestriction.Unrestricted)
    val restriction = pool.fold[EngineRootRestriction](EngineRootRestriction.Unrestricted) { full =>
      val missing = List(q.captureMoves, q.declineMoves).filterNot(moves => searches.exists(search =>
        (search.work.rootRestriction match
          case EngineRootRestriction.Unrestricted => true
          case EngineRootRestriction.Restricted(scope) => moves.forall(scope.contains)) &&
          search.lines.exists(line => moves.contains(line.evaluation.moves.head))))
      EngineRootRestriction.Restricted((missing.flatten ++
        List(q.referenceMove, q.alternativeMove, full.lines.head.evaluation.moves.head)).distinct.sorted)
    }
    // At most one pool and one class comparison. Missing proof remains missing;
    // it cannot start a new chain or extend the original deadline.
    if searches.size >= 2 || active.remainingBudgetMs < 2500 then
      Some(finishPurpose)
    else issueResponseComparison(active, baseline, Nil, responses,
      EngineSearchAnchor(q.rootMove, q.rootMoveOccurrenceId, q.childPositionId), restriction, q.id,
      requiredTarget = true).orElse(Some(finishPurpose))

  private def responseAnswer(active: ActiveCommentaryJob, question: TargetResponseInvestigation.Question,
      search: TargetResponseInvestigation.Search): Option[CommentaryResponseAnswer] =
    // Explicit one-ply proposal anchors and the default target anchor own the
    // same canonical child. Compare that scope, not its optional storage form.
    Option.when(search.anchor.rootMove == question.rootMove &&
      search.anchor.rootOccurrenceId == question.rootMoveOccurrenceId &&
      search.anchor.positionId == question.childPositionId &&
      search.anchor.prefixMoves == List(question.rootMove))(())
      .flatMap(_ => search.pairAnswer(active.packet, question.referenceMove, question.alternativeMove))
      .map(pair => CommentaryResponseAnswer(pair, question.childPositionId))

  private def afterResponseReport(admitted: ActiveCommentaryJob, phase: InvestigatingResponses,
      evaluations: List[AdmittedCandidateLine]): CommentaryJobState =
    val active = readPrincipalVariations(admitted, reviewReference(phase.baseline).getOrElse(admitted.playedMoveUci))
    if !active.responseSearches.exists(_.work == active.issuedWork) then
      stop(active, CommentaryJobStopCondition.InvalidEngineWorkReport)
    else
      // A valid survey need not return both classes. Reselect all questions sharing this child.
      afterConditionalReport(active, phase.baseline, phase.questions)

  private def readResponseInquiries(active: ActiveCommentaryJob, questions: List[TargetResponseInvestigation.Question])
      : List[CommentaryResponseInquiry] = questions.map { question =>
    val answer = active.responseSearches.reverseIterator.flatMap(search =>
      responseAnswer(active, question, search)).take(1).toList.headOption
    val prior = active.responseInquiries.find(_.question.id == question.id)
    val inquiry = prior.filter(value => value.question == question && value.answer == answer)
      .getOrElse(CommentaryResponseInquiry(question, if answer.nonEmpty then "answered" else "deferred", answer,
        retainedReadings = prior.toList.flatMap(value => value.retainedReadings ++ value.reading)))
    val bound = bindResponseReading(active, inquiry)
    if answer.nonEmpty then bound.copy(state = "answered", stopReason = None)
    else if bound.reading.exists(_.complete) then bound.copy(state = "observed", stopReason = None)
    else bound.copy(state = "deferred", stopReason = Some("investigation_limit"))
  }

  private def afterConditionalReport(active: ActiveCommentaryJob, baseline: CompletedCommentaryJobRecord,
      questions: List[CommentaryInquiry]): CommentaryJobState =
    val challenge = for
      verification <- active.verification.filter(_.state == "complete")
      anchor <- active.issuedWork.searchAnchor
      history <- active.searchHistory
      if history.currentPosition.color != active.positionHistory.currentPosition.color
      search <- active.responseSearches.find(_.work == active.issuedWork)
      if CommentaryInvestigation.candidatesReady(search.work, search.lines)
      review <- baseline.result match
        case CompletedPositionCommentary.FocusedMoveReview(_, review) => Some(review)
        case _ => None
      expected <- review.judgmentPacket.assembly.input.assessmentCandidates.map(_.evaluation)
        .find(line => line.moves.startsWith(anchor.prefixMoves))
      candidate <- search.lines.find { line =>
        val mover = active.positionHistory.currentPosition.color
        val oldMate = expected.engineLine.flatMap(_.mate).map(Integer.signum)
        val newMate = line.evaluation.engineLine.flatMap(_.mate).map(Integer.signum)
        val expectedReply = expected.moves.lift(anchor.plyOffset).flatMap(move =>
          search.lines.find(_.evaluation.moves.headOption.contains(move)))
        val strongerReply = expectedReply.exists(reference =>
          line.evaluation.rankingScoreForMover(mover) < reference.evaluation.rankingScoreForMover(mover))
        // Child scores only trigger a root check; they never replace the root assessment.
        // Reuse the verdict policy for drift in the same reply instead of treating every cp as a refutation.
        val materialShift = materialScoreChange(mover, expected, line.evaluation)
        // Shorter remaining mate distance is expected along a prefix, not a new contradiction.
        !(oldMate.nonEmpty && oldMate == newMate) &&
          line.evaluation.rankingScoreForMover(mover) < expected.rankingScoreForMover(mover) &&
          (strongerReply || materialShift || oldMate != newMate)
      }
    yield (verification, anchor, candidate)
    challenge match
      case None => beginResponses(active, baseline, questions)
      case Some((previous, anchor, candidate)) =>
        val reference = reviewReference(baseline).getOrElse(active.playedMoveUci)
        val observed = active.copy(continuationInquiries = continuationQuestions(active, reference))
        val discovered = if active.policy.investigateChoices then TargetResponseInvestigation.discover(active.packet,
          reference, active.preparedPaths.get, active.responseSearches) else Nil
        val responses = readResponseInquiries(observed,
          (discovered ++ active.responseInquiries.map(_.question)).distinctBy(_.id))
        val needed = candidate.evaluation.engineLine.map(_.depth).getOrElse(active.issuedWork.requiredDepth) + anchor.plyOffset
        val reason = if needed > MaximumAnalysisDepth then Some("depth_limit")
          else Option.when(active.childRechecksIssued >= 1)("verification_limit")
        val verification = previous.copy(targetDepth = math.max(previous.targetDepth, math.min(100, needed)),
          state = if reason.nonEmpty then "stopped" else "pending", reason = "child_challenge",
          triggerWorkIds = (previous.triggerWorkIds :+ active.issuedWork.workId).distinct, stopReason = reason)
        val retained = refreshProposalReadings(readPrincipalVariations(
          observed.copy(verification = Some(verification)), reference))
        val currentQuestions = questions.map(reconsiderInquiry(retained, _))
        if reason.nonEmpty then finishInvestigation(retained, baseline, currentQuestions, responses, needsRefresh = true)
        else issueVerification(retained.copy(childRechecksIssued = active.childRechecksIssued + 1),
          baseline.copy(inquiries = currentQuestions, responseInquiries = responses), verification)

  private def issuedWork(
      engineProfile: CommentaryEngineProfile,
      workId: String,
      purpose: EngineWorkPurpose,
      engineHistory: CanonicalPositionHistory,
      rootRestriction: EngineRootRestriction,
      commentaryLineOriginFen: String,
      commentaryLinePrefixUci: List[String],
      limits: EngineSearchLimits,
      maxSearchElapsedMs: Int,
      searchAnchor: Option[EngineSearchAnchor] = None
  ): IssuedEngineWork =
    require(limits.depth <= MaximumAnalysisDepth, "commentary work must remain within the depth18 horizon")
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
      maxSearchElapsedMs,
      searchAnchor
    )

  private def progressFor(
      active: ActiveCommentaryJob,
      phase: JobPhase,
      ledger: EngineWorkLedger
  ): CommentaryJobProgress = phase match
    case root: CollectingRoot =>
      progress(CommentaryJobPhase.RootSearch, active.legalMoves.size,
        root.baseline.fold(0)(_.progress.rootCandidateLinesAdmitted), if root.baseline.nonEmpty then 1 else 0, ledger)
    case focus: CollectingFocus =>
      progress(
        CommentaryJobPhase.FocusComparison,
        active.legalMoves.size,
        focus.rootEvaluations.size,
        if focus.baseline.nonEmpty then 1 else 0,
        ledger
      )
    case investigation: Investigating =>
      progress(CommentaryJobPhase.InvestigationComparison, active.legalMoves.size,
        investigation.baseline.progress.rootCandidateLinesAdmitted, 1, ledger)
    case refresh: RefreshingAssessment =>
      progress(CommentaryJobPhase.InvestigationComparison, active.legalMoves.size,
        refresh.baseline.progress.rootCandidateLinesAdmitted, 1, ledger)
    case investigation: InvestigatingResponses =>
      progress(CommentaryJobPhase.ResponseComparison, active.legalMoves.size,
        investigation.baseline.progress.rootCandidateLinesAdmitted, 1, ledger)
    case investigation: InvestigatingProposals =>
      progress(if investigation.selected.prefixMovesUci.isEmpty then CommentaryJobPhase.InvestigationComparison
        else CommentaryJobPhase.ResponseComparison, active.legalMoves.size,
        investigation.baseline.progress.rootCandidateLinesAdmitted, 1, ledger)
    case investigation: InvestigatingFacts =>
      progress(CommentaryJobPhase.ResponseComparison, active.legalMoves.size,
        investigation.baseline.progress.rootCandidateLinesAdmitted, 1, ledger)
    case investigation: ProbingThreat =>
      progress(CommentaryJobPhase.ThreatProbe, active.legalMoves.size,
        investigation.baseline.progress.rootCandidateLinesAdmitted, 1, ledger)

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
      condition: CommentaryJobStopCondition,
      preparedBaseline: Option[CompletedCommentaryJobRecord] = None
  ): CommentaryJobState =
    val materialized = active.phase match
      case focus: CollectingFocus if active.assessment.isEmpty =>
        val input = MoveReviewInputAdmission.retainAdmittedLines(active.packet.assembly.input,
          focus.rootEvaluations, EvaluationSource.RootSearch)
        assembleRetainedPacket(input, active.packet).fold(active)((packet, _) => active.copy(packet = packet))
      case _ => active
    val reference = active.phase match
      case value: InvestigationPhase => reviewReference(value.baseline).getOrElse(active.playedMoveUci)
      case _ => active.playedMoveUci
    val variations = readPrincipalVariations(materialized, reference)
    val premiseFacts = active.proposalEntries.exists(_.request.requirements.exists {
      case fact: CommentaryProposal.Fact => fact.path.source.isInstanceOf[CommentaryFact.Premise]
      case _ => false
    })
    val sources = if premiseFacts && active.policy.investigateChoices then
      variations.copy(continuationInquiries = continuationQuestions(variations, reference))
    else variations
    val read = refreshProposalReadings(sources)
    val retained = read.copy(rootSearches = read.rootSearches.map(search =>
      if search.work == read.issuedWork then search.copy(stopReason = Some(condition)) else search),
      verification = read.verification.map(value => if value.state == "pending" then value.copy(state = "stopped",
        stopReason = Some(PositionCommentaryJobProtocol.stopConditionCode(condition))) else value),
      proposalEntries = CommentaryProposal.stop(read.proposalEntries, PositionCommentaryJobProtocol.stopConditionCode(condition)))
    preparedBaseline.filter(_ => !active.phase.isInstanceOf[InvestigationPhase]) match
      case Some(baseline) => finishInvestigation(retained, baseline, retained.inquiries, retained.responseInquiries)
      case None => finishStopped(retained, condition)

  private def finishStopped(
      active: ActiveCommentaryJob,
      condition: CommentaryJobStopCondition
  ): CommentaryJobState = active.phase match
    case root: CollectingRoot if root.baseline.nonEmpty =>
      finishInvestigation(active, root.baseline.get, active.inquiries, active.responseInquiries,
        needsRefresh = active.assessmentNeedsRefresh)
    case focus: CollectingFocus if focus.baseline.nonEmpty =>
      finishInvestigation(active, focus.baseline.get, active.inquiries, active.responseInquiries,
        needsRefresh = active.assessmentNeedsRefresh)
    case refresh: RefreshingAssessment =>
      val questions = refresh.questions.map { previous =>
        // A failed refresh may still contain a newer usable pair. Rebind it before
        // recording the failed assessment obligation, so final reading cannot erase the stop.
        val inquiry = reconsiderInquiry(active, previous)
        if refresh.selectedQuestionId.contains(previous.question.id) || previous.state == "awaiting_engine" then
          inquiry.copy(state = if inquiry.answer.nonEmpty then "answered" else "stopped",
            stopReason = Some(PositionCommentaryJobProtocol.stopConditionCode(condition)),
            assessmentEffect = previous.assessmentEffect)
        else inquiry
      }
      finishInvestigation(readPrincipalVariations(active, refresh.referenceMove), refresh.baseline,
        questions, refresh.responses, needsRefresh = refresh.refreshing)
    case investigation: InvestigatingProposals =>
      finishInvestigation(active, investigation.baseline, investigation.questions, investigation.responses)
    case investigation: InvestigatingFacts =>
      finishInvestigation(active, investigation.baseline, investigation.questions, investigation.responses)
    case investigation: ProbingThreat =>
      finishInvestigation(active, investigation.baseline, investigation.questions, investigation.responses)
    case investigation: Investigating =>
      val questions = investigation.questions.map { previous =>
        val inquiry = reconsiderInquiry(active, previous)
        if previous.question.id == investigation.selected.id || previous.state == "awaiting_engine" then
          inquiry.copy(state = if inquiry.answer.nonEmpty then "answered" else "stopped",
            stopReason = Some(PositionCommentaryJobProtocol.stopConditionCode(condition)),
            assessmentEffect = previous.assessmentEffect)
        else inquiry
      }
      finishInvestigation(readPrincipalVariations(active, investigation.selected.referenceMove),
        investigation.baseline, questions, investigation.responses)
    case investigation: InvestigatingResponses =>
      val responses = investigation.responses.map(inquiry => if inquiry.question.id == investigation.selectedId ||
          inquiry.state == "awaiting_engine" then
        inquiry.copy(state = if inquiry.answer.nonEmpty then "answered" else "stopped",
          stopReason = Some(PositionCommentaryJobProtocol.stopConditionCode(condition))) else inquiry)
      val retained = active.copy(continuationInquiries = active.continuationInquiries.map(inquiry =>
        if inquiry.question.id == investigation.selectedId && inquiry.routeChallenge.exists(_.state == "awaiting_engine") then
          inquiry.copy(routeChallenge = inquiry.routeChallenge.map(_.copy(state = "stopped",
            stopReason = Some(PositionCommentaryJobProtocol.stopConditionCode(condition)))))
        else if inquiry.question.id == investigation.selectedId || inquiry.state == "awaiting_engine" then
          inquiry.copy(state = "stopped", stopReason = Some(PositionCommentaryJobProtocol.stopConditionCode(condition)))
        else inquiry))
      finishInvestigation(readPrincipalVariations(retained,
        reviewReference(investigation.baseline).getOrElse(active.playedMoveUci)), investigation.baseline,
        investigation.questions, responses)
    case _ => stopped(
      active.engineProfile,
      active.playedMoveUci,
      active.resultingFen,
      active.progress.copy(phase = CommentaryJobPhase.Stopped),
      condition,
      active.observationsPacket,
      active.rootSearches,
      active.proposalEntries,
      active.localExpansionsUsed,
      active.discoveryState,
      active.verification
    )

  private def stopped(
      engineProfile: CommentaryEngineProfile,
      playedMoveUci: String,
      resultingFen: String,
      progress: CommentaryJobProgress,
      condition: CommentaryJobStopCondition,
      packet: Option[EvidenceBackedJudgmentPacket],
      rootSearches: List[CommentaryRootSearch] = Nil,
      proposalEntries: List[CommentaryProposal.Entry] = Nil,
      localExpansionsUsed: Int = 0,
      discoveryState: CommentaryDiscovery.State = CommentaryDiscovery.State(),
      verification: Option[CommentaryVerification] = None
  ): StoppedCommentaryJob =
    StoppedCommentaryJob(engineProfile, playedMoveUci, resultingFen, condition, progress, packet, rootSearches,
      proposalEntries, localExpansionsUsed, discoveryState, verification)
