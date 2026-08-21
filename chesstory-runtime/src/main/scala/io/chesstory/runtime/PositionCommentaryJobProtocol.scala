package io.chesstory.runtime

import scala.util.Try

import play.api.libs.json.*

object PositionCommentaryJobProtocol:
  val JobRequestSchema = "chesstory.position-commentary.job-request.v4"
  val EngineWorkReportSchema = "chesstory.position-commentary.engine-work-report.v4"
  val JobStatusSchema = "chesstory.position-commentary.job-status.v4"
  val JobErrorSchema = "chesstory.position-commentary.job-error.v4"

  private val MaximumFenLength = 128
  private val IssuedWorkId = "^work:[0-9]+$".r

  final case class PositionCommentaryJobRequest(
      requestId: String,
      initialFen: String,
      movePrefixUci: List[String],
      currentFen: String,
      engineProfile: CommentaryEngineProfile
  )

  enum PositionCommentaryJobWireFailure(val errorCode: String):
    case InvalidJson extends PositionCommentaryJobWireFailure("invalid_json")
    case InvalidJobRequest extends PositionCommentaryJobWireFailure("invalid_position_commentary_job_request")
    case InvalidRequestId extends PositionCommentaryJobWireFailure("invalid_request_id")
    case InvalidInitialFen extends PositionCommentaryJobWireFailure("invalid_initial_fen")
    case InvalidMovePrefixUci extends PositionCommentaryJobWireFailure("invalid_move_prefix_uci")
    case InvalidCurrentFen extends PositionCommentaryJobWireFailure("invalid_current_fen")
    case InvalidEngineProfile extends PositionCommentaryJobWireFailure("invalid_engine_profile")
    case UnsupportedEngineProfile extends PositionCommentaryJobWireFailure("unsupported_engine_profile")
    case UnsupportedJobRequestSchema extends PositionCommentaryJobWireFailure("unsupported_schema_version")
    case InvalidEngineWorkReport extends PositionCommentaryJobWireFailure("invalid_engine_work_report")
    case UnsupportedEngineWorkReportSchema extends PositionCommentaryJobWireFailure("unsupported_schema_version")

  def parseJobRequest(body: Array[Byte]): Either[PositionCommentaryJobWireFailure, PositionCommentaryJobRequest] =
    Try(Json.parse(body)).toEither.left.map(_ => PositionCommentaryJobWireFailure.InvalidJson).flatMap(
      parseJobRequestJson
    )

  def parseEngineWorkReport(body: Array[Byte]): Either[PositionCommentaryJobWireFailure, EngineWorkReport] =
    Try(Json.parse(body)).toEither.left.map(_ => PositionCommentaryJobWireFailure.InvalidJson).flatMap(
      parseEngineWorkReportJson
    )

  def projectJobSnapshot(snapshot: CommentaryJobSnapshot): JsObject = snapshot.state match
    case completed: CompletedCommentaryJob =>
      RuntimeProtocol.encodePositionCommentaryResponse(snapshot.requestId, completed)
    case awaiting: AwaitingEngineWork =>
      activeJobStatus(snapshot, awaiting)
    case stopped: StoppedCommentaryJob =>
      stoppedJobStatus(snapshot, stopped)

  def jobErrorJson(
      requestId: Option[String],
      jobId: Option[String],
      engineProfile: Option[CommentaryEngineProfile],
      error: String
  ): JsObject =
    Json.obj(
      "schema_version" -> JobErrorSchema,
      "error" -> error
    ) ++ requestId.map(value => Json.obj("request_id" -> value)).getOrElse(Json.obj()) ++
      jobId.map(value => Json.obj("job_id" -> value)).getOrElse(Json.obj()) ++
      engineProfile.map(value => Json.obj("engine_profile" -> value.wireId)).getOrElse(Json.obj())

  private def parseJobRequestJson(
      jobRequestValue: JsValue
  ): Either[PositionCommentaryJobWireFailure, PositionCommentaryJobRequest] =
    jobRequestValue match
      case jobRequest: JsObject if (jobRequest \ "schema_version").asOpt[String] != Some(JobRequestSchema) =>
        Left(PositionCommentaryJobWireFailure.UnsupportedJobRequestSchema)
      case jobRequest: JsObject if !jobRequest.keys.contains("engine_profile") =>
        Left(PositionCommentaryJobWireFailure.InvalidEngineProfile)
      case jobRequest: JsObject
          if !hasExactKeys(
            jobRequest,
            Set("schema_version", "request_id", "initial_fen", "move_prefix_uci", "current_fen", "engine_profile")
          ) =>
        Left(PositionCommentaryJobWireFailure.InvalidJobRequest)
      case jobRequest: JsObject =>
        for
          engineProfile <- parseEngineProfile(jobRequest.value("engine_profile"))
          requestId <- (jobRequest \ "request_id").asOpt[String]
            .filter(RuntimeProtocol.acceptsRequestId)
            .toRight(PositionCommentaryJobWireFailure.InvalidRequestId)
          initialFen <- (jobRequest \ "initial_fen").asOpt[String]
            .filter(fen => fen.nonEmpty && fen.length <= MaximumFenLength)
            .toRight(PositionCommentaryJobWireFailure.InvalidInitialFen)
          movePrefixUci <- (jobRequest \ "move_prefix_uci").asOpt[List[String]]
            .filter(_.forall(EngineLineAdmission.acceptsCanonicalLowercaseUci))
            .toRight(PositionCommentaryJobWireFailure.InvalidMovePrefixUci)
          currentFen <- (jobRequest \ "current_fen").asOpt[String]
            .filter(fen => fen.nonEmpty && fen.length <= MaximumFenLength)
            .toRight(PositionCommentaryJobWireFailure.InvalidCurrentFen)
        yield PositionCommentaryJobRequest(requestId, initialFen, movePrefixUci, currentFen, engineProfile)
      case _ => Left(PositionCommentaryJobWireFailure.InvalidJobRequest)

  private def parseEngineWorkReportJson(
      engineWorkReportValue: JsValue
  ): Either[PositionCommentaryJobWireFailure, EngineWorkReport] =
    engineWorkReportValue match
      case engineWorkReport: JsObject if (engineWorkReport \ "schema_version").asOpt[String] != Some(EngineWorkReportSchema) =>
        Left(PositionCommentaryJobWireFailure.UnsupportedEngineWorkReportSchema)
      case engineWorkReport: JsObject if !engineWorkReport.keys.contains("engine_profile") =>
        Left(PositionCommentaryJobWireFailure.InvalidEngineProfile)
      case engineWorkReport: JsObject if !hasExactKeys(engineWorkReport, Set("schema_version", "engine_profile", "work_id", "outcome")) =>
        Left(PositionCommentaryJobWireFailure.InvalidEngineWorkReport)
      case engineWorkReport: JsObject =>
        for
          engineProfile <- parseEngineProfile(engineWorkReport.value("engine_profile"))
          workId <- (engineWorkReport \ "work_id").asOpt[String]
            .filter(IssuedWorkId.matches)
            .toRight(PositionCommentaryJobWireFailure.InvalidEngineWorkReport)
          reportOutcome <- engineWorkReport.value.get("outcome")
            .flatMap(outcome => parseEngineWorkOutcome(engineProfile, workId, outcome))
            .toRight(PositionCommentaryJobWireFailure.InvalidEngineWorkReport)
        yield reportOutcome
      case _ => Left(PositionCommentaryJobWireFailure.InvalidEngineWorkReport)

  private def parseEngineWorkOutcome(
      engineProfile: CommentaryEngineProfile,
      workId: String,
      outcomeValue: JsValue
  ): Option[EngineWorkReport] =
    outcomeValue match
      case outcome: JsObject if hasExactKeys(outcome, Set("kind")) && (outcome \ "kind").asOpt[String].contains("executor_failed") =>
        Some(EngineWorkReport.ExecutorFailed(engineProfile, workId))
      case outcome: JsObject if hasExactKeys(outcome, Set("kind", "line_suffixes")) && (outcome \ "kind").asOpt[String].contains("completed") =>
        outcome.value.get("line_suffixes").flatMap(parseReportedLineSuffixes).map { lineSuffixes =>
          EngineWorkReport.Completed(engineProfile, workId, lineSuffixes)
        }
      case _ => None

  private def parseReportedLineSuffixes(
      lineSuffixesValue: JsValue
  ): Option[List[ReportedEngineLineSuffix]] =
    lineSuffixesValue match
      case JsArray(lineSuffixValues) =>
        val lineSuffixes = lineSuffixValues.toList.map(parseReportedLineSuffix)
        Option.when(lineSuffixes.forall(_.nonEmpty))(lineSuffixes.flatten)
      case _ => None

  private def parseReportedLineSuffix(
      lineSuffixValue: JsValue
  ): Option[ReportedEngineLineSuffix] =
    lineSuffixValue match
      case lineSuffix: JsObject if hasExactKeys(lineSuffix, Set("moves", "depth", "white_score")) =>
        for
          moves <- (lineSuffix \ "moves").asOpt[List[String]]
          depth <- (lineSuffix \ "depth").asOpt[Int]
          whiteScoreObject <- lineSuffix.value.get("white_score").collect { case whiteScore: JsObject => whiteScore }
          whiteScore <- parseReportedWhiteEngineScore(whiteScoreObject)
        yield ReportedEngineLineSuffix(moves, whiteScore, depth)
      case _ => None

  private def parseReportedWhiteEngineScore(whiteScore: JsObject): Option[ReportedWhiteEngineScore] =
    if !hasExactKeys(whiteScore, Set("kind", "value")) then None
    else
      ((whiteScore \ "kind").asOpt[String], (whiteScore \ "value").asOpt[Int]) match
        case (Some("cp"), Some(value)) => Some(ReportedWhiteEngineScore.Centipawns(value))
        case (Some("mate"), Some(value)) => Some(ReportedWhiteEngineScore.Mate(value))
        case _ => None

  private def activeJobStatus(snapshot: CommentaryJobSnapshot, awaiting: AwaitingEngineWork): JsObject =
    Json.obj(
      "schema_version" -> JobStatusSchema,
      "request_id" -> snapshot.requestId,
      "job_id" -> snapshot.jobId,
      "engine_profile" -> awaiting.engineProfile.wireId,
      "state" -> "awaiting_engine_work",
      "deadline_epoch_ms" -> snapshot.deadlineEpochMs,
      "progress" -> commentaryJobProgressJson(awaiting.progress),
      "issued_engine_work" -> Json.obj(
        "work_id" -> awaiting.issuedWork.workId,
        "engine_profile" -> awaiting.issuedWork.engineProfile.wireId,
        "engine_position_initial_fen" -> awaiting.issuedWork.enginePositionInitialFen,
        "engine_position_moves_uci" -> awaiting.issuedWork.enginePositionMovesUci,
        "search_fen" -> awaiting.issuedWork.searchFen,
        "required_depth" -> awaiting.issuedWork.requiredDepth,
        "multi_pv" -> awaiting.issuedWork.multiPv
      )
    )

  private def stoppedJobStatus(snapshot: CommentaryJobSnapshot, stopped: StoppedCommentaryJob): JsObject =
    Json.obj(
      "schema_version" -> JobStatusSchema,
      "request_id" -> snapshot.requestId,
      "job_id" -> snapshot.jobId,
      "engine_profile" -> stopped.engineProfile.wireId,
      "state" -> "stopped",
      "deadline_epoch_ms" -> snapshot.deadlineEpochMs,
      "progress" -> commentaryJobProgressJson(stopped.progress),
      "stop_condition" -> stopConditionCode(stopped.stopCondition)
    )

  private def commentaryJobProgressJson(progress: CommentaryJobProgress): JsObject =
    Json.obj(
      "legal_move_count" -> progress.legalMoveCount,
      "root_lines_collected" -> progress.rootLinesCollected,
      "move_commentaries_completed" -> progress.moveCommentariesCompleted,
      "engine_work_issued" -> progress.engineWorkIssued,
      "engine_work_reports_accepted" -> progress.engineWorkReportsAccepted
    )

  private def stopConditionCode(condition: CommentaryJobStopCondition): String = condition match
    case CommentaryJobStopCondition.DeadlineExceeded => "deadline_exceeded"
    case CommentaryJobStopCondition.BudgetExhausted => "budget_exhausted"
    case CommentaryJobStopCondition.Cancelled => "cancelled"
    case CommentaryJobStopCondition.EngineExecutionFailed => "engine_execution_failed"
    case CommentaryJobStopCondition.InvalidEngineWorkReport => "invalid_engine_work_report"
    case CommentaryJobStopCondition.ReviewConstructionFailed => "review_construction_failed"
    case CommentaryJobStopCondition.MoveReviewPreparationFailed => "move_review_preparation_failed"
    case CommentaryJobStopCondition.RepetitionHistoryUnavailable => "repetition_history_unavailable"

  private def hasExactKeys(value: JsObject, requiredKeys: Set[String]): Boolean =
    value.keys == requiredKeys

  private def parseEngineProfile(
      value: JsValue
  ): Either[PositionCommentaryJobWireFailure, CommentaryEngineProfile] =
    value.asOpt[String]
      .toRight(PositionCommentaryJobWireFailure.InvalidEngineProfile)
      .flatMap(raw =>
        CommentaryEngineProfile
          .parseWireId(raw)
          .toRight(PositionCommentaryJobWireFailure.UnsupportedEngineProfile)
      )
