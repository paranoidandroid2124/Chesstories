package io.chesstory.runtime

import scala.util.Try

import play.api.libs.json.*

object PositionCommentaryJobProtocol:
  val JobRequestSchema = "chesstory.position-commentary.job-request.v6"
  val EngineWorkReportSchema = "chesstory.position-commentary.engine-work-report.v6"
  val JobStatusSchema = "chesstory.position-commentary.job-status.v6"
  val JobErrorSchema = "chesstory.position-commentary.job-error.v6"
  val Variant = "standard"

  private val MaximumFenLength = 128
  private val MaximumFailureCodeLength = 64
  private val IssuedWorkId = "^work:[0-9]+$".r
  private val Sha256 = "^[a-f0-9]{64}$".r

  final case class PositionCommentaryJobRequest(
      requestId: String,
      initialFen: String,
      movePrefixUci: List[String],
      currentFen: String,
      playedMoveUci: String,
      resultingFen: String,
      engineProfile: CommentaryEngineProfile
  )

  enum PositionCommentaryJobWireFailure(val errorCode: String):
    case InvalidJson extends PositionCommentaryJobWireFailure("invalid_json")
    case InvalidJobRequest extends PositionCommentaryJobWireFailure("invalid_position_commentary_job_request")
    case InvalidRequestId extends PositionCommentaryJobWireFailure("invalid_request_id")
    case InvalidInitialFen extends PositionCommentaryJobWireFailure("invalid_initial_fen")
    case InvalidMovePrefixUci extends PositionCommentaryJobWireFailure("invalid_move_prefix_uci")
    case InvalidCurrentFen extends PositionCommentaryJobWireFailure("invalid_current_fen")
    case InvalidFocus extends PositionCommentaryJobWireFailure("invalid_focus")
    case UnsupportedVariant extends PositionCommentaryJobWireFailure("unsupported_variant")
    case InvalidEngineProfile extends PositionCommentaryJobWireFailure("invalid_engine_profile")
    case UnsupportedEngineProfile extends PositionCommentaryJobWireFailure("unsupported_engine_profile")
    case UnsupportedJobRequestSchema extends PositionCommentaryJobWireFailure("unsupported_schema_version")
    case InvalidEngineWorkReport extends PositionCommentaryJobWireFailure("invalid_engine_work_report")
    case UnsupportedEngineWorkReportSchema extends PositionCommentaryJobWireFailure("unsupported_schema_version")

  def parseJobRequest(body: Array[Byte]): Either[PositionCommentaryJobWireFailure, PositionCommentaryJobRequest] =
    Try(Json.parse(body)).toEither
      .left.map(_ => PositionCommentaryJobWireFailure.InvalidJson)
      .flatMap(parseJobRequestJson)

  def parseEngineWorkReport(body: Array[Byte]): Either[PositionCommentaryJobWireFailure, EngineWorkReport] =
    Try(Json.parse(body)).toEither
      .left.map(_ => PositionCommentaryJobWireFailure.InvalidJson)
      .flatMap(parseEngineWorkReportJson)

  def projectJobSnapshot(snapshot: CommentaryJobSnapshot): JsObject = snapshot.state match
    case completed: CompletedCommentaryJob =>
      RuntimeProtocol.encodePositionCommentaryResponse(snapshot, completed)
    case awaiting: AwaitingEngineWork => activeJobStatus(snapshot, awaiting)
    case stopped: StoppedCommentaryJob => stoppedJobStatus(snapshot, stopped)

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
      value: JsValue
  ): Either[PositionCommentaryJobWireFailure, PositionCommentaryJobRequest] = value match
    case request: JsObject if (request \ "schema_version").asOpt[String] != Some(JobRequestSchema) =>
      Left(PositionCommentaryJobWireFailure.UnsupportedJobRequestSchema)
    case request: JsObject
        if !hasExactKeys(
          request,
          Set(
            "schema_version",
            "request_id",
            "variant",
            "initial_fen",
            "move_prefix_uci",
            "current_fen",
            "focus",
            "engine_profile"
          )
        ) =>
      Left(PositionCommentaryJobWireFailure.InvalidJobRequest)
    case request: JsObject if (request \ "variant").asOpt[String] != Some(Variant) =>
      Left(PositionCommentaryJobWireFailure.UnsupportedVariant)
    case request: JsObject =>
      for
        engineProfile <- request.value.get("engine_profile")
          .toRight(PositionCommentaryJobWireFailure.InvalidEngineProfile)
          .flatMap(parseEngineProfile)
        requestId <- (request \ "request_id").asOpt[String]
          .filter(RuntimeProtocol.acceptsRequestId)
          .toRight(PositionCommentaryJobWireFailure.InvalidRequestId)
        initialFen <- boundedFen(request \ "initial_fen")
          .toRight(PositionCommentaryJobWireFailure.InvalidInitialFen)
        movePrefixUci <- (request \ "move_prefix_uci").asOpt[List[String]]
          .filter(_.forall(EngineLineAdmission.acceptsCanonicalLowercaseUci))
          .toRight(PositionCommentaryJobWireFailure.InvalidMovePrefixUci)
        currentFen <- boundedFen(request \ "current_fen")
          .toRight(PositionCommentaryJobWireFailure.InvalidCurrentFen)
        focus <- request.value.get("focus")
          .flatMap(parseFocus)
          .toRight(PositionCommentaryJobWireFailure.InvalidFocus)
      yield PositionCommentaryJobRequest(
        requestId,
        initialFen,
        movePrefixUci,
        currentFen,
        focus._1,
        focus._2,
        engineProfile
      )
    case _ => Left(PositionCommentaryJobWireFailure.InvalidJobRequest)

  private def parseFocus(value: JsValue): Option[(String, String)] = value match
    case focus: JsObject
        if hasExactKeys(focus, Set("kind", "played_move_uci", "resulting_fen")) &&
          (focus \ "kind").asOpt[String].contains("played_move") =>
      for
        move <- (focus \ "played_move_uci").asOpt[String]
          .filter(EngineLineAdmission.acceptsCanonicalLowercaseUci)
        resultingFen <- boundedFen(focus \ "resulting_fen")
      yield move -> resultingFen
    case _ => None

  private def parseEngineWorkReportJson(
      value: JsValue
  ): Either[PositionCommentaryJobWireFailure, EngineWorkReport] = value match
    case report: JsObject
        if (report \ "schema_version").asOpt[String] != Some(EngineWorkReportSchema) =>
      Left(PositionCommentaryJobWireFailure.UnsupportedEngineWorkReportSchema)
    case report: JsObject
        if !hasExactKeys(
          report,
          Set("schema_version", "engine_profile", "work_id", "execution_key_sha256", "outcome")
        ) =>
      Left(PositionCommentaryJobWireFailure.InvalidEngineWorkReport)
    case report: JsObject =>
      for
        engineProfile <- report.value.get("engine_profile")
          .toRight(PositionCommentaryJobWireFailure.InvalidEngineProfile)
          .flatMap(parseEngineProfile)
        workId <- (report \ "work_id").asOpt[String]
          .filter(IssuedWorkId.matches)
          .toRight(PositionCommentaryJobWireFailure.InvalidEngineWorkReport)
        executionKey <- (report \ "execution_key_sha256").asOpt[String]
          .filter(Sha256.matches)
          .toRight(PositionCommentaryJobWireFailure.InvalidEngineWorkReport)
        outcome <- report.value.get("outcome")
          .flatMap(parseEngineWorkOutcome(engineProfile, workId, executionKey, _))
          .toRight(PositionCommentaryJobWireFailure.InvalidEngineWorkReport)
      yield outcome
    case _ => Left(PositionCommentaryJobWireFailure.InvalidEngineWorkReport)

  private def parseEngineWorkOutcome(
      engineProfile: CommentaryEngineProfile,
      workId: String,
      executionKey: String,
      value: JsValue
  ): Option[EngineWorkReport] = value match
    case outcome: JsObject
        if hasExactKeys(outcome, Set("kind", "failure_code")) &&
          (outcome \ "kind").asOpt[String].contains("executor_failed") &&
          (outcome \ "failure_code").asOpt[String].exists(code =>
            code.nonEmpty && code.length <= MaximumFailureCodeLength
          ) =>
      Some(EngineWorkReport.ExecutorFailed(engineProfile, workId, executionKey))
    case outcome: JsObject
        if hasExactKeys(outcome, Set("kind", "line_suffixes")) &&
          (outcome \ "kind").asOpt[String].contains("completed") =>
      outcome.value.get("line_suffixes")
        .flatMap(parseReportedLineSuffixes)
        .map(EngineWorkReport.Completed(engineProfile, workId, executionKey, _))
    case _ => None

  private def parseReportedLineSuffixes(value: JsValue): Option[List[ReportedEngineLineSuffix]] = value match
    case JsArray(values) =>
      val parsed = values.toList.map(parseReportedLineSuffix)
      Option.when(parsed.forall(_.nonEmpty))(parsed.flatten)
    case _ => None

  private def parseReportedLineSuffix(value: JsValue): Option[ReportedEngineLineSuffix] = value match
    case suffix: JsObject if hasExactKeys(suffix, Set("moves", "depth", "white_score")) =>
      for
        moves <- (suffix \ "moves").asOpt[List[String]]
        depth <- (suffix \ "depth").asOpt[Int]
        score <- suffix.value.get("white_score").flatMap(parseReportedWhiteEngineScore)
      yield ReportedEngineLineSuffix(moves, score, depth)
    case _ => None

  private def parseReportedWhiteEngineScore(value: JsValue): Option[ReportedWhiteEngineScore] = value match
    case score: JsObject if hasExactKeys(score, Set("kind", "value")) =>
      ((score \ "kind").asOpt[String], (score \ "value").asOpt[Int]) match
        case (Some("cp"), Some(number))   => Some(ReportedWhiteEngineScore.Centipawns(number))
        case (Some("mate"), Some(number)) => Some(ReportedWhiteEngineScore.Mate(number))
        case _                             => None
    case _ => None

  private def activeJobStatus(
      snapshot: CommentaryJobSnapshot,
      awaiting: AwaitingEngineWork
  ): JsObject =
    commonStatus(snapshot, awaiting) ++ Json.obj(
      "state" -> "awaiting_engine_work",
      "progress" -> commentaryJobProgressJson(awaiting.progress),
      "issued_engine_work" -> issuedEngineWorkJson(awaiting.issuedWork)
    )

  private def stoppedJobStatus(
      snapshot: CommentaryJobSnapshot,
      stopped: StoppedCommentaryJob
  ): JsObject =
    commonStatus(snapshot, stopped) ++ Json.obj(
      "state" -> "stopped",
      "progress" -> commentaryJobProgressJson(stopped.progress),
      "stop_condition" -> stopConditionCode(stopped.stopCondition)
    )

  private def commonStatus(
      snapshot: CommentaryJobSnapshot,
      state: CommentaryJobState
  ): JsObject =
    Json.obj(
      "schema_version" -> JobStatusSchema,
      "request_id" -> snapshot.requestId,
      "job_id" -> snapshot.jobId,
      "engine_profile" -> state.engineProfile.wireId,
      "variant" -> Variant,
      "deadline_epoch_ms" -> snapshot.deadlineEpochMs,
      "focus" -> focusJson(state.playedMoveUci, state.resultingFen)
    )

  private def issuedEngineWorkJson(work: IssuedEngineWork): JsObject =
    Json.obj(
      "work_id" -> work.workId,
      "purpose" -> work.purpose.wireId,
      "engine_profile" -> work.engineProfile.wireId,
      "execution_key_sha256" -> work.executionKeySha256,
      "variant" -> Variant,
      "engine_position_initial_fen" -> work.enginePositionInitialFen,
      "engine_position_moves_uci" -> work.enginePositionMovesUci,
      "search_fen" -> work.searchFen,
      "root_restriction" -> rootRestrictionJson(work.rootRestriction),
      "search_limits" -> Json.obj(
        "depth" -> work.searchLimits.depth,
        "nodes" -> work.searchLimits.nodes,
        "movetime_ms" -> work.searchLimits.movetimeMs,
        "multi_pv" -> work.searchLimits.multiPv
      ),
      "max_search_elapsed_ms" -> work.maxSearchElapsedMs
    )

  private def rootRestrictionJson(value: EngineRootRestriction): JsObject = value match
    case EngineRootRestriction.Unrestricted => Json.obj("kind" -> "unrestricted")
    case EngineRootRestriction.Restricted(moves) =>
      Json.obj("kind" -> "restricted", "moves_uci" -> moves)

  private def commentaryJobProgressJson(progress: CommentaryJobProgress): JsObject =
    Json.obj(
      "phase" -> progress.phase.wireId,
      "legal_move_count" -> progress.legalMoveCount,
      "root_candidate_lines_admitted" -> progress.rootCandidateLinesAdmitted,
      "selected_commentaries_completed" -> progress.selectedCommentariesCompleted,
      "physical_works_issued" -> progress.physicalWorksIssued,
      "physical_reports_accepted" -> progress.physicalReportsAccepted,
      "causal_waves_completed" -> progress.causalWavesCompleted
    )

  private[runtime] def focusJson(playedMoveUci: String, resultingFen: String): JsObject =
    Json.obj(
      "kind" -> "played_move",
      "played_move_uci" -> playedMoveUci,
      "resulting_fen" -> resultingFen
    )

  private[runtime] def completedProgressJson(progress: CommentaryJobProgress): JsObject =
    commentaryJobProgressJson(progress)

  private def stopConditionCode(condition: CommentaryJobStopCondition): String = condition match
    case CommentaryJobStopCondition.DeadlineExceeded              => "deadline_exceeded"
    case CommentaryJobStopCondition.BudgetExhausted               => "budget_exhausted"
    case CommentaryJobStopCondition.Cancelled                     => "cancelled"
    case CommentaryJobStopCondition.EngineExecutionFailed         => "engine_execution_failed"
    case CommentaryJobStopCondition.InvalidEngineWorkReport       => "invalid_engine_work_report"
    case CommentaryJobStopCondition.ReviewConstructionFailed      => "review_construction_failed"
    case CommentaryJobStopCondition.MoveReviewPreparationFailed   => "move_review_preparation_failed"
    case CommentaryJobStopCondition.RepetitionHistoryUnavailable  => "repetition_history_unavailable"

  private def boundedFen(value: JsLookupResult): Option[String] =
    value.asOpt[String].filter(fen => fen.nonEmpty && fen.length <= MaximumFenLength)

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
