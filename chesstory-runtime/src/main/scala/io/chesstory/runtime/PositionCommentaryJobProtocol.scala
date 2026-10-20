package io.chesstory.runtime

import scala.util.Try

import play.api.libs.json.*
import lila.chessjudgment.model.judgment.{ CommentaryLineSource, SuppliedCommentaryLine }

object PositionCommentaryJobProtocol:
  val JobRequestSchema = "chesstory.position-commentary.job-request.v6"
  val EngineWorkReportSchema = "chesstory.position-commentary.engine-work-report.v6"
  val JobStatusSchema = "chesstory.position-commentary.job-status.v6"
  val JobErrorSchema = "chesstory.position-commentary.job-error.v6"
  val Variant = "standard"

  private val MaximumFenLength = 128
  private val IssuedWorkId = "^work:[0-9]+$".r
  private val Sha256 = "^[a-f0-9]{64}$".r

  final case class PositionCommentaryJobRequest(
      requestId: String,
      initialFen: String,
      movePrefixUci: List[String],
      currentFen: String,
      playedMoveUci: String,
      resultingFen: String,
      engineProfile: CommentaryEngineProfile,
      inputLines: List[SuppliedCommentaryLine] = Nil,
      remainingBudgetMs: Option[Int] = None,
      proposals: List[CommentaryProposal.Request] = Nil
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

  def projectJobSnapshot(delivery: CommentaryJobDelivery): JsObject = delivery match
    case retained: RetainedCommentaryJobResult => Json.parse(retained.jsonBytes).as[JsObject]
    case snapshot: CommentaryJobSnapshot => snapshot.state match
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
          request - "input_lines" - "remaining_budget_ms" - "proposals",
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
        inputLines <- request.value.get("input_lines").fold(Option(List.empty[SuppliedCommentaryLine]))(parseInputLines)
          .toRight(PositionCommentaryJobWireFailure.InvalidJobRequest)
        remainingBudgetMs <- request.value.get("remaining_budget_ms")
          .fold(Option(Option.empty[Int]))(_.asOpt[Int].filter(ms => ms >= 1 && ms <= 180000).map(Some(_)))
          .toRight(PositionCommentaryJobWireFailure.InvalidJobRequest)
        proposals <- request.value.get("proposals").fold(Option(List.empty[CommentaryProposal.Request]))(parseProposals)
          .toRight(PositionCommentaryJobWireFailure.InvalidJobRequest)
      yield PositionCommentaryJobRequest(
        requestId,
        initialFen,
        movePrefixUci,
        currentFen,
        focus._1,
        focus._2,
        engineProfile,
        inputLines,
        remainingBudgetMs,
        proposals
      )
    case _ => Left(PositionCommentaryJobWireFailure.InvalidJobRequest)

  private def parseProposals(value: JsValue): Option[List[CommentaryProposal.Request]] = value match
    case JsArray(values) if values.size <= 600 =>
      val parsed = values.toList.map {
        case obj: JsObject if hasExactKeys(obj, Set("id", "origin", "requirements")) =>
          for
            id <- (obj \ "id").asOpt[String]
            origin <- obj.value.get("origin").flatMap(parseProposalOrigin)
            requirements <- obj.value.get("requirements").flatMap {
              case JsArray(rows) if rows.nonEmpty && rows.size <= 600 =>
                val parsedRows = rows.toList.map(parseProposalRequirement)
                Option.when(parsedRows.forall(_.nonEmpty))(parsedRows.flatten)
              case _ => None
            }
          yield CommentaryProposal.Request(id, origin, requirements)
        case _ => None
      }
      Option.when(parsed.forall(_.nonEmpty))(parsed.flatten).filter(CommentaryProposal.validRequests)
    case _ => None

  private def parseProposalOrigin(value: JsValue): Option[CommentaryProposal.Origin] = value match
    case obj: JsObject if hasExactKeys(obj, Set("question", "candidates", "priority", "evidence")) =>
      for
        question <- obj.value.get("question").flatMap(parseProposalSource)
        candidates <- obj.value.get("candidates").flatMap(parseProposalSource)
        priority <- obj.value.get("priority").flatMap(parseProposalSource)
        evidence <- obj.value.get("evidence").flatMap(parseProposalSource)
      yield CommentaryProposal.Origin(question, candidates, priority, evidence)
    case _ => None

  private def parseProposalSource(value: JsValue): Option[CommentaryProposal.Source] = value match
    case obj: JsObject if hasExactKeys(obj - "version" - "source_refs", Set("kind", "producer")) =>
      for
        kind <- (obj \ "kind").asOpt[String]
        producer <- (obj \ "producer").asOpt[String]
        version <- obj.value.get("version").fold(Option(Option.empty[String]))(
          _.asOpt[String].map(Some(_)))
        refs <- obj.value.get("source_refs").fold(Option(List.empty[String]))(_.asOpt[List[String]])
      yield CommentaryProposal.Source(kind, producer, version, refs)
    case _ => None

  private def parseProposalRequirement(value: JsValue): Option[CommentaryProposal.Requirement] = value match
    case obj: JsObject =>
      def id = (obj \ "id").asOpt[String]
      def prefix = (obj \ "prefix_moves_uci").asOpt[List[String]]
      def move(field: String) = (obj \ field).asOpt[String]
      (obj \ "kind").asOpt[String] match
        case Some("observe_move") if hasExactKeys(obj, Set("id", "kind", "prefix_moves_uci", "move_uci")) =>
          for
            requirementId <- id
            moves <- prefix
            candidate <- move("move_uci")
          yield CommentaryProposal.ObserveMove(requirementId, moves, candidate)
        case Some("compare_moves") if hasExactKeys(obj - "expected_relation",
            Set("id", "kind", "prefix_moves_uci", "reference_move", "alternative_move")) =>
          for
            requirementId <- id
            moves <- prefix
            reference <- move("reference_move")
            alternative <- move("alternative_move")
            expected <- obj.value.get("expected_relation").fold(Option(Option.empty[String]))(
              _.asOpt[String].map(Some(_)))
          yield CommentaryProposal.CompareMoves(requirementId, moves, reference, alternative, expected)
        case Some("uninterpreted") if hasExactKeys(obj, Set("id", "kind", "text")) =>
          for
            requirementId <- id
            text <- (obj \ "text").asOpt[String]
          yield CommentaryProposal.Uninterpreted(requirementId, text)
        case Some("fact") if hasExactKeys(obj - "through" - "expected", Set("id", "kind", "path", "predicate", "at")) =>
          for
            requirementId <- id
            path <- obj.value.get("path").flatMap(parseFactPath)
            predicate <- obj.value.get("predicate").flatMap(parseFactPredicate)
            at <- obj.value.get("at").flatMap(parseFactPoint)
            through <- obj.value.get("through").fold(Option(Option.empty[CommentaryFact.Point]))(value =>
              parseFactPoint(value).map(Some(_)))
            expected <- obj.value.get("expected").fold(Option(Option.empty[String]))(_.asOpt[String].map(Some(_)))
          yield CommentaryProposal.Fact(requirementId, path, predicate, at, through, expected)
        case Some("fact_comparison") if hasExactKeys(obj,
            Set("id", "kind", "left_requirement_id", "right_requirement_id", "relation")) =>
          for
            requirementId <- id
            left <- (obj \ "left_requirement_id").asOpt[String]
            right <- (obj \ "right_requirement_id").asOpt[String]
            relation <- (obj \ "relation").asOpt[String]
          yield CommentaryProposal.FactComparison(requirementId, left, right, relation)
        case Some("fact_group") if hasExactKeys(obj, Set("id", "kind", "mode", "members")) =>
          for
            requirementId <- id
            mode <- (obj \ "mode").asOpt[String]
            members <- (obj \ "members").asOpt[List[String]]
          yield CommentaryProposal.FactGroup(requirementId, mode, members)
        case _ => None
    case _ => None

  private def parseFactPiece(value: JsValue): Option[CommentaryFact.Piece] = value match
    case obj: JsObject if hasExactKeys(obj, Set("side", "role", "square")) =>
      for
        side <- (obj \ "side").asOpt[String]
        role <- (obj \ "role").asOpt[String]
        square <- (obj \ "square").asOpt[String]
      yield CommentaryFact.Piece(side, role, square)
    case _ => None

  private def parseFactPath(value: JsValue): Option[CommentaryFact.Path] = value match
    case obj: JsObject if hasExactKeys(obj - "extend", Set("binding", "source")) =>
      for
        binding <- (obj \ "binding").asOpt[String]
        extend <- obj.value.get("extend").fold(Option(false))(_.asOpt[Boolean])
        source <- obj.value.get("source").flatMap {
          case source: JsObject if hasExactKeys(source, Set("kind", "moves_uci")) &&
              (source \ "kind").asOpt[String].contains("declared") =>
            (source \ "moves_uci").asOpt[List[String]].map(CommentaryFact.Declared.apply)
          case source: JsObject if hasExactKeys(source, Set("kind", "requirement_id", "arm")) &&
              (source \ "kind").asOpt[String].contains("comparison") =>
            for
              id <- (source \ "requirement_id").asOpt[String]
              arm <- (source \ "arm").asOpt[String]
            yield CommentaryFact.Comparison(id, arm)
          case source: JsObject if hasExactKeys(source, Set("kind", "inquiry_id", "premise_id")) &&
              (source \ "kind").asOpt[String].contains("premise") =>
            for
              inquiry <- (source \ "inquiry_id").asOpt[String]
              premise <- (source \ "premise_id").asOpt[String]
            yield CommentaryFact.Premise(inquiry, premise)
          case _ => None
        }
      yield CommentaryFact.Path(binding, source, extend)
    case _ => None

  private def parseFactPoint(value: JsValue): Option[CommentaryFact.Point] = value match
    case obj: JsObject if hasExactKeys(obj, Set("kind", "ply_count")) &&
        (obj \ "kind").asOpt[String].contains("ply") =>
      (obj \ "ply_count").asOpt[Int].map(CommentaryFact.Ply.apply)
    case obj: JsObject if hasExactKeys(obj - "actor" - "target" - "square",
        Set("kind", "event", "ordinal", "phase", "from_ply", "through_ply")) &&
        (obj \ "kind").asOpt[String].contains("event") =>
      for
        kind <- (obj \ "event").asOpt[String]
        ordinal <- (obj \ "ordinal").asOpt[Int]
        phase <- (obj \ "phase").asOpt[String]
        from <- (obj \ "from_ply").asOpt[Int]
        through <- (obj \ "through_ply").asOpt[Int]
        actor <- obj.value.get("actor").fold(Option(Option.empty[CommentaryFact.Piece]))(parseFactPiece(_).map(Some(_)))
        target <- obj.value.get("target").fold(Option(Option.empty[CommentaryFact.Piece]))(parseFactPiece(_).map(Some(_)))
        square <- obj.value.get("square").fold(Option(Option.empty[String]))(_.asOpt[String].map(Some(_)))
      yield CommentaryFact.Event(kind, ordinal, phase, from, through, actor, target, square)
    case _ => None

  private def parseFactPredicate(value: JsValue): Option[CommentaryFact.Predicate] = value match
    case obj: JsObject if hasExactKeys(obj - "subject" - "target" - "square" - "side" - "property", Set("kind")) =>
      def text(key: String) = obj.value.get(key).fold(Option(Option.empty[String]))(_.asOpt[String].map(Some(_)))
      for
        kind <- (obj \ "kind").asOpt[String]
        subject <- obj.value.get("subject").fold(Option(Option.empty[CommentaryFact.Piece]))(parseFactPiece(_).map(Some(_)))
        target <- obj.value.get("target").fold(Option(Option.empty[CommentaryFact.Piece]))(parseFactPiece(_).map(Some(_)))
        square <- text("square")
        side <- text("side")
        property <- text("property")
      yield CommentaryFact.Predicate(kind, subject, target, square, side, property)
    case _ => None

  private def parseInputLines(value: JsValue): Option[List[SuppliedCommentaryLine]] = value match
    case JsArray(values) if values.size <= 600 =>
      val lines = values.toList.map {
        case obj: JsObject if hasExactKeys(obj, Set("source", "moves")) =>
          for
            source <- (obj \ "source").asOpt[String].flatMap(id => CommentaryLineSource.values.find(_.wireId == id))
            moves <- (obj \ "moves").asOpt[List[String]].filter(moves =>
              moves.nonEmpty && moves.size <= 600 && moves.forall(EngineLineAdmission.acceptsCanonicalLowercaseUci))
          yield SuppliedCommentaryLine(source, moves)
        case _ => None
      }
      Option.when(lines.forall(_.nonEmpty))(lines.flatten)
    case _ => None

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
          (outcome \ "kind").asOpt[String].contains("executor_failed") =>
      (outcome \ "failure_code").asOpt[String].flatMap {
        case "cancelled"               => Some(EngineWorkFailure.Cancelled)
        case "lease_expired"           => Some(EngineWorkFailure.BudgetExhausted)
        case "search_budget_exhausted" => Some(EngineWorkFailure.BudgetExhausted)
        case "engine_failure"          => Some(EngineWorkFailure.ExecutionFailed)
        case "engine_unavailable"      => Some(EngineWorkFailure.EngineUnavailable)
        case "browser_unsupported"     => Some(EngineWorkFailure.BrowserUnsupported)
        case _                         => None
      }.map(EngineWorkReport.ExecutorFailed(engineProfile, workId, executionKey, _))
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
    ) ++ awaiting.assessment.map(completed => Json.obj(
      "assessment" -> RuntimeProtocol.completedResultJson(completed),
      "assessment_state" -> (if awaiting.assessmentNeedsRefresh then "needs_refresh" else "current"),
      "assessment_scope" -> awaiting.assessmentScope)).getOrElse(Json.obj())

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
    // Intermediate short-job work carries its independent assessment without
    // eagerly serializing the whole proof graph. Terminal states retain it.
    ) ++ verificationJson(state.verification) ++ state.observationsPacket.filterNot(_ =>
      state.deferObservationDelivery).map(packet => Json.obj("observations" ->
      (state match
        case pending: AwaitingEngineWork if pending.issuedWork.purpose == EngineWorkPurpose.ResponseComparison &&
            pending.issuedWork.searchAnchor.nonEmpty && pending.issuedWork.proposalRefs.isEmpty &&
            state.proposalEntries.isEmpty => RuntimeProtocol.responseWorkObservationsJson(packet,
          state.preparedPaths.get, state.responseInquiries, state.responseSearches, pending.issuedWork.searchAnchor)
        case _ => RuntimeProtocol.observationsJson(packet, state.inquiries, state.pvComparisons,
        state.responseInquiries, state.preparedPaths, state.responseSearches,
        state.continuationReadings, state.continuationInquiries, state.rootSearches,
        state.proposalEntries, state.localExpansionsUsed, state.discoveryState))))
      .getOrElse(Json.obj())

  private[runtime] def verificationJson(value: Option[CommentaryVerification]): JsObject =
    value.map(value => Json.obj("verification" -> Json.obj(
      "state" -> value.state, "target_depth" -> value.targetDepth, "reason" -> value.reason,
      "trigger_work_ids" -> value.triggerWorkIds, "completed_depth" -> value.completedDepth,
      "stop_reason" -> value.stopReason))).getOrElse(Json.obj())

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
    ) ++ work.threatOriginPrefixUci.map(prefix => Json.obj("threat_origin_prefix_uci" -> prefix)).getOrElse(Json.obj()) ++
      work.searchAnchor.map(anchor => Json.obj("search_anchor" -> Json.obj(
      "root_move" -> anchor.rootMove, "root_occurrence_id" -> anchor.rootOccurrenceId,
      "position_id" -> anchor.positionId, "prefix_moves_uci" -> anchor.prefixMoves))).getOrElse(Json.obj())

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
      "physical_reports_accepted" -> progress.physicalReportsAccepted
    )

  private[runtime] def focusJson(playedMoveUci: String, resultingFen: String): JsObject =
    Json.obj(
      "kind" -> "played_move",
      "played_move_uci" -> playedMoveUci,
      "resulting_fen" -> resultingFen
    )

  private[runtime] def completedProgressJson(progress: CommentaryJobProgress): JsObject =
    commentaryJobProgressJson(progress)

  private[runtime] def stopConditionCode(condition: CommentaryJobStopCondition): String = condition match
    case CommentaryJobStopCondition.EngineUnavailable            => "engine_unavailable"
    case CommentaryJobStopCondition.BrowserUnsupported           => "browser_unsupported"
    case CommentaryJobStopCondition.DeadlineExceeded              => "deadline_exceeded"
    case CommentaryJobStopCondition.Cancelled                     => "cancelled"
    case CommentaryJobStopCondition.SearchBudgetExhausted         => "search_budget_exhausted"
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
