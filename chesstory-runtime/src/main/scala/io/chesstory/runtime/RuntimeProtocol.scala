package io.chesstory.runtime

import scala.util.Try

import lila.chessjudgment.analysis.assembly.{
  JudgmentBoundaryExecution,
  JudgmentBoundaryIntervention,
  MoveReviewJudgmentOrchestrator,
  RawMoveReviewInput,
  RawOpeningContext
}
import lila.chessjudgment.analysis.opening.{ OpeningRecognitionIndex, OpeningThemePriorIndex }
import lila.chessjudgment.model.{ EndgameTablebaseEvidence, ProbeHorizon, ProbePurpose, ProbeRequest, ProbeResult }
import lila.chessjudgment.model.evaluation.{ PerspectiveMath, VerdictThresholdPolicy }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.strategic.EngineLine
import play.api.libs.json.*

object RuntimeProtocol:
  val RequestSchema = "chesstory.move-meaning.request.v1"
  val ResponseSchema = "chesstory.move-meaning.response.v3"
  private[chesstory] val DirectCauseImportanceRelationPolicyVersion =
    "chesstory.direct-cause-importance.relation.v3"
  private[chesstory] val PlayerFacingIdeaOrderingPolicyVersion =
    "chesstory.player-facing-idea-ordering.dominance-layers.v1"
  private[chesstory] val VerdictClassificationPolicyVersion =
    VerdictThresholdPolicy.PolicyVersion
  private[chesstory] val CauseDispositionSummaryAuthority =
    "cause_disposition_ledger.v1"

  final case class Result(httpStatus: Int, body: JsObject)

  /** Canonical public codec for the optional, read-only proof compression used
    * by both the runtime response and the audit adapter.
    */
  private[chesstory] def directCauseProofSegmentPublicJson(
      segment: DirectCauseProofSegment
  ): JsObject =
    Json.obj(
      "terminal_relation" -> code(segment.terminalRelation.toString),
      "steps" -> segment.steps.map(step =>
        Json.obj(
          "ply_offset" -> step.plyOffset,
          "move_uci" -> EvidenceRef.normalizeMove(step.moveUci),
          "role" -> code(step.role.toString)
        )
      )
    )

  private def code(value: String): String =
    value.trim
      .replaceAll("([a-z0-9])([A-Z])", "$1_$2")
      .replaceAll("[^A-Za-z0-9]+", "_")
      .replaceAll("_+", "_")
      .stripPrefix("_")
      .stripSuffix("_")
      .toLowerCase

  /** A packet-selected Cause is an invariant, not an optional presentation
    * hint. Projection therefore fails closed instead of silently omitting it.
    */
  private[chesstory] def requireSelectedCauseProjection[A](
      causeEvidenceId: String,
      projected: Option[A]
  ): A =
    projected.getOrElse(
      throw IllegalStateException(
        s"packet-selected Cause cannot be projected from its registered evidence and channels: $causeEvidenceId"
      )
    )

  private[chesstory] def requireSelectedCauseHost[A](
      claimId: String,
      host: Option[A]
  ): A =
    host.getOrElse(
      throw IllegalStateException(
        s"packet-selected Cause host is not a registered packet claim: $claimId"
      )
    )

  /** Public Cause ids are a projection of the ledger's selected terminal
    * state, never an independently filtered subset. Any omission, duplicate,
    * or addition is a projection-boundary failure.
    */
  private[chesstory] def requirePublicIdeaCauseCoverage(
      ledger: CauseDispositionLedger,
      publicCauseIds: List[String]
  ): Unit =
    val canonicalPublicIds = publicCauseIds.map(_.trim).filter(_.nonEmpty)
    if canonicalPublicIds.size != publicCauseIds.size ||
        canonicalPublicIds.distinct.size != canonicalPublicIds.size ||
        canonicalPublicIds.toSet != ledger.selectedCauseEvidenceIds
    then
      throw IllegalStateException(
        "public idea Cause ids must exactly equal the CauseDispositionLedger selected set"
      )

  /** Canonical, judgment-free summary of the terminal Cause ledger. */
  private[chesstory] def causeDispositionPublicJson(
      ledger: CauseDispositionLedger
  ): JsObject =
    val selectedIds = ledger.selectedCauseEvidenceIds.toList.sorted
    val statusCounts = JsObject(
      CauseDispositionStatus.values.toList.map { status =>
        code(status.toString) -> JsNumber(ledger.dispositions.count(_.status == status))
      }
    )
    val reasonCounts = JsObject(
      CauseDispositionReason.values.toList.map { reason =>
        code(reason.toString) -> JsNumber(ledger.dispositions.count(_.reason == reason))
      }
    )
    val abstentionCodes =
      if ledger.dispositions.isEmpty then List("no_relative_cause_generated")
      else if selectedIds.isEmpty then
        ledger.dispositions.map(disposition => code(disposition.reason.toString)).distinct.sorted
      else Nil
    Json.obj(
      "authority" -> CauseDispositionSummaryAuthority,
      "total_cause_count" -> ledger.dispositions.size,
      "selected_cause_ids" -> selectedIds,
      "status_counts" -> statusCounts,
      "reason_counts" -> reasonCounts,
      "abstention_codes" -> abstentionCodes
    )

  /** Empty public meaning keeps engine availability separate from the Cause
    * ledger.  The ledger explains abstention, while only an engine-backed
    * primary comparison licenses the differential-idea status.
    */
  private[chesstory] def emptyMoveMeaningPayloadJson(
      primaryEngineBacked: Boolean,
      ledger: CauseDispositionLedger
  ): JsObject =
    MoveMeaningProjection.emptyPayloadJson(primaryEngineBacked, ledger)

  enum ProjectionPayloadSource(val id: String):
    case Full extends ProjectionPayloadSource("full")
    case Empty extends ProjectionPayloadSource("empty")

  enum ProjectionStatus(val id: String):
    case Ready extends ProjectionStatus("ready")
    case Withheld extends ProjectionStatus("withheld")

  enum ProjectionWithheldReason(val id: String):
    case InsufficientEngineDepth extends ProjectionWithheldReason("insufficient_engine_depth")

  enum VerbalizationStatus:
    case Unavailable

  final case class ProjectionDecision(
      fullPayload: Option[JsObject],
      selectedPayloadSource: ProjectionPayloadSource,
      selectedPayload: JsObject,
      renderable: Boolean,
      status: ProjectionStatus,
      reason: Option[ProjectionWithheldReason],
      publicProbeRequests: List[JsObject]
  )

  final case class ProjectionBoundaryInput(
      requestId: Option[String],
      packet: EvidenceBackedJudgmentPacket
  )

  final case class ProjectionTrace(
      input: ProjectionBoundaryInput,
      primaryEngineBacked: Boolean,
      fullPayload: Option[JsObject],
      selectedPayloadSource: ProjectionPayloadSource,
      selectedPayload: JsObject,
      hasExplanation: Boolean,
      hasEngineMateOutcome: Boolean,
      primaryRenderable: Boolean,
      renderable: Boolean,
      status: ProjectionStatus,
      reason: Option[ProjectionWithheldReason],
      probeRequests: List[ProbeRequest],
      publicProbeRequests: List[JsObject],
      moveReview: JsObject,
      actualDecision: ProjectionDecision,
      finalPublicResponse: Result
  ):
    def decision: ProjectionDecision =
      ProjectionDecision(
        fullPayload = fullPayload,
        selectedPayloadSource = selectedPayloadSource,
        selectedPayload = selectedPayload,
        renderable = renderable,
        status = status,
        reason = reason,
        publicProbeRequests = publicProbeRequests
      )

  final case class RuntimeBoundaryIntervention(
      judgment: JudgmentBoundaryIntervention = JudgmentBoundaryIntervention.identity,
      p: Option[ProjectionBoundaryInput => ProjectionDecision] = None
  )

  object RuntimeBoundaryIntervention:
    val identity: RuntimeBoundaryIntervention = RuntimeBoundaryIntervention()

    def fromJudgment(intervention: JudgmentBoundaryIntervention): RuntimeBoundaryIntervention =
      RuntimeBoundaryIntervention(judgment = intervention)

  final case class BoundaryEvaluationTrace(
      boundaryExecution: Option[JudgmentBoundaryExecution],
      projection: Option[ProjectionTrace],
      verbalization: VerbalizationStatus,
      finalPublicResponse: Result
  ):
    def result: Result = finalPublicResponse

  private final case class WireVariation(
      moves: List[String],
      scoreCp: Int,
      mate: Option[Int],
      depth: Int
  ):
    def toCore: EngineLine = EngineLine(moves, scoreCp, mate, depth)

  private object WireVariation:
    given Reads[WireVariation] = Json.reads[WireVariation]

  private final case class WireProbeResult(
      id: String,
      fen: String,
      replyLines: Option[List[WireVariation]],
      purpose: Option[ProbePurpose],
      probedMove: Option[String],
      depth: Option[Int],
      candidateMove: Option[String],
      opponentResourceMove: Option[String],
      horizon: Option[String],
      variationHash: Option[String],
      generatedAtEpochMs: Option[Long],
      tablebase: Option[EndgameTablebaseEvidence]
  ):
    def toCore: ProbeResult =
      ProbeResult(
        id = id,
        fen = Some(fen),
        replyLines = replyLines.map(_.map(_.toCore)),
        purpose = purpose,
        probedMove = probedMove,
        depth = depth,
        candidateMove = candidateMove,
        opponentResourceMove = opponentResourceMove,
        horizon = horizon,
        variationHash = variationHash,
        generatedAtEpochMs = generatedAtEpochMs,
        tablebase = tablebase
      )

  private object WireProbeResult:
    given Reads[WireProbeResult] = Json.reads[WireProbeResult]

  private final case class WireInput(
      fen: String,
      playedMoveUci: String,
      variations: List[WireVariation],
      ply: Option[Int],
      openingContext: Option[RawOpeningContext],
      movePrefixUci: Option[List[String]],
      probeResults: Option[List[WireProbeResult]]
  ):
    def toCore: RawMoveReviewInput =
      RawMoveReviewInput(
        fen = fen,
        playedMoveUci = playedMoveUci,
        variations = variations.map(_.toCore),
        ply = ply,
        openingContext = openingContext,
        movePrefixUci = movePrefixUci.getOrElse(Nil),
        probeResults = probeResults.getOrElse(Nil).map(_.toCore)
      )

  private object WireInput:
    given Reads[WireInput] = Json.reads[WireInput]

  def evaluate(bytes: Array[Byte]): Result =
    evaluateWithBoundary(bytes, JudgmentBoundaryIntervention.identity).result

  def evaluate(json: JsValue): Result =
    evaluateWithBoundary(json, JudgmentBoundaryIntervention.identity).result

  def evaluateWithBoundary(
      bytes: Array[Byte],
      intervention: JudgmentBoundaryIntervention
  ): BoundaryEvaluationTrace =
    evaluateWithBoundary(bytes, RuntimeBoundaryIntervention.fromJudgment(intervention))

  def evaluateWithBoundary(
      bytes: Array[Byte],
      intervention: RuntimeBoundaryIntervention
  ): BoundaryEvaluationTrace =
    Try(Json.parse(bytes)).fold(
      _ => unavailableTrace(failure(None, 400, "invalid_json")),
      json => evaluateWithBoundary(json, intervention)
    )

  def evaluateWithBoundary(
      json: JsValue,
      intervention: JudgmentBoundaryIntervention
  ): BoundaryEvaluationTrace =
    evaluateWithBoundary(json, RuntimeBoundaryIntervention.fromJudgment(intervention))

  def evaluateWithBoundary(
      json: JsValue,
      intervention: RuntimeBoundaryIntervention
  ): BoundaryEvaluationTrace =
    json match
      case obj: JsObject =>
        (obj \ "request_id").validateOpt[String] match
          case JsError(_) => unavailableTrace(failure(None, 400, "invalid_request_id"))
          case JsSuccess(requestId, _) if !requestId.forall(InputLimits.validRequestId) =>
            unavailableTrace(failure(None, 400, "invalid_request_id"))
          case JsSuccess(requestId, _) =>
            if (obj \ "schema_version").asOpt[String] != Some(RequestSchema) then
              unavailableTrace(failure(requestId, 400, "unsupported_schema_version"))
            else
              (obj \ "input").validate[WireInput] match
                case JsError(_) => unavailableTrace(failure(requestId, 400, "invalid_move_review_input"))
                case JsSuccess(input, _) =>
                  evaluateWithBoundary(requestId, input.toCore, intervention)
      case _ => unavailableTrace(failure(None, 400, "invalid_request"))

  def evaluateWithBoundary(
      raw: RawMoveReviewInput,
      intervention: JudgmentBoundaryIntervention
  ): BoundaryEvaluationTrace =
    evaluateWithBoundary(raw, RuntimeBoundaryIntervention.fromJudgment(intervention))

  def evaluateWithBoundary(
      raw: RawMoveReviewInput,
      intervention: RuntimeBoundaryIntervention
  ): BoundaryEvaluationTrace =
    evaluateWithBoundary(None, raw, intervention)

  def evaluateWithBoundary(
      requestId: Option[String],
      raw: RawMoveReviewInput,
      intervention: JudgmentBoundaryIntervention
  ): BoundaryEvaluationTrace =
    evaluateWithBoundary(requestId, raw, RuntimeBoundaryIntervention.fromJudgment(intervention))

  def evaluateWithBoundary(
      requestId: Option[String],
      raw: RawMoveReviewInput,
      intervention: RuntimeBoundaryIntervention
  ): BoundaryEvaluationTrace =
    if !requestId.forall(InputLimits.validRequestId) then
      unavailableTrace(failure(None, 400, "invalid_request_id"))
    else if !InputLimits.accepts(raw) then
      unavailableTrace(failure(requestId, 400, "input_limits_exceeded"))
    else
      MoveReviewJudgmentOrchestrator.execute(raw, intervention.judgment) match
        case None =>
          unavailableTrace(failure(requestId, 422, "move_review_not_buildable"))
        case Some(execution) if !InputLimits.accepts(execution.q) =>
          unavailableTrace(failure(requestId, 400, "input_limits_exceeded"))
        case Some(execution) =>
          execution.packet match
            case None =>
              BoundaryEvaluationTrace(
                boundaryExecution = Some(execution),
                projection = None,
                verbalization = VerbalizationStatus.Unavailable,
                finalPublicResponse = failure(requestId, 422, "move_review_not_buildable")
              )
            case Some(packet) =>
              project(requestId, execution, packet, intervention.p)

  def publicProbeRequestJson(request: ProbeRequest): JsObject =
    Json.obj(
      "id" -> request.id,
      "fen" -> request.fen,
      "moves" -> request.moves,
      "depth" -> request.depth,
      "purpose" -> request.purpose,
      "multiPv" -> request.multiPv,
      "baselineEvalCp" -> request.baselineEvalCp,
      "horizon" -> request.horizon,
      "candidateMove" -> request.candidateMove,
      "depthFloor" -> request.depthFloor,
      "variationHash" -> request.variationHash,
      "comparisonFen" -> request.comparisonFen
    ) ++ Option
      .when(request.opponentResourceMove.nonEmpty)(Json.obj("opponentResourceMove" -> request.opponentResourceMove))
      .getOrElse(Json.obj())

  private def project(
      requestId: Option[String],
      execution: JudgmentBoundaryExecution,
      packet: EvidenceBackedJudgmentPacket,
      projectionIntervention: Option[ProjectionBoundaryInput => ProjectionDecision]
  ): BoundaryEvaluationTrace =
    val primaryEngineBacked = MoveMeaningProjection.engineBacked(packet)
    val actualDecision = actualProjectionDecision(packet, primaryEngineBacked)
    val projectionInput = ProjectionBoundaryInput(requestId = requestId, packet = packet)
    val actualTrace =
      assembleProjectionTrace(
        input = projectionInput,
        primaryEngineBacked = primaryEngineBacked,
        actualDecision = actualDecision,
        decision = actualDecision
      )
    val acceptedDecision =
      projectionIntervention
        .map(intervention =>
          Try(intervention(projectionInput)).toOption
            .flatMap(Option(_))
        )
        .getOrElse(Some(actualDecision))
        .filter(projectionClosed(_, packet, actualDecision, primaryEngineBacked))
    acceptedDecision match
      case None =>
        BoundaryEvaluationTrace(
          boundaryExecution = Some(execution),
          projection = None,
          verbalization = VerbalizationStatus.Unavailable,
          finalPublicResponse = failure(requestId, 422, "projection_intervention_rejected")
        )
      case Some(decision) =>
        val projection =
          if projectionIntervention.isEmpty then actualTrace
          else
            assembleProjectionTrace(
              input = projectionInput,
              primaryEngineBacked = primaryEngineBacked,
              actualDecision = actualDecision,
              decision = decision
            )
        BoundaryEvaluationTrace(
          boundaryExecution = Some(execution),
          projection = Some(projection),
          verbalization = VerbalizationStatus.Unavailable,
          finalPublicResponse = projection.finalPublicResponse
        )

  private def actualProjectionDecision(
      packet: EvidenceBackedJudgmentPacket,
      primaryEngineBacked: Boolean
  ): ProjectionDecision =
    val fullPayload =
      Option.when(primaryEngineBacked)(MoveMeaningProjection.publicPayloadJson(packet))
    val signals = projectionSignals(primaryEngineBacked, fullPayload)
    val renderable = signals.primaryRenderable
    val (selectedPayloadSource, selectedPayload) =
      if signals.primaryRenderable then ProjectionPayloadSource.Full -> fullPayload.get
      else
        ProjectionPayloadSource.Empty -> emptyMoveMeaningPayloadJson(
          primaryEngineBacked,
          packet.causeDispositionLedger
        )
    val status = if renderable then ProjectionStatus.Ready else ProjectionStatus.Withheld
    val reason =
      Option.unless(primaryEngineBacked)(ProjectionWithheldReason.InsufficientEngineDepth)
    ProjectionDecision(
      fullPayload = fullPayload,
      selectedPayloadSource = selectedPayloadSource,
      selectedPayload = selectedPayload,
      renderable = renderable,
      status = status,
      reason = reason,
      publicProbeRequests = packet.probeRequests.map(publicProbeRequestJson)
    )

  private final case class ProjectionSignals(
      hasExplanation: Boolean,
      hasEngineMateOutcome: Boolean,
      primaryRenderable: Boolean
  )

  private def projectionSignals(
      primaryEngineBacked: Boolean,
      fullPayload: Option[JsObject]
  ): ProjectionSignals =
    val hasExplanation =
      fullPayload.exists(payload =>
        (payload \ "explanations").asOpt[JsArray].exists(_.value.nonEmpty)
      )
    val hasEngineMateOutcome =
      fullPayload.exists(payload =>
        (payload \ "verdict" \ "outcome" \ "state").asOpt[String].exists(state =>
          state == "forced_win" || state == "forced_loss"
        )
      )
    ProjectionSignals(
      hasExplanation = hasExplanation,
      hasEngineMateOutcome = hasEngineMateOutcome,
      // The exact engine-backed comparison owns verdict availability. Cause
      // abstention only makes `ideas` empty; it must not erase the verdict.
      primaryRenderable = primaryEngineBacked
    )

  private def assembleProjectionTrace(
      input: ProjectionBoundaryInput,
      primaryEngineBacked: Boolean,
      actualDecision: ProjectionDecision,
      decision: ProjectionDecision
  ): ProjectionTrace =
    val requestId = input.requestId
    val packet = input.packet
    val signals = projectionSignals(primaryEngineBacked, decision.fullPayload)
    val moveReview = Json.obj("renderable" -> decision.renderable) ++ decision.selectedPayload
    val finalPublicResponse =
      Result(
        200,
        Json.obj(
          "schema_version" -> ResponseSchema,
          "request_id" -> requestId,
          "ok" -> true,
          "status" -> decision.status.id,
          "availability" -> Json.obj("state" -> decision.status.id, "reason" -> decision.reason.map(_.id)),
          "probe_requests" -> decision.publicProbeRequests,
          "move_review" -> moveReview
        )
      )
    ProjectionTrace(
      input = input,
      primaryEngineBacked = primaryEngineBacked,
      fullPayload = decision.fullPayload,
      selectedPayloadSource = decision.selectedPayloadSource,
      selectedPayload = decision.selectedPayload,
      hasExplanation = signals.hasExplanation,
      hasEngineMateOutcome = signals.hasEngineMateOutcome,
      primaryRenderable = signals.primaryRenderable,
      renderable = decision.renderable,
      status = decision.status,
      reason = decision.reason,
      probeRequests = packet.probeRequests,
      publicProbeRequests = decision.publicProbeRequests,
      moveReview = moveReview,
      actualDecision = actualDecision,
      finalPublicResponse = finalPublicResponse
    )

  private def projectionClosed(
      decision: ProjectionDecision,
      packet: EvidenceBackedJudgmentPacket,
      actualDecision: ProjectionDecision,
      primaryEngineBacked: Boolean
  ): Boolean =
    // An intervention may choose which already-certified packet projection to
    // expose (or a probe subset), but it cannot manufacture a second public
    // meaning DTO. The full payload is derived once from the packet by the
    // production projector and then used as the closure authority.
    val packetPayloadsClosed = decision.fullPayload == actualDecision.fullPayload
    val selectedPayloadClosed =
      decision.selectedPayloadSource match
        case ProjectionPayloadSource.Full =>
          decision.fullPayload.contains(decision.selectedPayload)
        case ProjectionPayloadSource.Empty =>
          decision.selectedPayload == emptyMoveMeaningPayloadJson(
            primaryEngineBacked,
            packet.causeDispositionLedger
          )
    val signals =
      projectionSignals(primaryEngineBacked, decision.fullPayload)
    val selectedRenderable =
      decision.selectedPayloadSource match
        case ProjectionPayloadSource.Full  => signals.primaryRenderable
        case ProjectionPayloadSource.Empty => false
    val expectedReason =
      Option.unless(selectedRenderable)(ProjectionWithheldReason.InsufficientEngineDepth)
    val availabilityClosed =
      decision.status match
        case ProjectionStatus.Ready =>
          decision.renderable && selectedRenderable && decision.reason.isEmpty &&
            decision.selectedPayloadSource != ProjectionPayloadSource.Empty
        case ProjectionStatus.Withheld =>
          !decision.renderable && !signals.primaryRenderable &&
            decision.reason == expectedReason &&
            decision.selectedPayloadSource == ProjectionPayloadSource.Empty
    val payloadsClosed =
      decision.fullPayload.forall(payload => !payload.keys.contains("renderable")) &&
        !decision.selectedPayload.keys.contains("renderable")
    packetPayloadsClosed &&
      selectedPayloadClosed &&
      availabilityClosed &&
      payloadsClosed &&
      publicProbeProjectionClosed(packet.probeRequests, decision.publicProbeRequests)

  private def publicProbeProjectionClosed(
      nativeProbeRequests: List[ProbeRequest],
      publicProbeRequests: List[JsObject]
  ): Boolean =
    val projectedIds = publicProbeRequests.flatMap(json => (json \ "id").asOpt[String])
    val nativeIds = nativeProbeRequests.map(_.id)
    val nativeById = nativeProbeRequests.map(request => request.id -> request).toMap
    projectedIds.size == publicProbeRequests.size &&
      projectedIds.distinct.size == projectedIds.size &&
      projectedIds == nativeIds.filter(projectedIds.toSet) &&
      publicProbeRequests.zip(projectedIds).forall { case (json, id) =>
        nativeById.get(id).exists { request =>
          val allowedProjection = publicProbeRequestJson(request)
          json == allowedProjection
        }
      }

  private def unavailableTrace(result: Result): BoundaryEvaluationTrace =
    BoundaryEvaluationTrace(
      boundaryExecution = None,
      projection = None,
      verbalization = VerbalizationStatus.Unavailable,
      finalPublicResponse = result
    )

  private[runtime] def failure(requestId: Option[String], httpStatus: Int, error: String): Result =
    Result(
      httpStatus,
      Json.obj(
        "schema_version" -> ResponseSchema,
        "request_id" -> requestId,
        "ok" -> false,
        "status" -> "error",
        "error" -> error
      )
    )

  /** Canonical public projection of R's typed importance result. Evaluation
    * adapters reuse this codec so packet and public observations cannot drift.
    */
  private[chesstory] def directCauseImportancePublicJson(
      resolution: DirectCauseImportanceResolution
  ): JsObject =
    val comparableProfilePairs = resolution.relations.count(
      _.relation != DirectCauseImportanceRelation.Incomparable
    )
    Json.obj(
      "authority" -> "root_owned_effect_partial_order",
      "relation_policy_version" -> DirectCauseImportanceRelationPolicyVersion,
      "ordering_policy_version" -> PlayerFacingIdeaOrderingPolicyVersion,
      "comparison_exposure_rank_meaning" -> "comparison_exposure_authority",
      "selected_cause_ids" -> resolution.selectedCauseEvidenceIds,
      "frontier_cause_ids" -> resolution.frontierCauseEvidenceIds,
      "dominated_within_domain_cause_ids" -> resolution.dominatedWithinDomainCauseEvidenceIds,
      "unique_top" -> resolution.uniqueTopCauseEvidenceId.fold[JsValue](JsNull)(JsString.apply),
      "profile_summary" -> Json.obj(
        "measured_channels" -> resolution.profiles.size,
        "measured_causes" -> resolution.profiles.map(_.causeEvidenceId).distinct.size,
        "fully_measured_causes" -> resolution.decisions.count(_.fullyMeasured),
        "unmeasured_causes" -> resolution.decisions.count(_.unmeasuredChannelSignatures.nonEmpty)
      ),
      "profiles" -> resolution.profiles.map(directCauseImportanceProfileJson),
      "relations" -> resolution.relations.map { relation =>
        Json.obj(
          "left_cause_id" -> relation.leftCauseEvidenceId,
          "left_causal_signature" -> relation.leftCausalSignature,
          "right_cause_id" -> relation.rightCauseEvidenceId,
          "right_causal_signature" -> relation.rightCausalSignature,
          "relation" -> importanceCode(relation.relation.toString),
          "domain" -> relation.domainKey
        )
      },
      "decisions" -> resolution.decisions.map { decision =>
        Json.obj(
          "cause_evidence_id" -> decision.causeEvidenceId,
          "measured_channel_signatures" -> decision.measuredChannelSignatures,
          "unmeasured_channel_signatures" -> decision.unmeasuredChannelSignatures,
          "dominating_cause_ids" -> decision.dominatingCauseEvidenceIds,
          "dominated_within_domain" -> decision.dominatedWithinDomain,
          "fully_measured" -> decision.fullyMeasured
        )
      },
      "relation_summary" -> Json.obj(
        "comparable_profile_pairs" -> comparableProfilePairs,
        "incomparable_profile_pairs" -> resolution.incomparableProfilePairCount
      ),
      "unmeasured" -> resolution.decisions
        .filter(_.unmeasuredChannelSignatures.nonEmpty)
        .map(decision =>
          Json.obj(
            "cause_evidence_id" -> decision.causeEvidenceId,
            "channel_count" -> decision.unmeasuredChannelSignatures.size
          )
        )
    )

  private def directCauseImportanceProfileJson(
      profile: DirectCauseImportanceProfile
  ): JsObject =
    Json.obj(
      "cause_evidence_id" -> profile.causeEvidenceId,
      "causal_signature" -> profile.causalSignature,
      "universe" -> directCauseImportanceUniverseJson(profile.universe),
      "domain_kind" -> directCauseImportanceDomainKind(profile.domain),
      "domain" -> profile.domain.stableKey,
      "stake" -> profile.frame.stake.stableKey,
      "event_line" -> profile.frame.eventLine.stableKey,
      "effect_mode" -> importanceCode(profile.frame.effectMode.toString),
      "direct_change" -> importanceCode(profile.frame.directChange.toString),
      "played_change" -> importanceCode(profile.frame.playedChange.toString),
      "measure" -> directCauseImportanceMeasureJson(profile.measure),
      "effect_scope" -> rootOwnedEffectIdentityJson(profile.effectIdentity)
    )

  private[chesstory] def directCauseImportanceUniverseJson(
      universe: DirectCauseImportanceUniverse
  ): JsObject =
    Json.obj(
      "root_board_state" -> universe.rootBoardState,
      "impact" -> universe.impact.stableKey
    )

  private[chesstory] def directCauseImportanceDomainKind(
      domain: DirectCauseImportanceDomain
  ): String =
    domain match
      case DirectCauseImportanceDomain.BoardMate                 => "board_mate"
      case DirectCauseImportanceDomain.Material                  => "material"
      case DirectCauseImportanceDomain.Threat(_, _)               => "threat"
      case DirectCauseImportanceDomain.Structural(_, _, _, _)     => "structural"

  private[chesstory] def directCauseImportanceMeasureJson(
      measure: DirectCauseImportanceMeasure
  ): JsObject =
    measure match
      case DirectCauseImportanceMeasure.MateArrival(plyOffset) =>
        Json.obj("kind" -> "mate_arrival", "ply_offset" -> plyOffset)
      case DirectCauseImportanceMeasure.MaterialOutcome(durableNetCp, onsetPlyOffset) =>
        Json.obj(
          "kind" -> "material_outcome",
          "durable_net_cp" -> durableNetCp,
          "onset_ply_offset" -> onsetPlyOffset
        )
      case DirectCauseImportanceMeasure.ThreatHorizon(turnsToImpact) =>
        Json.obj("kind" -> "threat_horizon", "turns_to_impact" -> turnsToImpact)
      case DirectCauseImportanceMeasure.StructuralStrength(units) =>
        Json.obj("kind" -> "structural_strength", "units" -> units)
      case DirectCauseImportanceMeasure.StrategicStrength(units) =>
        Json.obj("kind" -> "strategic_strength", "units" -> units)

  /** Exact typed effect measured from this channel's own root proof. Both the
    * runtime response and the audit adapter use this single projection so a
    * profile cannot be rebound by reinterpreting descriptor text.
    */
  private[chesstory] def directCauseMeasuredEffectPublicJson(
      channel: DirectCauseChannel
  ): Option[JsObject] =
    DirectCauseMeasuredEffect.fromChannel(channel).map { effect =>
      Json.obj(
        "stake" -> effect.stake.stableKey,
        "domain_kind" -> directCauseImportanceDomainKind(effect.domain),
        "domain" -> effect.domain.stableKey,
        "measure" -> directCauseImportanceMeasureJson(effect.measure),
        "effect_scope" -> rootOwnedEffectIdentityJson(effect.effectIdentity)
      )
    }

  private[chesstory] def directEffectAdmissionPublicJson(
      admission: DirectEffectAdmission
  ): JsObject =
    val status = admission match
      case DirectEffectAdmission.Unresolved    => "unresolved"
      case DirectEffectAdmission.Restricted(_) => "restricted"
    Json.obj(
      "status" -> status,
      "causal_signatures" -> admission.admittedCausalSignatures.toList.sorted
    )

  private[chesstory] def rootOwnedEffectDescriptorPublicJson(
      descriptor: RootOwnedEffectDescriptor
  ): JsObject =
    Json.obj(
      "effect_scope" -> rootOwnedEffectIdentityJson(descriptor.identity),
      "magnitude_status" -> directEffectMagnitudeStatus(descriptor.magnitude),
      "measure" -> descriptor.exactMagnitude.map(directCauseImportanceMeasureJson),
      "material_event_salience" -> descriptor.materialEventSalience.map(rootOwnedMaterialEventSalienceJson)
    )

  private[chesstory] def rootOwnedMaterialEventSalienceJson(
      event: RootOwnedMaterialEventSalience
  ): JsObject =
    Json.obj(
      "move_uci" -> event.moveUci,
      "ply_offset" -> event.plyOffset,
      "captured_role" -> event.capturedRole.name,
      "square" -> event.square.key,
      "target_value_cp" -> event.targetValueCp
    )

  private[chesstory] def directEffectMagnitudeStatus(
      magnitude: DirectEffectMagnitudeKnowledge
  ): String =
    magnitude match
      case DirectEffectMagnitudeKnowledge.NotApplicable     => "not_applicable"
      case _: DirectEffectMagnitudeKnowledge.Exact          => "exact"
      case DirectEffectMagnitudeKnowledge.ExpectedButMissing => "expected_but_missing"
      case DirectEffectMagnitudeKnowledge.Ambiguous         => "ambiguous"

  private[chesstory] def rootOwnedEffectIdentityJson(
      identity: RootOwnedEffectIdentity
  ): JsObject =
    Json.obj(
      "primitive_kind" -> importanceCode(identity.primitiveKind.toString),
      "target_signatures" -> identity.targetSignatures,
      "plan_ids" -> identity.planIds,
      "strategic_axes" -> identity.strategicAxes.map(axis =>
        Json.obj(
          "kind" -> importanceCode(axis.kind.toString),
          "polarity" -> importanceCode(axis.polarity.toString),
          "label" -> axis.label,
          "comparison_outcome" -> axis.comparisonOutcome.map(value => importanceCode(value.toString))
        )
      )
    )

  private def importanceCode(value: String): String =
    value.trim
      .replaceAll("([a-z0-9])([A-Z])", "$1_$2")
      .replaceAll("[^A-Za-z0-9]+", "_")
      .replaceAll("_+", "_")
      .stripPrefix("_")
      .stripSuffix("_")
      .toLowerCase

  /** The exact engine-backed PlayedVsBest verdict is a comparison DTO. It is
    * intentionally independent of Cause generation, claim hosting, and public
    * idea selection so a sentence-ready verdict survives legitimate Cause
    * abstention.
    */
  private[chesstory] def primaryEngineBackedVerdictPublicJson(
      packet: EvidenceBackedJudgmentPacket
  ): Option[JsObject] =
    MoveMeaningProjection.engineBackedVerdictJson(packet)

  /** Audit-only central classification authority. This is recomputed from the
    * registered PlayedVsBest line nodes through VerdictThresholdPolicy; it is
    * not copied from a public verdict label or any Cause/claim evidence.
    */
  private[chesstory] def primaryEngineBackedVerdictClassificationAuthorityJson(
      packet: EvidenceBackedJudgmentPacket
  ): Option[JsObject] =
    MoveMeaningProjection.engineBackedClassificationAuthorityJson(packet)

  private object MoveMeaningProjection:
    private final case class PrimaryComparison(
        assessment: RelativeMoveAssessment,
        fact: CandidateComparisonFact
    )

    private def engineBackedPrimaryComparison(
        packet: EvidenceBackedJudgmentPacket
    ): Option[PrimaryComparison] =
      primaryComparison(packet).filter { primary =>
        packet.evidenceGraph.candidateComparisonRecord(primary.assessment.primaryComparisonEvidence).exists {
          case EvidenceRecord(ref, CandidateComparisonEvidence(fact), _) =>
            ref.confidence == EvidenceConfidence.EngineBacked &&
              fact == primary.fact &&
              recomputedComparison(primary) == primary.fact.comparison
          case _ =>
            false
        }
      }

    private def recomputedComparison(primary: PrimaryComparison): EvalComparison =
      primary.fact.recomputedComparison(
        reference = primary.assessment.reference,
        candidate = primary.assessment.candidate
      )

    def engineBacked(packet: EvidenceBackedJudgmentPacket): Boolean =
      engineBackedPrimaryComparison(packet).nonEmpty

    def engineBackedVerdictJson(packet: EvidenceBackedJudgmentPacket): Option[JsObject] =
      engineBackedPrimaryComparison(packet).map(primary =>
        verdictJson(primary.assessment, primary.fact)
      )

    def engineBackedClassificationAuthorityJson(
        packet: EvidenceBackedJudgmentPacket
    ): Option[JsObject] =
      engineBackedPrimaryComparison(packet).map { primary =>
        val comparison = recomputedComparison(primary)
        val referenceMate = PerspectiveMath.mateForMover(
          comparison.mover,
          primary.assessment.reference.mate
        )
        val playedMate = PerspectiveMath.mateForMover(
          comparison.mover,
          primary.assessment.candidate.mate
        )
        Json.obj(
          "policy_version" -> VerdictClassificationPolicyVersion,
          "comparison_kind" -> code(primary.fact.kind.toString),
          "mover" -> (if comparison.mover.white then "white" else "black"),
          "played_move" -> primary.assessment.played.moveUci,
          "reference_move" -> primary.assessment.reference.ref.rootMove,
          "candidate_set_type" -> primary.fact.candidateSetType.map(item => code(item.toString)),
          "candidate_win_percent_delta_for_mover" -> comparison.candidateWinPercentDeltaForMover,
          "win_percent_loss_for_mover" -> comparison.winPercentLossForMover,
          "expected_verdict_code" -> code(comparison.verdict.toString),
          "expected_move_quality" -> quality(comparison.verdict),
          "reference_mate_for_mover" -> referenceMate,
          "played_mate_for_mover" -> playedMate,
          "mate_distance_loss_for_mover" -> PerspectiveMath.mateDistanceLoss(
            referenceMate,
            playedMate
          )
        )
      }

    def publicPayloadJson(packet: EvidenceBackedJudgmentPacket): JsObject =
      val claims = packet.claims
      val primary = engineBackedPrimaryComparison(packet)
      val assessment = primary.map(_.assessment)
      val playedLine = packet.candidateLine(LineNodeRole.Played)
      val referenceLine = packet.candidateLine(LineNodeRole.BestReference)
      val playedMove =
        assessment.map(_.played.moveUci).orElse(playedLine.map(_.ref.rootMove)).getOrElse("")
      val referenceMove =
        assessment.map(_.reference.ref.rootMove).orElse(referenceLine.map(_.ref.rootMove)).getOrElse("")
      val moveQuality = primary.map(value => quality(value.fact.comparison.verdict)).getOrElse("unknown")
      val claimsById = claims.map(claim => claim.id -> claim).toMap
      val ideaUnits = packet.causeExposureResolution.ideaUnits.sortBy(_.serializationOrder)
      val ideaUnitByCauseId = ideaUnits.flatMap(unit =>
        unit.memberCauseEvidenceIds.map(_ -> unit)
      ).toMap
      val hostedSelections = packet.playerFacingClaimDecisions.flatMap { decision =>
        val claim = requireSelectedCauseHost(decision.claimId, claimsById.get(decision.claimId))
        decision.causeSelections.map(selection => (claim, decision, selection))
      }.sortBy(_._3.selectionOrder)
      val ideas = hostedSelections.map { case (claim, decision, selection) =>
        val unit = ideaUnitByCauseId.getOrElse(
          selection.causeEvidence.id,
          throw IllegalStateException(
            s"packet-selected Cause has no sentence-ready idea unit: ${selection.causeEvidence.id}"
          )
        )
        selectedCauseIdeaJson(claim, decision, selection, unit, packet)
      }
      val importance = directCauseImportancePublicJson(packet.directCauseImportanceResolution)
      val ideaImportance = directCauseImportancePublicJson(
        packet.causeExposureResolution.ideaImportanceResolution
      )
      val explanation =
        Option.when(playedMove.nonEmpty && ideas.nonEmpty)(
          Json.obj(
            "role" -> "played move",
            "move" -> playedMove,
            "reference_move" -> referenceMove,
            "move_quality" -> moveQuality,
            "ideas" -> ideas,
            "idea_units" -> ideaUnits.map(playerFacingIdeaUnitJson),
            "idea_importance" -> ideaImportance,
            "importance" -> importance
          )
        )
      val publicIdeaCauseIds =
        Option.when(explanation.nonEmpty)(hostedSelections.map(_._3.causeEvidence.id)).getOrElse(Nil)
      requirePublicIdeaCauseCoverage(packet.causeDispositionLedger, publicIdeaCauseIds)
      val dispositionSummary = causeDispositionPublicJson(packet.causeDispositionLedger)
      Json.obj(
        "verdict" -> engineBackedVerdictJson(packet),
        "idea_status" -> (
          if publicIdeaCauseIds.nonEmpty then "certified"
          else "no_certified_differential_idea"
        ),
        "idea_status_detail" -> dispositionSummary,
        "explanations" -> explanation.toList
      )

    def emptyPayloadJson(
        primaryEngineBacked: Boolean,
        ledger: CauseDispositionLedger
    ): JsObject =
      // With an engine-backed primary comparison, an empty idea projection is
      // a Cause abstention and therefore requires an empty selected ledger.
      // Without that primary authority the whole projection is withheld:
      // secondary-comparison Causes may still be selected at R and must remain
      // visible in the diagnostic ledger summary rather than being deleted or
      // rejected merely to make an unavailable public payload empty.
      if primaryEngineBacked then requirePublicIdeaCauseCoverage(ledger, Nil)
      Json.obj(
        "verdict" -> JsNull,
        "idea_status" -> (
          if primaryEngineBacked then "no_certified_differential_idea"
          else "unavailable"
        ),
        "idea_status_detail" -> causeDispositionPublicJson(ledger),
        "explanations" -> Json.arr()
      )

    private def primaryComparison(
        packet: EvidenceBackedJudgmentPacket
    ): Option[PrimaryComparison] =
      for
        assessment <- packet.primaryRelativeAssessment
        fact <- packet.evidenceGraph.comparisonFor(assessment)
        played <- packet.candidateLine(LineNodeRole.Played)
        reference <- packet.candidateLine(LineNodeRole.BestReference)
        if assessment.candidate == played
        if assessment.reference == reference
        if fact.kind == CandidateComparisonKind.PlayedVsBest
        if fact.referenceLine == reference.ref
        if fact.candidateLine == played.ref
      yield PrimaryComparison(assessment, fact)

    private def verdictJson(
        assessment: RelativeMoveAssessment,
      fact: CandidateComparisonFact
    ): JsObject =
      val comparison = fact.comparison
      val referenceMate = PerspectiveMath.mateForMover(comparison.mover, assessment.reference.mate)
      val playedMate = PerspectiveMath.mateForMover(comparison.mover, assessment.candidate.mate)
      val mateDistanceLoss = PerspectiveMath.mateDistanceLoss(referenceMate, playedMate)
      Json.obj(
        "comparison_kind" -> code(fact.kind.toString),
        "mover" -> (if comparison.mover.white then "white" else "black"),
        "verdict_code" -> code(comparison.verdict.toString),
        "move_quality" -> quality(comparison.verdict),
        "played_move" -> assessment.played.moveUci,
        "reference_move" -> assessment.reference.ref.rootMove,
        "candidate_win_percent_delta_for_mover" -> comparison.candidateWinPercentDeltaForMover,
        "win_percent_loss_for_mover" -> comparison.winPercentLossForMover,
        "outcome" -> playedMate.map(mate =>
          Json.obj(
            "state" -> mateState(mate),
            "certainty" -> "engine_mate_score",
            "reference_state" -> referenceMate.map(mateState),
            "resistance" -> resistance(referenceMate, playedMate, mateDistanceLoss)
          )
        ),
        "mate" -> Option.when(referenceMate.nonEmpty || playedMate.nonEmpty)(
          Json.obj(
            "reference_for_mover" -> referenceMate,
            "played_for_mover" -> playedMate,
            "distance_loss" -> mateDistanceLoss
          )
        )
      )

    private def selectedCauseIdeaJson(
        claim: JudgmentClaim,
        decision: PlayerFacingClaimDecision,
        selection: PlayerFacingCauseSelection,
        ideaUnit: PlayerFacingIdeaUnit,
        packet: EvidenceBackedJudgmentPacket
    ): JsObject =
      val graph = packet.evidenceGraph
      val projected = for
        record <- graph.record(selection.causeEvidence)
        cause <- record.payload match
          case RelativeCauseFactEvidence(value) => Some(value)
          case _                                => None
        comparison <- graph.comparisonFor(cause)
        binding <- graph.relativeCauseBinding(cause)
        channels = selection.channels.flatMap(selected =>
          packet.directCauseChannel(selection, selected).map(channel => selected -> channel)
        )
        if channels.size == selection.channels.size && channels.nonEmpty
      yield Json.obj(
        "cause_evidence_id" -> selection.causeEvidence.id,
        "item_role" -> "cause_facet",
        "idea_unit_id" -> ideaUnit.id,
        "comparison_exposure_rank" -> selection.comparisonExposureRank,
        "selection_order" -> selection.selectionOrder,
        "host" -> Json.obj(
          "claim_id" -> claim.id,
          "family" -> code(claim.family.toString),
          "tier" -> code(decision.tier.toString)
        ),
        "cause" -> Json.obj(
          "kind" -> code(cause.kind.toString),
          "effect_mode" -> code(selection.effectMode.toString),
          "exposure" -> code(selection.exposure.toString),
          "source_side" -> code(cause.sourceSide.toString),
          "direct_effect_admission" -> directEffectAdmissionPublicJson(cause.directEffectAdmission),
          "attribution" -> Json.obj(
            "kind" -> code(cause.attribution.kind.toString),
            "root_move_matched" -> cause.attribution.rootMoveMatched,
            "direct_proof_eligible" -> cause.attribution.directProofEligible
          )
        ),
        "comparison" -> comparisonJson(cause, comparison, binding),
        "channels" -> channels.map { case (selected, channel) =>
          channelJson(selected, channel)
        },
        "only_move" -> selection.onlyMoveQualifier.map(onlyMoveQualifierJson)
      )
      requireSelectedCauseProjection(selection.causeEvidence.id, projected)

    private def playerFacingIdeaUnitJson(unit: PlayerFacingIdeaUnit): JsObject =
      Json.obj(
        "idea_id" -> unit.id,
        "kind" -> code(unit.kind.toString),
        "lead_cause_evidence_id" -> unit.leadCauseEvidenceId,
        "member_cause_evidence_ids" -> unit.memberCauseEvidenceIds,
        "importance_layer" -> unit.importanceLayer,
        "priority_status" -> code(unit.priorityStatus.toString),
        "serialization_order" -> unit.serializationOrder
      )

    private def comparisonJson(
        cause: RelativeCauseFact,
        comparison: CandidateComparisonFact,
        binding: RelativeCauseBinding
    ): JsObject =
      val eventLine = binding.eventLine
      val comparedMove =
        if eventLine == comparison.referenceLine then comparison.candidateLine.rootMove
        else comparison.referenceLine.rootMove
      Json.obj(
        "evidence_id" -> cause.comparisonEvidence.id,
        "kind" -> code(comparison.kind.toString),
        "reference" -> lineRefJson(comparison.referenceLine),
        "candidate" -> lineRefJson(comparison.candidateLine),
        "event" -> lineRefJson(eventLine),
        "compared_move" -> EvidenceRef.normalizeMove(comparedMove),
        "verdict" -> code(comparison.comparison.verdict.toString),
        "mover" -> (if comparison.comparison.mover.white then "white" else "black"),
        "delta" -> Json.obj(
          "candidate_win_percent_delta_for_mover" -> comparison.comparison.candidateWinPercentDeltaForMover,
          "win_percent_loss_for_mover" -> comparison.comparison.winPercentLossForMover
        )
      )

    private def channelJson(
        selected: PlayerFacingCauseChannelSelection,
        channel: DirectCauseChannel
    ): JsObject =
      val binding = channel.binding
      Json.obj(
        "causal_signature" -> selected.causalSignature,
        "direct_change" -> code(selected.directChange.toString),
        "played_change" -> code(selected.playedChange.toString),
        "carrier" -> evidenceRefJson(binding.source),
        "provenance" -> binding.provenance.map(evidenceRefJson),
        "actor" -> rootActorJson(binding),
        "targets" -> objectArrayJson(binding.target),
        "mechanisms" -> objectArrayJson(binding.mechanism),
        "consequences" -> objectArrayJson(binding.consequence),
        "witnesses" -> objectArrayJson(binding.witness),
        "line" -> binding.line.map(lineRefJson),
        "horizon" -> binding.horizon,
        "proof_segment" -> channel.proofSegment.map(directCauseProofSegmentPublicJson),
        "effect_descriptor" -> channel.rootOwnedEffectDescriptor.map(rootOwnedEffectDescriptorPublicJson),
        "importance_effect" -> directCauseMeasuredEffectPublicJson(channel),
        "descriptor_ambiguous" -> channel.importanceDescriptorAmbiguous,
        "proof_segment_ambiguous" -> channel.proofSegmentAmbiguous
      )

    private def rootActorJson(binding: EvidenceObjectBinding): JsObject =
      val rootMove = binding.line.map(_.rootMove).map(EvidenceRef.normalizeMove).getOrElse("")
      def first(kind: EvidenceObjectKind): Option[String] =
        binding.actor.find(_.kind == kind).map(_.key.trim.toLowerCase)
      Json.obj(
        "move" -> rootMove,
        "side" -> first(EvidenceObjectKind.Side),
        "piece" -> first(EvidenceObjectKind.Piece),
        "from" -> Option.when(rootMove.length >= 4)(rootMove.take(2)),
        "to" -> Option.when(rootMove.length >= 4)(rootMove.slice(2, 4))
      )

    private def objectArrayJson(objects: List[ConcreteChessObject]): List[JsObject] =
      objects
        .distinctBy(_.signaturePart)
        .sortBy(_.signaturePart)
        .map(obj => Json.obj("kind" -> code(obj.kind.toString), "key" -> obj.key))

    private def evidenceRefJson(ref: EvidenceRef): JsObject =
      Json.obj(
        "id" -> ref.id,
        "producer" -> code(ref.producer.toString),
        "layer" -> code(ref.layer.toString),
        "scope" -> code(ref.scope.toString),
        "line" -> ref.line.map(lineRefJson)
      )

    private def lineRefJson(line: LineNodeRef): JsObject =
      Json.obj(
        "id" -> line.id,
        "root_move" -> EvidenceRef.normalizeMove(line.rootMove),
        "role" -> code(line.role.toString),
        "rank" -> line.rank
      )

    private def onlyMoveQualifierJson(qualifier: OnlyMoveCauseQualifier): JsObject =
      Json.obj(
        "comparison_evidence_id" -> qualifier.comparisonEvidence.id,
        "cause_evidence_id" -> qualifier.causeEvidence.id,
        "reference" -> lineRefJson(qualifier.referenceLine),
        "relation" -> code(qualifier.relation.toString),
        "licenses_causal_because" -> qualifier.licensesCausalBecause
      )

    private def quality(verdict: MoveChoiceVerdict): String =
      if verdict.isActionableLoss then "bad"
      else
        verdict match
          case MoveChoiceVerdict.ImprovesOnReference | MoveChoiceVerdict.MatchesReference => "good"
          case MoveChoiceVerdict.PlayableLoss                                             => "playable"
          case _ =>
            throw IllegalStateException(s"non-actionable verdict has no public quality: $verdict")

    private def mateState(mate: Int): String =
      if mate > 0 then "forced_win"
      else if mate < 0 then "forced_loss"
      else throw IllegalStateException("engine mate score must be non-zero")

    private def resistance(
        referenceMate: Option[Int],
        playedMate: Option[Int],
        distanceLoss: Option[Int]
    ): Option[String] =
      referenceMate.zip(playedMate).collect {
        case (reference, played) if reference < 0 && played < 0 && distanceLoss.exists(_ > 0) =>
          "shortened"
        case (reference, played) if reference < 0 && played < 0 && played.abs > reference.abs =>
          "extended"
        case (reference, played) if reference < 0 && played < 0 =>
          "held"
      }

    private def label(value: String): String =
      code(value).replace('_', ' ')

  private object InputLimits:
    private val MaxFenLength = 128
    private val MaxVariations = 16
    private val MaxPvMoves = 80
    private val MaxMovePrefix = 600
    private val MaxProbeResults = 12
    private val MaxReplyLines = 8
    private val MaxTablebaseMoves = 256
    private val MaxDepth = 100
    private val MaxPly = 1000
    private val MaxEval = 100000L
    private val MaxMate = 1000L
    private val MaxText = 256
    private val Uci = "[a-h][1-8][a-h][1-8][nbrq]?".r
    private val RequestId = "[A-Za-z0-9._:-]{1,128}".r

    def validRequestId(value: String): Boolean = RequestId.matches(value)

    def accepts(raw: RawMoveReviewInput): Boolean =
      val lines = raw.variations ++ raw.probeResults.flatMap(_.replyLines.getOrElse(Nil))
      raw.fen.nonEmpty && raw.fen.length <= MaxFenLength &&
        validUci(raw.playedMoveUci) &&
        raw.variations.nonEmpty && raw.variations.size <= MaxVariations &&
        raw.movePrefixUci.size <= MaxMovePrefix && raw.movePrefixUci.forall(validUci) &&
        raw.probeResults.size <= MaxProbeResults &&
        raw.probeResults.map(_.id).distinct.size == raw.probeResults.size &&
        raw.probeResults.forall(validProbe) &&
        raw.ply.forall(ply => ply >= 0 && ply <= MaxPly) &&
        raw.openingContext.forall(validOpening) &&
        lines.forall(validLine)

    private def validLine(line: EngineLine): Boolean =
      line.moves.nonEmpty && line.moves.size <= MaxPvMoves && line.moves.forall(validUci) &&
        bounded(line.scoreCp, MaxEval) && line.mate.forall(mate => mate != 0 && bounded(mate, MaxMate)) &&
        line.depth >= 0 && line.depth <= MaxDepth

    private def validProbe(probe: ProbeResult): Boolean =
      probe.id.nonEmpty && probe.id.length <= MaxText &&
        probe.fen.exists(fen => fen.nonEmpty && fen.length <= MaxFenLength) &&
        probe.replyLines.forall(_.size <= MaxReplyLines) &&
        probe.depth.forall(depth => depth >= 0 && depth <= MaxDepth) &&
        probe.probedMove.forall(validUci) && probe.candidateMove.forall(validUci) &&
        probe.opponentResourceMove.forall(validUci) &&
        probe.horizon.forall(value => value.length <= MaxText && ProbeHorizon.plyOffset(value).nonEmpty) &&
        probe.variationHash.forall(hash => hash.nonEmpty && hash.length <= MaxText) &&
        probe.tablebase.forall(validTablebase)

    private def validTablebase(tablebase: EndgameTablebaseEvidence): Boolean =
      val moves = tablebase.legalMoves
      validTablebasePosition(tablebase.terminal) && validTablebasePosition(tablebase.comparison) &&
        moves.nonEmpty && moves.size <= MaxTablebaseMoves &&
        moves.map(_.moveUci.trim.toLowerCase).distinct.size == moves.size &&
        moves.forall(move => validUci(move.moveUci) && validWdl(move.resultingWdl) && move.resultingDtm.forall(validDtm))

    private def validTablebasePosition(position: lila.chessjudgment.model.TablebasePositionEvidence): Boolean =
      position.fen.nonEmpty && position.fen.length <= MaxFenLength && validWdl(position.wdl) && position.dtm.forall(validDtm)

    private def validWdl(wdl: Int): Boolean = wdl >= -2 && wdl <= 2

    private def validDtm(dtm: Int): Boolean = dtm != 0 && bounded(dtm, MaxMate)

    private def validOpening(opening: RawOpeningContext): Boolean =
      List(opening.eco, opening.name, opening.family).flatten.forall(_.length <= MaxText)

    private def validUci(value: String): Boolean = Uci.matches(value.trim.toLowerCase)

    private def bounded(value: Int, limit: Long): Boolean = value.toLong.abs <= limit

object RuntimeResources:
  private val ThemePriorEntries = 43

  def verify(): Unit =
    require(
      OpeningRecognitionIndex.default.allEntries.nonEmpty,
      "scalachess opening database is empty"
    )
    require(
      OpeningThemePriorIndex.default.allEntries.size == ThemePriorEntries,
      "opening theme-prior resource is missing or incomplete"
    )
