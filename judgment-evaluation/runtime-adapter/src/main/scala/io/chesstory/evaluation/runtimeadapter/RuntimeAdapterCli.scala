package io.chesstory.evaluation.runtimeadapter

import java.io.{ BufferedReader, InputStreamReader, PrintWriter }
import java.nio.charset.StandardCharsets

import scala.util.control.NonFatal

import io.chesstory.runtime.RuntimeProtocol
import play.api.libs.json.{ JsNull, JsObject, JsValue, Json }

object RuntimeAdapterCli:
  private val ObservationSchema = "chesstory.runtime-observation.v3"
  private val NativeContractVersion = "chesstory.runtime-native-boundary.v3"
  private val AdapterName = "chesstory-judgment-runtime-adapter"
  private val AdapterVersion = "0.3.0"

  private final case class CapturedStage(document: JsObject, artifactSha256: Option[String])

  def main(args: Array[String]): Unit =
    val reader = BufferedReader(InputStreamReader(System.in, StandardCharsets.UTF_8))
    val writer = PrintWriter(System.out, true, StandardCharsets.UTF_8)
    var line = reader.readLine()
    while line != null do
      writer.println(Json.stringify(observe(line)))
      line = reader.readLine()

  private[runtimeadapter] def observe(requestLine: String): JsObject =
    observeWithIntervention(requestLine, RuntimeProtocol.RuntimeBoundaryIntervention.identity)

  private[runtimeadapter] def observeWithIntervention(
      requestLine: String,
      intervention: RuntimeProtocol.RuntimeBoundaryIntervention
  ): JsObject =
    val requestSha256 = NativeTreeEncoder.sha256Utf8(requestLine)
    val requestBytes = requestLine.getBytes(StandardCharsets.UTF_8)
    try
      val trace = RuntimeProtocol.evaluateWithBoundary(
        requestBytes,
        intervention
      )
      try completeObservation(requestSha256, trace)
      catch
        case error: NativeTreeEncoder.EncodingFailure =>
          adapterErrorObservation(
            requestSha256 = requestSha256,
            trace = Some(trace),
            errorKind = "native_snapshot_encoding_failed",
            error = error,
            nativeType = error.nativeType
          )
        case NonFatal(error) =>
          adapterErrorObservation(
            requestSha256 = requestSha256,
            trace = Some(trace),
            errorKind = "observation_assembly_failed",
            error = error,
            nativeType = None
          )
    catch
      case NonFatal(error) =>
        adapterErrorObservation(
          requestSha256 = requestSha256,
          trace = None,
          errorKind = "runtime_evaluation_failed",
          error = error,
          nativeType = None
        )

  private def completeObservation(
      requestSha256: String,
      trace: RuntimeProtocol.BoundaryEvaluationTrace
  ): JsObject =
    var upstream: Option[String] = None

    def captured(
        stageId: String,
        value: Any,
        captureRole: String = "observed-native-output"
    ): CapturedStage =
      val result = observedStage(stageId, value, captureRole, upstream)
      upstream = result.artifactSha256
      result

    def missing(stageId: String, reason: String): CapturedStage =
      unavailableStage(stageId, boundaryReached = false, reason = reason, upstream = upstream)

    val boundary = trace.boundaryExecution
    val q = boundary.map(value => captured("Q", value.q)).getOrElse(
      missing("Q", "runtime_boundary_execution_absent")
    )
    val f = boundary.map(value => captured("F", value.f)).getOrElse(
      missing("F", "runtime_boundary_execution_absent")
    )
    val c = boundary.map(value => captured("C", value.c)).getOrElse(
      missing("C", "runtime_boundary_execution_absent")
    )
    val jp = boundary.map(value => captured("Jp", value.jp)).getOrElse(
      missing("Jp", "runtime_boundary_execution_absent")
    )
    val ja = boundary.map(value => captured("Ja", value.ja)).getOrElse(
      missing("Ja", "runtime_boundary_execution_absent")
    )
    val r = boundary.map(value => captured("R", value.r)).getOrElse(
      missing("R", "runtime_boundary_execution_absent")
    )
    val p = (boundary.flatMap(_.packet), trace.projection) match
      case (Some(packet), Some(projection)) if projection.input.packet eq packet =>
        captured(
          "P",
          projection,
          captureRole = "observed-native-input-output"
        )
      case (_, Some(_)) =>
        throw IllegalStateException("projection trace input is not the exact native packet from the boundary execution")
      case (_, None) =>
        unavailableStage(
          "P",
          boundaryReached = false,
          reason = "projection_boundary_not_reached",
          upstream = upstream
        )
    val vObserved = captured(
      "V",
      trace.verbalization,
      captureRole = "typed-unavailability"
    )
    val v = vObserved.copy(
      document = vObserved.document ++ Json.obj(
        "status" -> "unavailable",
        "boundary_reached" -> false,
        "reason" -> "runtime_declares_verbalization_unavailable"
      )
    )

    val publicResponse = publicResponseJson(trace.finalPublicResponse)
    Json.obj(
      "schema_version" -> ObservationSchema,
      "native_contract_version" -> NativeContractVersion,
      "observation_status" -> "complete",
      "request_sha256" -> requestSha256,
      "runtime" -> (runtimeMetadata ++ Json.obj(
        "http_status" -> trace.finalPublicResponse.httpStatus,
        "boundary_execution_present" -> boundary.nonEmpty,
        "projection_present" -> trace.projection.nonEmpty
      )),
      "public_response" -> publicResponse,
      "public_response_sha256" -> NativeTreeEncoder.sha256Canonical(publicResponse),
      "hash_contract" -> NativeTreeEncoder.HashContract,
      "artifact_chain_head_sha256" -> upstream,
      "stages" -> Json.obj(
        "Q" -> q.document,
        "F" -> f.document,
        "C" -> c.document,
        "Jp" -> jp.document,
        "Ja" -> ja.document,
        "R" -> r.document,
        "P" -> p.document,
        "V" -> v.document
      )
    )

  private def observedStage(
      stageId: String,
      value: Any,
      captureRole: String,
      upstream: Option[String]
  ): CapturedStage =
    val artifact = Json.obj(
      "native_contract_version" -> NativeContractVersion,
      "native_tree_contract_version" -> NativeTreeEncoder.ContractVersion,
      "stage" -> stageId,
      "capture_role" -> captureRole,
      "upstream_artifact_sha256" -> upstream,
      "native_tree" -> NativeTreeEncoder.encode(value, RuntimeNativeViews.adapters)
    )
    val hash = NativeTreeEncoder.sha256Canonical(artifact)
    CapturedStage(
      document = stageBase(
        stageId = stageId,
        status = "observed-native",
        boundaryReached = true,
        reason = "native_runtime_boundary_value_captured",
        upstream = upstream,
        artifactSha256 = Some(hash)
      ) ++ Json.obj(
        "artifact_role" -> captureRole,
        "artifact" -> artifact
      ),
      artifactSha256 = Some(hash)
    )

  private def unavailableStage(
      stageId: String,
      boundaryReached: Boolean,
      reason: String,
      upstream: Option[String]
  ): CapturedStage =
    CapturedStage(
      document = stageBase(
        stageId = stageId,
        status = "unavailable",
        boundaryReached = boundaryReached,
        reason = reason,
        upstream = upstream,
        artifactSha256 = None
      ),
      artifactSha256 = None
    )

  private def stageBase(
      stageId: String,
      status: String,
      boundaryReached: Boolean,
      reason: String,
      upstream: Option[String],
      artifactSha256: Option[String]
  ): JsObject =
    Json.obj(
      "stage_id" -> stageId,
      "status" -> status,
      "boundary_reached" -> boundaryReached,
      "reason" -> reason,
      "native_contract_version" -> NativeContractVersion,
      "v1_semantic_mapping_status" -> "unavailable",
      "v1_semantic_mapping_reason" -> v1MappingReason(stageId),
      "upstream_artifact_sha256" -> upstream,
      "artifact_sha256" -> artifactSha256
    )

  private def v1MappingReason(stageId: String): String =
    stageId match
      case "Q" =>
        "native_q_is_authorized_raw_move_review_input_not_the_v1_engine_evidence_pack"
      case "F" =>
        "native_f_is_a_typed_assembly_context_not_the_v1_fact_bundle_contract"
      case "C" =>
        "native_c_is_a_typed_assembly_context_not_the_v1_cause_bundle_contract"
      case "Jp" =>
        "native_claims_do_not_have_an_exact_lossless_mapping_to_v1_proposal_assertions_and_confidence"
      case "Ja" =>
        "native_certified_deferred_rejected_decisions_do_not_match_the_v1_admitted_rejected_contract"
      case "R" =>
          "native_claim_host_exposure_and_deduplication_trace_do_not_have_an_exact_v1_mapping"
      case "P" =>
        "native_packet_and_projection_trace_do_not_match_the_v1_projection_contract"
      case "V" =>
        "runtime_has_no_verbalization_stage_to_map_to_the_v1_verbalization_contract"
      case _ =>
        throw IllegalArgumentException("unknown judgment stage")

  private def adapterErrorObservation(
      requestSha256: String,
      trace: Option[RuntimeProtocol.BoundaryEvaluationTrace],
      errorKind: String,
      error: Throwable,
      nativeType: Option[String]
  ): JsObject =
    val boundaryPresent = trace.flatMap(_.boundaryExecution).nonEmpty
    val projectionPresent = trace.flatMap(_.projection).nonEmpty
    val stageReason = s"artifact_capture_failed:$errorKind"
    val stages = List("Q", "F", "C", "Jp", "Ja", "R", "P", "V").map { stageId =>
      val reached =
        if Set("Q", "F", "C", "Jp", "Ja", "R")(stageId) then boundaryPresent
        else if stageId == "P" then projectionPresent
        else false
      stageId -> unavailableStage(stageId, reached, stageReason, upstream = None).document
    }
    val responseFields = trace.map(value =>
      val response = publicResponseJson(value.finalPublicResponse)
      Json.obj(
        "public_response" -> response,
        "public_response_sha256" -> NativeTreeEncoder.sha256Canonical(response)
      )
    ).getOrElse(Json.obj())
    Json.obj(
      "schema_version" -> ObservationSchema,
      "native_contract_version" -> NativeContractVersion,
      "observation_status" -> "adapter-error",
      "request_sha256" -> requestSha256,
      "runtime" -> (runtimeMetadata ++ Json.obj(
        "boundary_execution_present" -> boundaryPresent,
        "projection_present" -> projectionPresent
      )),
      "hash_contract" -> NativeTreeEncoder.HashContract,
      "artifact_chain_head_sha256" -> JsNull,
      "adapter_error" -> (Json.obj(
        "kind" -> errorKind,
        "class" -> error.getClass.getName
      ) ++ nativeType.fold(Json.obj())(value => Json.obj("native_type" -> value))),
      "stages" -> JsObject(stages)
    ) ++ responseFields

  private def publicResponseJson(result: RuntimeProtocol.Result): JsObject =
    Json.obj(
      "http_status" -> result.httpStatus,
      "body" -> result.body
    )

  private def runtimeMetadata: JsObject =
    Json.obj(
      "adapter_name" -> AdapterName,
      "adapter_version" -> AdapterVersion,
      "request_schema" -> RuntimeProtocol.RequestSchema,
      "response_schema" -> RuntimeProtocol.ResponseSchema
    )
