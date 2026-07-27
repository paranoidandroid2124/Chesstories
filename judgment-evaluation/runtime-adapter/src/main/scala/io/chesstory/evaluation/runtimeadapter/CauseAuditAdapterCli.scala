package io.chesstory.evaluation.runtimeadapter

import java.io.{ BufferedReader, InputStreamReader, PrintWriter }
import java.nio.charset.StandardCharsets
import java.util.Locale

import scala.collection.mutable
import scala.util.control.NonFatal

import io.chesstory.runtime.RuntimeProtocol
import lila.chessjudgment.analysis.assembly.{
  ClaimCandidateGraph,
  ClaimDeduplicationTrace,
  ClaimRankingResult,
  JudgmentBoundaryExecution
}
import lila.chessjudgment.analysis.policy.ClaimAdmissionDecision
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.{ JsArray, JsNull, JsObject, JsValue, Json }

/** Compact, read-only C -> P observation for external cause audits.
  *
  * The adapter invokes the production runtime once with the identity boundary
  * intervention. It only follows public ADT references and calls public graph
  * derivations; it does not infer or rebuild a cause, admission, ranking, or
  * projection decision.
  */
object CauseAuditAdapterCli:
  private val ObservationSchema = "chesstory.cause-audit-runtime-observation.v1"
  private val AdapterName = "chesstory-cause-audit-runtime-adapter"
  private val AdapterVersion = "0.1.0"

  private final case class ClaimLink(claim: JudgmentClaim, linkKind: String)

  private final case class RankResolution(
      sourceClaimId: String,
      path: List[String],
      finalClaimId: String,
      cycleDetected: Boolean
  )

  def main(args: Array[String]): Unit =
    val reader = BufferedReader(InputStreamReader(System.in, StandardCharsets.UTF_8))
    val writer = PrintWriter(System.out, true, StandardCharsets.UTF_8)
    var line = reader.readLine()
    while line != null do
      writer.println(Json.stringify(observe(line)))
      line = reader.readLine()

  private[runtimeadapter] def observe(requestLine: String): JsObject =
    val requestSha256 = NativeTreeEncoder.sha256Utf8(requestLine)
    try
      val trace = RuntimeProtocol.evaluateWithBoundary(
        requestLine.getBytes(StandardCharsets.UTF_8),
        RuntimeProtocol.RuntimeBoundaryIntervention.identity
      )
      trace.boundaryExecution match
        case None => unavailableObservation(requestSha256, trace)
        case Some(execution) => completeObservation(requestSha256, execution, trace)
    catch
      case error: NativeTreeEncoder.EncodingFailure =>
        errorObservation(
          requestSha256,
          "source_record_hash_failed",
          error.getClass.getName,
          error.nativeType
        )
      case NonFatal(error) =>
        errorObservation(
          requestSha256,
          "cause_audit_observation_failed",
          error.getClass.getName,
          None
        )

  private def completeObservation(
      requestSha256: String,
      execution: JudgmentBoundaryExecution,
      trace: RuntimeProtocol.BoundaryEvaluationTrace
  ): JsObject =
    val graph = execution.c.evidenceGraph
    val recordHashes = RecordHashes(graph)
    val projectionJson = projectionSummary(trace)
    val probeRequests = publicProbeRequests(trace)
    val selectedIdeaIds = trace.projection.toList.flatMap(item => projectedIdeaIds(item.selectedPayload)).distinct.sorted
    val causes = graph.records.collect {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        causeCascade(
          record = record,
          cause = cause,
          graph = graph,
          execution = execution,
          selectedIdeaIds = selectedIdeaIds,
          recordHashes = recordHashes
        )
    }.sortBy(item => (item \ "cause_record" \ "id").asOpt[String].getOrElse(""))
    val kindCounts = causes
      .flatMap(item => (item \ "c" \ "kind").asOpt[String])
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .toList
      .sortBy(_._1)

    Json.obj(
      "schema_version" -> ObservationSchema,
      "status" -> "complete",
      "request_sha256" -> requestSha256,
      "hash_contract" -> NativeTreeEncoder.HashContract,
      "runtime" -> runtimeMetadata,
      "public_response" -> publicResponseSummary(trace.finalPublicResponse),
      "boundary" -> Json.obj(
        "c_reached" -> true,
        "jp_reached" -> true,
        "ja_reached" -> true,
        "r_reached" -> true,
        "packet_present" -> execution.packet.nonEmpty,
        "p_reached" -> trace.projection.nonEmpty,
        "cause_count" -> causes.size,
        "cause_kind_counts" -> JsObject(kindCounts.map { case (kind, count) => kind -> Json.toJson(count) })
      ),
      "projection" -> projectionJson,
      "probe_request_count" -> probeRequests.size,
      "probe_requests" -> probeRequests,
      "causes" -> causes
    )

  private def causeCascade(
      record: EvidenceRecord,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      execution: JudgmentBoundaryExecution,
      selectedIdeaIds: List[String],
      recordHashes: RecordHashes
  ): JsObject =
    val comparison = graph.comparisonFor(cause)
    val binding = graph.relativeCauseBinding(cause)
    val claimLinks = linkedClaims(record.ref, execution.jp)
    val linkedClaimIds = claimLinks.map(_.claim.id).toSet
    val admissions = execution.ja.decisions.filter(decision => linkedClaimIds(decision.claim.id))
    val resolutions = claimLinks.map(link => rankResolution(link.claim.id, execution.r.deduplicationTrace))
    val finalRankIds = resolutions.map(_.finalClaimId).toSet
    val finalClaimsById = execution.r.ranked.map(_.claim).map(claim => claim.id -> claim).toMap
    val directlyOwnedFinalRankIds = finalRankIds.filter(id =>
      finalClaimsById.get(id).exists(claimDirectlyOwnsCause(_, record.ref))
    )
    val ranked = execution.r.ranked.zipWithIndex.filter { case (item, _) => directlyOwnedFinalRankIds(item.claim.id) }
    val relevantDedupIds = resolutions.flatMap(_.path).toSet
    val relevantDedup = execution.r.deduplicationTrace.filter(item =>
      relevantDedupIds(item.originalClaimId) || relevantDedupIds(item.keptClaimId)
    )
    val packetClaimsById = execution.packet.toList.flatMap(_.claims).map(claim => claim.id -> claim).toMap
    val packetClaimIds = packetClaimsById.values
      .filter(claim => directlyOwnedFinalRankIds(claim.id) && claimDirectlyOwnsCause(claim, record.ref))
      .map(_.id)
      .toList
      .distinct
      .sorted
    val selectedLinkedIds = selectedIdeaIds
      .filter(directlyOwnedFinalRankIds)
      .filter(id =>
        packetClaimsById.get(id).exists(claim =>
          execution.packet.exists(
            _.playerFacingRelativeCauses(claim).exists { case (_, ref) => ref.id == record.ref.id }
          )
        )
      )
      .distinct
      .sorted
    val dominance = execution.r.causeDominanceDecisions.find(_.causeEvidenceId == record.ref.id)

    Json.obj(
      "cause_record" -> sourceRefJson(record.ref, graph, recordHashes),
      "c" -> Json.obj(
        "kind" -> code(cause.kind),
        "source_side" -> code(cause.sourceSide),
        "comparison_evidence" -> sourceRefJson(cause.comparisonEvidence, graph, recordHashes),
        "support_evidence" -> cause.supportEvidence.distinctBy(_.id).sortBy(_.id).map(sourceRefJson(_, graph, recordHashes)),
        "comparison" -> comparison.fold[JsValue](JsNull)(comparisonJson),
        "attribution" -> attributionJson(cause),
        "binding" -> binding.fold[JsValue](JsNull)(bindingJson),
        "proof" -> proofJson(cause, graph, recordHashes),
        "proof_state" -> Json.obj(
          "raw_typed_depth" -> cause.hasRawTypedDepth(graph),
          "owned_typed_depth" -> cause.hasOwnedTypedDepth(graph),
          "strategic_contrast_depth" -> cause.hasStrategicContrastDepth(graph),
          "owned_strategic_contrast_depth" -> cause.hasOwnedStrategicContrastDepth(graph),
          "owned_admissible_long_term_proof" -> cause.hasOwnedAdmissibleLongTermProof(graph),
          "owned_tactical_proof" -> cause.hasOwnedTacticalProof(graph)
        ),
        "objects" -> objectSummary(cause, graph, recordHashes)
      ),
      "jp" -> Json.obj(
        "linked_claims" -> claimLinks.map(link => claimJson(link.claim) ++ Json.obj("link_kind" -> link.linkKind))
      ),
      "ja" -> Json.obj(
        "linked_decisions" -> admissions.sortBy(_.claim.id).map(admissionJson)
      ),
      "r" -> Json.obj(
        "resolutions" -> resolutions.sortBy(_.sourceClaimId).map(resolutionJson(_, execution.r)),
        "ranked" -> ranked.map { case (item, index) => rankedJson(item, index) },
        "deduplication" -> relevantDedup.sortBy(_.order).map(dedupJson),
        "fallback_dominance" -> dominance.fold[JsValue](JsNull)(dominanceJson)
      ),
      "p" -> Json.obj(
        "packet_present" -> execution.packet.nonEmpty,
        "packet_linked_claim_ids" -> packetClaimIds,
        "selected_public_idea_claim_ids" -> selectedLinkedIds
      )
    )

  private def comparisonJson(fact: CandidateComparisonFact): JsObject =
    Json.obj(
      "kind" -> code(fact.kind),
      "reference_line" -> lineJson(fact.referenceLine),
      "candidate_line" -> lineJson(fact.candidateLine),
      "reference_root_move" -> fact.referenceLine.rootMove,
      "candidate_root_move" -> fact.candidateLine.rootMove,
      "mover" -> code(fact.comparison.mover),
      "candidate_win_percent_delta_for_mover" -> fact.comparison.candidateWinPercentDeltaForMover,
      "win_percent_loss_for_mover" -> fact.comparison.winPercentLossForMover,
      "verdict" -> code(fact.comparison.verdict),
      "candidate_set_type" -> fact.candidateSet.map(item => code(item.candidateSetType))
    )

  private def attributionJson(cause: RelativeCauseFact): JsObject =
    Json.obj(
      "kind" -> code(cause.attribution.kind),
      "root_move_matched" -> cause.attribution.rootMoveMatched,
      "direct_proof_eligible" -> cause.attribution.directProofEligible,
      "reason" -> cause.attribution.reason
    )

  private def bindingJson(binding: RelativeCauseBinding): JsObject =
    Json.obj(
      "role" -> code(binding.role),
      "source_side" -> code(binding.sourceSide),
      "event_line" -> lineJson(binding.eventLine),
      "evidence_lines" -> binding.evidenceLines.sortBy(_.id).map(lineJson),
      "importance" -> code(binding.importance)
    )

  private def proofJson(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      recordHashes: RecordHashes
  ): JsObject =
    cause.proof match
      case None => Json.obj("present" -> false)
      case Some(proof) =>
        Json.obj(
          "present" -> true,
          "direct" -> proofSectionJson(cause, proof.directProof, graph, recordHashes),
          "contrast" -> proofSectionJson(cause, proof.contrastProof, graph, recordHashes),
          "context" -> proofSectionJson(cause, proof.contextSupport, graph, recordHashes),
          "strategic_identity" -> strategicIdentityJson(cause.strategicProofIdentity(graph))
        )

  private def proofSectionJson(
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection,
      graph: TypedEvidenceGraph,
      recordHashes: RecordHashes
  ): JsObject =
    val sources = section.sourceRefs.distinctBy(_.id).sortBy(_.id).map { ref =>
      val singleton = section.copy(sourceRefs = List(ref))
      sourceRefJson(ref, graph, recordHashes) ++ Json.obj(
        "proof_kind_labels" -> graph.relativeCauseProofKindLabels(cause, singleton).sorted
      )
    }
    Json.obj(
      "role" -> code(section.role),
      "strength" -> code(section.strength),
      "proof_kind_labels" -> graph.relativeCauseProofKindLabels(cause, section).distinct.sorted,
      "sources" -> sources
    )

  private def strategicIdentityJson(identity: RelativeCauseStrategicProofIdentity): JsObject =
    Json.obj(
      "axis_keys" -> identity.axisKeys,
      "mechanism_kinds" -> identity.mechanismKinds.map(code).sorted,
      "mechanism_source_ids" -> identity.mechanismSourceIds,
      "signal_source_ids" -> identity.signalSourceIds
    )

  private def objectSummary(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      recordHashes: RecordHashes
  ): JsObject =
    val bindings = EvidenceObjectBinding.fromRelativeCauseForProjection(cause, graph).sortBy(_.signature)
    val rawBindings = EvidenceObjectBinding.fromRelativeCause(cause, graph)
    val relativeTargets = EvidenceObjectBinding.relativeCauseTargets(cause, graph).sortBy(_.signaturePart)
    val deterrenceRefs = graph.opponentResourceDeterrenceEventRefs(cause).distinctBy(_.id).sortBy(_.id)
    val restrictionTargets = EvidenceObjectBinding
      .opponentRestrictionTargets(graph, deterrenceRefs)
      .sortBy(_.signaturePart)
    Json.obj(
      "binding_summary" -> Json.obj(
        "binding_count" -> bindings.size,
        "raw_binding_count" -> rawBindings.size,
        "source_ids" -> bindings.map(_.source.id).distinct.sorted,
        "specific_target_mechanism_ready_count" -> bindings.count(_.specificTargetMechanismReady),
        "actor" -> aggregateObjects(bindings.flatMap(_.actor)),
        "target" -> aggregateObjects(bindings.flatMap(_.target)),
        "mechanism" -> aggregateObjects(bindings.flatMap(_.mechanism)),
        "consequence" -> aggregateObjects(bindings.flatMap(_.consequence)),
        "witness" -> aggregateObjects(bindings.flatMap(_.witness))
      ),
      "owned_bindings" -> bindings.map(objectBindingJson),
      "relative_cause_targets" -> relativeTargets.map(concreteObjectJson),
      "opponent_restriction_event_sources" -> deterrenceRefs.map(sourceRefJson(_, graph, recordHashes)),
      "opponent_restriction_targets" -> restrictionTargets.map(concreteObjectJson)
    )

  private def objectBindingJson(binding: EvidenceObjectBinding): JsObject =
    Json.obj(
      "signature" -> binding.signature,
      "source_id" -> binding.source.id,
      "proof_role" -> binding.proofRole.map(value => code(value.toString)),
      "line_id" -> binding.line.map(_.id),
      "actor" -> binding.actor.map(concreteObjectJson),
      "target" -> binding.target.map(concreteObjectJson),
      "mechanism" -> binding.mechanism.map(concreteObjectJson),
      "consequence" -> binding.consequence.map(concreteObjectJson),
      "witness" -> binding.witness.map(concreteObjectJson),
      "specific_target_mechanism_ready" -> binding.specificTargetMechanismReady
    )

  private def aggregateObjects(values: List[ConcreteChessObject]): List[JsObject] =
    values.distinctBy(_.signaturePart).sortBy(_.signaturePart).map(concreteObjectJson)

  private def concreteObjectJson(value: ConcreteChessObject): JsObject =
    Json.obj(
      "kind" -> code(value.kind),
      "key" -> value.key
    )

  private def linkedClaims(
      causeRef: EvidenceRef,
      claims: List[JudgmentClaim]
  ): List[ClaimLink] =
    claims
      .filter(claimDirectlyOwnsCause(_, causeRef))
      .map(claim => ClaimLink(claim, "direct_evidence"))
      .sortBy(_.claim.id)

  private def claimDirectlyOwnsCause(claim: JudgmentClaim, causeRef: EvidenceRef): Boolean =
    claim.evidence.exists(_.id == causeRef.id)

  private def registeredClosure(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph
  ): Option[List[EvidenceRecord]] =
    val roots = claim.evidence.flatMap(ref => graph.byId.get(ref.id))
    Option.when(claim.evidence.nonEmpty && roots.size == claim.evidence.size)(
      (roots ++ roots.flatMap(graph.parentClosure)).distinctBy(_.ref.id)
    )

  private def claimJson(claim: JudgmentClaim): JsObject =
    Json.obj(
      "id" -> claim.id,
      "family" -> code(claim.family),
      "subject" -> code(claim.subject),
      "primary_line" -> claim.primaryLine.map(lineJson),
      "subject_move" -> claim.subjectMove,
      "scope" -> code(claim.scope),
      "confidence" -> code(claim.confidence),
      "evidence_ids" -> claim.evidence.map(_.id).distinct.sorted
    )

  private def admissionJson(decision: ClaimAdmissionDecision): JsObject =
    Json.obj(
      "claim_id" -> decision.claim.id,
      "status" -> code(decision.status),
      "present_layers" -> decision.presentLayers.map(code).toList.sorted,
      "missing_layer_groups" -> decision.missingLayerGroups
        .map(group => group.map(code).toList.sorted)
        .sortBy(_.mkString("|")),
      "missing_evidence_ids" -> decision.missingEvidence.map(_.id).distinct.sorted
    )

  private def rankResolution(
      claimId: String,
      trace: List[ClaimDeduplicationTrace]
  ): RankResolution =
    val nextById = trace.map(item => item.originalClaimId -> item.keptClaimId).toMap
    var path = List(claimId)
    var current = claimId
    var visited = Set(claimId)
    var cycle = false
    var continue = true
    while continue do
      nextById.get(current) match
        case None => continue = false
        case Some(next) if visited(next) =>
          path = path :+ next
          current = next
          cycle = true
          continue = false
        case Some(next) =>
          path = path :+ next
          current = next
          visited = visited + next
    RankResolution(claimId, path, current, cycle)

  private def resolutionJson(resolution: RankResolution, ranking: ClaimRankingResult): JsObject =
    val rankedIndex = ranking.ranked.indexWhere(_.claim.id == resolution.finalClaimId)
    Json.obj(
      "source_claim_id" -> resolution.sourceClaimId,
      "claim_id_path" -> resolution.path,
      "final_claim_id" -> resolution.finalClaimId,
      "cycle_detected" -> resolution.cycleDetected,
      "ranked" -> (rankedIndex >= 0),
      "rank_index" -> Option.when(rankedIndex >= 0)(rankedIndex)
    )

  private def rankedJson(
      item: lila.chessjudgment.analysis.assembly.RankedClaimDecision,
      index: Int
  ): JsObject =
    Json.obj(
      "claim_id" -> item.claim.id,
      "rank_index" -> index,
      "exposure_tier" -> code(item.exposureTier),
      "exposure_priority" -> item.exposurePriority,
      "priority" -> item.priority,
      "player_facing_cause_evidence_ids" -> item.playerFacingCauseEvidenceIds,
      "salience" -> Json.obj(
        "score" -> item.salience.score,
        "drivers" -> item.salience.drivers.map(code).distinct.sorted
      )
    )

  private def dominanceJson(decision: RelativeCauseDominanceDecision): JsObject =
    Json.obj(
      "status" -> code(decision.status),
      "retained" -> decision.retained,
      "dominating_cause_evidence_ids" -> decision.dominatingCauseEvidenceIds
    )

  private def dedupJson(item: ClaimDeduplicationTrace): JsObject =
    Json.obj(
      "order" -> item.order,
      "original_claim_id" -> item.originalClaimId,
      "kept_claim_id" -> item.keptClaimId,
      "reason" -> item.reason.id
    )

  private def projectionSummary(trace: RuntimeProtocol.BoundaryEvaluationTrace): JsObject =
    trace.projection match
      case None =>
        Json.obj(
          "present" -> false,
          "packet_present" -> trace.boundaryExecution.flatMap(_.packet).nonEmpty,
          "selected_public_idea_claim_ids" -> Json.arr()
        )
      case Some(projection) =>
        val selectedIds = projectedIdeaIds(projection.selectedPayload).distinct.sorted
        Json.obj(
          "present" -> true,
          "packet_present" -> trace.boundaryExecution.flatMap(_.packet).nonEmpty,
          "status" -> projection.status.id,
          "reason" -> projection.reason.map(_.id),
          "renderable" -> projection.renderable,
          "selected_payload_source" -> projection.selectedPayloadSource.id,
          "selected_payload_sha256" -> NativeTreeEncoder.sha256Canonical(projection.selectedPayload),
          "selected_public_idea_claim_ids" -> selectedIds
        )

  private def projectedIdeaIds(payload: JsObject): List[String] =
    (payload \ "explanations").asOpt[JsArray].toList
      .flatMap(_.value)
      .flatMap(explanation => (explanation \ "ideas").asOpt[JsArray].toList.flatMap(_.value))
      .flatMap(idea => (idea \ "id").asOpt[String])

  private def publicProbeRequests(trace: RuntimeProtocol.BoundaryEvaluationTrace): List[JsObject] =
    trace.projection
      .map(_.publicProbeRequests)
      .getOrElse(
        (trace.finalPublicResponse.body \ "probe_requests")
          .asOpt[JsArray]
          .toList
          .flatMap(_.value.collect { case item: JsObject => item })
      )

  private def sourceRefJson(
      ref: EvidenceRef,
      graph: TypedEvidenceGraph,
      recordHashes: RecordHashes
  ): JsObject =
    val record = graph.byId.get(ref.id).filter(_.ref == ref)
    Json.obj(
      "id" -> ref.id,
      "producer" -> code(ref.producer),
      "layer" -> code(ref.layer),
      "scope" -> code(ref.scope),
      "confidence" -> code(ref.confidence),
      "line" -> ref.line.map(lineJson),
      "record_registered" -> record.nonEmpty,
      "record_payload_type" -> record.map(item => item.payload.getClass.getSimpleName.stripSuffix("$")),
      "record_sha256" -> record.map(recordHashes.hash)
    )

  private def lineJson(line: LineNodeRef): JsObject =
    Json.obj(
      "id" -> line.id,
      "root_move" -> line.rootMove,
      "rank" -> line.rank,
      "role" -> code(line.role)
    )

  private def unavailableObservation(
      requestSha256: String,
      trace: RuntimeProtocol.BoundaryEvaluationTrace
  ): JsObject =
    Json.obj(
      "schema_version" -> ObservationSchema,
      "status" -> "unavailable",
      "reason" -> "runtime_boundary_execution_absent",
      "request_sha256" -> requestSha256,
      "hash_contract" -> NativeTreeEncoder.HashContract,
      "runtime" -> runtimeMetadata,
      "public_response" -> publicResponseSummary(trace.finalPublicResponse),
      "boundary" -> Json.obj(
        "c_reached" -> false,
        "jp_reached" -> false,
        "ja_reached" -> false,
        "r_reached" -> false,
        "packet_present" -> false,
        "p_reached" -> false,
        "cause_count" -> 0
      ),
      "projection" -> Json.obj("present" -> false, "packet_present" -> false),
      "probe_request_count" -> publicProbeRequests(trace).size,
      "probe_requests" -> publicProbeRequests(trace),
      "causes" -> Json.arr()
    )

  private def errorObservation(
      requestSha256: String,
      kind: String,
      errorClass: String,
      nativeType: Option[String]
  ): JsObject =
    Json.obj(
      "schema_version" -> ObservationSchema,
      "status" -> "adapter_error",
      "request_sha256" -> requestSha256,
      "hash_contract" -> NativeTreeEncoder.HashContract,
      "runtime" -> runtimeMetadata,
      "adapter_error" -> (Json.obj(
        "kind" -> kind,
        "class" -> errorClass
      ) ++ nativeType.fold(Json.obj())(value => Json.obj("native_type" -> value))),
      "causes" -> Json.arr()
    )

  private def publicResponseSummary(result: RuntimeProtocol.Result): JsObject =
    val body = result.body
    Json.obj(
      "http_status" -> result.httpStatus,
      "body_sha256" -> NativeTreeEncoder.sha256Canonical(body),
      "schema_version" -> (body \ "schema_version").asOpt[String],
      "request_id" -> (body \ "request_id").asOpt[String],
      "ok" -> (body \ "ok").asOpt[Boolean],
      "status" -> (body \ "status").asOpt[String],
      "availability" -> (body \ "availability").toOption,
      "error" -> (body \ "error").toOption
    )

  private def runtimeMetadata: JsObject =
    Json.obj(
      "adapter_name" -> AdapterName,
      "adapter_version" -> AdapterVersion,
      "request_schema" -> RuntimeProtocol.RequestSchema,
      "response_schema" -> RuntimeProtocol.ResponseSchema
    )

  private def code(value: Any): String =
    value.toString
      .replaceAll("([a-z0-9])([A-Z])", "$1_$2")
      .replace('-', '_')
      .toLowerCase(Locale.ROOT)

  private final class RecordHashes private (graph: TypedEvidenceGraph):
    private val cache = mutable.Map.empty[String, String]

    def hash(record: EvidenceRecord): String =
      cache.getOrElseUpdate(
        record.ref.id,
        NativeTreeEncoder.sha256Canonical(
          NativeTreeEncoder.encode(record, RuntimeNativeViews.adapters)
        )
      )

  private object RecordHashes:
    def apply(graph: TypedEvidenceGraph): RecordHashes = new RecordHashes(graph)
