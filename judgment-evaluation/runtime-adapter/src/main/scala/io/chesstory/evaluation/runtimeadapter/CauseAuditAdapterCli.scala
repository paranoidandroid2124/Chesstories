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
  JudgmentBoundaryExecution,
  RelativeAssessmentAssembler
}
import lila.chessjudgment.analysis.policy.ClaimAdmissionDecision
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.{ JsArray, JsLookupResult, JsNull, JsObject, JsString, JsValue, Json }

/** Compact, read-only C -> P observation for external cause audits.
  *
  * The adapter invokes the production runtime once with the identity boundary
  * intervention. It only follows public ADT references and calls public graph
  * derivations; it does not infer or rebuild a cause, admission, ranking, or
  * projection decision.
  */
object CauseAuditAdapterCli:
  private val AdapterName = "chesstory-cause-audit-runtime-adapter"
  private val AdapterVersion = "0.3.2"

  private enum ObservationContract(val schemaVersion: String):
    case V2 extends ObservationContract("chesstory.cause-audit-runtime-observation.v2")
    case V3 extends ObservationContract("chesstory.cause-audit-runtime-observation.v3")

    def capturesNativeRSelections: Boolean = this == V3

  private object ObservationContract:
    private val V3Argument = "--observation-schema-version=3"
    private val V2Argument = "--historical-observation-schema-version=2"

    def fromArgs(args: Array[String]): ObservationContract =
      args.toList match
        case Nil                  => ObservationContract.V3
        case V3Argument :: Nil    => ObservationContract.V3
        case V2Argument :: Nil    => ObservationContract.V2
        case _ =>
          throw IllegalArgumentException(
            "cause-audit adapter accepts only no arguments, " +
              V3Argument + ", or " + V2Argument
          )

  private[runtimeadapter] def observationSchemaForArgs(args: Array[String]): String =
    ObservationContract.fromArgs(args).schemaVersion

  private final case class ClaimLink(claim: JudgmentClaim, linkKind: String)
  private final case class HostedCauseSelection(
      claim: JudgmentClaim,
      decision: PlayerFacingClaimDecision,
      selection: PlayerFacingCauseSelection
  )

  private final case class RankResolution(
      sourceClaimId: String,
      path: List[String],
      finalClaimId: String,
      cycleDetected: Boolean
  )

  /** Line ids are retained only as transport/provenance identity. Chess
    * selection and dominance use `semanticKey`, never the id.
    */
  private final case class ProjectedSemanticLine(
      lineId: String,
      role: String,
      rank: Int,
      rootMove: String
  ):
    def semanticKey: (String, Int, String) = (role, rank, rootMove)

    def json: JsObject =
      Json.obj(
        "line_id" -> lineId,
        "role" -> role,
        "rank" -> rank,
        "root_move" -> rootMove
      )

  private final case class ProjectedEvidenceRef(
      id: String,
      producer: String,
      layer: String,
      scope: String,
      line: Option[ProjectedSemanticLine]
  ):
    def sortKey: (String, String, String, String, String) =
      (id, producer, layer, scope, line.map(_.semanticKey.toString).getOrElse(""))

    def json: JsObject =
      Json.obj(
        "id" -> id,
        "producer" -> producer,
        "layer" -> layer,
        "scope" -> scope,
        "line" -> line.map(_.json)
      )

  private final case class ProjectedChessObject(kind: String, key: String):
    def sortKey: (String, String) = (kind, key)
    def json: JsObject = Json.obj("kind" -> kind, "key" -> key)
    def concrete: Option[ConcreteChessObject] =
      EvidenceObjectKind.values.find(item => code(item) == kind).map(ConcreteChessObject(_, key))

  private final case class ProjectedChannelSelection(
      carrier: ProjectedEvidenceRef,
      provenance: List[ProjectedEvidenceRef],
      causalSignature: String,
      directChange: String,
      playedChange: String,
      actor: List[ProjectedChessObject],
      targets: List[ProjectedChessObject],
      mechanisms: List[ProjectedChessObject],
      consequences: List[ProjectedChessObject],
      witnesses: List[ProjectedChessObject],
      line: Option[ProjectedSemanticLine],
      horizon: Option[String],
      proofSegment: Option[JsObject],
      effectDescriptor: Option[JsObject],
      importanceEffect: Option[JsObject],
      descriptorAmbiguous: Boolean,
      proofSegmentAmbiguous: Boolean
  ):
    def sortKey: (String, String, String, String) =
      (causalSignature, carrier.id, directChange, playedChange)

    def json(contract: ObservationContract): JsObject =
      val projected = Json.obj(
        "carrier" -> carrier.json,
        "provenance" -> provenance.sortBy(_.sortKey).map(_.json),
        "causal_signature" -> causalSignature,
        "direct_change" -> directChange,
        "played_change" -> playedChange,
        "actor" -> actor.sortBy(_.sortKey).map(_.json),
        "targets" -> targets.sortBy(_.sortKey).map(_.json),
        "mechanisms" -> mechanisms.sortBy(_.sortKey).map(_.json),
        "consequences" -> consequences.sortBy(_.sortKey).map(_.json),
        "witnesses" -> witnesses.sortBy(_.sortKey).map(_.json),
        "line" -> line.map(_.json),
        "horizon" -> horizon
      )
      if contract.capturesNativeRSelections then
        projected ++ Json.obj(
          "proof_segment" -> proofSegment,
          "effect_descriptor" -> effectDescriptor,
          "importance_effect" -> importanceEffect,
          "descriptor_ambiguous" -> descriptorAmbiguous,
          "proof_segment_ambiguous" -> proofSegmentAmbiguous
        )
      else projected

  private final case class ProjectedHostSelection(
      claimId: String,
      family: String,
      tier: String
  ):
    def json: JsObject =
      Json.obj("claim_id" -> claimId, "family" -> family, "tier" -> tier)

  private final case class ProjectedAttributionSelection(
      kind: String,
      rootMoveMatched: Boolean,
      directProofEligible: Boolean
  ):
    def json: JsObject =
      Json.obj(
        "kind" -> kind,
        "root_move_matched" -> rootMoveMatched,
        "direct_proof_eligible" -> directProofEligible
      )

  private final case class ProjectedOnlyMoveQualifier(
      comparisonEvidenceId: String,
      causeEvidenceId: String,
      reference: ProjectedSemanticLine,
      relation: String,
      licensesCausalBecause: Boolean
  ):
    def json: JsObject =
      Json.obj(
        "comparison_evidence_id" -> comparisonEvidenceId,
        "cause_evidence_id" -> causeEvidenceId,
        "reference" -> reference.json,
        "relation" -> relation,
        "licenses_causal_because" -> licensesCausalBecause
      )

  private final case class ProjectedComparisonSelection(
      evidenceId: String,
      kind: String,
      mover: String,
      reference: ProjectedSemanticLine,
      candidate: ProjectedSemanticLine,
      event: ProjectedSemanticLine,
      comparedMove: String,
      verdict: String,
      candidateWinPercentDeltaForMover: Double,
      winPercentLossForMover: Double
  ):
    def json: JsObject =
      Json.obj(
        "evidence_id" -> evidenceId,
        "kind" -> kind,
        "mover" -> mover,
        "reference" -> reference.json,
        "candidate" -> candidate.json,
        "event" -> event.json,
        "compared_move" -> comparedMove,
        "verdict" -> verdict,
        "candidate_win_percent_delta_for_mover" -> candidateWinPercentDeltaForMover,
        "win_percent_loss_for_mover" -> winPercentLossForMover
      )

  private final case class ProjectedCauseSelection(
      causeEvidenceId: String,
      comparisonExposureRank: Int,
      selectionOrder: Int,
      exposure: String,
      effectMode: String,
      causeKind: String,
      sourceSide: String,
      directEffectAdmission: JsObject,
      attribution: ProjectedAttributionSelection,
      host: ProjectedHostSelection,
      comparison: ProjectedComparisonSelection,
      comparisonSemanticKey: String,
      channels: List[ProjectedChannelSelection],
      onlyMove: Option[ProjectedOnlyMoveQualifier]
  ):
    def json(contract: ObservationContract): JsObject =
      val rank =
        if contract.capturesNativeRSelections then
          Json.obj("comparison_exposure_rank" -> comparisonExposureRank)
        else Json.obj("priority_rank" -> comparisonExposureRank)
      val projected = Json.obj(
        "cause_evidence_id" -> causeEvidenceId,
        "selection_order" -> selectionOrder,
        "exposure" -> exposure,
        "effect_mode" -> effectMode,
        "cause_kind" -> causeKind,
        "source_side" -> sourceSide,
        "attribution" -> attribution.json,
        "host" -> host.json,
        "comparison" -> comparison.json,
        "channels" -> channels.sortBy(_.sortKey).map(_.json(contract)),
        "only_move" -> onlyMove.map(_.json)
      ) ++ rank
      if contract.capturesNativeRSelections then
        projected ++ Json.obj(
          "direct_effect_admission" -> directEffectAdmission,
          "comparison_semantic_key" -> comparisonSemanticKey
        )
      else projected

  private final case class ProjectedPublicIdeaItemLink(
      causeEvidenceId: String,
      itemRole: String,
      ideaUnitId: String
  ):
    def json: JsObject =
      Json.obj(
        "cause_evidence_id" -> causeEvidenceId,
        "item_role" -> itemRole,
        "idea_unit_id" -> ideaUnitId
      )

  private final case class ProjectedPublicIdeaItem(
      selection: ProjectedCauseSelection,
      link: ProjectedPublicIdeaItemLink
  )

  private final case class ProjectedIdeaUnit(
      ideaId: String,
      kind: String,
      leadCauseEvidenceId: String,
      memberCauseEvidenceIds: List[String],
      importanceLayer: Int,
      priorityStatus: String,
      serializationOrder: Int
  ):
    def json: JsObject =
      Json.obj(
        "idea_id" -> ideaId,
        "kind" -> kind,
        "lead_cause_evidence_id" -> leadCauseEvidenceId,
        "member_cause_evidence_ids" -> memberCauseEvidenceIds,
        "importance_layer" -> importanceLayer,
        "priority_status" -> priorityStatus,
        "serialization_order" -> serializationOrder
      )

    def itemLinks: List[ProjectedPublicIdeaItemLink] =
      memberCauseEvidenceIds.map(causeId =>
        ProjectedPublicIdeaItemLink(causeId, "cause_facet", ideaId)
      )

  private final case class ProjectedPublicIdeaUnitParse(
      rawUnitCount: Int,
      units: List[ProjectedIdeaUnit],
      containersClosed: Boolean
  ):
    def parseClosed: Boolean = containersClosed && units.size == rawUnitCount

    def diagnosticJson: JsObject =
      Json.obj(
        "raw_public_idea_unit_count" -> rawUnitCount,
        "parsed_public_idea_unit_count" -> units.size,
        "public_idea_units_parse_closed" -> parseClosed
      )

  private object ProjectedPublicIdeaUnitParse:
    val absent: ProjectedPublicIdeaUnitParse =
      ProjectedPublicIdeaUnitParse(0, Nil, containersClosed = true)

  def main(args: Array[String]): Unit =
    val contract = ObservationContract.fromArgs(args)
    val reader = BufferedReader(InputStreamReader(System.in, StandardCharsets.UTF_8))
    val writer = PrintWriter(System.out, true, StandardCharsets.UTF_8)
    var line = reader.readLine()
    while line != null do
      writer.println(Json.stringify(observe(line, contract)))
      line = reader.readLine()

  private[runtimeadapter] def observeHistoricalV2(requestLine: String): JsObject =
    observe(requestLine, ObservationContract.V2)

  private[runtimeadapter] def observeV3(requestLine: String): JsObject =
    observe(requestLine, ObservationContract.V3)

  private def observe(
      requestLine: String,
      contract: ObservationContract
  ): JsObject =
    val requestSha256 = NativeTreeEncoder.sha256Utf8(requestLine)
    try
      val trace = RuntimeProtocol.evaluateWithBoundary(
        requestLine.getBytes(StandardCharsets.UTF_8),
        RuntimeProtocol.RuntimeBoundaryIntervention.identity
      )
      trace.boundaryExecution match
        case None => unavailableObservation(requestSha256, trace, contract)
        case Some(execution) => completeObservation(requestSha256, execution, trace, contract)
    catch
      case error: NativeTreeEncoder.EncodingFailure =>
        errorObservation(
          requestSha256,
          "source_record_hash_failed",
          error.getClass.getName,
          error.nativeType,
          contract
        )
      case NonFatal(error) =>
        errorObservation(
          requestSha256,
          "cause_audit_observation_failed",
          error.getClass.getName,
          None,
          contract
        )

  private def completeObservation(
      requestSha256: String,
      execution: JudgmentBoundaryExecution,
      trace: RuntimeProtocol.BoundaryEvaluationTrace,
      contract: ObservationContract
  ): JsObject =
    val graph = execution.c.evidenceGraph
    val recordHashes = RecordHashes(graph)
    val fGraph = execution.f.evidenceGraph
    val fRecordHashes = RecordHashes(fGraph)
    val comparisonEndpointEvidenceSnapshots =
      if contract.capturesNativeRSelections then
        RelativeAssessmentAssembler
          .comparisonEndpointEvidenceSnapshots(execution.f)
          .map(comparisonEndpointEvidenceSnapshotJson(_, fGraph, fRecordHashes))
      else Nil
    val projectionJson = projectionSummary(trace, graph, contract)
    val verdictObservation =
      Option.when(contract.capturesNativeRSelections)(
        exactPlayedVsBestVerdictObservation(execution, trace)
      )
    val probeRequests = publicProbeRequests(trace)
    val publicIdeaParse = trace.projection
      .map(item =>
        projectedPublicIdeaParse(
          item.selectedPayload,
          graph,
          requireIdeaUnitLink = contract.capturesNativeRSelections
        )
      )
      .getOrElse(ProjectedPublicIdeaParse.empty)
    val publicIdeaUnitParse = trace.projection.map(item =>
      projectedPublicIdeaUnitParse(item.selectedPayload)
    )
    val selectedPublicCauseIdOccurrences = publicIdeaParse.causeEvidenceIdOccurrences
    val selectedPublicCauseIds = selectedPublicCauseIdOccurrences
      .distinct
      .sorted
    val selectedPublicCauseSelections = publicIdeaParse.selections
      .sortBy(item => (item.selectionOrder, item.causeEvidenceId))
    val rNativeCauseSelections =
      if contract.capturesNativeRSelections then projectedRSelections(execution.r, graph)
      else Nil
    val importanceObservation =
      if contract.capturesNativeRSelections then
        Some(
          projectedImportanceObservation(
            rNative = execution.r.causeExposureResolution.importanceResolution,
            packet = execution.packet.map(_.directCauseImportanceResolution),
            public = trace.projection.flatMap(item =>
              projectedPublicImportance(item.selectedPayload, "importance")
            )
          )
        )
      else None
    val ideaUnitObservation =
      if contract.capturesNativeRSelections then
        Some(
          projectedIdeaUnitObservation(
            rNative = execution.r.causeExposureResolution.ideaUnits,
            packet = execution.packet.map(_.causeExposureResolution.ideaUnits),
            public = publicIdeaUnitParse,
            publicItems = publicIdeaParse
          )
        )
      else None
    val ideaImportanceObservation =
      if contract.capturesNativeRSelections then
        Some(
          projectedImportanceObservation(
            rNative = execution.r.causeExposureResolution.ideaImportanceResolution,
            packet = execution.packet.map(_.causeExposureResolution.ideaImportanceResolution),
            public = trace.projection.flatMap(item =>
              projectedPublicImportance(item.selectedPayload, "idea_importance")
            )
          )
        )
      else None
    val causeDispositionLedgerObservation =
      if contract.capturesNativeRSelections then
        Some(causeDispositionLedgerObservationFor(execution, trace))
      else None
    val rNativeSelectionByCauseId = rNativeCauseSelections
      .groupBy(_.causeEvidenceId)
      .map { case (causeId, selections) =>
        selections match
          case selection :: Nil => causeId -> selection
          case _ =>
            throw IllegalStateException(
              s"R emitted multiple native selections for Cause $causeId"
            )
      }
    val onlyMoveConstraints = execution.packet.toList
      .flatMap(_.onlyMoveConstraintResolutions)
      .map(onlyMoveConstraintJson(_, graph, recordHashes))
    val causalEpisodeInventory = graph.records.flatMap {
      case EvidenceRecord(ref, line: LineFactEvidence, _) =>
        line.rootMove.toList.flatMap(line.rootOwnedCausalEpisodes).map(ref -> _)
      case _ =>
        Nil
    }.distinct
    val causes = graph.records.collect {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        causeCascade(
          record = record,
          cause = cause,
          graph = graph,
          execution = execution,
          selectedPublicCauseIds = selectedPublicCauseIds,
          selectedPublicCauseIdOccurrences = selectedPublicCauseIdOccurrences,
          selectedPublicCauseSelections = selectedPublicCauseSelections,
          recordHashes = recordHashes,
          rNativeSelection = rNativeSelectionByCauseId.get(record.ref.id),
          contract = contract
        )
    }.sortBy(item => (item \ "cause_record" \ "id").asOpt[String].getOrElse(""))
    val kindCounts = causes
      .flatMap(item => (item \ "c" \ "kind").asOpt[String])
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .toList
      .sortBy(_._1)

    val observation = Json.obj(
      "schema_version" -> contract.schemaVersion,
      "status" -> "complete",
      "request_sha256" -> requestSha256,
      "hash_contract" -> NativeTreeEncoder.HashContract,
      "runtime" -> runtimeMetadata,
      "public_response" -> publicResponseSummary(
        trace.finalPublicResponse,
        contract,
        trace.projection.exists(_.primaryEngineBacked)
      ),
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
      "root_owned_causal_episode_inventory" -> causalEpisodeInventory.map(causalEpisodeJson(_, graph)),
      "only_move_constraints" -> onlyMoveConstraints,
      "causes" -> causes
    )
    if contract.capturesNativeRSelections then
      observation ++ Json.obj(
        "verdict" -> verdictObservation,
        "r_native_cause_selections" -> JsArray(
          rNativeCauseSelections
            .sortBy(item => (item.selectionOrder, item.causeEvidenceId))
            .map(_.json(contract))
        ),
        "importance" -> importanceObservation,
        "idea_units" -> ideaUnitObservation,
        "idea_importance" -> ideaImportanceObservation,
        "cause_disposition_ledger" -> causeDispositionLedgerObservation,
        "comparison_endpoint_evidence_snapshots" -> comparisonEndpointEvidenceSnapshots
      )
    else observation

  private def causeCascade(
      record: EvidenceRecord,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      execution: JudgmentBoundaryExecution,
      selectedPublicCauseIds: List[String],
      selectedPublicCauseIdOccurrences: List[String],
      selectedPublicCauseSelections: List[ProjectedCauseSelection],
      recordHashes: RecordHashes,
      rNativeSelection: Option[ProjectedCauseSelection],
      contract: ObservationContract
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
    val packetHostedSelection = execution.packet.flatMap(packet =>
      hostedCauseSelection(packet, record.ref.id)
    )
    val packetSelected = packetHostedSelection.nonEmpty
    val packetProjectedSelection = for
      packet <- execution.packet
      hosted <- packetHostedSelection
      exactComparison <- comparison
      exactBinding <- binding
      projected <- projectedPacketSelection(packet, hosted, cause, exactComparison, exactBinding)
    yield projected
    val selectedPublicIds = selectedPublicCauseIds.filter(_ == record.ref.id)
    val publicSelectionCount = selectedPublicCauseIdOccurrences.count(_ == record.ref.id)
    val matchingPublicSelections = selectedPublicCauseSelections.filter(_.causeEvidenceId == record.ref.id)
    val publicSelection = Option.when(publicSelectionCount == 1)(matchingPublicSelections).flatMap {
      case selection :: Nil => Some(selection)
      case _                => None
    }
    val dominance = execution.r.causeDominanceDecisions.find(_.causeEvidenceId == record.ref.id)
    val crossComparisonExposure =
      execution.r.crossComparisonExposureDecisions.find(_.causeEvidenceId == record.ref.id)
    val causalEpisodes = cause.proof.toList.flatMap(proof =>
      graph.relativeCauseRootOwnedCausalEpisodes(cause, proof.directProof)
    ).distinct
    val nativeR = Json.obj(
      "resolutions" -> resolutions.sortBy(_.sourceClaimId).map(resolutionJson(_, execution.r)),
      "ranked" -> ranked.map { case (item, index) => rankedJson(item, index) },
      "deduplication" -> relevantDedup.sortBy(_.order).map(dedupJson),
      "fallback_dominance" -> dominance.fold[JsValue](JsNull)(dominanceJson),
      "cross_comparison_exposure" -> crossComparisonExposure
        .fold[JsValue](JsNull)(crossComparisonExposureJson(_, contract))
    )
    val rObservation =
      if contract.capturesNativeRSelections then
        nativeR + ("native_selection" -> rNativeSelection.fold[JsValue](JsNull)(_.json(contract)))
      else nativeR

    val cObservationBase = Json.obj(
      "kind" -> code(cause.kind),
      "source_side" -> code(cause.sourceSide),
      "comparison_evidence" -> sourceRefJson(cause.comparisonEvidence, graph, recordHashes),
      "support_evidence" -> cause.supportEvidence.distinctBy(_.id).sortBy(_.id).map(sourceRefJson(_, graph, recordHashes)),
      "comparison" -> comparison.fold[JsValue](JsNull)(comparisonJson),
      "attribution" -> attributionJson(cause),
      "binding" -> binding.fold[JsValue](JsNull)(bindingJson(_, contract)),
      "proof" -> proofJson(cause, graph, recordHashes),
      "proof_state" -> Json.obj(
        "raw_typed_depth" -> cause.hasRawTypedDepth(graph),
        "owned_typed_depth" -> cause.hasOwnedTypedDepth(graph),
        "strategic_contrast_depth" -> cause.hasStrategicContrastDepth(graph),
        "owned_strategic_contrast_depth" -> cause.hasOwnedStrategicContrastDepth(graph),
        "owned_admissible_long_term_proof" -> cause.hasOwnedAdmissibleLongTermProof(graph),
        "owned_tactical_proof" -> cause.hasOwnedTacticalProof(graph),
        "root_owned_causal_episode_count" -> causalEpisodes.size,
        "root_owned_forcing_tactical_episode_count" -> causalEpisodes.count(forcingTacticalResource(_, graph))
      ),
      "causal_episodes" -> causalEpisodes.map(causalEpisodeJson(_, graph)),
      "objects" -> objectSummary(cause, graph, recordHashes, contract)
    )
    val cObservation =
      if contract.capturesNativeRSelections then
        cObservationBase + (
          "direct_effect_admission" -> RuntimeProtocol.directEffectAdmissionPublicJson(
            cause.directEffectAdmission
          )
        )
      else cObservationBase

    Json.obj(
      "cause_record" -> sourceRefJson(record.ref, graph, recordHashes),
      "c" -> cObservation,
      "jp" -> Json.obj(
        "linked_claims" -> claimLinks.map(link => claimJson(link.claim) ++ Json.obj("link_kind" -> link.linkKind))
      ),
      "ja" -> Json.obj(
        "linked_decisions" -> admissions.sortBy(_.claim.id).map(admissionJson)
      ),
      "r" -> rObservation,
      "p" -> Json.obj(
        "packet_present" -> execution.packet.nonEmpty,
        "cause_evidence_id" -> record.ref.id,
        "packet_selected" -> packetSelected,
        "packet_selection" -> packetProjectedSelection.map(_.json(contract)),
        "public_selection_count" -> publicSelectionCount,
        "public_selection" -> publicSelection.map(_.json(contract)),
        "selected_public_cause_evidence_ids" -> selectedPublicIds
      )
    )

  /** Serializes the selection authority held by R itself. The packet is not
    * consulted: every selected channel is resolved against the immutable
    * channel inventory captured in `causeExposureResolution`.
    */
  private def projectedRSelections(
      ranking: ClaimRankingResult,
      graph: TypedEvidenceGraph
  ): List[ProjectedCauseSelection] =
    val resolution = ranking.causeExposureResolution
    val rankedByClaimId = ranking.ranked.map(item => item.claim.id -> item).toMap
    resolution.selections.map { selection =>
      val causeId = selection.causeEvidence.id
      val projected = for
        causeRecord <- graph.byId.get(causeId)
        if causeRecord.ref == selection.causeEvidence
        cause <- causeRecord.payload match
          case RelativeCauseFactEvidence(value) => Some(value)
          case _                                => None
        comparison <- graph.comparisonFor(cause)
        binding <- graph.relativeCauseBinding(cause)
        ownerClaimId <- resolution.ownerClaimIdByCauseId.get(causeId)
        host <- rankedByClaimId.get(ownerClaimId)
        if host.playerFacingCauseSelections.count(_ == selection) == 1
        directChannels <- resolution.directChannelsByCauseId.get(causeId)
        channels <- projectedRChannels(selection, directChannels)
      yield projectedNativeSelection(
        selection = selection,
        cause = cause,
        comparison = comparison,
        binding = binding,
        host = ProjectedHostSelection(
          claimId = host.claim.id,
          family = code(host.claim.family),
          tier = code(host.exposureTier)
        ),
        channels = channels,
      )
      projected.getOrElse(
        throw IllegalStateException(
          s"R native Cause selection cannot be resolved from its own exposure state: $causeId"
        )
      )
    }.sortBy(item => (item.selectionOrder, item.causeEvidenceId))

  /** Captures the typed importance authority without consulting exposure rank.
    * `r_native` retains every relation, including explicit incomparability.
    * The other three fields use the exact public projection shape so the
    * harness can prove R -> packet -> public copying independently.
    */
  private def projectedImportanceObservation(
      rNative: DirectCauseImportanceResolution,
      packet: Option[DirectCauseImportanceResolution],
      public: Option[JsObject]
  ): JsObject =
    Json.obj(
      "r_native" -> directCauseImportanceNativeJson(rNative),
      "packet_native" -> packet.map(directCauseImportanceNativeJson),
      "r_public_projection" -> RuntimeProtocol.directCauseImportancePublicJson(rNative),
      "packet_public_projection" -> packet.map(RuntimeProtocol.directCauseImportancePublicJson),
      "public_projection" -> public
    )

  private def directCauseImportanceNativeJson(
      resolution: DirectCauseImportanceResolution
  ): JsObject =
    Json.obj(
      "relation_policy_version" -> RuntimeProtocol.DirectCauseImportanceRelationPolicyVersion,
      "ordering_policy_version" -> RuntimeProtocol.PlayerFacingIdeaOrderingPolicyVersion,
      "selected_cause_ids" -> resolution.selectedCauseEvidenceIds,
      "frontier_cause_ids" -> resolution.frontierCauseEvidenceIds,
      "dominated_within_domain_cause_ids" -> resolution.dominatedWithinDomainCauseEvidenceIds,
      "unique_top" -> resolution.uniqueTopCauseEvidenceId.fold[JsValue](JsNull)(JsString.apply),
      "profiles" -> resolution.profiles.map { profile =>
        Json.obj(
          "cause_evidence_id" -> profile.causeEvidenceId,
          "causal_signature" -> profile.causalSignature,
          "universe" -> RuntimeProtocol.directCauseImportanceUniverseJson(profile.universe),
          "frame" -> Json.obj(
            "comparison" -> profile.frame.comparison.stableKey,
            "event_line" -> profile.frame.eventLine.stableKey,
            "source_side" -> code(profile.frame.sourceSide),
            "attribution" -> code(profile.frame.attribution),
            "exposure" -> code(profile.frame.exposure),
            "effect_mode" -> code(profile.frame.effectMode),
            "actor" -> (if profile.frame.actor.white then "white" else "black"),
            "direct_change" -> code(profile.frame.directChange),
            "played_change" -> code(profile.frame.playedChange),
            "stake" -> profile.frame.stake.stableKey
          ),
          "domain_kind" -> RuntimeProtocol.directCauseImportanceDomainKind(profile.domain),
          "domain" -> profile.domain.stableKey,
          "measure" -> RuntimeProtocol.directCauseImportanceMeasureJson(profile.measure),
          "effect_scope" -> RuntimeProtocol.rootOwnedEffectIdentityJson(profile.effectIdentity)
        )
      },
      "relations" -> resolution.relations.map { relation =>
        Json.obj(
          "left_cause_id" -> relation.leftCauseEvidenceId,
          "left_causal_signature" -> relation.leftCausalSignature,
          "right_cause_id" -> relation.rightCauseEvidenceId,
          "right_causal_signature" -> relation.rightCausalSignature,
          "relation" -> code(relation.relation),
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
      }
    )

  private def projectedCauseDisposition(disposition: CauseDisposition): JsObject =
    Json.obj(
      "cause_evidence_id" -> disposition.causeEvidence.id,
      "status" -> code(disposition.status),
      "reason" -> code(disposition.reason),
      "proposed_claim_ids" -> disposition.proposedClaimIds,
      "certified_claim_ids" -> disposition.certifiedClaimIds,
      "rank_eligible_claim_ids" -> disposition.rankEligibleClaimIds,
      "selected_owner_claim_id" -> disposition.selectedOwnerClaimId,
      "related_cause_evidence_ids" -> disposition.relatedCauseEvidenceIds,
      "related_claim_ids" -> disposition.relatedClaimIds
    )

  private def projectedCauseDispositionLedger(
      ledger: CauseDispositionLedger
  ): List[JsObject] =
    ledger.dispositions.map(projectedCauseDisposition)

  private def strictCauseDispositionSummary(
      value: JsLookupResult,
      label: String
  ): JsObject =
    val summary = value.toOption match
      case Some(item: JsObject) => item
      case Some(_) =>
        throw IllegalStateException(s"$label idea_status_detail must be an object")
      case None =>
        throw IllegalStateException(s"$label idea_status_detail is missing")
    val expectedFields = Set(
      "authority",
      "total_cause_count",
      "selected_cause_ids",
      "status_counts",
      "reason_counts",
      "abstention_codes"
    )
    if summary.keys != expectedFields then
      throw IllegalStateException(s"$label idea_status_detail has non-canonical fields")
    if (summary \ "authority").asOpt[String]
        .forall(_ != RuntimeProtocol.CauseDispositionSummaryAuthority)
    then
      throw IllegalStateException(s"$label idea_status_detail has the wrong authority")
    if (summary \ "total_cause_count").asOpt[Int].forall(_ < 0) then
      throw IllegalStateException(s"$label idea_status_detail has an invalid Cause count")

    def canonicalIds(field: String): List[String] =
      val values = (summary \ field).asOpt[List[String]].getOrElse(
        throw IllegalStateException(s"$label idea_status_detail.$field must be a string array")
      )
      if values.exists(_.isEmpty) || values != values.distinct.sorted then
        throw IllegalStateException(s"$label idea_status_detail.$field is not canonical")
      values

    canonicalIds("selected_cause_ids")
    canonicalIds("abstention_codes")

    def exactCounts(field: String, expectedKeys: Set[String]): Unit =
      val counts = (summary \ field).asOpt[JsObject].getOrElse(
        throw IllegalStateException(s"$label idea_status_detail.$field must be an object")
      )
      if counts.keys != expectedKeys || counts.values.exists(_.asOpt[Int].forall(_ < 0)) then
        throw IllegalStateException(s"$label idea_status_detail.$field is not canonical")

    exactCounts(
      "status_counts",
      CauseDispositionStatus.values.toSet.map(code)
    )
    exactCounts(
      "reason_counts",
      CauseDispositionReason.values.toSet.map(code)
    )
    summary

  private def causeDispositionLedgerObservationFor(
      execution: JudgmentBoundaryExecution,
      trace: RuntimeProtocol.BoundaryEvaluationTrace
  ): JsObject =
    val rLedger = execution.r.causeDispositionLedger
    val packetLedger = execution.packet.map(_.causeDispositionLedger)
    val selectedProjection = trace.projection.map(projection =>
      strictCauseDispositionSummary(
        projection.selectedPayload \ "idea_status_detail",
        "selected projection"
      )
    )
    val finalPublicResponse = trace.projection.map(_ =>
      strictCauseDispositionSummary(
        trace.finalPublicResponse.body \ "move_review" \ "idea_status_detail",
        "final public response"
      )
    )
    Json.obj(
      "r_native" -> projectedCauseDispositionLedger(rLedger),
      "packet_native" -> packetLedger.map(projectedCauseDispositionLedger),
      "r_public_summary" -> RuntimeProtocol.causeDispositionPublicJson(rLedger),
      "packet_public_summary" -> packetLedger.map(RuntimeProtocol.causeDispositionPublicJson),
      "selected_projection" -> selectedProjection,
      "final_public_response" -> finalPublicResponse
    )

  private def projectedIdeaUnit(unit: PlayerFacingIdeaUnit): ProjectedIdeaUnit =
    ProjectedIdeaUnit(
      ideaId = unit.id,
      kind = code(unit.kind),
      leadCauseEvidenceId = unit.leadCauseEvidenceId,
      memberCauseEvidenceIds = unit.memberCauseEvidenceIds,
      importanceLayer = unit.importanceLayer,
      priorityStatus = code(unit.priorityStatus),
      serializationOrder = unit.serializationOrder
    )

  private def parseProjectedIdeaUnit(value: JsValue): Option[ProjectedIdeaUnit] =
    for
      ideaId <- (value \ "idea_id").asOpt[String]
      kind <- (value \ "kind").asOpt[String]
      if PlayerFacingIdeaUnitKind.values.exists(item => code(item) == kind)
      leadCauseEvidenceId <- (value \ "lead_cause_evidence_id").asOpt[String]
      memberCauseEvidenceIds <- (value \ "member_cause_evidence_ids").asOpt[List[String]]
      importanceLayer <- (value \ "importance_layer").asOpt[Int]
      priorityStatus <- (value \ "priority_status").asOpt[String]
      if PlayerFacingIdeaPriorityStatus.values.exists(item => code(item) == priorityStatus)
      serializationOrder <- (value \ "serialization_order").asOpt[Int]
      if ideaId.nonEmpty && leadCauseEvidenceId.nonEmpty
      if memberCauseEvidenceIds.nonEmpty && memberCauseEvidenceIds.forall(_.nonEmpty)
      if memberCauseEvidenceIds.distinct.size == memberCauseEvidenceIds.size
      if memberCauseEvidenceIds.head == leadCauseEvidenceId
      if kind != code(PlayerFacingIdeaUnitKind.SingleCause) || memberCauseEvidenceIds.size == 1
      if kind != code(PlayerFacingIdeaUnitKind.ExactPvbResponsibility) || memberCauseEvidenceIds.size >= 2
      if importanceLayer >= 0 && serializationOrder >= 0
    yield ProjectedIdeaUnit(
      ideaId,
      kind,
      leadCauseEvidenceId,
      memberCauseEvidenceIds,
      importanceLayer,
      priorityStatus,
      serializationOrder
    )

  private def projectedPublicIdeaUnitParse(
      payload: JsObject
  ): ProjectedPublicIdeaUnitParse =
    val explanations = (payload \ "explanations").asOpt[JsArray]
    val unitArrays = explanations.toList.flatMap(_.value).map(explanation =>
      (explanation \ "idea_units").asOpt[JsArray]
    )
    val rawUnits = unitArrays.flatten.flatMap(_.value)
    ProjectedPublicIdeaUnitParse(
      rawUnitCount = rawUnits.size,
      units = rawUnits.flatMap(parseProjectedIdeaUnit).toList,
      containersClosed = explanations.nonEmpty && unitArrays.forall(_.nonEmpty)
    )

  private def projectedIdeaUnitObservation(
      rNative: List[PlayerFacingIdeaUnit],
      packet: Option[List[PlayerFacingIdeaUnit]],
      public: Option[ProjectedPublicIdeaUnitParse],
      publicItems: ProjectedPublicIdeaParse
  ): JsObject =
    val rUnits = rNative.map(projectedIdeaUnit)
    val packetUnits = packet.map(_.map(projectedIdeaUnit))
    val publicUnits = public.map(_.units)
    val membershipClosed = public match
      case None => publicItems.rawIdeaCount == 0 && publicItems.parseClosed
      case Some(parsed) =>
        val orderedUnits = parsed.units.sortBy(_.serializationOrder)
        parsed.parseClosed &&
          publicItems.parseClosed &&
          orderedUnits.map(_.serializationOrder) == orderedUnits.indices.toList &&
          orderedUnits.map(_.ideaId).distinct.size == orderedUnits.size &&
          orderedUnits.flatMap(_.itemLinks) == publicItems.itemLinks
    Json.obj(
      "r_native" -> rUnits.map(_.json),
      "packet_native" -> packetUnits.map(_.map(_.json)),
      "public_projection" -> publicUnits.map(_.map(_.json)),
      "public_item_links" -> publicItems.itemLinks.map(_.json),
      "public_item_membership_closed" -> membershipClosed
    ) ++ public.getOrElse(ProjectedPublicIdeaUnitParse.absent).diagnosticJson

  private def projectedPublicImportance(
      payload: JsObject,
      field: String
  ): Option[JsObject] =
    val values = (payload \ "explanations").asOpt[JsArray].toList
      .flatMap(_.value)
      .flatMap(explanation => (explanation \ field).asOpt[JsObject])
    values match
      case Nil           => None
      case value :: Nil  => Some(value)
      case _ =>
        throw IllegalStateException(s"public payload emitted multiple $field projections")

  private def projectedRChannels(
      selection: PlayerFacingCauseSelection,
      directChannels: List[DirectCauseChannel]
  ): Option[List[ProjectedChannelSelection]] =
    val projected = selection.channels.map { selected =>
      directChannels.filter(channel =>
        channel.binding.source == selected.carrierEvidence &&
          channel.causalSignature == selected.causalSignature &&
          channel.directChange == selected.directChange
      ) match
        case channel :: Nil => Some(projectedChannelSelection(selected, channel))
        case _              => None
    }
    Option.when(projected.nonEmpty && projected.forall(_.nonEmpty))(
      projected.flatten.sortBy(_.sortKey)
    )

  private def hostedCauseSelection(
      packet: EvidenceBackedJudgmentPacket,
      causeEvidenceId: String
  ): Option[HostedCauseSelection] =
    val claimsById = packet.claims.map(claim => claim.id -> claim).toMap
    packet.playerFacingClaimDecisions.flatMap(decision =>
      claimsById.get(decision.claimId).toList.flatMap(claim =>
        decision.causeSelections
          .filter(_.causeEvidence.id == causeEvidenceId)
          .map(selection => HostedCauseSelection(claim, decision, selection))
      )
    ) match
      case hosted :: Nil => Some(hosted)
      case _             => None

  private def projectedPacketSelection(
      packet: EvidenceBackedJudgmentPacket,
      hosted: HostedCauseSelection,
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact,
      binding: RelativeCauseBinding
  ): Option[ProjectedCauseSelection] =
    val selection = hosted.selection
    val projectedChannels = selection.channels.map(selected =>
      packet.directCauseChannel(selection, selected).map(channel =>
        projectedChannelSelection(selected, channel)
      )
    )
    Option.when(projectedChannels.nonEmpty && projectedChannels.forall(_.nonEmpty))(
      projectedNativeSelection(
        selection = selection,
        cause = cause,
        comparison = comparison,
        binding = binding,
        host = ProjectedHostSelection(
          claimId = hosted.claim.id,
          family = code(hosted.claim.family),
          tier = code(hosted.decision.tier)
        ),
        channels = projectedChannels.flatten.sortBy(_.sortKey)
      )
    )

  private def projectedNativeSelection(
      selection: PlayerFacingCauseSelection,
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact,
      binding: RelativeCauseBinding,
      host: ProjectedHostSelection,
      channels: List[ProjectedChannelSelection]
  ): ProjectedCauseSelection =
    ProjectedCauseSelection(
      causeEvidenceId = selection.causeEvidence.id,
      comparisonExposureRank = selection.comparisonExposureRank,
      selectionOrder = selection.selectionOrder,
      exposure = code(selection.exposure),
      effectMode = code(selection.effectMode),
      causeKind = code(cause.kind),
      sourceSide = code(cause.sourceSide),
      directEffectAdmission = RuntimeProtocol.directEffectAdmissionPublicJson(
        cause.directEffectAdmission
      ),
      attribution = ProjectedAttributionSelection(
        kind = code(cause.attribution.kind),
        rootMoveMatched = cause.attribution.rootMoveMatched,
        directProofEligible = cause.attribution.directProofEligible
      ),
      host = host,
      comparison = projectedComparisonSelection(cause, comparison, binding),
      comparisonSemanticKey = CandidateComparisonSemanticKey.from(comparison).stableKey,
      channels = channels.sortBy(_.sortKey),
      onlyMove = selection.onlyMoveQualifier.map(projectedOnlyMoveQualifier)
    )

  private def projectedComparisonSelection(
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact,
      binding: RelativeCauseBinding
  ): ProjectedComparisonSelection =
    val comparedMove =
      if binding.eventLine == comparison.referenceLine then comparison.candidateLine.rootMove
      else comparison.referenceLine.rootMove
    ProjectedComparisonSelection(
      evidenceId = cause.comparisonEvidence.id,
      kind = code(comparison.kind),
      mover = code(comparison.comparison.mover),
      reference = projectedSemanticLine(comparison.referenceLine),
      candidate = projectedSemanticLine(comparison.candidateLine),
      event = projectedSemanticLine(binding.eventLine),
      comparedMove = EvidenceRef.normalizeMove(comparedMove),
      verdict = code(comparison.comparison.verdict),
      candidateWinPercentDeltaForMover = comparison.comparison.candidateWinPercentDeltaForMover,
      winPercentLossForMover = comparison.comparison.winPercentLossForMover
    )

  private def projectedChannelSelection(
      selected: PlayerFacingCauseChannelSelection,
      channel: DirectCauseChannel
  ): ProjectedChannelSelection =
    val binding = channel.binding
    ProjectedChannelSelection(
      carrier = projectedEvidenceRef(binding.source),
      provenance = binding.provenance.map(projectedEvidenceRef).distinct.sortBy(_.sortKey),
      causalSignature = selected.causalSignature,
      directChange = code(selected.directChange),
      playedChange = code(selected.playedChange),
      actor = projectedPublicActor(binding),
      targets = projectedObjects(binding.target),
      mechanisms = projectedObjects(binding.mechanism),
      consequences = projectedObjects(binding.consequence),
      witnesses = projectedObjects(binding.witness),
      line = binding.line.map(projectedSemanticLine),
      horizon = binding.horizon,
      proofSegment = channel.proofSegment.map(RuntimeProtocol.directCauseProofSegmentPublicJson),
      effectDescriptor = channel.rootOwnedEffectDescriptor.map(
        RuntimeProtocol.rootOwnedEffectDescriptorPublicJson
      ),
      importanceEffect = RuntimeProtocol.directCauseMeasuredEffectPublicJson(channel),
      descriptorAmbiguous = channel.importanceDescriptorAmbiguous,
      proofSegmentAmbiguous = channel.proofSegmentAmbiguous
    )

  private def projectedOnlyMoveQualifier(
      qualifier: OnlyMoveCauseQualifier
  ): ProjectedOnlyMoveQualifier =
    ProjectedOnlyMoveQualifier(
      comparisonEvidenceId = qualifier.comparisonEvidence.id,
      causeEvidenceId = qualifier.causeEvidence.id,
      reference = projectedSemanticLine(qualifier.referenceLine),
      relation = code(qualifier.relation),
      licensesCausalBecause = qualifier.licensesCausalBecause
    )

  private def projectedEvidenceRef(ref: EvidenceRef): ProjectedEvidenceRef =
    ProjectedEvidenceRef(
      id = ref.id,
      producer = code(ref.producer),
      layer = code(ref.layer),
      scope = code(ref.scope),
      line = ref.line.map(projectedSemanticLine)
    )

  private def projectedObjects(values: List[ConcreteChessObject]): List[ProjectedChessObject] =
    values
      .map(value => ProjectedChessObject(code(value.kind), value.key))
      .distinct
      .sortBy(_.sortKey)

  private def projectedPublicActor(binding: EvidenceObjectBinding): List[ProjectedChessObject] =
    val rootMove = binding.line.map(_.rootMove).map(EvidenceRef.normalizeMove).getOrElse("")
    def first(kind: EvidenceObjectKind): Option[String] =
      binding.actor
        .find(_.kind == kind)
        .map(_.key.trim.toLowerCase(Locale.ROOT))
    projectedObjects(
      List(
        Option.when(rootMove.nonEmpty)(ConcreteChessObject(EvidenceObjectKind.Move, rootMove)),
        first(EvidenceObjectKind.Side).map(ConcreteChessObject(EvidenceObjectKind.Side, _)),
        first(EvidenceObjectKind.Piece).map(ConcreteChessObject(EvidenceObjectKind.Piece, _)),
        Option.when(rootMove.length >= 4)(
          ConcreteChessObject(EvidenceObjectKind.Square, rootMove.take(2))
        ),
        Option.when(rootMove.length >= 4)(
          ConcreteChessObject(EvidenceObjectKind.Square, rootMove.slice(2, 4))
        )
      ).flatten
    )

  private def projectedSemanticLine(line: LineNodeRef): ProjectedSemanticLine =
    ProjectedSemanticLine(
      lineId = line.id,
      role = code(line.role),
      rank = line.rank,
      rootMove = EvidenceRef.normalizeMove(line.rootMove)
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

  private def comparisonEndpointEvidenceSnapshotJson(
      snapshot: ComparisonEndpointEvidenceSnapshot,
      graph: TypedEvidenceGraph,
      recordHashes: RecordHashes
  ): JsObject =
    Json.obj(
      "comparison_evidence" -> sourceRefJson(snapshot.comparisonEvidence, graph, recordHashes),
      "comparison" -> comparisonJson(snapshot.comparison),
      "reference" -> comparisonEndpointEvidenceSideJson(
        snapshot.reference,
        graph,
        recordHashes
      ),
      "candidate" -> comparisonEndpointEvidenceSideJson(
        snapshot.candidate,
        graph,
        recordHashes
      )
    )

  private def comparisonEndpointEvidenceSideJson(
      side: ComparisonEndpointEvidenceSideSnapshot,
      graph: TypedEvidenceGraph,
      recordHashes: RecordHashes
  ): JsObject =
    Json.obj(
      "source_side" -> code(side.sourceSide),
      "line" -> projectedSemanticLine(side.line).json,
      "witnesses" -> side.witnesses.map(
        comparisonEndpointEvidenceWitnessJson(_, graph, recordHashes)
      )
    )

  private def comparisonEndpointEvidenceWitnessJson(
      witness: ComparisonEndpointEvidenceWitness,
      graph: TypedEvidenceGraph,
      recordHashes: RecordHashes
  ): JsObject =
    Json.obj(
      "source_side" -> code(witness.sourceSide),
      "line" -> projectedSemanticLine(witness.line).json,
      "carrier" -> sourceRefJson(witness.carrier, graph, recordHashes),
      "provenance" -> witness.provenance
        .distinctBy(_.id)
        .sortBy(_.id)
        .map(sourceRefJson(_, graph, recordHashes)),
      "carrier_ancestor_source_ids" -> witness.carrierAncestorSourceIds.distinct.sorted,
      "primitive_proof_source" -> sourceRefJson(
        witness.primitiveProofSource,
        graph,
        recordHashes
      ),
      "actor" -> aggregateObjects(witness.binding.actor),
      "target" -> aggregateObjects(witness.binding.target),
      "mechanism" -> aggregateObjects(witness.binding.mechanism),
      "consequence" -> aggregateObjects(witness.binding.consequence),
      "witness" -> aggregateObjects(witness.binding.witness),
      "horizon" -> witness.binding.horizon,
      "proof_segment" -> RuntimeProtocol.directCauseProofSegmentPublicJson(
        witness.proofSegment
      ),
      "effect_descriptor" -> RuntimeProtocol.rootOwnedEffectDescriptorPublicJson(
        witness.effectDescriptor
      )
    )

  private def onlyMoveConstraintJson(
      resolution: OnlyMoveConstraintResolution,
      graph: TypedEvidenceGraph,
      recordHashes: RecordHashes
  ): JsObject =
    Json.obj(
      "comparison_evidence" -> sourceRefJson(resolution.comparisonEvidence, graph, recordHashes),
      "reference_line" -> lineJson(resolution.referenceLine),
      "candidate_line" -> lineJson(resolution.candidateLine),
      "disposition" -> code(resolution.disposition),
      "qualifiers" -> resolution.qualifiers.map(qualifier =>
        Json.obj(
          "cause_evidence" -> sourceRefJson(qualifier.causeEvidence, graph, recordHashes),
          "relation" -> code(qualifier.relation),
          "licenses_causal_because" -> qualifier.licensesCausalBecause
        )
      )
    )

  private def attributionJson(cause: RelativeCauseFact): JsObject =
    Json.obj(
      "kind" -> code(cause.attribution.kind),
      "root_move_matched" -> cause.attribution.rootMoveMatched,
      "direct_proof_eligible" -> cause.attribution.directProofEligible,
      "reason" -> cause.attribution.reason
    )

  private def bindingJson(
      binding: RelativeCauseBinding,
      contract: ObservationContract
  ): JsObject =
    val tier =
      if contract.capturesNativeRSelections then
        Json.obj("binding_tier" -> code(binding.bindingTier))
      else Json.obj("importance" -> code(binding.bindingTier))
    Json.obj(
      "role" -> code(binding.role),
      "source_side" -> code(binding.sourceSide),
      "event_line" -> lineJson(binding.eventLine),
      "evidence_lines" -> binding.evidenceLines.sortBy(_.id).map(lineJson)
    ) ++ tier

  private def causalEpisodeJson(
      item: (EvidenceRef, RootOwnedCausalEpisode),
      graph: TypedEvidenceGraph
  ): JsObject =
    val (source, episode) = item
    Json.obj(
      "source_id" -> source.id,
      "line" -> lineJson(episode.line),
      "actor" -> Json.obj(
        "move" -> episode.actor.moveUci,
        "role" -> episode.actor.role.name,
        "color" -> code(episode.actor.color),
        "from" -> episode.actor.from.key,
        "to" -> episode.actor.to.key
      ),
      "target" -> episode.target.key,
      "mechanisms" -> episode.links.map(link => code(link.kind)).distinct,
      "consequence" -> code(episode.consequence.kind),
      "event_move" -> episode.consequence.eventMove,
      "event_ply_offset" -> episode.eventPlyOffset,
      "chain_moves" -> episode.chainMoves,
      "forcing_tactical_resource" -> forcingTacticalResource(item, graph)
    )

  private def forcingTacticalResource(
      item: (EvidenceRef, RootOwnedCausalEpisode),
      graph: TypedEvidenceGraph
  ): Boolean =
    val (source, episode) = item
    graph.record(source).exists {
      case EvidenceRecord(_, line: LineFactEvidence, _) =>
        episode.forcingTacticalResource(line)
      case _ =>
        false
    }

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
      recordHashes: RecordHashes,
      contract: ObservationContract
  ): JsObject =
    val rawChannels = EvidenceObjectBinding
      .directCauseChannelsForProjection(cause, graph)
      .sortBy(_.causalSignature)
    val preAdmissionChannels = RelativeCauseConstructionAdmission
      .diagnosticRawDirectChannels(cause, graph)
      .sortBy(_.causalSignature)
    val channels = RelativeCauseConstructionAdmission
      .admittedDirectChannels(cause, graph)
      .sortBy(_.causalSignature)
    val bindings = channels.map(_.binding)
    val rawBindings = EvidenceObjectBinding.fromRelativeCause(cause, graph)
    val relativeTargets = bindings
      .flatMap(_.target)
      .distinctBy(_.signaturePart)
      .sortBy(_.signaturePart)
    val deterrenceRefs = graph.opponentResourceDeterrenceEventRefs(cause).distinctBy(_.id).sortBy(_.id)
    val deterrenceIds = deterrenceRefs.map(_.id).toSet
    val restrictionTargets = bindings
      .filter(binding => (binding.source :: binding.provenance).exists(ref => deterrenceIds(ref.id)))
      .flatMap(_.target)
      .distinctBy(_.signaturePart)
      .sortBy(_.signaturePart)
    val base = Json.obj(
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
      "owned_bindings" -> channels.map(directCauseChannelJson(_, contract, graph, recordHashes)),
      "raw_owned_bindings" -> rawChannels.map(directCauseChannelJson(_, contract, graph, recordHashes)),
      "relative_cause_targets" -> relativeTargets.map(concreteObjectJson),
      "opponent_restriction_event_sources" -> deterrenceRefs.map(sourceRefJson(_, graph, recordHashes)),
      "opponent_restriction_targets" -> restrictionTargets.map(concreteObjectJson)
    )
    if contract.capturesNativeRSelections then
      base + (
        "pre_admission_owned_bindings" -> JsArray(
          preAdmissionChannels.map(directCauseChannelJson(_, contract, graph, recordHashes))
        )
      )
    else base

  private def directCauseChannelJson(
      channel: DirectCauseChannel,
      contract: ObservationContract,
      graph: TypedEvidenceGraph,
      recordHashes: RecordHashes
  ): JsObject =
    val binding = channel.binding
    def bindingObjects(values: List[ConcreteChessObject]): List[JsObject] =
      if contract.capturesNativeRSelections then aggregateObjects(values)
      else values.map(concreteObjectJson)
    val projected = Json.obj(
      "signature" -> binding.signature,
      "causal_signature" -> channel.causalSignature,
      "primitive_signature" -> channel.primitiveSignature,
      "direct_change" -> code(channel.directChange),
      "source_id" -> binding.source.id,
      "provenance_source_ids" -> binding.provenance.map(_.id).distinct,
      "proof_role" -> binding.proofRole.map(value => code(value.toString)),
      "line_id" -> binding.line.map(_.id),
      "actor" -> bindingObjects(binding.actor),
      "target" -> bindingObjects(binding.target),
      "mechanism" -> bindingObjects(binding.mechanism),
      "consequence" -> bindingObjects(binding.consequence),
      "witness" -> bindingObjects(binding.witness),
      "specific_target_mechanism_ready" -> binding.specificTargetMechanismReady
    )
    if contract.capturesNativeRSelections then
      val carrier = sourceRefJson(binding.source, graph, recordHashes)
      val provenance = binding.provenance
        .map(sourceRefJson(_, graph, recordHashes))
        .distinct
        .sortBy(item => (item \ "id").as[String])
      val primitiveProofSource = channel.rootOwnedProof
        .map(_.primitiveSource)
        .map(sourceRefJson(_, graph, recordHashes))
      projected ++ Json.obj(
        "carrier" -> carrier,
        "carrier_ancestor_source_ids" -> graph
          .record(binding.source)
          .toList
          .flatMap(graph.parentClosure)
          .map(_.ref.id)
          .distinct
          .sorted,
        "provenance" -> provenance,
        "primitive_proof_source" -> primitiveProofSource,
        "evidence_identity_sha256" -> NativeTreeEncoder.sha256Canonical(Json.obj(
          "carrier" -> carrier,
          "provenance" -> provenance,
          "primitive_proof_source" -> primitiveProofSource
        )),
        "line" -> binding.line.map(line => projectedSemanticLine(line).json),
        "horizon" -> binding.horizon,
        "proof_segment" -> channel.proofSegment.map(RuntimeProtocol.directCauseProofSegmentPublicJson),
        "effect_descriptor" -> channel.rootOwnedEffectDescriptor.map(
          RuntimeProtocol.rootOwnedEffectDescriptorPublicJson
        ),
        "importance_effect" -> RuntimeProtocol.directCauseMeasuredEffectPublicJson(channel),
        "importance_descriptor_ambiguous" -> channel.importanceDescriptorAmbiguous,
        "proof_segment_ambiguous" -> channel.proofSegmentAmbiguous
      )
    else projected

  private[runtimeadapter] def projectedCOwnedChannelDocument(
      channel: DirectCauseChannel,
      schemaVersion: Int,
      graph: TypedEvidenceGraph
  ): JsObject =
    val contract = schemaVersion match
      case 2 => ObservationContract.V2
      case 3 => ObservationContract.V3
      case unsupported =>
        throw IllegalArgumentException(s"unsupported Cause audit observation version: $unsupported")
    directCauseChannelJson(channel, contract, graph, RecordHashes(graph))

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
      "player_facing_cause_evidence_ids" -> item.playerFacingCauseEvidenceIds,
      "only_move_qualifiers" -> item.onlyMoveQualifiers.map(qualifier =>
        Json.obj(
          "comparison_evidence_id" -> qualifier.comparisonEvidence.id,
          "cause_evidence_id" -> qualifier.causeEvidence.id,
          "relation" -> code(qualifier.relation),
          "licenses_causal_because" -> qualifier.licensesCausalBecause
        )
      )
    )

  private def dominanceJson(decision: RelativeCauseDominanceDecision): JsObject =
    Json.obj(
      "status" -> code(decision.status),
      "retained" -> decision.retained,
      "dominating_cause_evidence_ids" -> decision.dominatingCauseEvidenceIds
    )

  private def crossComparisonExposureJson(
      decision: CrossComparisonExposureDecision,
      contract: ObservationContract
  ): JsObject =
    val rank =
      if contract.capturesNativeRSelections then
        Json.obj("comparison_exposure_rank" -> decision.comparisonExposureRank)
      else Json.obj("priority_rank" -> decision.comparisonExposureRank)
    Json.obj(
      "status" -> code(decision.status),
      "selected" -> decision.selected,
      "reason" -> code(decision.reason),
      "representative_cause_evidence_id" -> decision.representativeCauseEvidenceId,
      "effect_mode" -> decision.effectMode.fold[JsValue](JsNull)(mode => JsString(code(mode)))
    ) ++ rank

  private def dedupJson(item: ClaimDeduplicationTrace): JsObject =
    Json.obj(
      "order" -> item.order,
      "original_claim_id" -> item.originalClaimId,
      "kept_claim_id" -> item.keptClaimId,
      "reason" -> item.reason.id
    )

  private def projectionSummary(
      trace: RuntimeProtocol.BoundaryEvaluationTrace,
      graph: TypedEvidenceGraph,
      contract: ObservationContract
  ): JsObject =
    trace.projection match
      case None =>
        val summary = Json.obj(
          "present" -> false,
          "packet_present" -> trace.boundaryExecution.flatMap(_.packet).nonEmpty,
          "selected_public_cause_selections" -> Json.arr(),
          "selected_public_cause_evidence_ids" -> Json.arr()
        )
        if contract.capturesNativeRSelections then
          summary ++ ProjectedPublicIdeaParse.empty.diagnosticJson
        else summary
      case Some(projection) =>
        val parsed = projectedPublicIdeaParse(
          projection.selectedPayload,
          graph,
          requireIdeaUnitLink = contract.capturesNativeRSelections
        )
        val selectedIds = parsed.causeEvidenceIdOccurrences.distinct.sorted
        val selections = parsed.selections
          .sortBy(item => (item.selectionOrder, item.causeEvidenceId))
        val summary = Json.obj(
          "present" -> true,
          "packet_present" -> trace.boundaryExecution.flatMap(_.packet).nonEmpty,
          "status" -> projection.status.id,
          "reason" -> projection.reason.map(_.id),
          "renderable" -> projection.renderable,
          "selected_payload_source" -> projection.selectedPayloadSource.id,
          "selected_payload_sha256" -> NativeTreeEncoder.sha256Canonical(projection.selectedPayload),
          "selected_public_cause_selections" -> selections.map(_.json(contract)),
          "selected_public_cause_evidence_ids" -> selectedIds
        )
        if contract.capturesNativeRSelections then summary ++ parsed.diagnosticJson
        else summary

  /** V3 observes the exact comparison DTO independently of Cause selection.
    * The packet codec is authoritative; the actual full projection, selected
    * projection, and final public response must carry that DTO unchanged.
    */
  private def exactPlayedVsBestVerdictObservation(
      execution: JudgmentBoundaryExecution,
      trace: RuntimeProtocol.BoundaryEvaluationTrace
  ): JsObject =
    val packetCanonical = execution.packet.flatMap(
      RuntimeProtocol.primaryEngineBackedVerdictPublicJson
    )
    val classificationAuthority = execution.packet.flatMap(
      RuntimeProtocol.primaryEngineBackedVerdictClassificationAuthorityJson
    )
    val actualFullProjection = trace.projection
      .flatMap(_.actualDecision.fullPayload)
      .flatMap(payload => nullableVerdict(payload \ "verdict", "actual packet projection"))
    val selectedProjection = trace.projection.flatMap(projection =>
      nullableVerdict(projection.selectedPayload \ "verdict", "selected projection")
    )
    val finalPublicResponse = trace.projection.flatMap(_ =>
      nullableVerdict(
        trace.finalPublicResponse.body \ "move_review" \ "verdict",
        "final public response"
      )
    )
    if packetCanonical != actualFullProjection then
      throw IllegalStateException(
        "packet canonical PlayedVsBest verdict differs from the actual full projection"
      )
    if packetCanonical != selectedProjection || packetCanonical != finalPublicResponse then
      throw IllegalStateException(
        "PlayedVsBest verdict changed between packet, selected projection, and final public response"
      )
    if packetCanonical.isDefined != classificationAuthority.isDefined ||
        packetCanonical.zip(classificationAuthority).exists { case (verdict, authority) =>
          !classificationAuthorityMatchesVerdict(authority, verdict)
        }
    then
      throw IllegalStateException(
        "public PlayedVsBest verdict differs from central registered-line classification authority"
      )
    Json.obj(
      "packet_canonical" -> packetCanonical,
      "selected_projection" -> selectedProjection,
      "final_public_response" -> finalPublicResponse,
      "classification_authority" -> classificationAuthority
    )

  private def classificationAuthorityMatchesVerdict(
      authority: JsObject,
      verdict: JsObject
  ): Boolean =
    def valueAt(value: JsLookupResult): JsValue = value.toOption.getOrElse(JsNull)
    val mate = (verdict \ "mate").asOpt[JsObject]
    List(
      valueAt(authority \ "comparison_kind") -> valueAt(verdict \ "comparison_kind"),
      valueAt(authority \ "mover") -> valueAt(verdict \ "mover"),
      valueAt(authority \ "played_move") -> valueAt(verdict \ "played_move"),
      valueAt(authority \ "reference_move") -> valueAt(verdict \ "reference_move"),
      valueAt(authority \ "candidate_win_percent_delta_for_mover") ->
        valueAt(verdict \ "candidate_win_percent_delta_for_mover"),
      valueAt(authority \ "win_percent_loss_for_mover") ->
        valueAt(verdict \ "win_percent_loss_for_mover"),
      valueAt(authority \ "expected_verdict_code") -> valueAt(verdict \ "verdict_code"),
      valueAt(authority \ "expected_move_quality") -> valueAt(verdict \ "move_quality"),
      valueAt(authority \ "reference_mate_for_mover") ->
        mate.map(item => valueAt(item \ "reference_for_mover")).getOrElse(JsNull),
      valueAt(authority \ "played_mate_for_mover") ->
        mate.map(item => valueAt(item \ "played_for_mover")).getOrElse(JsNull),
      valueAt(authority \ "mate_distance_loss_for_mover") ->
        mate.map(item => valueAt(item \ "distance_loss")).getOrElse(JsNull)
    ).forall { case (left, right) => left == right }

  private def nullableVerdict(value: JsLookupResult, label: String): Option[JsObject] =
    value.toOption match
      case Some(JsNull)         => None
      case Some(item: JsObject) => Some(item)
      case Some(_) =>
        throw IllegalStateException(s"$label verdict must be an object or null")
      case None =>
        throw IllegalStateException(s"$label verdict field is missing")

  private final case class ProjectedPublicIdeaParse(
      rawIdeaCount: Int,
      causeEvidenceIdOccurrences: List[String],
      items: List[ProjectedPublicIdeaItem]
  ):
    def selections: List[ProjectedCauseSelection] = items.map(_.selection)
    def itemLinks: List[ProjectedPublicIdeaItemLink] = items.map(_.link)

    def parseClosed: Boolean =
      causeEvidenceIdOccurrences.size == rawIdeaCount && items.size == rawIdeaCount

    def diagnosticJson: JsObject =
      Json.obj(
        "raw_public_idea_count" -> rawIdeaCount,
        "parsed_public_idea_count" -> selections.size,
        "public_ideas_parse_closed" -> parseClosed
      )

  private object ProjectedPublicIdeaParse:
    val empty: ProjectedPublicIdeaParse = ProjectedPublicIdeaParse(0, Nil, Nil)

  private def projectedPublicIdeaValues(payload: JsObject): List[JsValue] =
    (payload \ "explanations").asOpt[JsArray].toList
      .flatMap(_.value)
      .flatMap(explanation => (explanation \ "ideas").asOpt[JsArray].toList.flatMap(_.value))

  private def projectedPublicIdeaParse(
      payload: JsObject,
      graph: TypedEvidenceGraph,
      requireIdeaUnitLink: Boolean = true
  ): ProjectedPublicIdeaParse =
    val rawIdeas = projectedPublicIdeaValues(payload)
    ProjectedPublicIdeaParse(
      rawIdeaCount = rawIdeas.size,
      causeEvidenceIdOccurrences = rawIdeas.flatMap(idea => (idea \ "cause_evidence_id").asOpt[String]),
      items = rawIdeas.flatMap(idea =>
        for
          selection <- parseProjectedCauseSelection(idea, graph)
          link <-
            if requireIdeaUnitLink then parseProjectedPublicIdeaItemLink(idea)
            else
              Some(
                ProjectedPublicIdeaItemLink(
                  selection.causeEvidenceId,
                  "cause_facet",
                  selection.causeEvidenceId
                )
              )
          if link.causeEvidenceId == selection.causeEvidenceId
        yield ProjectedPublicIdeaItem(selection, link)
      )
    )

  private def parseProjectedPublicIdeaItemLink(
      value: JsValue
  ): Option[ProjectedPublicIdeaItemLink] =
    for
      causeEvidenceId <- (value \ "cause_evidence_id").asOpt[String]
      itemRole <- (value \ "item_role").asOpt[String]
      ideaUnitId <- (value \ "idea_unit_id").asOpt[String]
      if causeEvidenceId.nonEmpty && itemRole == "cause_facet" && ideaUnitId.nonEmpty
    yield ProjectedPublicIdeaItemLink(causeEvidenceId, itemRole, ideaUnitId)

  private def parseProjectedCauseSelection(
      value: JsValue,
      graph: TypedEvidenceGraph
  ): Option[ProjectedCauseSelection] =
    value match
      case idea: JsObject =>
        for
            causeEvidenceId <- (idea \ "cause_evidence_id").asOpt[String]
            causeRecord <- graph.byId.get(causeEvidenceId)
            cause <- causeRecord.payload match
              case RelativeCauseFactEvidence(value) => Some(value)
              case _                                => None
            comparison <- graph.comparisonFor(cause)
            binding <- graph.relativeCauseBinding(cause)
            comparisonExposureRank <- (idea \ "comparison_exposure_rank").asOpt[Int]
            selectionOrder <- (idea \ "selection_order").asOpt[Int]
            exposure <- (idea \ "cause" \ "exposure").asOpt[String]
            if PlayerFacingCauseExposureTier.values.exists(item => code(item) == exposure)
            effectModeCode <- (idea \ "cause" \ "effect_mode").asOpt[String]
            effectMode <- PlayerFacingCauseEffectMode.values.find(item => code(item) == effectModeCode)
            causeKind <- (idea \ "cause" \ "kind").asOpt[String]
            sourceSide <- (idea \ "cause" \ "source_side").asOpt[String]
            directEffectAdmission <- (idea \ "cause" \ "direct_effect_admission").asOpt[JsObject]
            attribution <- (idea \ "cause" \ "attribution").toOption.flatMap(parseProjectedAttribution)
            expectedAttribution = ProjectedAttributionSelection(
              code(cause.attribution.kind),
              cause.attribution.rootMoveMatched,
              cause.attribution.directProofEligible
            )
            if causeKind == code(cause.kind)
            if sourceSide == code(cause.sourceSide)
            if cause.directEffectAdmission.productionReady
            if directEffectAdmission == RuntimeProtocol.directEffectAdmissionPublicJson(
              cause.directEffectAdmission
            )
            if attribution == expectedAttribution
            host <- (idea \ "host").toOption.flatMap(parseProjectedHost)
            projectedComparison <- (idea \ "comparison").toOption.flatMap(parseProjectedComparison)
            expectedComparison = projectedComparisonSelection(cause, comparison, binding)
            if projectedComparison == expectedComparison
            channelValues <- (idea \ "channels").asOpt[JsArray].map(_.value.toList)
            typedChannels <- parseProjectedChannels(channelValues, graph)
            expectedChannels = expectedProjectedChannels(cause, graph, effectMode)
            if typedChannels.nonEmpty && typedChannels.sortBy(_.sortKey) == expectedChannels.sortBy(_.sortKey)
            onlyMove <- parseOptionalOnlyMove((idea \ "only_move").toOption)
            if onlyMove.forall(qualifier =>
              qualifier.causeEvidenceId == causeEvidenceId &&
                qualifier.comparisonEvidenceId == cause.comparisonEvidence.id &&
                qualifier.reference == projectedSemanticLine(comparison.referenceLine)
            )
            if causeEvidenceId.nonEmpty && comparisonExposureRank >= 0 && selectionOrder >= 0
        yield ProjectedCauseSelection(
            causeEvidenceId,
            comparisonExposureRank,
            selectionOrder,
            exposure,
            effectModeCode,
            causeKind,
            sourceSide,
            directEffectAdmission,
            attribution,
            host,
            projectedComparison,
            CandidateComparisonSemanticKey.from(comparison).stableKey,
            typedChannels.sortBy(_.sortKey),
            onlyMove
        )
      case _ => None

  private def parseProjectedSemanticLine(value: JsValue): Option[ProjectedSemanticLine] =
    for
      lineId <- (value \ "id").asOpt[String]
      role <- (value \ "role").asOpt[String]
      rank <- (value \ "rank").asOpt[Int]
      rootMove <- (value \ "root_move").asOpt[String]
      if lineId.nonEmpty && LineNodeRole.values.exists(item => code(item) == role)
      if rank >= 0 && EvidenceRef.normalizeMove(rootMove).nonEmpty
    yield ProjectedSemanticLine(lineId, role, rank, EvidenceRef.normalizeMove(rootMove))

  private def parseOptionalProjectedLine(
      value: Option[JsValue]
  ): Option[Option[ProjectedSemanticLine]] =
    value match
      case Some(JsNull) => Some(None)
      case Some(line)   => parseProjectedSemanticLine(line).map(Some(_))
      case None         => None

  private def parseProjectedEvidenceRef(
      value: JsValue,
      graph: TypedEvidenceGraph
  ): Option[ProjectedEvidenceRef] =
    for
      id <- (value \ "id").asOpt[String]
      registered <- graph.byId.get(id).map(_.ref)
      producer <- (value \ "producer").asOpt[String]
      layer <- (value \ "layer").asOpt[String]
      scope <- (value \ "scope").asOpt[String]
      line <- parseOptionalProjectedLine((value \ "line").toOption)
      emitted = ProjectedEvidenceRef(id, producer, layer, scope, line)
      expected = projectedEvidenceRef(registered)
      if emitted == expected
    yield expected

  private def parseProjectedAttribution(value: JsValue): Option[ProjectedAttributionSelection] =
    for
      kind <- (value \ "kind").asOpt[String]
      if CauseAttributionKind.values.exists(item => code(item) == kind)
      rootMoveMatched <- (value \ "root_move_matched").asOpt[Boolean]
      directProofEligible <- (value \ "direct_proof_eligible").asOpt[Boolean]
    yield ProjectedAttributionSelection(kind, rootMoveMatched, directProofEligible)

  private def parseProjectedHost(value: JsValue): Option[ProjectedHostSelection] =
    for
      claimId <- (value \ "claim_id").asOpt[String]
      family <- (value \ "family").asOpt[String]
      tier <- (value \ "tier").asOpt[String]
      if claimId.nonEmpty && ClaimFamily.values.exists(item => code(item) == family)
      if PlayerFacingClaimTier.values.exists(item => code(item) == tier)
    yield ProjectedHostSelection(claimId, family, tier)

  private def parseProjectedComparison(value: JsValue): Option[ProjectedComparisonSelection] =
    for
      evidenceId <- (value \ "evidence_id").asOpt[String]
      kind <- (value \ "kind").asOpt[String]
      mover <- (value \ "mover").asOpt[String]
      reference <- (value \ "reference").toOption.flatMap(parseProjectedSemanticLine)
      candidate <- (value \ "candidate").toOption.flatMap(parseProjectedSemanticLine)
      event <- (value \ "event").toOption.flatMap(parseProjectedSemanticLine)
      comparedMove <- (value \ "compared_move").asOpt[String]
      verdict <- (value \ "verdict").asOpt[String]
      candidateDelta <- (value \ "delta" \ "candidate_win_percent_delta_for_mover").asOpt[Double]
      winPercentLoss <- (value \ "delta" \ "win_percent_loss_for_mover").asOpt[Double]
      if evidenceId.nonEmpty && kind.nonEmpty && mover.nonEmpty && verdict.nonEmpty
      if EvidenceRef.normalizeMove(comparedMove).nonEmpty
    yield ProjectedComparisonSelection(
      evidenceId,
      kind,
      mover,
      reference,
      candidate,
      event,
      EvidenceRef.normalizeMove(comparedMove),
      verdict,
      candidateDelta,
      winPercentLoss
    )

  private def parseOptionalOnlyMove(
      value: Option[JsValue]
  ): Option[Option[ProjectedOnlyMoveQualifier]] =
    value match
      case Some(JsNull) => Some(None)
      case Some(qualifier) =>
        (for
          comparisonEvidenceId <- (qualifier \ "comparison_evidence_id").asOpt[String]
          causeEvidenceId <- (qualifier \ "cause_evidence_id").asOpt[String]
          reference <- (qualifier \ "reference").toOption.flatMap(parseProjectedSemanticLine)
          relation <- (qualifier \ "relation").asOpt[String]
          exactRelation <- OnlyMoveMechanismRelation.values.find(item => code(item) == relation)
          licenses <- (qualifier \ "licenses_causal_because").asOpt[Boolean]
          if comparisonEvidenceId.nonEmpty && causeEvidenceId.nonEmpty
          if licenses == exactRelation.licensesCausalBecause
        yield ProjectedOnlyMoveQualifier(
          comparisonEvidenceId,
          causeEvidenceId,
          reference,
          relation,
          licenses
        )).map(Some(_))
      case None => None

  private def expectedProjectedChannels(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      effectMode: PlayerFacingCauseEffectMode
  ): List[ProjectedChannelSelection] =
    PlayerFacingCauseSelectionPolicy
      .compatibleChannels(
        effectMode,
        RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph)
      )
      .map { case (channel, playedChange) =>
        projectedChannelSelection(
          PlayerFacingCauseChannelSelection(
            channel.binding.source,
            channel.causalSignature,
            channel.directChange,
            playedChange
          ),
          channel
        )
      }
      .groupBy(_.causalSignature)
      .values
      .map(_.minBy(_.carrier.id))
      .toList
      .sortBy(_.sortKey)

  private def parseProjectedChannels(
      values: List[JsValue],
      graph: TypedEvidenceGraph
  ): Option[List[ProjectedChannelSelection]] =
    val parsed = values.map(parseProjectedChannel(_, graph))
    Option.when(parsed.forall(_.nonEmpty))(parsed.flatten)

  private def parseProjectedChannel(
      value: JsValue,
      graph: TypedEvidenceGraph
  ): Option[ProjectedChannelSelection] =
    for
      carrier <- (value \ "carrier").toOption.flatMap(parseProjectedEvidenceRef(_, graph))
      provenanceValues <- (value \ "provenance").asOpt[JsArray].map(_.value.toList)
      provenance <- parseProjectedEvidenceRefs(provenanceValues, graph)
      emittedSignature <- (value \ "causal_signature").asOpt[String]
      directChangeCode <- (value \ "direct_change").asOpt[String]
      directChange <- DirectCausalChange.values.find(item => code(item) == directChangeCode)
      playedChangeCode <- (value \ "played_change").asOpt[String]
      if PlayerFacingCausalChange.values.exists(item => code(item) == playedChangeCode)
      actor <- parseProjectedActor((value \ "actor").toOption)
      targets <- parseProjectedObjects((value \ "targets").toOption)
      mechanisms <- parseProjectedObjects((value \ "mechanisms").toOption)
      consequences <- parseProjectedObjects((value \ "consequences").toOption)
      witnesses <- parseProjectedObjects((value \ "witnesses").toOption)
      line <- parseOptionalProjectedLine((value \ "line").toOption)
      horizon <- parseOptionalString((value \ "horizon").toOption)
      proofSegment <- parseOptionalObject((value \ "proof_segment").toOption)
      effectDescriptor <- parseOptionalObject((value \ "effect_descriptor").toOption)
      importanceEffect <- parseOptionalObject((value \ "importance_effect").toOption)
      descriptorAmbiguous <- (value \ "descriptor_ambiguous").asOpt[Boolean]
      proofSegmentAmbiguous <- (value \ "proof_segment_ambiguous").asOpt[Boolean]
      if !descriptorAmbiguous || effectDescriptor.exists(descriptor =>
        (descriptor \ "magnitude_status").asOpt[String].contains("ambiguous") &&
          (descriptor \ "measure").toOption.contains(JsNull)
      )
      if !proofSegmentAmbiguous || proofSegment.isEmpty
      concreteActor <- concreteObjects(actor)
      concreteTargets <- concreteObjects(targets)
      concreteMechanisms <- concreteObjects(mechanisms)
      concreteConsequences <- concreteObjects(consequences)
      concreteWitnesses <- concreteObjects(witnesses)
      lineNode <- line match
        case None        => Some(None)
        case Some(value) => projectedLineNode(value).map(Some(_))
      reconstructed = DirectCauseChannel(
        EvidenceObjectBinding(
          source = graph.byId(carrier.id).ref,
          actor = concreteActor,
          target = concreteTargets,
          mechanism = concreteMechanisms,
          consequence = concreteConsequences,
          witness = concreteWitnesses,
          line = lineNode,
          horizon = horizon,
          provenance = provenance.map(item => graph.byId(item.id).ref)
        ),
        directChange
      )
      if emittedSignature == reconstructed.causalSignature
    yield ProjectedChannelSelection(
      carrier = carrier,
      provenance = provenance.distinct.sortBy(_.sortKey),
      causalSignature = emittedSignature,
      directChange = directChangeCode,
      playedChange = playedChangeCode,
      actor = actor,
      targets = targets,
      mechanisms = mechanisms,
      consequences = consequences,
      witnesses = witnesses,
      line = line,
      horizon = horizon,
      proofSegment = proofSegment,
      effectDescriptor = effectDescriptor,
      importanceEffect = importanceEffect,
      descriptorAmbiguous = descriptorAmbiguous,
      proofSegmentAmbiguous = proofSegmentAmbiguous
    )

  private def parseOptionalObject(value: Option[JsValue]): Option[Option[JsObject]] =
    value match
      case Some(JsNull)          => Some(None)
      case Some(segment: JsObject) => Some(Some(segment))
      case _                     => None

  private def parseProjectedEvidenceRefs(
      values: List[JsValue],
      graph: TypedEvidenceGraph
  ): Option[List[ProjectedEvidenceRef]] =
    val parsed = values.map(parseProjectedEvidenceRef(_, graph))
    Option.when(parsed.forall(_.nonEmpty))(parsed.flatten)

  private def parseProjectedActor(value: Option[JsValue]): Option[List[ProjectedChessObject]] =
    value.flatMap { actor =>
      for
        move <- (actor \ "move").asOpt[String]
        side <- (actor \ "side").asOpt[String]
        piece <- (actor \ "piece").asOpt[String]
        from <- (actor \ "from").asOpt[String]
        to <- (actor \ "to").asOpt[String]
      yield projectedObjects(
        List(
          ConcreteChessObject(EvidenceObjectKind.Move, EvidenceRef.normalizeMove(move)),
          ConcreteChessObject(EvidenceObjectKind.Side, side.trim.toLowerCase(Locale.ROOT)),
          ConcreteChessObject(EvidenceObjectKind.Piece, piece.trim.toLowerCase(Locale.ROOT)),
          ConcreteChessObject(EvidenceObjectKind.Square, from.trim.toLowerCase(Locale.ROOT)),
          ConcreteChessObject(EvidenceObjectKind.Square, to.trim.toLowerCase(Locale.ROOT))
        )
      )
    }

  private def parseProjectedObjects(value: Option[JsValue]): Option[List[ProjectedChessObject]] =
    value.flatMap(_.asOpt[JsArray]).flatMap { array =>
      val parsed = array.value.toList.map { item =>
        for
          kindCode <- (item \ "kind").asOpt[String]
          kind <- EvidenceObjectKind.values.find(value => code(value) == kindCode)
          key <- (item \ "key").asOpt[String]
          if key.nonEmpty
        yield ProjectedChessObject(code(kind), key)
      }
      Option.when(parsed.forall(_.nonEmpty))(parsed.flatten.distinct.sortBy(_.sortKey))
    }

  private def concreteObjects(
      values: List[ProjectedChessObject]
  ): Option[List[ConcreteChessObject]] =
    val concrete = values.map(_.concrete)
    Option.when(concrete.forall(_.nonEmpty))(concrete.flatten)

  private def projectedLineNode(value: ProjectedSemanticLine): Option[LineNodeRef] =
    LineNodeRole.values.find(item => code(item) == value.role).map(role =>
      LineNodeRef(value.lineId, value.rootMove, value.rank, role)
    )

  private def parseOptionalString(value: Option[JsValue]): Option[Option[String]] =
    value match
      case Some(JsNull) => Some(None)
      case Some(item)   => item.asOpt[String].map(Some(_))
      case None         => None

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
      trace: RuntimeProtocol.BoundaryEvaluationTrace,
      contract: ObservationContract
  ): JsObject =
    val observation = Json.obj(
      "schema_version" -> contract.schemaVersion,
      "status" -> "unavailable",
      "reason" -> "runtime_boundary_execution_absent",
      "request_sha256" -> requestSha256,
      "hash_contract" -> NativeTreeEncoder.HashContract,
      "runtime" -> runtimeMetadata,
      "public_response" -> publicResponseSummary(
        trace.finalPublicResponse,
        contract,
        trace.projection.exists(_.primaryEngineBacked)
      ),
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
    if contract.capturesNativeRSelections then
      observation ++ Json.obj(
        "verdict" -> emptyPlayedVsBestVerdictObservation,
        "r_native_cause_selections" -> Json.arr(),
        "importance" -> projectedImportanceObservation(
          DirectCauseImportanceResolution.empty,
          None,
          None
        ),
        "idea_units" -> projectedIdeaUnitObservation(
          Nil,
          None,
          None,
          ProjectedPublicIdeaParse.empty
        ),
        "idea_importance" -> projectedImportanceObservation(
          DirectCauseImportanceResolution.empty,
          None,
          None
        ),
        "cause_disposition_ledger" -> emptyCauseDispositionLedgerObservation,
        "comparison_endpoint_evidence_snapshots" -> Json.arr()
      )
    else observation

  private def errorObservation(
      requestSha256: String,
      kind: String,
      errorClass: String,
      nativeType: Option[String],
      contract: ObservationContract
  ): JsObject =
    val observation = Json.obj(
      "schema_version" -> contract.schemaVersion,
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
    if contract.capturesNativeRSelections then
      observation ++ Json.obj(
        "public_response" -> Json.obj(
          "primary_engine_backed" -> false,
          "idea_status" -> "unavailable",
          "idea_status_detail" -> JsNull
        ),
        "verdict" -> emptyPlayedVsBestVerdictObservation,
        "r_native_cause_selections" -> Json.arr(),
        "importance" -> projectedImportanceObservation(
          DirectCauseImportanceResolution.empty,
          None,
          None
        ),
        "idea_units" -> projectedIdeaUnitObservation(
          Nil,
          None,
          None,
          ProjectedPublicIdeaParse.empty
        ),
        "idea_importance" -> projectedImportanceObservation(
          DirectCauseImportanceResolution.empty,
          None,
          None
        ),
        "cause_disposition_ledger" -> emptyCauseDispositionLedgerObservation,
        "comparison_endpoint_evidence_snapshots" -> Json.arr()
      )
    else observation

  private def emptyPlayedVsBestVerdictObservation: JsObject =
    Json.obj(
      "packet_canonical" -> JsNull,
      "selected_projection" -> JsNull,
      "final_public_response" -> JsNull,
      "classification_authority" -> JsNull
    )

  private def emptyCauseDispositionLedgerObservation: JsObject =
    Json.obj(
      "r_native" -> Json.arr(),
      "packet_native" -> JsNull,
      "r_public_summary" -> RuntimeProtocol.causeDispositionPublicJson(
        CauseDispositionLedger.empty
      ),
      "packet_public_summary" -> JsNull,
      "selected_projection" -> JsNull,
      "final_public_response" -> JsNull
    )

  private def publicResponseSummary(
      result: RuntimeProtocol.Result,
      contract: ObservationContract,
      primaryEngineBacked: Boolean
  ): JsObject =
    val body = result.body
    val summary = Json.obj(
      "http_status" -> result.httpStatus,
      "body_sha256" -> NativeTreeEncoder.sha256Canonical(body),
      "schema_version" -> (body \ "schema_version").asOpt[String],
      "request_id" -> (body \ "request_id").asOpt[String],
      "ok" -> (body \ "ok").asOpt[Boolean],
      "status" -> (body \ "status").asOpt[String],
      "availability" -> (body \ "availability").toOption,
      "error" -> (body \ "error").toOption
    )
    if contract.capturesNativeRSelections then
      summary ++ Json.obj(
        "primary_engine_backed" -> primaryEngineBacked,
        "idea_status" -> (body \ "move_review" \ "idea_status").asOpt[String].getOrElse(
          if primaryEngineBacked then "no_certified_differential_idea" else "unavailable"
        ),
        "idea_status_detail" -> (body \ "move_review" \ "idea_status_detail").asOpt[JsObject]
      )
    else summary

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
