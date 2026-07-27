package io.chesstory.evaluation.runtimeadapter

import java.io.{ BufferedReader, InputStreamReader, PrintWriter }
import java.nio.charset.StandardCharsets

import scala.util.control.NonFatal

import io.chesstory.runtime.RuntimeProtocol
import lila.chessjudgment.analysis.assembly.{
  JudgmentBoundaryIntervention,
  JudgmentQuestionInput
}
import lila.chessjudgment.model.judgment.TypedEvidenceGraph
import play.api.libs.json.{ JsArray, JsObject, JsString, JsValue, Json }

/** External, diagnostic-only native intervention adapter.
  *
  * Every intervention is an exact lookup of immutable values that the runtime
  * already produced under the same conditioned invocation.  This adapter does
  * not deserialize native snapshots, synthesize chess meaning, or reproduce a
  * production decision rule.  Unsupported boundaries remain typed unavailable.
  */
object NativeInterventionAdapterCli:
  private val InvocationSchema = "chesstory.runtime-native-engineering-invocation.v1"
  private val ResultSchema = "chesstory.runtime-native-engineering-result.v1"
  private val FrozenSourceSemantics = "not-claimed"
  private val AllowedDirectiveStages = Set("Q", "F", "C", "Jp", "R", "P")
  private val AllowedProducerTiers =
    Set("deterministic-engineering", "blind-model-proxy", "held-out-human-rater")

  private final case class ParsedInvocation(
      document: JsObject,
      binding: JsObject,
      runtimeRequest: JsObject,
      realizationKind: String,
      producerTier: String,
      directives: JsObject
  )

  private final case class DirectiveFailure(code: String)

  def main(args: Array[String]): Unit =
    val reader = BufferedReader(InputStreamReader(System.in, StandardCharsets.UTF_8))
    val writer = PrintWriter(System.out, true, StandardCharsets.UTF_8)
    var line = reader.readLine()
    while line != null do
      writer.println(Json.stringify(handle(line)))
      line = reader.readLine()

  private[runtimeadapter] def handle(invocationLine: String): JsObject =
    val parsedJson =
      try Right(Json.parse(invocationLine))
      catch case NonFatal(error) => Left(error)
    parsedJson match
      case Left(error) => adapterError(None, None, "invalid_json", error)
      case Right(document: JsObject) =>
        parseInvocation(document) match
          case Left(failure) =>
            adapterError(
              Some(document),
              (document \ "binding").asOpt[JsObject],
              failure.code,
              IllegalArgumentException(failure.code)
            )
          case Right(invocation) => execute(invocation)
      case Right(_) =>
        adapterError(None, None, "invocation_must_be_object", IllegalArgumentException("object required"))

  private def execute(invocation: ParsedInvocation): JsObject =
    val requestLine = Json.stringify(invocation.runtimeRequest)
    val baselineObservation = RuntimeAdapterCli.observe(requestLine)
    try
      if invocation.realizationKind == "identity" then
        completedResult(invocation, baselineObservation, Nil)
      else
        buildIntervention(invocation.runtimeRequest, invocation.directives) match
          case Left(failure) =>
            unavailableResult(invocation, failure.code, baselineObservation, None, Nil)
          case Right((intervention, receipts)) =>
            val finalTrace = RuntimeProtocol.evaluateWithBoundary(invocation.runtimeRequest, intervention)
            val attemptedObservation = RuntimeAdapterCli.observeWithIntervention(requestLine, intervention)
            val judgmentDirectivePresent =
              invocation.directives.keys.exists(stage => stage != "P")
            val projectionDirectivePresent = invocation.directives.keys.contains("P")
            if judgmentDirectivePresent && finalTrace.boundaryExecution.isEmpty then
              unavailableResult(
                invocation,
                "runtime_judgment_intervention_rejected",
                baselineObservation,
                Some(attemptedObservation),
                receipts
              )
            else if projectionDirectivePresent && finalTrace.projection.isEmpty then
              unavailableResult(
                invocation,
                "runtime_projection_intervention_rejected",
                baselineObservation,
                Some(attemptedObservation),
                receipts
              )
            else completedResult(invocation, attemptedObservation, receipts)
    catch
      case NonFatal(error) =>
        adapterError(
          Some(invocation.document),
          Some(invocation.binding),
          "native_intervention_adapter_failed",
          error
        ) ++ Json.obj("baseline_observation" -> baselineObservation)

  private def buildIntervention(
      runtimeRequest: JsObject,
      directives: JsObject
  ): Either[DirectiveFailure, (RuntimeProtocol.RuntimeBoundaryIntervention, List[JsObject])] =
    var current = RuntimeProtocol.RuntimeBoundaryIntervention.identity
    val receipts = List.newBuilder[JsObject]

    def currentTrace: RuntimeProtocol.BoundaryEvaluationTrace =
      RuntimeProtocol.evaluateWithBoundary(runtimeRequest, current)

    def judgmentBoundary(
        stage: String
    ): Either[DirectiveFailure, lila.chessjudgment.analysis.assembly.JudgmentBoundaryExecution] =
      currentTrace.boundaryExecution.toRight(DirectiveFailure(s"conditioned_${stage}_boundary_unavailable"))

    for stage <- List("Q", "F", "C", "Jp", "R", "P") do
      directives.value.get(stage) match
        case None => ()
        case Some(value: JsObject) =>
          val applied = stage match
            case "Q" =>
              for
                _ <- requireExactKeys(value, Set("operation", "variation_ids", "probe_result_ids"), stage)
                _ <- requireSelectExisting(value, stage)
                variationIds <- stringArray(value, "variation_ids", stage)
                probeIds <- stringArray(value, "probe_result_ids", stage)
                boundary <- judgmentBoundary(stage)
                variations = boundary.q.variations.map(line => variationId(line) -> line)
                probes = boundary.q.probeResults.map(result => s"probe-result:${result.id}" -> result)
                selectedVariations <- selectExisting(variations, variationIds, allowEmpty = false, stage)
                selectedProbes <- selectExisting(probes, probeIds, allowEmpty = true, stage)
              yield
                val expectedQuestion = JudgmentQuestionInput.fromRaw(boundary.q)
                val selected = boundary.q.copy(
                  variations = selectedVariations,
                  probeResults = selectedProbes
                )
                val provider = (question: JudgmentQuestionInput) =>
                  if !sameNativeValue(question, expectedQuestion) then
                    throw IllegalArgumentException("Q_conditioned_input_changed")
                  selected
                current = current.copy(
                  judgment = current.judgment.copy(q = Some(provider))
                )
                receipts += receipt("Q", "variations", variations.map(_._1), variationIds)
                receipts += receipt("Q", "probe-results", probes.map(_._1), probeIds)
            case "F" | "C" =>
              for
                _ <- requireExactKeys(value, Set("operation", "evidence_record_ids"), stage)
                _ <- requireSelectExisting(value, stage)
                recordIds <- stringArray(value, "evidence_record_ids", stage)
                boundary <- judgmentBoundary(stage)
                context = if stage == "F" then boundary.f else boundary.c
                records = context.evidenceGraph.records.map(record => s"evidence:${record.ref.id}" -> record)
                selected <- selectExisting(records, recordIds, allowEmpty = false, stage)
              yield
                val selectedGraph = selected.foldLeft(TypedEvidenceGraph.empty)(_.add(_))
                val selectedContext = context.copy(evidenceGraph = selectedGraph)
                if stage == "F" then
                  val expected = boundary.q
                  val provider = (input: lila.chessjudgment.analysis.assembly.RawMoveReviewInput) =>
                    if !sameNativeValue(input, expected) then
                      throw IllegalArgumentException("F_conditioned_input_changed")
                    selectedContext
                  current = current.copy(
                    judgment = current.judgment.copy(f = Some(provider))
                  )
                else
                  val expected = boundary.f
                  val provider = (input: lila.chessjudgment.model.judgment.JudgmentAssemblyContext) =>
                    if !sameNativeValue(input, expected) then
                      throw IllegalArgumentException("C_conditioned_input_changed")
                    selectedContext
                  current = current.copy(
                    judgment = current.judgment.copy(c = Some(provider))
                  )
                receipts += receipt(stage, "evidence-records", records.map(_._1), recordIds)
            case "Jp" =>
              for
                _ <- requireExactKeys(value, Set("operation", "claim_ids"), stage)
                _ <- requireSelectExisting(value, stage)
                claimIds <- stringArray(value, "claim_ids", stage)
                boundary <- judgmentBoundary(stage)
                claims = boundary.jp.map(claim => s"claim:${claim.id}" -> claim)
                selected <- selectExisting(claims, claimIds, allowEmpty = true, stage)
              yield
                val expected = boundary.c
                val provider = (input: lila.chessjudgment.model.judgment.JudgmentAssemblyContext) =>
                  if !sameNativeValue(input, expected) then
                    throw IllegalArgumentException("Jp_conditioned_input_changed")
                  selected
                current = current.copy(
                  judgment = current.judgment.copy(jp = Some(provider))
                )
                receipts += receipt("Jp", "claims", claims.map(_._1), claimIds)
            case "R" =>
              for
                _ <- requireExactKeys(value, Set("operation", "ranked_claim_ids"), stage)
                _ <- requireSelectExisting(value, stage)
                claimIds <- stringArray(value, "ranked_claim_ids", stage)
                boundary <- judgmentBoundary(stage)
                ranked = boundary.r.ranked.map(decision => s"ranked-claim:${decision.claim.id}" -> decision)
                selected <- selectExisting(ranked, claimIds, allowEmpty = true, stage)
              yield
                val expectedContext = boundary.c
                val expectedGraph = boundary.ja
                val selectedRanking = boundary.r.copy(ranked = selected)
                val provider = (
                    context: lila.chessjudgment.model.judgment.JudgmentAssemblyContext,
                    graph: lila.chessjudgment.analysis.assembly.ClaimCandidateGraph
                ) =>
                  if !sameNativeValue(context, expectedContext) ||
                      !sameNativeValue(graph, expectedGraph)
                  then
                    throw IllegalArgumentException("R_conditioned_input_changed")
                  selectedRanking
                current = current.copy(
                  judgment = current.judgment.copy(r = Some(provider))
                )
                receipts += receipt("R", "ranked-claims", ranked.map(_._1), claimIds)
            case "P" =>
              for
                _ <- requireExactKeys(value, Set("operation", "probe_request_ids"), stage)
                _ <- requireSelectExisting(value, stage)
                probeIds <- stringArray(value, "probe_request_ids", stage)
                projection <- currentTrace.projection.toRight(
                  DirectiveFailure("conditioned_P_boundary_unavailable")
                )
                probes <- projection.actualDecision.publicProbeRequests.traverseWithIds("P")
                selected <- selectExisting(probes, probeIds, allowEmpty = true, stage)
                _ <- requireSubsequence(probes.map(_._1), probeIds, stage)
              yield
                val expectedPacket = projection.input.packet
                val selectedDecision = projection.actualDecision.copy(publicProbeRequests = selected)
                val provider = (input: RuntimeProtocol.ProjectionBoundaryInput) =>
                  if !sameNativeValue(input.packet, expectedPacket) then
                    throw IllegalArgumentException("P_conditioned_input_changed")
                  selectedDecision
                current = current.copy(p = Some(provider))
                receipts += receipt("P", "probe-requests", probes.map(_._1), probeIds)
            case _ => Left(DirectiveFailure("unsupported_directive_stage"))
          applied match
            case Left(failure) => return Left(failure)
            case Right(_)      => ()
        case Some(_) => return Left(DirectiveFailure(s"${stage}_directive_must_be_object"))
    Right(current -> receipts.result())

  extension (values: List[JsObject])
    private def traverseWithIds(stage: String): Either[DirectiveFailure, List[(String, JsObject)]] =
      val pairs = values.map(value =>
        (value \ "id").asOpt[String].filter(_.nonEmpty).map(id => s"probe-request:$id" -> value)
      )
      if pairs.forall(_.nonEmpty) then Right(pairs.flatten)
      else Left(DirectiveFailure(s"${stage}_existing_item_has_no_id"))

  private def variationId(value: Any): String =
    val tree = NativeTreeEncoder.encode(value, RuntimeNativeViews.adapters)
    s"variation:${NativeTreeEncoder.sha256Canonical(tree)}"

  private def sameNativeValue(left: Any, right: Any): Boolean =
    NativeTreeEncoder.sha256Canonical(
      NativeTreeEncoder.encode(left, RuntimeNativeViews.adapters)
    ) == NativeTreeEncoder.sha256Canonical(
      NativeTreeEncoder.encode(right, RuntimeNativeViews.adapters)
    )

  private def selectExisting[A](
      available: List[(String, A)],
      selectedIds: List[String],
      allowEmpty: Boolean,
      stage: String
  ): Either[DirectiveFailure, List[A]] =
    val availableIds = available.map(_._1)
    if availableIds.distinct.size != availableIds.size then
      Left(DirectiveFailure(s"${stage}_existing_item_ids_are_ambiguous"))
    else if selectedIds.distinct.size != selectedIds.size then
      Left(DirectiveFailure(s"${stage}_selected_item_ids_are_not_unique"))
    else if !allowEmpty && selectedIds.isEmpty then
      Left(DirectiveFailure(s"${stage}_selection_must_not_be_empty"))
    else
      val byId = available.toMap
      val missing = selectedIds.filterNot(byId.contains)
      if missing.nonEmpty then Left(DirectiveFailure(s"${stage}_selected_item_id_not_found"))
      else Right(selectedIds.flatMap(byId.get))

  private def requireSubsequence(
      availableIds: List[String],
      selectedIds: List[String],
      stage: String
  ): Either[DirectiveFailure, Unit] =
    val selectedSet = selectedIds.toSet
    Either.cond(
      availableIds.filter(selectedSet) == selectedIds,
      (),
      DirectiveFailure(s"${stage}_selection_must_preserve_runtime_order")
    )

  private def receipt(
      stage: String,
      collection: String,
      availableIds: List[String],
      selectedIds: List[String]
  ): JsObject =
    Json.obj(
      "stage" -> stage,
      "operation" -> "select-existing",
      "collection" -> collection,
      "status" -> "applied",
      "available_item_ids" -> availableIds,
      "selected_item_ids" -> selectedIds,
      "candidate_inventory_sha256" -> NativeTreeEncoder.sha256Canonical(Json.toJson(availableIds))
    )

  private def parseInvocation(document: JsObject): Either[DirectiveFailure, ParsedInvocation] =
    for
      _ <- exactObjectKeys(
        document,
        Set("schema_version", "binding", "runtime_request", "realization"),
        "invocation"
      )
      _ <- Either.cond(
        (document \ "schema_version").asOpt[String].contains(InvocationSchema),
        (),
        DirectiveFailure("unsupported_invocation_schema")
      )
      binding <- objectField(document, "binding", "invocation")
      _ <- exactObjectKeys(
        binding,
        Set("run_id", "sample_id", "atomic_cluster_id", "arm_id", "plan_sha256"),
        "binding"
      )
      _ <- requireNonEmptyStrings(
        binding,
        Set("run_id", "sample_id", "atomic_cluster_id", "arm_id", "plan_sha256"),
        "binding"
      )
      request <- objectField(document, "runtime_request", "invocation")
      realization <- objectField(document, "realization", "invocation")
      _ <- exactObjectKeys(
        realization,
        Set("kind", "directive_producer_tier", "frozen_source_semantics", "directives"),
        "realization"
      )
      kind <- stringField(realization, "kind", "realization")
      _ <- Either.cond(
        Set("identity", "native-selection")(kind),
        (),
        DirectiveFailure("unsupported_realization_kind")
      )
      producerTier <- stringField(realization, "directive_producer_tier", "realization")
      _ <- Either.cond(
        AllowedProducerTiers(producerTier),
        (),
        DirectiveFailure("unsupported_directive_producer_tier")
      )
      _ <- Either.cond(
        (realization \ "frozen_source_semantics").asOpt[String].contains(FrozenSourceSemantics),
        (),
        DirectiveFailure("frozen_source_semantics_must_not_be_claimed")
      )
      directives <- objectField(realization, "directives", "realization")
      _ <- Either.cond(
        directives.keys.subsetOf(AllowedDirectiveStages),
        (),
        DirectiveFailure("unsupported_directive_stage")
      )
      _ <- Either.cond(
        (kind == "identity" && directives.keys.isEmpty) ||
          (kind == "native-selection" && directives.keys.nonEmpty),
        (),
        DirectiveFailure("realization_kind_directive_mismatch")
      )
    yield ParsedInvocation(document, binding, request, kind, producerTier, directives)

  private def requireExactKeys(
      value: JsObject,
      expected: Set[String],
      stage: String
  ): Either[DirectiveFailure, Unit] =
    exactObjectKeys(value, expected, s"${stage}_directive")

  private def requireSelectExisting(
      value: JsObject,
      stage: String
  ): Either[DirectiveFailure, Unit] =
    Either.cond(
      (value \ "operation").asOpt[String].contains("select-existing"),
      (),
      DirectiveFailure(s"${stage}_unsupported_operation")
    )

  private def stringArray(
      value: JsObject,
      field: String,
      stage: String
  ): Either[DirectiveFailure, List[String]] =
    (value \ field).toOption match
      case Some(JsArray(items)) if items.forall(_.isInstanceOf[JsString]) =>
        val strings = items.toList.collect { case JsString(item) => item }
        Either.cond(
          strings.forall(_.nonEmpty),
          strings,
          DirectiveFailure(s"${stage}_${field}_contains_empty_id")
        )
      case _ => Left(DirectiveFailure(s"${stage}_${field}_must_be_string_array"))

  private def exactObjectKeys(
      value: JsObject,
      expected: Set[String],
      label: String
  ): Either[DirectiveFailure, Unit] =
    Either.cond(
      value.keys == expected,
      (),
      DirectiveFailure(s"${label}_fields_are_not_exact")
    )

  private def objectField(
      value: JsObject,
      field: String,
      label: String
  ): Either[DirectiveFailure, JsObject] =
    (value \ field).asOpt[JsObject].toRight(DirectiveFailure(s"${label}_${field}_must_be_object"))

  private def stringField(
      value: JsObject,
      field: String,
      label: String
  ): Either[DirectiveFailure, String] =
    (value \ field).asOpt[String].filter(_.nonEmpty).toRight(
      DirectiveFailure(s"${label}_${field}_must_be_non_empty_string")
    )

  private def requireNonEmptyStrings(
      value: JsObject,
      fields: Set[String],
      label: String
  ): Either[DirectiveFailure, Unit] =
    Either.cond(
      fields.forall(field => (value \ field).asOpt[String].exists(_.nonEmpty)),
      (),
      DirectiveFailure(s"${label}_contains_invalid_string")
    )

  private def completedResult(
      invocation: ParsedInvocation,
      observation: JsObject,
      receipts: List[JsObject]
  ): JsObject =
    resultBase(invocation, "completed", receipts) ++ Json.obj("observation" -> observation)

  private def unavailableResult(
      invocation: ParsedInvocation,
      reason: String,
      baselineObservation: JsObject,
      attemptedObservation: Option[JsObject],
      receipts: List[JsObject]
  ): JsObject =
    resultBase(invocation, "typed-unavailable", receipts) ++ Json.obj(
      "unavailability_reason" -> reason,
      "baseline_observation" -> baselineObservation
    ) ++ attemptedObservation.fold(Json.obj())(value => Json.obj("attempted_observation" -> value))

  private def resultBase(
      invocation: ParsedInvocation,
      status: String,
      receipts: List[JsObject]
  ): JsObject =
    Json.obj(
      "schema_version" -> ResultSchema,
      "invocation_sha256" -> NativeTreeEncoder.sha256Canonical(invocation.document),
      "binding" -> invocation.binding,
      "status" -> status,
      "realization" -> Json.obj(
        "kind" -> invocation.realizationKind,
        "directive_producer_tier" -> invocation.producerTier,
        "frozen_source_semantics" -> FrozenSourceSemantics
      ),
      "production_attribution_eligible" -> false,
      "inference_eligible" -> false,
      "directive_receipts" -> receipts
    )

  private def adapterError(
      invocation: Option[JsObject],
      binding: Option[JsObject],
      kind: String,
      error: Throwable
  ): JsObject =
    Json.obj(
      "schema_version" -> ResultSchema,
      "invocation_sha256" -> invocation.map(NativeTreeEncoder.sha256Canonical),
      "binding" -> binding,
      "status" -> "adapter-error",
      "production_attribution_eligible" -> false,
      "inference_eligible" -> false,
      "directive_receipts" -> Json.arr(),
      "adapter_error" -> Json.obj(
        "kind" -> kind,
        "class" -> error.getClass.getName
      )
    )
