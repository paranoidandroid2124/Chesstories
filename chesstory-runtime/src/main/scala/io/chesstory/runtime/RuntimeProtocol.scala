package io.chesstory.runtime

import scala.util.Try

import lila.chessjudgment.analysis.assembly.{
  MoveReviewJudgmentOrchestrator,
  RawMoveReviewInput
}
import lila.chessjudgment.model.{ ProbeHorizon, ProbeRequest, ProbeResolution, ProbeResult, ProbeVariant }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ AutomaticTerminal, CandidateLineEvaluation, DrawClaimAction, DrawClaimRule }
import lila.chessjudgment.model.line.EngineLine
import play.api.libs.json.*

object RuntimeProtocol:
  val RequestSchema = "chesstory.move-meaning.request.v1"
  val ResponseSchema = "chesstory.move-meaning.response.v6"

  final case class Result(httpStatus: Int, body: JsObject)

  private[runtime] def acceptsRequestId(value: String): Boolean =
    InputLimits.validRequestId(value)

  private final case class WireVariation(
      moves: List[String],
      scoreCp: Int,
      mate: Option[Int],
      depth: Int
  ):
    def toCore: EngineLine = EngineLine(moves, scoreCp, mate, depth)

  private object WireVariation:
    given Reads[WireVariation] = Json.reads[WireVariation]

  private final case class WireAutomaticTerminal(
      kind: String,
      winner: Option[String]
  ):
    def toCore: Option[AutomaticTerminal] =
      (kind, winner) match
        case ("checkmate", Some("white")) => Some(AutomaticTerminal.Checkmate(chess.Color.White))
        case ("checkmate", Some("black")) => Some(AutomaticTerminal.Checkmate(chess.Color.Black))
        case ("stalemate", None) => Some(AutomaticTerminal.Stalemate)
        case ("insufficient_material", None) => Some(AutomaticTerminal.InsufficientMaterial)
        case ("fivefold_repetition", None) => Some(AutomaticTerminal.FivefoldRepetition)
        case ("seventy_five_move_rule", None) => Some(AutomaticTerminal.SeventyFiveMoveRule)
        case _ => None

  private object WireAutomaticTerminal:
    given Reads[WireAutomaticTerminal] = Json.reads[WireAutomaticTerminal]

  private final case class WireProbeResult(
      id: String,
      replyLines: Option[List[WireVariation]],
      depth: Option[Int],
      moves: Option[List[String]],
      terminal: Option[WireAutomaticTerminal]
  ):
    def toCore: Option[ProbeResult] =
      (replyLines, depth, moves, terminal) match
        case (Some(lines), Some(realizedDepth), None, None) if lines.nonEmpty && realizedDepth > 0 =>
          Some(ProbeResult(
            id = id,
            resolution = ProbeResolution.EngineSearch(
              lines.map(line => CandidateLineEvaluation.EngineSearch(line.toCore)),
              realizedDepth
            )
          ))
        case (None, None, Some(lineMoves), Some(terminalValue)) if lineMoves.nonEmpty =>
          terminalValue.toCore.map(value =>
            ProbeResult(
              id = id,
              resolution = ProbeResolution.ExactAutomaticTerminal(
                CandidateLineEvaluation.ExactAutomaticTerminal(lineMoves, value)
              )
            )
          )
        case _ => None

  private object WireProbeResult:
    given Reads[WireProbeResult] = Json.reads[WireProbeResult]

  private final case class WireInput(
      fen: String,
      playedMoveUci: String,
      variations: List[WireVariation],
      ply: Option[Int],
      movePrefixUci: Option[List[String]],
      probeResults: Option[List[WireProbeResult]]
  ):
    def toCore: Option[RawMoveReviewInput] =
      val coreProbes = probeResults.getOrElse(Nil).map(_.toCore)
      Option.when(coreProbes.forall(_.nonEmpty))(
        RawMoveReviewInput(
        fen = fen,
        playedMoveUci = playedMoveUci,
        variations = variations.map(_.toCore),
        ply = ply,
        movePrefixUci = movePrefixUci.getOrElse(Nil),
        probeResults = coreProbes.flatten
        )
      )

  private object WireInput:
    given Reads[WireInput] = Json.reads[WireInput]

  def evaluate(bytes: Array[Byte]): Result =
    Try(Json.parse(bytes)).fold(
      _ => failure(None, 400, "invalid_json"),
      json => evaluate(json)
    )

  def evaluate(json: JsValue): Result =
    json match
      case obj: JsObject =>
        (obj \ "request_id").validateOpt[String] match
          case JsError(_) => failure(None, 400, "invalid_request_id")
          case JsSuccess(requestId, _) if !requestId.forall(InputLimits.validRequestId) =>
            failure(None, 400, "invalid_request_id")
          case JsSuccess(requestId, _) =>
            if (obj \ "schema_version").asOpt[String] != Some(RequestSchema) then
              failure(requestId, 400, "unsupported_schema_version")
            else
              (obj \ "input").validate[WireInput] match
                case JsError(_) => failure(requestId, 400, "invalid_move_review_input")
                case JsSuccess(input, _) =>
                  input.toCore.fold(failure(requestId, 400, "invalid_probe_result"))(
                    evaluate(requestId, _)
                  )
      case _ => failure(None, 400, "invalid_request")

  private def evaluate(
      requestId: Option[String],
      raw: RawMoveReviewInput
  ): Result =
    if !requestId.forall(InputLimits.validRequestId) then
      failure(None, 400, "invalid_request_id")
    else if !InputLimits.accepts(raw) then
      failure(requestId, 400, "input_limits_exceeded")
    else
      MoveReviewJudgmentOrchestrator.execute(raw).fold(
        failure(requestId, 422, "move_review_not_buildable")
      )(packet => encodeMoveMeaningResult(requestId, packet))

  def publicProbeRequestJson(request: ProbeRequest): JsObject =
    val variant = request.variant match
      case ProbeVariant.BranchReply(_, requiredHorizonPlyOffset) =>
        Json.obj("horizon" -> ProbeHorizon.renderPlyOffset(requiredHorizonPlyOffset))
    Json.obj(
      "id" -> request.id,
      "fen" -> request.fen,
      "moves" -> request.moves,
      "depth" -> request.depth,
      "purpose" -> request.kind.key,
      "multiPv" -> 1,
      "candidateMove" -> request.candidateMove,
      "variationHash" -> request.variationHash
    ) ++ variant

  private[runtime] def encodeMoveMeaningResult(
      requestId: Option[String],
      packet: EvidenceBackedJudgmentPacket
  ): Result =
    val requestField = requestId.map(value => Json.obj("request_id" -> value)).getOrElse(Json.obj())
    if packet.probeRequests.nonEmpty then
      Result(
        202,
        Json.obj(
          "schema_version" -> ResponseSchema,
          "status" -> "engine_work_required",
          "probe_requests" -> packet.probeRequests.map(publicProbeRequestJson)
        ) ++ requestField
      )
    else
      Result(
        200,
        Json.obj(
          "schema_version" -> ResponseSchema,
          "status" -> "ready",
          "move_commentary" -> moveCommentaryJson(packet)
        ) ++ requestField
      )

  private[runtime] def moveCommentaryJson(packet: EvidenceBackedJudgmentPacket): JsObject =
    MoveMeaningProjection.moveCommentaryJson(packet)

  private[runtime] def encodePositionCommentaryResponse(
      snapshot: CommentaryJobSnapshot,
      completed: CompletedCommentaryJob
  ): JsObject =
    val result = completed.result match
      case CompletedPositionCommentary.PositionAction(action) =>
        MoveMeaningProjection.positionActionJson(action)
      case CompletedPositionCommentary.FocusedMoveReview(drawClaims, record) =>
        val commentary = moveCommentaryJson(record.judgmentPacket)
        val playedRoles =
          if record.moveUci == record.bestMoveUci then List("best", "played") else List("played")
        val playedReview = Json.obj(
          "legal_move_index" -> record.legalMoveIndex,
          "move_uci" -> record.moveUci,
          "selection" -> (Json.obj("roles" -> playedRoles) ++ record.playedRootRank
            .map(rank => Json.obj("root_rank" -> rank))
            .getOrElse(Json.obj())),
          "commentary" -> commentary
        )
        val selectedReviews =
          if record.moveUci == record.bestMoveUci then List(playedReview)
          else
            List(
              Json.obj(
                "legal_move_index" -> record.bestLegalMoveIndex,
                "move_uci" -> record.bestMoveUci,
                "selection" -> Json.obj("roles" -> Json.arr("best"), "root_rank" -> 1),
                "line_insight" -> MoveMeaningProjection.referenceLineInsightJson(commentary)
              ),
              playedReview
            )
        Json.obj(
          "kind" -> "selected_move_choices",
          "selected_move_reviews" -> selectedReviews
        ) ++ Option.when(drawClaims.nonEmpty)(
          Json.obj("draw_claims" -> drawClaims.map(MoveMeaningProjection.drawClaimJson))
        ).getOrElse(Json.obj())
    Json.obj(
      "schema_version" -> "chesstory.position-commentary.response.v6",
      "annotation_policy_revision" -> "chesstory.verdict-threshold-policy.v2",
      "request_id" -> snapshot.requestId,
      "job_id" -> snapshot.jobId,
      "engine_profile" -> completed.engineProfile.wireId,
      "variant" -> PositionCommentaryJobProtocol.Variant,
      "current_fen" -> completed.currentFen,
      "focus" -> PositionCommentaryJobProtocol.focusJson(
        completed.playedMoveUci,
        completed.resultingFen
      ),
      "progress" -> PositionCommentaryJobProtocol.completedProgressJson(completed.progress),
      "result" -> result
    )

  private[runtime] def failure(requestId: Option[String], httpStatus: Int, error: String): Result =
    Result(
      httpStatus,
      Json.obj(
        "schema_version" -> ResponseSchema,
        "status" -> "error",
        "error" -> error
      ) ++ requestId.map(value => Json.obj("request_id" -> value)).getOrElse(Json.obj())
    )

  private object MoveMeaningProjection:
    def moveCommentaryJson(packet: EvidenceBackedJudgmentPacket): JsObject =
      val comparisonEvidence = packet.primary match
        case PlayerFacingPrimary.MoveVerdict(ref) => ref
        case PlayerFacingPrimary.BestChoice(ref)  => ref
      val comparisonRecord =
        packet.evidenceGraph
          .candidateComparisonRecord(comparisonEvidence)
          .getOrElse(
            throw IllegalStateException(
              s"R-selected primary comparison is not registered: ${comparisonEvidence.id}"
            )
          )
      val fact = comparisonRecord.payload match
        case CandidateComparisonEvidence(value) => value
        case _ =>
          throw IllegalStateException(
            s"R-selected primary is not a candidate comparison: ${comparisonEvidence.id}"
          )
      val reference = packet.candidateLines.find(_.ref == fact.referenceLine).getOrElse(
        throw IllegalStateException("R-selected reference endpoint is not in the packet")
      )
      val candidate = packet.candidateLines.find(_.ref == fact.candidateLine).getOrElse(
        throw IllegalStateException("R-selected candidate endpoint is not in the packet")
      )
      val comparison = fact.comparison
      val verdictConfidence = fact.verdictConfidence match
        case VerdictConfidence.EngineBacked        => "engine_backed"
        case VerdictConfidence.LegalReplayVerified => "legal_replay_verified"
        case VerdictConfidence.MixedVerified       => "mixed_verified"
      val verdictCode = comparison.verdict match
        case MoveChoiceVerdict.ImprovesOnReference => "improves_on_reference"
        case MoveChoiceVerdict.MatchesReference    => "matches_reference"
        case MoveChoiceVerdict.PlayableLoss        => "playable_loss"
        case MoveChoiceVerdict.Inaccuracy          => "inaccuracy"
        case MoveChoiceVerdict.Mistake             => "mistake"
        case MoveChoiceVerdict.Blunder             => "blunder"
      val base = packet.primary match
        case PlayerFacingPrimary.MoveVerdict(_) =>
          Json.obj(
            "primary" -> Json.obj(
              "kind" -> "move_verdict",
              "comparison_evidence_id" -> comparisonEvidence.id,
              "verdict_code" -> verdictCode,
              "verdict_confidence" -> verdictConfidence,
              "mover" -> (if comparison.mover.white then "white" else "black"),
              "delta" -> comparisonDeltaJson(comparison),
              "reference_endpoint" -> endpointJson(reference.evaluation, comparison.mover),
              "played_endpoint" -> endpointJson(candidate.evaluation, comparison.mover)
            )
          )
        case PlayerFacingPrimary.BestChoice(_) =>
          val candidateSet = fact.candidateSet.getOrElse(
            throw IllegalStateException("R-selected best choice has no complete candidate-set descriptor")
          )
          val candidateSetType = candidateSet.candidateSetType match
            case CandidateSetType.OnlyMove     => "only_move"
            case CandidateSetType.NarrowChoice => "narrow_choice"
            case CandidateSetType.StyleChoice  => "style_choice"
          Json.obj(
            "primary" -> Json.obj(
              "kind" -> "best_choice",
              "comparison_evidence_id" -> comparisonEvidence.id,
              "runner_up_verdict_code" -> verdictCode,
              "verdict_confidence" -> verdictConfidence,
              "mover" -> (if comparison.mover.white then "white" else "black"),
              "delta" -> comparisonDeltaJson(comparison),
              "best_endpoint" -> endpointJson(reference.evaluation, comparison.mover),
              "runner_up_endpoint" -> endpointJson(candidate.evaluation, comparison.mover),
              "candidate_set" -> Json.obj(
                "type" -> candidateSetType
              )
            )
          )
      val causalExplanations = packet.causeExposureResolution.narrativeIdeas.map { idea =>
        val unitKind = idea.unit.kind match
          case PlayerFacingIdeaUnitKind.SingleCause             => "single_cause"
          case PlayerFacingIdeaUnitKind.ExactPvbResponsibility => "exact_pvb_responsibility"
        val facets = idea.facets.map(facetJson(_, packet))
        Json.obj(
          "kind" -> unitKind,
          "facets" -> facets
        )
      }
      base ++ Option
        .when(causalExplanations.nonEmpty)(Json.obj("causal_explanations" -> causalExplanations))
        .getOrElse(Json.obj())

    private def colorCode(side: chess.Color): String =
      if side.white then "white" else "black"

    def referenceLineInsightJson(commentary: JsObject): JsObject =
      val primary = (commentary \ "primary").as[JsObject]
      val endpoint = (primary \ "reference_endpoint").asOpt[JsObject]
        .orElse((primary \ "best_endpoint").asOpt[JsObject])
        .getOrElse(throw IllegalStateException("focused review has no reference endpoint"))
      Json.obj("endpoint" -> endpoint)

    def positionActionJson(action: PlayerFacingPositionAction): JsObject =
      action match
        case PlayerFacingPositionAction.AutomaticTerminal(terminal) =>
          Json.obj(
            "kind" -> "automatic_terminal",
            "terminal" -> automaticTerminalJson(terminal)
          )
        case PlayerFacingPositionAction.ForcedSingleMove(moveUci, mover, supportingEvaluation, drawClaims) =>
          Json.obj(
            "kind" -> "forced_single_move",
            "move_uci" -> EvidenceRef.normalizeMove(moveUci),
            "supporting_endpoint" -> endpointJson(supportingEvaluation, mover)
          ) ++ Option.when(drawClaims.nonEmpty)(
            Json.obj("draw_claims" -> drawClaims.map(drawClaimJson))
          ).getOrElse(Json.obj())
        case PlayerFacingPositionAction.DominantDrawClaim(claims) =>
          Json.obj(
            "kind" -> "draw_claim_action",
            "claims" -> claims.map(drawClaimJson)
          )

    def drawClaimJson(claim: DrawClaimAction): JsObject =
      claim match
        case DrawClaimAction.AvailableNow(rule) =>
          Json.obj(
            "rule" -> drawClaimRule(rule),
            "availability" -> "available_now"
          )
        case DrawClaimAction.AvailableByDeclaredMoves(rule, moveUcis) =>
          Json.obj(
            "rule" -> drawClaimRule(rule),
            "availability" -> "available_by_declared_move",
            "move_ucis" -> moveUcis.toList.map(EvidenceRef.normalizeMove).sorted
          )


    private def facetJson(
        facet: PlayerFacingNarrativeIdeaFacet,
        packet: EvidenceBackedJudgmentPacket
    ): JsObject =
      val selection = facet.selection
      val record = packet.evidenceGraph.record(selection.causeEvidence).getOrElse(
        throw IllegalStateException(
          s"R-selected Cause is not registered: ${selection.causeEvidence.id}"
        )
      )
      val cause = record.payload match
        case RelativeCauseFactEvidence(value) => value
        case _ =>
          throw IllegalStateException(
            s"R-selected Cause has the wrong payload: ${selection.causeEvidence.id}"
          )
      val comparison = packet.evidenceGraph.comparisonFor(cause).getOrElse(
        throw IllegalStateException(
          s"R-selected Cause has no registered comparison: ${selection.causeEvidence.id}"
        )
      )
      val binding = packet.evidenceGraph.requiredRelativeCauseBinding(cause)
      val facetRole = facet.role match
        case PlayerFacingIdeaFacetRole.Lead       => "lead"
        case PlayerFacingIdeaFacetRole.Supporting => "supporting"
      val causeKind = cause.kind match
        case RelativeCauseKind.MissedTacticalResource     => "missed_tactical_resource"
        case RelativeCauseKind.TacticalRefutationOfPlayed => "tactical_refutation_of_played"
        case RelativeCauseKind.CandidateTacticalLiability => "candidate_tactical_liability"
        case RelativeCauseKind.RecaptureRecoveryWindow   => "recapture_recovery_window"
        case RelativeCauseKind.WrongMoveOrder             => "wrong_move_order"
        case RelativeCauseKind.ConversionSecured          => "conversion_secured"
        case RelativeCauseKind.PassedPawnResult            => "passed_pawn_result"
        case RelativeCauseKind.DrawResource               => "draw_resource"
        case RelativeCauseKind.KingForcing                => "king_forcing"
        case RelativeCauseKind.MaterialSwing              => "material_swing"
      val effectMode = selection.effectMode match
        case PlayerFacingCauseEffectMode.PlayedLiability     => "played_liability"
        case PlayerFacingCauseEffectMode.AlternativeResource => "alternative_resource"
        case PlayerFacingCauseEffectMode.PlayedValue         => "played_value"
      val exposure = selection.exposure match
        case PlayerFacingCauseExposureTier.Primary       => "primary"
        case PlayerFacingCauseExposureTier.Complementary => "complementary"
      val sourceSide = cause.sourceSide match
        case RelativeCauseSourceSide.Reference => "reference"
        case RelativeCauseSourceSide.Candidate => "candidate"
        case RelativeCauseSourceSide.Shared    => "shared"
        case RelativeCauseSourceSide.Mixed     => "mixed"
      val comparisonKind = comparison.kind match
        case CandidateComparisonKind.PlayedVsBest          => "played_vs_best"
        case CandidateComparisonKind.BestVsSecond          => "best_vs_second"
        case CandidateComparisonKind.PlayedVsAlternative   => "played_vs_alternative"
        case CandidateComparisonKind.ReferenceVsAlternative => "reference_vs_alternative"
      val resourceDifferentialFacet = cause.kind == RelativeCauseKind.WrongMoveOrder
      val passedPawnResultFacet = cause.kind == RelativeCauseKind.PassedPawnResult
      if resourceDifferentialFacet then
        require(
          selection.causeEvidence.confidence == EvidenceConfidence.LegalReplayVerified &&
            selection.effectMode == PlayerFacingCauseEffectMode.AlternativeResource &&
            selection.exposure == PlayerFacingCauseExposureTier.Primary &&
            cause.sourceSide == RelativeCauseSourceSide.Reference &&
            comparison.kind == CandidateComparisonKind.PlayedVsBest &&
            facet.directChannels.forall(channel =>
              channel.rootOwnedProof.exists {
                case RootOwnedEffectProof.ForcedReplyResourceDifferential(_, _) => true
                case _                                                          => false
              }
            ),
          s"WrongMoveOrder public facet is not backed exclusively by its exact L2 proof: ${selection.causeEvidence.id}"
        )
      else if passedPawnResultFacet then
        require(
          selection.causeEvidence.confidence == EvidenceConfidence.LegalReplayVerified &&
            comparison.kind == CandidateComparisonKind.PlayedVsBest &&
            Set(RelativeCauseSourceSide.Reference, RelativeCauseSourceSide.Candidate)(cause.sourceSide) &&
            (cause.sourceSide match
              case RelativeCauseSourceSide.Reference =>
                selection.effectMode == PlayerFacingCauseEffectMode.AlternativeResource
              case RelativeCauseSourceSide.Candidate =>
                selection.effectMode == PlayerFacingCauseEffectMode.PlayedValue
              case _ => false
            ) &&
            facet.directChannels.nonEmpty && facet.directChannels.forall(channel =>
              channel.rootOwnedProof.exists {
                case RootOwnedEffectProof.PassedPawnResult(_, result) => result.hasCompleteProofPaths
                case _                                          => false
              }
            ),
          s"PassedPawnResult public facet is not backed exclusively by its exact L2 proof: ${selection.causeEvidence.id}"
        )
      else
        require(
          facet.directChannels.forall(channel =>
            !channel.rootOwnedProof.exists {
              case RootOwnedEffectProof.ForcedReplyResourceDifferential(_, _) => true
              case RootOwnedEffectProof.PassedPawnResult(_, _)                      => true
              case _                                                          => false
            }
          ),
          s"typed L2 proof is exposed under a foreign Cause kind: ${selection.causeEvidence.id}"
        )
      val facetObject = Json.obj(
        "facet_role" -> facetRole,
        "cause_evidence_id" -> selection.causeEvidence.id,
        "kind" -> causeKind,
        "proof_confidence" -> (selection.causeEvidence.confidence match
          case EvidenceConfidence.LegalReplayVerified => "legal_replay_verified"
          case EvidenceConfidence.EngineBacked        => "engine_backed"
          case EvidenceConfidence.BoardDerived        => "board_derived"
          case EvidenceConfidence.Mixed               => "mixed"
        ),
        "effect_mode" -> effectMode,
        "exposure" -> exposure,
        "source_side" -> sourceSide,
        "comparison_kind" -> comparisonKind,
        "channels" -> selection.channels.zip(facet.directChannels).map { case (selected, channel) =>
          channelJson(selected, channel, binding, packet)
        }
      )
      if resourceDifferentialFacet || passedPawnResultFacet then facetObject
      else
        facetObject ++ Json.obj(
          "event_move" -> EvidenceRef.normalizeMove(binding.eventLine.rootMove)
        )

    private def exactChannelProofLineMoves(
        channel: DirectCauseChannel,
        binding: RelativeCauseBinding,
        packet: EvidenceBackedJudgmentPacket
    ): List[String] =
      val registeredEventLine = eventLineMoves(binding, packet)
      channel.proofSegment match
        case None =>
          registeredEventLine
        case Some(segment) =>
          val requiredMoves = segment.steps.map(step =>
            step.plyOffset -> EvidenceRef.normalizeMove(step.moveUci)
          ).toMap
          val terminalOffset = requiredMoves.keys.max
          require(
            registeredEventLine.size > terminalOffset &&
              requiredMoves.forall((offset, move) =>
                EvidenceRef.sameMove(registeredEventLine(offset), move)
              ),
            s"R-selected direct channel proof does not belong to its registered event line: ${channel.binding.source.id}"
          )
          registeredEventLine.take(terminalOffset + 1)

    private def eventLineMoves(
        binding: RelativeCauseBinding,
        packet: EvidenceBackedJudgmentPacket
    ): List[String] =
      packet.candidateLines
        .find(_.ref == binding.eventLine)
        .map(_.evaluation.moves.map(EvidenceRef.normalizeMove))
        .getOrElse(
          throw IllegalStateException(
            s"R-selected Cause event line is not in the packet: ${binding.eventLine.id}"
          )
        )

    private def channelJson(
        selected: PlayerFacingCauseChannelSelection,
        channel: DirectCauseChannel,
        causeBinding: RelativeCauseBinding,
        packet: EvidenceBackedJudgmentPacket
    ): JsObject =
      val binding = channel.binding
      channel.rootOwnedProof match
        case Some(proof @ RootOwnedEffectProof.ForcedReplyResourceDifferential(_, _)) =>
          require(
            selected.directChange == DirectCausalChange.Occurred &&
              selected.playedChange == PlayerFacingCausalChange.Missed,
            s"L2 resource differential public change semantics diverged: ${selected.channelId}"
          )
          Json.obj(
            "channel_id" -> selected.channelId,
            "causal_signature" -> selected.causalSignature,
            "direct_change" -> "occurred",
            "played_change" -> "missed",
            "resource_differential_proof" -> forcedReplyResourceProofJson(proof).getOrElse(
              throw IllegalStateException(
                s"L2 resource differential proof lost its typed serializer: ${selected.channelId}"
              )
            )
          )
        case Some(proof @ RootOwnedEffectProof.PassedPawnResult(_, result)) =>
          require(
            selected.directChange == DirectCausalChange.Occurred &&
              result.hasCompleteProofPaths,
            s"L2 passed-pawn result public change semantics diverged: ${selected.channelId}"
          )
          Json.obj(
            "channel_id" -> selected.channelId,
            "causal_signature" -> selected.causalSignature,
            "direct_change" -> "occurred",
            "passed_pawn_result_proof" -> passedPawnResultProofJson(proof).getOrElse(
              throw IllegalStateException(
                s"L2 passed-pawn result lost its typed serializer: ${selected.channelId}"
              )
            )
          )
        case _ =>
          require(
            channel.proofSegment.forall(segment =>
              segment.terminalRelation != DirectCauseProofTerminalRelation.RealizesPassedPawnResult &&
                segment.steps.forall(_.passedPawnResultEventOccurrence.isEmpty)
            ),
            s"a generic public channel cannot serialize a passed-pawn-result proof: ${selected.channelId}"
          )
          Json.obj(
            "channel_id" -> selected.channelId,
            "causal_signature" -> selected.causalSignature,
            "direct_change" -> directChangeCode(selected.directChange),
            "played_change" -> playedChangeCode(selected.playedChange),
            "actor" -> rootActorJson(channel.rootActor),
            "targets" -> objectArrayJson(binding.target),
            "mechanisms" -> objectArrayJson(binding.mechanism),
            "consequences" -> objectArrayJson(binding.consequence),
            "witnesses" -> objectArrayJson(binding.witness),
            "proof_line_moves" -> exactChannelProofLineMoves(channel, causeBinding, packet)
          ) ++ binding.horizon
            .map(horizon => Json.obj("horizon" -> horizon))
            .getOrElse(Json.obj()) ++ channel.proofSegment
            .map(segment => Json.obj("proof_segment" -> proofSegmentJson(segment)))
            .getOrElse(Json.obj())

    private def forcedReplyResourceProofJson(proof: RootOwnedEffectProof): Option[JsObject] =
      proof match
        case RootOwnedEffectProof.ForcedReplyResourceDifferential(source, result) =>
          require(result.referenceLine.role == LineNodeRole.BestReference)
          require(result.playedLine.role == LineNodeRole.Played)
          val branchPayloads = List(result.referenceBranch, result.playedBranch).map { branch =>
            val line = branch.line
            Json.obj(
              "branch_id" -> branch.branchId,
              "line_id" -> line.id,
              "line_role" -> (line.role match
                case LineNodeRole.BestReference => "best_reference"
                case LineNodeRole.Played        => "played"
                case LineNodeRole.Alternative   => "alternative"
                case LineNodeRole.BranchReply   => "branch_reply"
              ),
              "branch_role" -> causalRoleCode(branch.role.stableKey),
              "root_provenance" -> causalEnumCode(branch.rootProvenance.toString),
              "line_rank" -> line.rank,
              "root_move" -> EvidenceRef.normalizeMove(line.rootMove),
              "steps" -> branch.steps.map(step =>
                Json.obj(
                  "step_index" -> step.index,
                  "provenance" -> causalEnumCode(step.provenance.toString),
                  "ply" -> step.step.ply,
                  "move_uci" -> EvidenceRef.normalizeMove(step.step.moveUci),
                  "fen_before" -> step.step.fenBefore,
                  "fen_after" -> step.step.fenAfter
                )
              )
            )
          }
          val proofPathPayloads = result.proofPaths.map { path =>
            Json.obj(
              "path_occurrence_id" -> path.pathOccurrenceId,
              "premises" -> path.premiseUses.map(premise =>
                Json.obj(
                  "role" -> causalRoleCode(premise.role.stableKey),
                  "contract" -> causalEnumCode(premise.contract.toString),
                  "result_id" -> premise.result.stableKey,
                  "source_premise_ids" -> premise.sourcePremiseIds,
                  "branch_id" -> premise.branchId,
                  "branch_role" -> causalRoleCode(premise.branchRole.stableKey),
                  "step_index" -> premise.stepIndex
                )
              ),
              "closed_absence_uses" -> path.closedAbsenceUses.map { use =>
                val binding = use.binding
                Json.obj(
                  "use_id" -> use.useId,
                  "role" -> causalRoleCode(binding.role.stableKey),
                  "semantic_proof_id" -> binding.semanticProofId,
                  "issuer" -> "position_relation_extractor.closed_relation_inventory",
                  "issuer_evidence_id" -> binding.issuerEvidenceId,
                  "issuer_occurrence_id" -> binding.issuerOccurrenceId,
                  "query" -> binding.queryKey,
                  "branch_id" -> binding.branchId,
                  "branch_role" -> causalRoleCode(binding.branchRole.stableKey),
                  "after_step_index" -> binding.afterStepIndex,
                  "position" -> Json.obj(
                    "fen" -> binding.position.fen,
                    "ply" -> binding.position.ply,
                    "scope" -> evidenceScopeCode(binding.scope)
                  )
                )
              }
            )
          }
          Some(
            Json.obj(
              "family" -> "immediate_forced_reply_resource_differential",
              "trigger_mechanism" -> result.triggerMechanism,
              "source_evidence_id" -> source.id,
              "semantic_id" -> result.semanticId,
              "occurrence_id" -> result.occurrenceId,
              "dependency_fingerprint" -> result.dependencyId,
              "counterfactual_reference_branch" -> branchPayloads.head,
              "played_root_branch" -> branchPayloads(1),
              "proof_paths" -> proofPathPayloads,
              "participants" -> Json.obj(
                "trigger" -> movementWitnessJson(result.triggerMovement),
                "forced_reply" -> legalResourceWitnessJson(result.forcedReplyResource),
                "realizer" -> movementWitnessJson(result.realizerMovement),
                "captured_target" -> coloredPieceWitnessJson(result.capturedTarget),
                "played_defense" -> legalResourceWitnessJson(result.playedDefenseResource),
                "disabled_defender" -> coloredPieceWitnessJson(result.disabledDefender)
              ),
              "realizing_move" -> EvidenceRef.normalizeMove(result.realizingMove),
              "played_root_branch_legal_defense_move" -> EvidenceRef.normalizeMove(result.playedDefenseMove)
            )
          )
        case _ => None

    private def passedPawnResultProofJson(proof: RootOwnedEffectProof): Option[JsObject] =
      proof match
        case RootOwnedEffectProof.PassedPawnResult(source, result) =>
          val inventory = result.publicClosedReplyInventory
          val branches = result.publicBranches.map { branch =>
            Json.obj(
              "branch_id" -> branch.branchId,
              "role" -> branch.role,
              "line" -> passedPawnResultLineJson(
                branch.lineId,
                branch.lineRole,
                branch.lineRank,
                branch.lineRootMove
              ),
              "root_provenance" -> branch.rootProvenance,
              "steps" -> branch.steps.map { step =>
                Json.obj(
                  "step_index" -> step.index,
                  "step_key" -> step.stepKey,
                  "ply" -> step.ply,
                  "move_uci" -> EvidenceRef.normalizeMove(step.moveUci),
                  "fen_before" -> step.fenBefore,
                  "fen_after" -> step.fenAfter,
                  "line" -> passedPawnResultLineJson(
                    step.lineId,
                    step.lineRole,
                    step.lineRank,
                    step.lineRootMove
                  ),
                  "provenance" -> step.provenance
                ) ++ step.incomingLink
                  .map(link =>
                    Json.obj(
                      "incoming_link" -> Json.obj(
                        "kind" -> link.kind,
                        "from_step_key" -> link.fromStepKey,
                        "to_step_key" -> link.toStepKey,
                          "occurrence_link_key" -> link.occurrenceLinkKey
                      )
                    )
                  )
                  .getOrElse(Json.obj())
              }
            ) ++ branch.replyMove
              .map(move => Json.obj("reply_move" -> EvidenceRef.normalizeMove(move)))
              .getOrElse(Json.obj()) ++ branch.sourceProbeId
              .map(probe => Json.obj("source_probe_id" -> probe))
              .getOrElse(Json.obj())
          }
          val proofPaths = result.publicProofPaths.map { path =>
            Json.obj(
              "path_occurrence_id" -> path.pathOccurrenceId,
              "reply_branch_id" -> path.replyBranchId,
              "realization_actor" -> passedPawnResultActorJson(path.realizationActor),
              "realization_move" -> EvidenceRef.normalizeMove(path.realizationMove),
              "realization_ply" -> path.realizationPly,
              "realization_match_kind" -> causalEnumCode(path.realizationMatchKind.toString),
              "premises" -> path.premises.map(premise =>
                Json.obj(
                  "role" -> premise.role,
                  "lower_kind" -> premise.lowerKind,
                  "lower_semantic_key" -> premise.lowerSemanticKey,
                  "source_premise_ids" -> premise.sourcePremiseIds,
                  "branch_id" -> premise.branchId,
                  "branch_role" -> premise.branchRole,
                  "related_branch_ids" -> premise.relatedBranchIds,
                  "from_step_index" -> premise.fromStepIndex,
                  "to_step_index" -> premise.toStepIndex
                )
              ),
              "closure_use_ids" -> path.closureUseIds
            )
          }
          Some(
            Json.obj(
              "contract" -> result.contract,
              "source_evidence_id" -> source.id,
              "event_evidence_id" -> result.eventSource.id,
              "comparison_evidence_id" -> result.comparisonDemand.id,
              "semantic_id" -> result.semanticId,
              "occurrence_id" -> result.occurrenceId,
              "dependency_fingerprint" -> result.dependencyFingerprint,
              "consequence_kind" -> causalEnumCode(result.consequenceKind.toString),
              "result_target_subjects" -> result.resultTargetSubjects,
              "root_actor" -> passedPawnResultActorJson(result.rootActor),
              "realizing_actor" -> passedPawnResultActorJson(result.resultActor),
              "root_line" -> passedPawnResultLineJson(result.rootLine),
              "root_move" -> EvidenceRef.normalizeMove(result.rootMove),
              "root_ply" -> result.rootPly,
              "realizing_move" -> EvidenceRef.normalizeMove(result.realizingMove),
              "realizing_ply" -> result.realizingPly,
              "result_ply_offset" -> result.resultPlyOffset,
              "closed_legal_reply_inventory" -> Json.obj(
                "issuer" -> "structural_delta.canonical_legal_reply_inventory",
                "issuer_evidence_id" -> inventory.issuerEvidenceId,
                "coverage_issuer" -> "passed_pawn_result_event.branch_complete_reply_coverage",
                "coverage_evidence_id" -> inventory.coverageEvidenceId,
                "root_after" -> Json.obj(
                  "fen" -> inventory.rootAfterFen,
                  "ply" -> inventory.rootAfterPly,
                  "scope" -> inventory.scope
                ),
                "legal_reply_moves" -> inventory.legalReplyMoves.map(EvidenceRef.normalizeMove),
                "branch_by_reply" -> inventory.branchByReply.map { case (replyMove, branchId) =>
                  Json.obj(
                    "reply_move" -> EvidenceRef.normalizeMove(replyMove),
                    "branch_id" -> branchId
                  )
                },
                "certified_horizon_ply_offset" -> inventory.certifiedHorizonPlyOffset
              ),
              "branches" -> branches,
              "proof_paths" -> proofPaths,
              "lower_premise_ids" -> result.lowerPremiseIds
            )
          )
        case _ => None

    private def passedPawnResultLineJson(line: LineNodeRef): JsObject =
      passedPawnResultLineJson(
        line.id,
        line.role match
          case LineNodeRole.Played        => "played"
          case LineNodeRole.BestReference => "best_reference"
          case LineNodeRole.Alternative   => "alternative"
          case LineNodeRole.BranchReply   => "branch_reply",
        line.rank,
        line.rootMove
      )

    private def passedPawnResultLineJson(
        lineId: String,
        lineRole: String,
        lineRank: Int,
        rootMove: String
    ): JsObject =
      Json.obj(
        "line_id" -> lineId,
        "line_role" -> lineRole,
        "line_rank" -> lineRank,
        "root_move" -> EvidenceRef.normalizeMove(rootMove)
      )

    private def passedPawnResultActorJson(actor: lila.chessjudgment.model.PassedPawnResultActorOccurrence): JsObject =
      Json.obj(
        "side" -> colorCode(actor.side),
        "piece_before" -> actor.beforeRole,
        "piece_after" -> actor.afterRole,
        "from" -> actor.from,
        "to" -> actor.to,
        "legal_move_relation" -> actor.legalMoveSemanticId
      )

    private def causalRoleCode(value: String): String =
      value.replace('-', '_')

    private def causalEnumCode(value: String): String =
      value.zipWithIndex
        .flatMap { case (character, index) =>
          if character.isUpper && index > 0 then s"_${character.toLower}"
          else character.toLower.toString
        }
        .mkString

    private def movementWitnessJson(witness: RelationMoveTransitionWitness): JsObject =
      Json.obj(
        "side" -> colorCode(witness.side),
        "from" -> witness.from.key.toLowerCase,
        "to" -> witness.to.key.toLowerCase,
        "piece_before" -> witness.beforeRole.name.toLowerCase,
        "piece_after" -> witness.afterRole.name.toLowerCase
      )

    private def legalResourceWitnessJson(witness: RelationLegalMoveResourceWitness): JsObject =
      movementWitnessJson(witness.movement) ++ Json.obj(
        "move_uci" -> EvidenceRef.normalizeMove(witness.moveUci)
      )

    private def coloredPieceWitnessJson(witness: RelationColoredPieceWitness): JsObject =
      Json.obj(
        "side" -> colorCode(witness.side),
        "piece" -> witness.role.name.toLowerCase,
        "square" -> witness.square.key.toLowerCase
      )

    private def evidenceScopeCode(scope: EvidenceScope): String =
      scope match
        case EvidenceScope.BeforePosition         => "before_position"
        case EvidenceScope.AfterPlayedPosition    => "after_played_position"
        case EvidenceScope.AfterReferencePosition => "after_reference_position"
        case EvidenceScope.CurrentPosition        => "current_position"
        case EvidenceScope.PlayedTransition       => "played_transition"
        case EvidenceScope.ReferenceTransition    => "reference_transition"
        case EvidenceScope.AlternativeTransition  => "alternative_transition"
        case EvidenceScope.BestLine               => "best_line"
        case EvidenceScope.PlayedLine             => "played_line"
        case EvidenceScope.CandidateLine          => "candidate_line"
        case EvidenceScope.BranchReplyLine        => "branch_reply_line"
        case EvidenceScope.Counterfactual         => "counterfactual"

    private def rootActorJson(actor: RootCausalActor): JsObject =
      Json.obj(
        "move_uci" -> EvidenceRef.normalizeMove(actor.moveUci),
        "side" -> colorCode(actor.color),
        "piece" -> actor.role.name.toLowerCase,
        "from" -> actor.from.key.toLowerCase,
        "to" -> actor.to.key.toLowerCase
      )

    private def objectArrayJson(objects: List[ConcreteChessObject]): List[JsObject] =
      objects
        .distinctBy(_.signaturePart)
        .sortBy(_.signaturePart)
        .map(obj => Json.obj("kind" -> objectKindCode(obj.kind), "key" -> obj.key))

    private def proofSegmentJson(segment: DirectCauseProofSegment): JsObject =
      Json.obj(
        "terminal_relation" -> proofTerminalRelationCode(segment.terminalRelation),
        "steps" -> segment.steps.map(step =>
          Json.obj(
            "ply_offset" -> step.plyOffset,
            "move_uci" -> EvidenceRef.normalizeMove(step.moveUci),
            "role" -> proofStepRoleCode(step.role)
          )
        )
      )

    private def directChangeCode(change: DirectCausalChange): String =
      change match
        case DirectCausalChange.Occurred   => "occurred"
        case DirectCausalChange.Maintained => "maintained"
        case DirectCausalChange.Lost       => "lost"

    private def playedChangeCode(change: PlayerFacingCausalChange): String =
      change match
        case PlayerFacingCausalChange.Occurred   => "occurred"
        case PlayerFacingCausalChange.Maintained => "maintained"
        case PlayerFacingCausalChange.Lost       => "lost"
        case PlayerFacingCausalChange.Missed     => "missed"

    private def objectKindCode(kind: EvidenceObjectKind): String =
      kind match
        case EvidenceObjectKind.Move        => "move"
        case EvidenceObjectKind.Piece       => "piece"
        case EvidenceObjectKind.Side        => "side"
        case EvidenceObjectKind.Square      => "square"
        case EvidenceObjectKind.File        => "file"
        case EvidenceObjectKind.Pawn        => "pawn"
        case EvidenceObjectKind.PassedPawnSubject => "passed_pawn_subject"
        case EvidenceObjectKind.Relation    => "relation"
        case EvidenceObjectKind.Line        => "line"
        case EvidenceObjectKind.Mechanism   => "mechanism"
        case EvidenceObjectKind.Consequence => "consequence"

    private def proofTerminalRelationCode(relation: DirectCauseProofTerminalRelation): String =
      relation match
        case DirectCauseProofTerminalRelation.ProducesLineConsequence =>
          "produces_line_consequence"
        case DirectCauseProofTerminalRelation.IsRootLineEvent =>
          "is_root_line_event"
        case DirectCauseProofTerminalRelation.InstantiatesRelation =>
          "instantiates_relation"
        case DirectCauseProofTerminalRelation.RealizesPassedPawnResult =>
          throw IllegalStateException("a passed-pawn result requires the typed passed_pawn_result_proof serializer")

    private def proofStepRoleCode(role: DirectCauseProofStepRole): String =
      role match
        case DirectCauseProofStepRole.RootAction    => "root_action"
        case DirectCauseProofStepRole.CausalLink    => "causal_link"
        case DirectCauseProofStepRole.TerminalEvent => "terminal_event"

    private def comparisonDeltaJson(comparison: EvalComparison): JsObject =
      val base = Json.obj(
        "candidate_win_percent_delta_for_mover" -> comparison.candidateWinPercentDeltaForMover
      )
      comparison.detail match
        case CandidateComparisonDeltaDetail.EngineEvaluation(_, _) =>
          base ++ Json.obj("kind" -> "engine_evaluation")
        case CandidateComparisonDeltaDetail.OutcomeOnly =>
          base ++ Json.obj("kind" -> "outcome_only")

    private def endpointJson(evaluation: CandidateLineEvaluation, mover: chess.Color): JsObject =
      evaluation match
        case CandidateLineEvaluation.EngineSearch(line) =>
          Json.obj(
            "kind" -> "engine_search",
            "moves" -> line.moves.map(EvidenceRef.normalizeMove),
            "win_percent_for_mover" -> evaluation.winPercentForMover(mover),
            "depth" -> line.depth
          ) ++ line.mate
            .map(value => Json.obj("mate_forecast_white_pov" -> value))
            .getOrElse(Json.obj())
        case CandidateLineEvaluation.ExactAutomaticTerminal(moves, terminal) =>
          Json.obj(
            "kind" -> "exact_automatic_terminal",
            "moves" -> moves.map(EvidenceRef.normalizeMove),
            "terminal" -> automaticTerminalJson(terminal)
          )

    private def automaticTerminalJson(terminal: AutomaticTerminal): JsObject =
      terminal match
        case AutomaticTerminal.Checkmate(winner) =>
          Json.obj(
            "kind" -> "checkmate",
            "winner" -> (if winner.white then "white" else "black")
          )
        case AutomaticTerminal.Stalemate =>
          Json.obj("kind" -> "stalemate")
        case AutomaticTerminal.InsufficientMaterial =>
          Json.obj("kind" -> "insufficient_material")
        case AutomaticTerminal.FivefoldRepetition =>
          Json.obj("kind" -> "fivefold_repetition")
        case AutomaticTerminal.SeventyFiveMoveRule =>
          Json.obj("kind" -> "seventy_five_move_rule")

    private def drawClaimRule(rule: DrawClaimRule): String =
      rule match
        case DrawClaimRule.ThreefoldRepetition => "threefold_repetition"
        case DrawClaimRule.FiftyMoveRule        => "fifty_move_rule"
  private object InputLimits:
    private val MaxFenLength = 128
    private val MaxVariations = 16
    private val MaxMovePrefix = 600
    private val MaxReplyLines = 8
    private val MaxPly = 1000
    private val MaxText = 256
    private val RequestId = "[A-Za-z0-9._:-]{1,128}".r

    def validRequestId(value: String): Boolean = RequestId.matches(value)

    def accepts(raw: RawMoveReviewInput): Boolean =
      val lines = raw.variations ++ raw.probeResults.flatMap(_.resolution match
        case ProbeResolution.EngineSearch(evaluations, _) => evaluations.flatMap(_.engineLine)
        case ProbeResolution.ExactAutomaticTerminal(_)   => Nil
      )
      raw.fen.nonEmpty && raw.fen.length <= MaxFenLength &&
        validUci(raw.playedMoveUci) &&
        raw.variations.nonEmpty && raw.variations.size <= MaxVariations &&
        raw.movePrefixUci.size <= MaxMovePrefix && raw.movePrefixUci.forall(validUci) &&
        raw.probeResults.map(_.id).distinct.size == raw.probeResults.size &&
        raw.probeResults.forall(validProbe) &&
        raw.ply.forall(ply => ply >= 0 && ply <= MaxPly) &&
        lines.forall(validLine)

    private def validLine(line: EngineLine): Boolean =
      EngineLineAdmission.acceptsRuntimeInputLine(line)

    private def validProbe(probe: ProbeResult): Boolean =
      probe.id.nonEmpty && probe.id.length <= MaxText &&
        (probe.resolution match
          case ProbeResolution.EngineSearch(evaluations, depth) =>
            evaluations.nonEmpty && evaluations.size <= MaxReplyLines &&
              evaluations.forall(_.engineLine.nonEmpty) &&
              EngineLineAdmission.acceptsRequiredDepth(depth)
          case ProbeResolution.ExactAutomaticTerminal(evaluation) =>
            EngineLineAdmission.acceptsRuntimeInputMoves(evaluation.moves)
        )

    private def validUci(value: String): Boolean = EngineLineAdmission.acceptsRuntimeInputUci(value)
