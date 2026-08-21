package io.chesstory.runtime

import scala.util.Try

import lila.chessjudgment.analysis.assembly.{
  MoveReviewJudgmentOrchestrator,
  RawMoveReviewInput,
  RawOpeningContext
}
import lila.chessjudgment.analysis.opening.{ OpeningRecognitionIndex, OpeningThemePriorIndex }
import lila.chessjudgment.model.{ ProbeHorizon, ProbePurpose, ProbeRequest, ProbeResolution, ProbeResult }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ AutomaticTerminal, CandidateLineEvaluation, DrawClaimAction, DrawClaimRule }
import lila.chessjudgment.model.strategic.EngineLine
import play.api.libs.json.*

object RuntimeProtocol:
  val RequestSchema = "chesstory.move-meaning.request.v1"
  val ResponseSchema = "chesstory.move-meaning.response.v4"

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
      variationHash: Option[String]
  ):
    def toCore: Option[ProbeResult] =
      for
        lines <- replyLines.filter(_.nonEmpty)
        realizedDepth <- depth.filter(_ > 0)
      yield ProbeResult(
        id = id,
        fen = Some(fen),
        resolution = ProbeResolution.EngineSearch(
          lines.map(line => CandidateLineEvaluation.EngineSearch(line.toCore)),
          realizedDepth
        ),
        purpose = purpose,
        probedMove = probedMove,
        candidateMove = candidateMove,
        opponentResourceMove = opponentResourceMove,
        horizon = horizon,
        variationHash = variationHash
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
    def toCore: Option[RawMoveReviewInput] =
      val coreProbes = probeResults.getOrElse(Nil).map(_.toCore)
      Option.when(coreProbes.forall(_.nonEmpty))(
        RawMoveReviewInput(
        fen = fen,
        playedMoveUci = playedMoveUci,
        variations = variations.map(_.toCore),
        ply = ply,
        openingContext = openingContext,
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
    val purpose = request.purpose match
      case Some(ProbePurpose.ReplyMultipv) => "reply_multipv"
      case None =>
        throw IllegalStateException("public probe request has no purpose")
    val multiPv = request.multiPv match
      case Some(value) => value
      case None =>
        throw IllegalStateException("public probe request has no multiPv")
    val candidateMove = request.candidateMove match
      case Some(value) => value
      case None =>
        throw IllegalStateException("public probe request has no candidateMove")
    val variationHash = request.variationHash match
      case Some(value) => value
      case None =>
        throw IllegalStateException("public probe request has no variationHash")
    val variant = (request.horizon, request.opponentResourceMove) match
      case (Some(horizon), None) => Json.obj("horizon" -> horizon)
      case (None, Some(opponentResourceMove)) =>
        Json.obj("opponentResourceMove" -> opponentResourceMove)
      case (Some(_), Some(_)) =>
        throw IllegalStateException("public probe request cannot mix horizon and opponent resource variants")
      case (None, None) =>
        throw IllegalStateException("public probe request has no certified variant")
    Json.obj(
      "id" -> request.id,
      "fen" -> request.fen,
      "moves" -> request.moves,
      "depth" -> request.depth,
      "purpose" -> purpose,
      "multiPv" -> multiPv,
      "candidateMove" -> candidateMove,
      "variationHash" -> variationHash
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
          "move_commentary" -> MoveMeaningProjection.moveCommentaryJson(packet)
        ) ++ requestField
      )

  private[runtime] def encodePositionCommentaryResponse(
      requestId: String,
      completed: CompletedCommentaryJob
  ): JsObject =
    val result = completed.result match
      case CompletedPositionCommentary.PositionAction(action) =>
        MoveMeaningProjection.positionActionJson(action)
      case CompletedPositionCommentary.MoveChoices(drawClaims, moveReviews) =>
        Json.obj(
          "kind" -> "move_choices",
          "move_commentaries" -> moveReviews.map { record =>
            Json.obj(
              "legal_move_index" -> record.legalMoveIndex,
              "move_uci" -> record.moveUci,
              "commentary" -> MoveMeaningProjection.moveCommentaryJson(record.judgmentPacket)
            )
          }
        ) ++ Option.when(drawClaims.nonEmpty)(
          Json.obj(
            "draw_claims" -> drawClaims.map(MoveMeaningProjection.drawClaimJson),
            "presentation" -> MoveMeaningProjection.drawClaimPresentation(drawClaims)
          )
        ).getOrElse(Json.obj())
    Json.obj(
      "schema_version" -> "chesstory.position-commentary.response.v4",
      "request_id" -> requestId,
      "engine_profile" -> completed.engineProfile.wireId,
      "current_fen" -> completed.currentFen,
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
          val verdictText = fact.comparison.verdict match
            case MoveChoiceVerdict.ImprovesOnReference => "improves on"
            case MoveChoiceVerdict.MatchesReference    => "matches"
            case MoveChoiceVerdict.PlayableLoss        => "is playable but inferior to"
            case MoveChoiceVerdict.Inaccuracy          => "is an inaccuracy compared with"
            case MoveChoiceVerdict.Mistake             => "is a mistake compared with"
            case MoveChoiceVerdict.Blunder             => "is a blunder compared with"
          Json.obj(
            "primary" -> Json.obj(
              "kind" -> "move_verdict",
              "comparison_evidence_id" -> comparisonEvidence.id,
              "verdict_code" -> verdictCode,
              "verdict_confidence" -> verdictConfidence,
              "mover" -> (if comparison.mover.white then "white" else "black"),
              "delta" -> comparisonDeltaJson(comparison),
              "reference_endpoint" -> endpointJson(reference.evaluation, comparison.mover),
              "played_endpoint" -> endpointJson(candidate.evaluation, comparison.mover),
              "presentation" ->
                s"${EvidenceRef.normalizeMove(fact.candidateLine.rootMove)} $verdictText ${EvidenceRef.normalizeMove(fact.referenceLine.rootMove)}."
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
              ),
              "presentation" ->
                s"${EvidenceRef.normalizeMove(fact.referenceLine.rootMove)} is the best choice; ${EvidenceRef.normalizeMove(fact.candidateLine.rootMove)} is the runner-up."
            )
          )
      val causalExplanations = packet.causeExposureResolution.narrativeIdeas.map { idea =>
        val unitKind = idea.unit.kind match
          case PlayerFacingIdeaUnitKind.SingleCause             => "single_cause"
          case PlayerFacingIdeaUnitKind.ExactPvbResponsibility => "exact_pvb_responsibility"
        val facets = idea.facets.map(facetJson(_, packet))
        val presentation = idea.unit.kind match
          case PlayerFacingIdeaUnitKind.SingleCause =>
            val lead = facets match
              case head :: _ => head
              case Nil =>
                throw IllegalStateException("R-selected single-cause unit has no facet")
            val leadKind = (lead \ "kind").as[String]
            val leadEffectMode = (lead \ "effect_mode").as[String]
            val leadChanges = (lead \ "channels").as[List[JsObject]].map { channel =>
              s"${(channel \ "direct_change").as[String]} / ${(channel \ "played_change").as[String]}"
            }.mkString(", ")
            s"The selected $leadKind cause uses $leadEffectMode effect mode with $leadChanges change evidence."
          case PlayerFacingIdeaUnitKind.ExactPvbResponsibility =>
            val (lead, supporting) = facets match
              case lead :: tail if tail.nonEmpty => lead -> tail
              case Nil =>
                throw IllegalStateException("R-selected exact PVB responsibility unit has no facet")
              case _ :: Nil =>
                throw IllegalStateException("R-selected exact PVB responsibility unit has no supporting facet")
            val leadKind = (lead \ "kind").as[String]
            val leadEffectMode = (lead \ "effect_mode").as[String]
            val supportingKinds = supporting.map { facet =>
              s"${(facet \ "kind").as[String]} ${(facet \ "effect_mode").as[String]}"
            }.mkString(", ")
            s"The selected $leadKind $leadEffectMode cause and $supportingKinds facets form one exact same-effect responsibility relation."
        Json.obj(
          "kind" -> unitKind,
          "presentation" -> presentation,
          "facets" -> facets
        )
      }
      base ++ Option
        .when(causalExplanations.nonEmpty)(Json.obj("causal_explanations" -> causalExplanations))
        .getOrElse(Json.obj())

    def positionActionJson(action: PlayerFacingPositionAction): JsObject =
      action match
        case PlayerFacingPositionAction.AutomaticTerminal(terminal) =>
          Json.obj(
            "kind" -> "automatic_terminal",
            "terminal" -> automaticTerminalJson(terminal),
            "presentation" -> terminalPresentation(terminal)
          )
        case PlayerFacingPositionAction.ForcedSingleMove(moveUci, mover, supportingEvaluation, drawClaims) =>
          Json.obj(
            "kind" -> "forced_single_move",
            "move_uci" -> EvidenceRef.normalizeMove(moveUci),
            "supporting_endpoint" -> endpointJson(supportingEvaluation, mover),
            "presentation" -> s"${EvidenceRef.normalizeMove(moveUci)} is the only legal move."
          ) ++ Option.when(drawClaims.nonEmpty)(
            Json.obj("draw_claims" -> drawClaims.map(drawClaimJson))
          ).getOrElse(Json.obj())
        case PlayerFacingPositionAction.DominantDrawClaim(claims) =>
          Json.obj(
            "kind" -> "draw_claim_action",
            "claims" -> claims.map(drawClaimJson),
            "presentation" -> "A legal draw claim is stronger than every played-move outcome."
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

    private[RuntimeProtocol] def drawClaimPresentation(claims: List[DrawClaimAction]): String =
      claims.map {
        case DrawClaimAction.AvailableNow(DrawClaimRule.ThreefoldRepetition) =>
          "A draw can be claimed now by threefold repetition."
        case DrawClaimAction.AvailableNow(DrawClaimRule.FiftyMoveRule) =>
          "A draw can be claimed now under the fifty-move rule."
        case DrawClaimAction.AvailableByDeclaredMoves(DrawClaimRule.ThreefoldRepetition, moveUcis) =>
          val moves = moveUcis.toList.map(EvidenceRef.normalizeMove).sorted.mkString(", ")
          s"A draw can be claimed by declaring $moves to create a threefold repetition."
        case DrawClaimAction.AvailableByDeclaredMoves(DrawClaimRule.FiftyMoveRule, moveUcis) =>
          val moves = moveUcis.toList.map(EvidenceRef.normalizeMove).sorted.mkString(", ")
          s"A draw can be claimed by declaring $moves under the fifty-move rule."
      }.mkString(" ")

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
        case RelativeCauseKind.WrongRecapturer            => "wrong_recapturer"
        case RelativeCauseKind.RecaptureRecoveryWindow   => "recapture_recovery_window"
        case RelativeCauseKind.WrongMoveOrder             => "wrong_move_order"
        case RelativeCauseKind.OnlyDefenseNecessity       => "only_defense_necessity"
        case RelativeCauseKind.TempoLoss                  => "tempo_loss"
        case RelativeCauseKind.ConversionMiss             => "conversion_miss"
        case RelativeCauseKind.ConversionSecured          => "conversion_secured"
        case RelativeCauseKind.SacrificeCompensation      => "sacrifice_compensation"
        case RelativeCauseKind.StructuralImprovement      => "structural_improvement"
        case RelativeCauseKind.TargetPressureGain         => "target_pressure_gain"
        case RelativeCauseKind.TargetPressureRelease      => "target_pressure_release"
        case RelativeCauseKind.CenterControlGain          => "center_control_gain"
        case RelativeCauseKind.KingSafetyConcession       => "king_safety_concession"
        case RelativeCauseKind.PawnWeaknessTarget         => "pawn_weakness_target"
        case RelativeCauseKind.PawnBreakOpportunity       => "pawn_break_opportunity"
        case RelativeCauseKind.ActivityGain               => "activity_gain"
        case RelativeCauseKind.ActivityLoss               => "activity_loss"
        case RelativeCauseKind.OpponentRestriction        => "opponent_restriction"
        case RelativeCauseKind.StrategicConcession        => "strategic_concession"
        case RelativeCauseKind.MissedStrategicImprovement => "missed_strategic_improvement"
        case RelativeCauseKind.PlanImprovement            => "plan_improvement"
        case RelativeCauseKind.PlanContradiction          => "plan_contradiction"
        case RelativeCauseKind.DefensiveResource          => "defensive_resource"
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
      Json.obj(
        "facet_role" -> facetRole,
        "cause_evidence_id" -> selection.causeEvidence.id,
        "kind" -> causeKind,
        "proof_confidence" -> (selection.causeEvidence.confidence match
          case EvidenceConfidence.LegalReplayVerified => "legal_replay_verified"
          case EvidenceConfidence.EngineBacked        => "engine_backed"
          case EvidenceConfidence.BoardDerived        => "board_derived"
          case EvidenceConfidence.Heuristic           => "heuristic"
          case EvidenceConfidence.Mixed               => "mixed"
        ),
        "effect_mode" -> effectMode,
        "exposure" -> exposure,
        "source_side" -> sourceSide,
        "event_move" -> EvidenceRef.normalizeMove(binding.eventLine.rootMove),
        "comparison_kind" -> comparisonKind,
        "channels" -> selection.channels.map { channel =>
          val directChange = channel.directChange match
            case DirectCausalChange.Occurred   => "occurred"
            case DirectCausalChange.Prevented  => "prevented"
            case DirectCausalChange.Maintained => "maintained"
            case DirectCausalChange.Lost       => "lost"
            case DirectCausalChange.Refuted    => "refuted"
            case DirectCausalChange.Missed     => "missed"
          val playedChange = channel.playedChange match
            case PlayerFacingCausalChange.Occurred   => "occurred"
            case PlayerFacingCausalChange.Prevented  => "prevented"
            case PlayerFacingCausalChange.Maintained => "maintained"
            case PlayerFacingCausalChange.Lost       => "lost"
            case PlayerFacingCausalChange.Refuted    => "refuted"
            case PlayerFacingCausalChange.Missed     => "missed"
          Json.obj(
            "causal_signature" -> channel.causalSignature,
            "direct_change" -> directChange,
            "played_change" -> playedChange
          )
        }
      )

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

    private def terminalPresentation(terminal: AutomaticTerminal): String =
      terminal match
        case AutomaticTerminal.Checkmate(winner) =>
          s"Checkmate: ${if winner.white then "White" else "Black"} wins."
        case AutomaticTerminal.Stalemate =>
          "The position is an automatic draw by stalemate."
        case AutomaticTerminal.InsufficientMaterial =>
          "The position is an automatic draw because checkmate is impossible."
        case AutomaticTerminal.FivefoldRepetition =>
          "The position is an automatic draw by fivefold repetition."
        case AutomaticTerminal.SeventyFiveMoveRule =>
          "The position is an automatic draw under the seventy-five-move rule."

    private def drawClaimRule(rule: DrawClaimRule): String =
      rule match
        case DrawClaimRule.ThreefoldRepetition => "threefold_repetition"
        case DrawClaimRule.FiftyMoveRule        => "fifty_move_rule"
  private object InputLimits:
    private val MaxFenLength = 128
    private val MaxVariations = 16
    private val MaxMovePrefix = 600
    private val MaxProbeResults = 12
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
        raw.probeResults.size <= MaxProbeResults &&
        raw.probeResults.map(_.id).distinct.size == raw.probeResults.size &&
        raw.probeResults.forall(validProbe) &&
        raw.ply.forall(ply => ply >= 0 && ply <= MaxPly) &&
        raw.openingContext.forall(validOpening) &&
        lines.forall(validLine)

    private def validLine(line: EngineLine): Boolean =
      EngineLineAdmission.acceptsRuntimeInputLine(line)

    private def validProbe(probe: ProbeResult): Boolean =
      probe.id.nonEmpty && probe.id.length <= MaxText &&
        probe.fen.exists(fen => fen.nonEmpty && fen.length <= MaxFenLength) &&
        (probe.resolution match
          case ProbeResolution.EngineSearch(evaluations, depth) =>
            evaluations.nonEmpty && evaluations.size <= MaxReplyLines &&
              evaluations.forall(_.engineLine.nonEmpty) &&
              EngineLineAdmission.acceptsRequiredDepth(depth)
          case ProbeResolution.ExactAutomaticTerminal(_) => false
        ) &&
        probe.probedMove.forall(validUci) && probe.candidateMove.forall(validUci) &&
        probe.opponentResourceMove.forall(validUci) &&
        probe.horizon.forall(value => value.length <= MaxText && ProbeHorizon.plyOffset(value).nonEmpty) &&
        probe.variationHash.forall(hash => hash.nonEmpty && hash.length <= MaxText)

    private def validOpening(opening: RawOpeningContext): Boolean =
      List(opening.eco, opening.name, opening.family).flatten.forall(_.length <= MaxText)

    private def validUci(value: String): Boolean = EngineLineAdmission.acceptsRuntimeInputUci(value)

object RuntimeResources:
  def verify(): Unit =
    require(
      OpeningRecognitionIndex.default.allEntries.nonEmpty,
      "scalachess opening database is empty"
    )
    OpeningThemePriorIndex.default
