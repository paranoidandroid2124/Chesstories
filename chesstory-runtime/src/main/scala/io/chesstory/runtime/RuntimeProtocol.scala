package io.chesstory.runtime

import scala.util.Try

import lila.chessjudgment.analysis.assembly.{
  MoveReviewJudgmentOrchestrator,
  RawMoveReviewInput
}
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

  private final case class WireInput(
      fen: String,
      playedMoveUci: String,
      variations: List[WireVariation],
      ply: Option[Int],
      movePrefixUci: Option[List[String]]
  ):
    def toCore: RawMoveReviewInput =
      RawMoveReviewInput(
        fen = fen,
        playedMoveUci = playedMoveUci,
        variations = variations.map(_.toCore),
        ply = ply,
        movePrefixUci = movePrefixUci.getOrElse(Nil)
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
                  evaluate(requestId, input.toCore)
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

  private[runtime] def encodeMoveMeaningResult(
      requestId: Option[String],
    packet: EvidenceBackedJudgmentPacket
  ): Result =
    val requestField = requestId.map(value => Json.obj("request_id" -> value)).getOrElse(Json.obj())
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
          Json.obj(
            "primary" -> Json.obj(
              "kind" -> "best_choice",
              "comparison_evidence_id" -> comparisonEvidence.id,
              "runner_up_verdict_code" -> verdictCode,
              "verdict_confidence" -> verdictConfidence,
              "mover" -> (if comparison.mover.white then "white" else "black"),
              "delta" -> comparisonDeltaJson(comparison),
              "best_endpoint" -> endpointJson(reference.evaluation, comparison.mover),
              "runner_up_endpoint" -> endpointJson(candidate.evaluation, comparison.mover)
            )
          )
      val occurrenceExplanations = packet.occurrenceExplanations.map(occurrenceExplanationJson)
      base ++ Option
        .when(occurrenceExplanations.nonEmpty)(
          Json.obj("occurrence_explanations" -> occurrenceExplanations)
        )
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

    private def occurrenceExplanationJson(
        certified: CertifiedOccurrenceExplanation
    ): JsObject =
      certified.rootOwnedProof match
        case proof: RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture =>
          Json.obj(
            "cause_evidence_id" -> certified.causeEvidence.id,
            "subject_occurrence" -> explanationSubjectJson(certified.subject),
            "proof_kind" -> "unique_check_reply_defender_displacement_before_capture",
            "proof" ->
              uniqueCheckReplyDefenderDisplacementBeforeCaptureProofJson(proof)
          )
        case proof: RootOwnedEffectProof.SoleRecapturerRemovalBeforeTargetCapture =>
          Json.obj(
            "cause_evidence_id" -> certified.causeEvidence.id,
            "subject_occurrence" -> explanationSubjectJson(certified.subject),
            "proof_kind" -> "sole_recapturer_removal_before_target_capture",
            "proof" ->
              soleRecapturerRemovalBeforeTargetCaptureProofJson(proof)
          )
        case proof: RootOwnedEffectProof.VacatedGateEnablesUnrecapturableSliderCapture =>
          Json.obj(
            "cause_evidence_id" -> certified.causeEvidence.id,
            "subject_occurrence" -> explanationSubjectJson(certified.subject),
            "proof_kind" -> "vacated_gate_enables_unrecapturable_slider_capture",
            "proof" ->
              vacatedGateEnablesUnrecapturableSliderCaptureProofJson(proof)
          )
        case proof: RootOwnedEffectProof.SquareReleaseRoute =>
          Json.obj(
            "cause_evidence_id" -> certified.causeEvidence.id,
            "subject_occurrence" -> explanationSubjectJson(certified.subject),
            "proof_kind" -> "square_release_route",
            "proof" -> squareReleaseRouteProofJson(proof)
          )
        case proof: RootOwnedEffectProof.CaptureExclusionMoveOrder =>
          Json.obj(
            "cause_evidence_id" -> certified.causeEvidence.id,
            "subject_occurrence" -> explanationSubjectJson(certified.subject),
            "proof_kind" -> "capture_exclusion_move_order",
            "proof" -> captureExclusionMoveOrderProofJson(proof)
          )
        case proof: RootOwnedEffectProof.RelocationEnablesRecapture =>
          Json.obj(
            "cause_evidence_id" -> certified.causeEvidence.id,
            "subject_occurrence" -> explanationSubjectJson(certified.subject),
            "proof_kind" -> "relocation_enables_recapture",
            "proof" -> relocationEnablesRecaptureProofJson(proof)
          )
        case proof: RootOwnedEffectProof.PassedPawnProgressRealizedAfterOnlyLegalReply =>
          Json.obj(
            "cause_evidence_id" -> certified.causeEvidence.id,
            "subject_occurrence" -> explanationSubjectJson(certified.subject),
            "proof_kind" -> "passed_pawn_progress_realized_after_only_legal_reply",
            "proof" ->
              passedPawnProgressRealizedAfterOnlyLegalReplyProofJson(proof)
          )

    private def causalBranchJson(branch: BoundedCausalPublicBranch): JsObject =
      Json.obj(
        "branch_id" -> branch.branchId,
        "line_id" -> branch.lineId,
        "line_owner_evidence_id" -> branch.lineOwnerEvidenceId,
        "root_transition_evidence_id" -> branch.rootTransitionEvidenceId,
        "branch_role" -> causalRoleCode(branch.role),
        "root_provenance" -> causalEnumCode(branch.rootProvenance),
        "root_move" -> EvidenceRef.normalizeMove(branch.rootMove),
        "steps" -> branch.steps.map(step =>
          Json.obj(
            "step_index" -> step.index,
            "provenance" -> causalEnumCode(step.provenance),
            "ply" -> step.ply,
            "move_uci" -> EvidenceRef.normalizeMove(step.moveUci),
            "fen_before" -> step.fenBefore,
            "fen_after" -> step.fenAfter
          )
        )
      )

    private def causalProofPathJson(path: BoundedCausalPublicProofPath): JsObject =
      Json.obj(
        "path_occurrence_id" -> path.pathOccurrenceId,
        "premises" -> path.premises.map(causalPremiseJson),
        "closed_absence_uses" -> path.closedAbsenceUses.map(causalAbsenceUseJson)
      )

    private def causalProofPathWithStateJson(path: BoundedCausalPublicProofPath): JsObject =
      Json.obj(
        "path_occurrence_id" -> path.pathOccurrenceId,
        "premises" -> path.premises.map(causalPremiseJson),
        "closed_absence_uses" -> path.closedAbsenceUses.map(causalAbsenceUseJson),
        "closed_state_uses" -> path.closedStateUses.map(causalStateUseJson)
      )

    private def causalPremiseJson(premise: BoundedCausalPublicPremise): JsObject =
      premise match
        case exact: BoundedCausalPublicPremiseUse =>
          Json.obj(
            "role" -> causalRoleCode(exact.role),
            "contract" -> exact.contract,
            "result_id" -> exact.resultId,
            "issuer_evidence_id" -> exact.issuerEvidenceId,
            "issuer_occurrence_id" -> exact.issuerOccurrenceId,
            "source_premise_ids" -> exact.sourcePremiseIds,
            "branch_id" -> exact.branchId,
            "branch_role" -> causalRoleCode(exact.branchRole),
            "step_index" -> exact.stepIndex
          )
        case exact: BoundedCausalPublicLegalMoveUse =>
          causalLegalMovePremiseJson(exact)
        case exact: BoundedCausalPublicObjectContinuityUse =>
          causalObjectContinuityPremiseJson(exact)

    private def causalLegalMovePremiseJson(
        premise: BoundedCausalPublicLegalMoveUse
    ): JsObject =
      Json.obj(
        "role" -> causalRoleCode(premise.role),
        "contract" -> "legal_move",
        "move_uci" -> EvidenceRef.normalizeMove(premise.moveUci),
        "movement" -> Json.obj(
          "side" -> premise.side,
          "from" -> premise.from,
          "to" -> premise.to,
          "piece_before" -> premise.beforePiece,
          "piece_after" -> premise.afterPiece
        ),
        "movement_mode" -> causalEnumCode(premise.movementMode),
        "legal_move_semantic_id" -> premise.legalMoveSemanticId,
        "issuer_evidence_id" -> premise.issuerEvidenceId,
        "issuer_occurrence_id" -> premise.issuerOccurrenceId,
        "source_premise_ids" -> premise.sourcePremiseIds,
        "branch_id" -> premise.branchId,
        "branch_role" -> causalRoleCode(premise.branchRole),
        "step_index" -> premise.stepIndex
      ) ++ premise.capturedSquare.map(square =>
        Json.obj(
          "capture" -> Json.obj(
            "square" -> square,
            "piece" -> premise.capturedPiece.get,
            "side" -> premise.capturedSide.get
          )
        )
      ).getOrElse(Json.obj())

    private def causalObjectContinuityPremiseJson(
        premise: BoundedCausalPublicObjectContinuityUse
    ): JsObject =
      Json.obj(
        "role" -> causalRoleCode(premise.role),
        "contract" -> "object_continuity_step",
        "transition_kind" -> causalEnumCode(premise.transitionKind),
        "overall_move_uci" -> EvidenceRef.normalizeMove(premise.overallMoveUci),
        "before" -> Json.obj(
          "side" -> premise.side,
          "square" -> premise.beforeSquare,
          "piece" -> premise.beforePiece
        ),
        "after" -> Json.obj(
          "side" -> premise.side,
          "square" -> premise.afterSquare,
          "piece" -> premise.afterPiece
        ),
        "legal_move_semantic_id" -> premise.legalMoveSemanticId,
        "transition_footprint_id" -> premise.transitionFootprintId,
        "issuer_evidence_id" -> premise.issuerEvidenceId,
        "issuer_occurrence_id" -> premise.issuerOccurrenceId,
        "source_premise_ids" -> premise.sourcePremiseIds,
        "branch_id" -> premise.branchId,
        "branch_role" -> causalRoleCode(premise.branchRole),
        "step_index" -> premise.stepIndex
      ) ++ premise.selectedTransition.map(transition =>
        Json.obj("selected_transition" -> movementWitnessJson(transition))
      ).getOrElse(Json.obj())

    private def causalAbsenceUseJson(use: BoundedCausalPublicClosedAbsenceUse): JsObject =
      Json.obj(
        "use_id" -> use.useId,
        "role" -> causalRoleCode(use.role),
        "semantic_proof_id" -> use.semanticProofId,
        "issuer" -> "position_relation_extractor.closed_relation_inventory",
        "issuer_evidence_id" -> use.issuerEvidenceId,
        "issuer_occurrence_id" -> use.issuerOccurrenceId,
        "query" -> use.query,
        "branch_id" -> use.branchId,
        "branch_role" -> causalRoleCode(use.branchRole),
        "after_step_index" -> use.afterStepIndex,
        "position" -> Json.obj(
          "fen" -> use.position.fen,
          "ply" -> use.position.ply,
          "scope" -> evidenceScopeCode(use.scope)
        )
      )

    private def causalStateUseJson(use: BoundedCausalPublicClosedStateUse): JsObject =
      Json.obj(
        "use_id" -> use.useId,
        "role" -> causalRoleCode(use.role),
        "semantic_proof_id" -> use.semanticProofId,
        "issuer" -> "position_relation_extractor.closed_position_state_inventory",
        "issuer_evidence_id" -> use.issuerEvidenceId,
        "issuer_occurrence_id" -> use.issuerOccurrenceId,
        "query" -> use.query,
        "branch_id" -> use.branchId,
        "branch_role" -> causalRoleCode(use.branchRole),
        "after_step_index" -> use.afterStepIndex,
        "position" -> Json.obj(
          "fen" -> use.position.fen,
          "ply" -> use.position.ply,
          "scope" -> evidenceScopeCode(use.scope)
        )
      )

    private def uniqueCheckReplyDefenderDisplacementBeforeCaptureProofJson(
        proof: RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture
    ): JsObject =
      val source = proof.source
      val result = proof.result
      Json.obj(
        "source_evidence_id" -> source.id,
        "semantic_id" -> result.semanticId,
        "occurrence_id" -> result.occurrenceId,
        "dependency_fingerprint" -> result.dependencyId,
        "displacement_branch" -> causalBranchJson(result.publicDisplacementBranch),
        "immediate_capture_branch" -> causalBranchJson(result.publicImmediateCaptureBranch),
        "proof_paths" -> result.publicProofPaths.map(causalProofPathWithStateJson),
        "participants" -> Json.obj(
          "trigger" -> movementWitnessJson(result.triggerMovement),
          "forced_reply" -> legalResourceWitnessJson(result.uniqueCheckReplyDefenderDisplacementBeforeCapture),
          "realizer" -> movementWitnessJson(result.realizerMovement),
          "captured_target" -> coloredPieceWitnessJson(result.capturedTarget),
          "immediate_defense" -> legalResourceWitnessJson(result.immediateDefenseResource),
          "disabled_defender" -> coloredPieceWitnessJson(result.disabledDefender)
        ),
        "realizing_move" -> EvidenceRef.normalizeMove(result.realizingMove),
        "immediate_capture_branch_legal_defense_move" -> EvidenceRef.normalizeMove(result.immediateDefenseMove)
      )

    private def soleRecapturerRemovalBeforeTargetCaptureProofJson(
        proof: RootOwnedEffectProof.SoleRecapturerRemovalBeforeTargetCapture
    ): JsObject =
      val source = proof.source
      val result = proof.result
      Json.obj(
        "source_evidence_id" -> source.id,
        "semantic_id" -> result.semanticId,
        "occurrence_id" -> result.occurrenceId,
        "dependency_fingerprint" -> result.dependencyId,
        "removal_branch" -> causalBranchJson(result.publicRemovalBranch),
        "immediate_capture_branch" -> causalBranchJson(result.publicImmediateCaptureBranch),
        "proof_paths" -> result.publicProofPaths.map(causalProofPathWithStateJson),
        "participants" -> Json.obj(
          "remover" -> movementWitnessJson(result.remover),
          "removed_defender" -> coloredPieceWitnessJson(result.removedDefender),
          "removal_recapture" -> legalResourceWitnessJson(result.removalRecapture),
          "post_removal_target_capture" -> movementWitnessJson(result.laterExploit),
          "captured_target" -> coloredPieceWitnessJson(result.capturedTarget),
          "immediate_sole_recapture" -> legalResourceWitnessJson(result.immediateSoleRecapture)
        ),
        "post_removal_target_capture_move" -> EvidenceRef.normalizeMove(result.postRemovalTargetCaptureMove),
        "immediate_sole_recapture_move" -> EvidenceRef.normalizeMove(result.immediateSoleRecaptureMove)
      )

    private def vacatedGateEnablesUnrecapturableSliderCaptureProofJson(
        proof: RootOwnedEffectProof.VacatedGateEnablesUnrecapturableSliderCapture
    ): JsObject =
      val source = proof.source
      val result = proof.result
      Json.obj(
        "source_evidence_id" -> source.id,
        "semantic_id" -> result.semanticId,
        "occurrence_id" -> result.occurrenceId,
        "dependency_fingerprint" -> result.dependencyId,
        "vacated_gate_branch" -> causalBranchJson(result.publicVacatedGateBranch),
        "retained_gate_branch" -> causalBranchJson(result.publicRetainedGateBranch),
        "proof_paths" -> result.publicProofPaths.map(causalProofPathWithStateJson),
        "participants" -> Json.obj(
          "enabler" -> movementWitnessJson(result.enabler),
          "slider" -> coloredPieceWitnessJson(result.slider),
          "gate_blocker" -> coloredPieceWitnessJson(result.gateBlocker),
          "exploit" -> movementWitnessJson(result.exploit),
          "captured_target" -> coloredPieceWitnessJson(result.capturedTarget)
        ),
        "exploit_move" -> EvidenceRef.normalizeMove(result.exploitMove)
      )

    private def squareReleaseRouteProofJson(
        proof: RootOwnedEffectProof.SquareReleaseRoute
    ): JsObject =
      val source = proof.source
      val result = proof.result
      Json.obj(
        "source_evidence_id" -> source.id,
        "semantic_id" -> result.semanticId,
        "occurrence_id" -> result.occurrenceId,
        "dependency_fingerprint" -> result.dependencyId,
        "released_route_branch" -> causalBranchJson(result.publicReleasedRouteBranch),
        "retained_blocker_branch" -> causalBranchJson(result.publicRetainedBlockerBranch),
        "proof_paths" -> result.publicProofPaths.map(causalProofPathWithStateJson),
        "participants" -> Json.obj(
          "releaser" -> movementWitnessJson(result.releaser),
          "released_blocker" -> coloredPieceWitnessJson(result.releasedBlocker),
          "route_piece" -> coloredPieceWitnessJson(result.routePiece)
        ),
        "route" -> result.route.zip(result.routeMoves).zip(result.routeStepIndices).map {
          case ((movement, move), stepIndex) =>
            movementWitnessJson(movement) ++ Json.obj(
              "move_uci" -> EvidenceRef.normalizeMove(move),
              "step_index" -> stepIndex
            )
        },
        "terminal_step_index" -> result.terminalStepIndex,
        "terminal" -> squareReleaseRouteTerminalJson(result.publicTerminal)
      ) ++ result.terminalReplyMove.map(move =>
        Json.obj("terminal_reply_move" -> EvidenceRef.normalizeMove(move))
      ).getOrElse(Json.obj())

    private def squareReleaseRouteTerminalJson(
        terminal: SquareReleaseRoutePublicTerminal
    ): JsObject =
      terminal match
        case SquareReleaseRoutePublicTerminal.Occupation =>
          Json.obj("kind" -> "occupation")
        case SquareReleaseRoutePublicTerminal.Capture(
              assertionId,
              captured,
              geometricRecapturers,
              legalRecaptures,
              restrictedRecaptures
            ) =>
          Json.obj(
            "kind" -> "capture",
            "assertion_id" -> assertionId,
            "captured_target" -> coloredPieceWitnessJson(captured),
            "geometric_recapturers" -> geometricRecapturers.map(pieceWitnessJson),
            "legal_recaptures" -> legalRecaptures.map(routeLegalResourceWitnessJson),
            "restricted_recaptures" -> restrictedRecaptures.map(restriction =>
              Json.obj(
                "piece" -> pieceWitnessJson(restriction.piece),
                "destination" -> restriction.resource.destination.key.toLowerCase,
                "king_square" -> restriction.kingSquare.key.toLowerCase,
                "post_move_controllers" -> restriction.postMoveControllers.map(pieceWitnessJson)
              )
            )
          )
        case SquareReleaseRoutePublicTerminal.CreatedCheck(
              assertionId,
              checkedSide,
              kingSquare,
              checkers,
              responses,
              controlledKingDestinations,
              terminalState
            ) =>
          Json.obj(
            "kind" -> "created_check",
            "assertion_id" -> assertionId,
            "checked_side" -> colorCode(checkedSide),
            "king_square" -> kingSquare.key.toLowerCase,
            "checkers" -> checkers.map(pieceWitnessJson),
            "responses" -> responses.map(response =>
              Json.obj(
                "resource" -> routeLegalResourceWitnessJson(response.resource),
                "modes" -> response.modes.map(mode => causalEnumCode(mode.toString))
              )
            ),
            "controlled_king_destinations" -> controlledKingDestinations.map(destination =>
              Json.obj(
                "destination" -> destination.resource.destination.key.toLowerCase,
                "controllers" -> destination.controllers.map(pieceWitnessJson)
              )
            ),
            "terminal_state" -> causalEnumCode(terminalState.toString)
          )

    private def captureExclusionMoveOrderProofJson(
        proof: RootOwnedEffectProof.CaptureExclusionMoveOrder
    ): JsObject =
      val source = proof.source
      val result = proof.result
      Json.obj(
        "source_evidence_id" -> source.id,
        "semantic_id" -> result.semanticId,
        "occurrence_id" -> result.occurrenceId,
        "dependency_fingerprint" -> result.dependencyId,
        "vacating_branch" -> causalBranchJson(result.publicVacatingBranch),
        "immediate_capture_branch" -> causalBranchJson(result.publicImmediateBranch),
        "proof_paths" -> result.publicProofPaths.map(causalProofPathWithStateJson),
        "participants" -> Json.obj(
          "vacating_move" -> movementWitnessJson(result.vacatingMove),
          "deferred_move" -> movementWitnessJson(result.deferredMove),
          "capture_reply" -> movementWitnessJson(result.captureReply),
          "captured_target" -> coloredPieceWitnessJson(result.capturedTarget)
        ),
        "later_deferred_step_index" -> result.laterDeferredStepIndex
      )

    private def relocationEnablesRecaptureProofJson(
        proof: RootOwnedEffectProof.RelocationEnablesRecapture
    ): JsObject =
      val source = proof.source
      val result = proof.result
      Json.obj(
        "source_evidence_id" -> source.id,
        "semantic_id" -> result.semanticId,
        "occurrence_id" -> result.occurrenceId,
        "dependency_fingerprint" -> result.dependencyId,
        "relocated_responder_branch" -> causalBranchJson(result.publicRelocatedBranch),
        "retained_responder_branch" -> causalBranchJson(result.publicRetainedBranch),
        "proof_paths" -> result.publicProofPaths.map(causalProofPathWithStateJson),
        "participants" -> Json.obj(
          "attacker_at_common_root" -> coloredPieceWitnessJson(result.attackerAtCommonRoot),
          "attacker_at_capture" -> coloredPieceWitnessJson(result.attackerAtCapture),
          "target_at_common_root" -> coloredPieceWitnessJson(result.targetAtCommonRoot),
          "captured_target" -> coloredPieceWitnessJson(result.capturedTarget),
          "recapture_square" -> result.recaptureSquare.key.toLowerCase,
          "tracked_responder_at_seed" -> coloredPieceWitnessJson(result.trackedResponderAtSeed),
          "tracked_responder_at_staging" ->
            coloredPieceWitnessJson(result.trackedResponderAtStaging),
          "other_recapturer" -> coloredPieceWitnessJson(result.otherRecapturer)
        ),
        "relocation" -> Json.obj(
          "movement" -> movementWitnessJson(result.relocationMove),
          "move_uci" -> EvidenceRef.normalizeMove(result.relocationMoveUci),
          "step_index" -> result.relocationStepIndex
        ),
        "target_capture" -> Json.obj(
          "movement" -> movementWitnessJson(result.targetCaptureMove),
          "move_uci" -> EvidenceRef.normalizeMove(result.targetCaptureMoveUci),
          "relocated_step_index" -> result.relocatedCaptureStepIndex,
          "retained_step_index" -> result.retainedCaptureStepIndex
        ),
        "relocated_responder_recapture" -> Json.obj(
          "movement" -> movementWitnessJson(result.relocatedResponderRecapture),
          "move_uci" -> EvidenceRef.normalizeMove(result.relocatedResponderRecaptureMoveUci),
          "step_index" -> result.relocatedRecaptureStepIndex
        ),
        "retained_other_recapture" -> Json.obj(
          "movement" -> movementWitnessJson(result.retainedOtherRecapture),
          "move_uci" -> EvidenceRef.normalizeMove(result.retainedOtherRecaptureMoveUci),
          "step_index" -> result.retainedRecaptureStepIndex
        )
      )

    private def explanationSubjectJson(subject: ExplanationSubjectOccurrence): JsObject =
      Json.obj(
        "occurrence_id" -> subject.occurrenceId,
        "line_owner_evidence_id" -> subject.lineOwnerEvidenceId,
        "transition_evidence_id" -> subject.transitionEvidenceId,
        "move_uci" -> EvidenceRef.normalizeMove(subject.moveUci),
        "root_provenance" -> causalEnumCode(subject.rootProvenance.toString),
        "line_id" -> subject.line.id,
        "start" -> Json.obj(
          "fen" -> subject.start.fen,
          "ply" -> subject.start.ply
        ),
        "destination" -> Json.obj(
          "fen" -> subject.destination.fen,
          "ply" -> subject.destination.ply
        )
      )

    private def passedPawnProgressRealizedAfterOnlyLegalReplyProofJson(
        proof: RootOwnedEffectProof.PassedPawnProgressRealizedAfterOnlyLegalReply
    ): JsObject =
      val source = proof.source
      val result = proof.result
      val inventory = result.publicClosedReplyInventory
      val branches = result.publicBranches.map { branch =>
        Json.obj(
          "branch_id" -> branch.branchId,
          "role" -> branch.role,
          "line" -> passedPawnResultLineJson(branch.lineId, branch.lineRootMove),
          "line_owner_evidence_id" -> branch.lineOwnerEvidenceId,
          "root_transition_evidence_id" -> branch.rootTransitionEvidenceId,
          "root_provenance" -> branch.rootProvenance,
          "reply_move" -> EvidenceRef.normalizeMove(branch.replyMove),
          "source_occurrence_id" -> branch.sourceOccurrenceId,
          "steps" -> branch.steps.map { step =>
            Json.obj(
              "step_index" -> step.index,
              "step_key" -> step.stepKey,
              "ply" -> step.ply,
              "move_uci" -> EvidenceRef.normalizeMove(step.moveUci),
              "fen_before" -> step.fenBefore,
              "fen_after" -> step.fenAfter,
              "line" -> passedPawnResultLineJson(step.lineId, step.lineRootMove),
              "provenance" -> step.provenance
            )
          }
        )
      }
      val proofPaths = result.publicProofPaths.map { path =>
        Json.obj(
          "path_occurrence_id" -> path.pathOccurrenceId,
          "analysis_continuation_branch_id" -> path.analysisContinuationBranchId,
          "realization_actor" -> passedPawnResultActorJson(path.realizationActor),
          "realization_move" -> EvidenceRef.normalizeMove(path.realizationMove),
          "realization_ply" -> path.realizationPly,
          "premises" -> path.premises.map(premise =>
            Json.obj(
              "role" -> premise.role,
              "lower_kind" -> premise.lowerKind,
              "lower_semantic_key" -> premise.lowerSemanticKey,
              "source_premise_ids" -> premise.sourcePremiseIds,
              "branch_id" -> premise.branchId,
              "branch_role" -> premise.branchRole,
              "from_step_index" -> premise.fromStepIndex,
              "to_step_index" -> premise.toStepIndex
            ) ++ premise.dependencyProof
              .map(proof => Json.obj("dependency_proof" -> passedPawnResultDependencyProofJson(proof)))
              .getOrElse(Json.obj())
          ),
          "closure_use_ids" -> path.closureUseIds
        )
      }
      Json.obj(
        "source_evidence_id" -> source.id,
        "event_evidence_id" -> result.eventSource.id,
        "semantic_id" -> result.semanticId,
        "occurrence_id" -> result.occurrenceId,
        "dependency_fingerprint" -> result.dependencyFingerprint,
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
          "issuer_evidence_id" -> inventory.issuerEvidenceId,
          "root_after" -> Json.obj(
            "fen" -> inventory.rootAfterFen,
            "ply" -> inventory.rootAfterPly,
            "scope" -> inventory.scope
          ),
          "legal_reply_move" -> EvidenceRef.normalizeMove(inventory.legalReplyMove),
          "analysis_continuation_branch_id" -> inventory.analysisContinuationBranchId
        ),
        "branches" -> branches,
        "proof_paths" -> proofPaths,
        "lower_premise_ids" -> result.lowerPremiseIds
      )

    private def passedPawnResultLineJson(line: LineNodeRef): JsObject =
      passedPawnResultLineJson(line.id, line.rootMove)

    private def passedPawnResultLineJson(
        lineId: String,
        rootMove: String
    ): JsObject =
      Json.obj(
        "line_id" -> lineId,
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

    private def passedPawnResultDependencyProofJson(proof: PassedPawnProgressPublicDependencyProof): JsObject =
      Json.obj(
        "dependency_kind" -> proof.dependencyKind,
        "proof_kind" -> proof.proofKind,
        "squares" -> proof.squares.map(witness =>
          Json.obj(
            "role" -> witness.role,
            "square" -> witness.square
          )
        ),
        "pieces" -> proof.pieces.map(witness =>
          Json.obj(
            "role" -> witness.role,
            "side" -> witness.side,
            "piece" -> witness.piece
          )
        ),
        "relation_issuers" -> proof.relationIssuers.map(issuer =>
          Json.obj(
            "contract" -> issuer.contract,
            "result_key" -> issuer.resultKey,
            "occurrence_id" -> issuer.occurrenceId,
            "step_key" -> issuer.stepKey,
            "source_premise_ids" -> issuer.sourcePremiseIds,
            "issuer_evidence_id" -> issuer.issuerEvidenceId,
            "line" -> passedPawnResultLineJson(issuer.line),
            "scope" -> evidenceScopeCode(issuer.scope)
          )
        ),
        "position_state_issuers" -> proof.positionStateIssuers.map(issuer =>
          Json.obj(
            "state" -> passedPawnPositionStateJson(issuer.state),
            "semantic_proof_id" -> issuer.semanticProofId,
            "issuer_evidence_id" -> issuer.issuerEvidenceId,
            "issuer_occurrence_id" -> issuer.issuerOccurrenceId,
            "step_key" -> issuer.stepKey,
            "ply" -> issuer.ply,
            "move_uci" -> issuer.moveUci,
            "fen_before" -> issuer.fenBefore,
            "fen_after" -> issuer.fenAfter,
            "line" -> passedPawnResultLineJson(issuer.line),
            "scope" -> evidenceScopeCode(issuer.scope)
          )
        )
      )

    private def passedPawnPositionStateJson(
        state: PassedPawnProgressPublicPositionStateWitness
    ): JsObject =
      state match
        case exact: PassedPawnProgressPublicPositionStateWitness.OccupiedBy =>
          Json.obj(
            "kind" -> exact.kind,
            "side" -> exact.side,
            "square" -> exact.square,
            "piece" -> exact.piece
          )
        case exact: PassedPawnProgressPublicPositionStateWitness.SliderReach =>
          Json.obj(
            "kind" -> exact.kind,
            "side" -> exact.side,
            "square" -> exact.square,
            "piece" -> exact.piece,
            "file_step" -> exact.fileStep,
            "rank_step" -> exact.rankStep,
            "segment" -> exact.segment.map(item =>
              Json.obj(
                "square" -> item.square,
                "target" -> item.target,
                "occupant_piece" -> item.occupantPiece
              )
            )
          )
        case exact: PassedPawnProgressPublicPositionStateWitness.PawnTopology =>
          Json.obj(
            "kind" -> exact.kind,
            "side" -> exact.side,
            "square" -> exact.square,
            "passed" -> exact.passed
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

    private def routeLegalResourceWitnessJson(witness: RelationLegalMoveResourceWitness): JsObject =
      legalResourceWitnessJson(witness) ++ witness.capture.map(capture =>
        Json.obj(
          "capture" -> Json.obj(
            "side" -> colorCode(capture.capturedSide),
            "piece" -> capture.capturedRole.name.toLowerCase,
            "square" -> capture.capturedSquare.key.toLowerCase
          )
        )
      ).getOrElse(Json.obj())

    private def pieceWitnessJson(witness: RelationPieceWitness): JsObject =
      Json.obj(
        "piece" -> witness.role.name.toLowerCase,
        "square" -> witness.square.key.toLowerCase
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
        case EvidenceScope.LegalLine              => "legal_line"
        case EvidenceScope.BestLine               => "best_line"
        case EvidenceScope.PlayedLine             => "played_line"
        case EvidenceScope.CandidateLine          => "candidate_line"
        case EvidenceScope.Counterfactual         => "counterfactual"

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
    private val MaxPly = 1000
    private val RequestId = "[A-Za-z0-9._:-]{1,128}".r

    def validRequestId(value: String): Boolean = RequestId.matches(value)

    def accepts(raw: RawMoveReviewInput): Boolean =
      raw.fen.nonEmpty && raw.fen.length <= MaxFenLength &&
        validUci(raw.playedMoveUci) &&
        raw.variations.nonEmpty && raw.variations.size <= MaxVariations &&
        raw.movePrefixUci.size <= MaxMovePrefix && raw.movePrefixUci.forall(validUci) &&
        raw.ply.forall(ply => ply >= 0 && ply <= MaxPly) &&
        raw.variations.forall(validLine)

    private def validLine(line: EngineLine): Boolean =
      EngineLineAdmission.acceptsRuntimeInputLine(line)

    private def validUci(value: String): Boolean = EngineLineAdmission.acceptsRuntimeInputUci(value)
