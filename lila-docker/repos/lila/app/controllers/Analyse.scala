package controllers

import play.api.libs.json.{ JsArray, JsObject, JsValue, Json }

import lila.app.*
import lila.chessjudgment.analysis.assembly.{ JudgmentPacketValidator, MoveReviewJudgmentOrchestrator, RawMoveReviewInput }
import lila.chessjudgment.model.judgment.*

final class Analyse(
    env: Env
) extends LilaController(env):

  def moveMeaning = OpenBodyOf(parse.json): (bodyCtx: BodyContext[JsValue]) ?=>
    limit.moveMeaning(bodyCtx.ip, rateLimitedJson.toFuccess):
      bodyCtx.body.body.validate[RawMoveReviewInput].fold(
        _ => BadRequest(Json.obj("ok" -> false, "error" -> "invalid_move_review_input")).toFuccess,
        raw =>
          MoveReviewJudgmentOrchestrator.packet(raw).fold(
            BadRequest(Json.obj("ok" -> false, "error" -> "move_review_not_buildable")).toFuccess
          ): packet =>
            val validation = JudgmentPacketValidator.validate(packet)
            val moveReview = packet.moveJudgmentView.map(moveJudgmentViewMeaningJson)
            val probeRequests = if validation.isValid then packet.probeRequests else Nil
            val hasPlayerFacingReason = validation.isValid && moveReview.exists(review =>
              (review \ "renderable").asOpt[Boolean].contains(true)
            )
            val status = publicReviewStatus(hasPlayerFacingReason)
            JsonOk(
              Json.obj(
                "ok" -> true,
                "status" -> status,
                "availability" -> publicAvailabilityJson(validation.isValid, hasPlayerFacingReason),
                "probe_requests" -> Json.toJson(probeRequests),
                "move_review" -> moveReview
              )
            ).toFuccess
      )

  private def moveJudgmentViewMeaningJson(view: MoveJudgmentView): JsObject =
    val payload = MoveMeaningSurface.publicPayloadJson(view)
    val hasPlayerFacingReason =
      (payload \ "idea_chains").asOpt[JsArray].exists(_.value.exists(chain => (chain \ "player_facing_reason_allowed").asOpt[Boolean].contains(true)))
    Json.obj("renderable" -> hasPlayerFacingReason) ++ payload

  private def publicReviewStatus(available: Boolean): String =
    if available then "ready" else "withheld"

  private def publicAvailabilityJson(valid: Boolean, hasPlayerFacingReason: Boolean): JsObject =
    Json.obj(
      "state" -> publicReviewStatus(hasPlayerFacingReason),
      "reason" ->
        (if !valid then Some("validation_failed")
         else if !hasPlayerFacingReason then Some("no_player_facing_reason")
         else None)
    )
