package controllers

import play.api.libs.json.{ JsArray, JsObject, JsValue, Json }

import lila.app.*
import lila.chessjudgment.analysis.assembly.{ JudgmentPacketValidator, MoveReviewJudgmentOrchestrator, RawMoveReviewInput }
import lila.chessjudgment.model.judgment.*

final class Analyse(
    env: Env
) extends LilaController(env):

  def externalEngineList = Auth { _ ?=> me ?=>
    env.analyse.externalEngine.list(me).map { list =>
      JsonOk(JsArray(list.map(lila.analyse.ExternalEngine.jsonWrites.writes)))
    }
  }

  def externalEngineShow(id: String) = Auth { _ ?=> me ?=>
    Found(env.analyse.externalEngine.find(me, id)): engine =>
      JsonOk(lila.analyse.ExternalEngine.jsonWrites.writes(engine))
  }

  def externalEngineCreate = AuthBody { _ ?=> me ?=>
    bindForm(lila.analyse.ExternalEngine.form)(
      jsonFormError,
      data =>
        env.analyse.externalEngine.create(me, data).map { engine =>
          Created(lila.analyse.ExternalEngine.jsonWrites.writes(engine))
        }
    )
  }

  def externalEngineUpdate(id: String) = AuthBody { _ ?=> me ?=>
    Found(env.analyse.externalEngine.find(me, id)): engine =>
      bindForm(lila.analyse.ExternalEngine.form)(
        jsonFormError,
        data =>
          env.analyse.externalEngine.update(engine, data).map { engine =>
            JsonOk(lila.analyse.ExternalEngine.jsonWrites.writes(engine))
          }
      )
  }

  def externalEngineDelete(id: String) = Auth { _ ?=> me ?=>
    env.analyse.externalEngine.delete(me, id).elseNotFound(jsonOkResult)
  }

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
            val renderable = validation.isValid && moveReview.exists(review =>
              (review \ "renderable").asOpt[Boolean].contains(true)
            )
            val status = publicReviewStatus(renderable)
            JsonOk(
              Json.obj(
                "ok" -> true,
                "status" -> status,
                "availability" -> publicAvailabilityJson(validation.isValid, renderable),
                "move_review" -> moveReview
              )
            ).toFuccess
      )

  private def moveJudgmentViewMeaningJson(view: MoveJudgmentView): JsObject =
    val payload = MoveMeaningSurface.publicPayloadJson(view)
    val hasApprovedChain = (payload \ "idea_chains").asOpt[JsArray].exists(
      _.value.exists(chain => (chain \ "player_facing_reason_allowed").asOpt[Boolean].contains(true))
    )
    Json.obj("renderable" -> hasApprovedChain) ++ payload

  private def publicReviewStatus(available: Boolean): String =
    if available then "ready" else "withheld"

  private def publicAvailabilityJson(valid: Boolean, renderable: Boolean): JsObject =
    Json.obj(
      "state" -> publicReviewStatus(renderable),
      "reason" ->
        (if !valid then Some("validation_failed")
         else if !renderable then Some("no_public_chain")
         else None)
    )
