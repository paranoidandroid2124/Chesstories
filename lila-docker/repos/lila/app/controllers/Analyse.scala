package controllers

import play.api.libs.json.{ JsArray, JsObject, JsValue, Json }

import lila.app.*
import lila.chessjudgment.analysis.assembly.{ MoveReviewJudgmentOrchestrator, RawMoveReviewInput }
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
          MoveReviewJudgmentOrchestrator.build(raw).fold(
            BadRequest(Json.obj("ok" -> false, "error" -> "move_review_not_buildable")).toFuccess
          ): result =>
            val status = publicReviewStatus(result.validation.isValid, result.quality.audit.isClean)
            val renderable = status == "ready"
            JsonOk(
              Json.obj(
                "ok" -> true,
                "status" -> status,
                "availability" -> publicAvailabilityJson(result.validation.isValid, result.quality.audit.isClean),
                "move_review" -> result.packet.moveJudgmentView.map(moveJudgmentViewMeaningJson(_, renderable))
              )
            ).toFuccess
      )

  private def moveJudgmentViewMeaningJson(view: MoveJudgmentView, renderable: Boolean): JsObject =
    Json.obj("renderable" -> renderable) ++
      (if renderable then
         Json.obj(
           "verdict" -> view.verdict.map(frame => publicVerdictJson(MoveMeaningSurface.verdict(frame))),
           "move_semantics" -> MoveMeaningSurface.from(view).map(publicMoveSemanticJson)
         )
       else Json.obj())

  private def publicReviewStatus(valid: Boolean, qualityClean: Boolean): String =
    if valid && qualityClean then "ready" else "withheld"

  private def publicAvailabilityJson(valid: Boolean, qualityClean: Boolean): JsObject =
    Json.obj(
      "state" -> publicReviewStatus(valid, qualityClean),
      "reason" ->
        (if !valid then Some("validation_failed")
         else if !qualityClean then Some("quality_gate")
         else None)
    )

  private def publicVerdictJson(verdict: MoveMeaningSurfaceVerdict): JsObject =
    Json.obj(
      "verdict_code" -> verdict.verdictCode,
      "move_quality" -> verdict.moveQuality,
      "played_move" -> verdict.playedMove,
      "reference_move" -> verdict.referenceMove
    )

  private def publicMoveSemanticJson(surface: MoveMeaningSurface): JsObject =
    Json.obj(
      "move_uci" -> surface.moveUci,
      "subject" -> surface.subject,
      "move_quality" -> surface.moveQuality,
      "idea_type" -> surface.ideaType,
      "idea" -> publicCodeJson(surface.idea),
      "idea_quality" -> surface.ideaQuality,
      "assessment" -> publicAssessmentJson(surface.assessment),
      "failure_family" -> surface.failureFamily,
      "problem" -> surface.problem,
      "target" -> publicTargetJson(surface.target),
      "targets" -> publicTargetJson(surface.target),
      "priority" -> surface.priority,
      "comparison_loss" -> surface.comparisonLostIdeas.map(publicComparisonLossJson),
      "endgame_technique" -> surface.endgameTechnique.map(publicEndgameTechniqueJson),
      "technique" -> surface.endgameTechnique.map(publicEndgameTechniqueJson),
      "comparison" -> surface.comparison.map(publicComparisonJson)
    )

  private def publicAssessmentJson(assessment: MoveMeaningSurfaceAssessment): JsObject =
    Json.obj(
      "move_quality" -> publicCodeJson(assessment.moveQuality),
      "idea_quality" -> publicCodeJson(assessment.ideaQuality),
      "priority" -> publicCodeJson(assessment.priority),
      "is_verdict_reason" -> assessment.verdictReason,
      "is_local_idea" -> assessment.localIdea,
      "failure_family" -> assessment.failureFamily.map(publicCodeJson),
      "problem" -> assessment.problem.map(publicCodeJson)
    )

  private def publicCodeJson(code: MoveMeaningSurfaceCode): JsObject =
    Json.obj(
      "code" -> code.code,
      "label" -> code.label
    )

  private def publicEndgameTechniqueJson(technique: MoveMeaningSurfaceEndgameTechnique): JsObject =
    Json.obj(
      "pattern" -> technique.pattern,
      "pattern_label" -> technique.patternLabel,
      "pattern_info" -> technique.pattern.zip(technique.patternLabel).headOption.map { case (code, label) =>
        publicCodeJson(MoveMeaningSurfaceCode(code, label))
      },
      "rook_pattern" -> technique.rookPattern,
      "rook_pattern_label" -> technique.rookPatternLabel,
      "rook_geometry" -> technique.rookPattern.zip(technique.rookPatternLabel).headOption.map { case (code, label) =>
        publicCodeJson(MoveMeaningSurfaceCode(code, label))
      },
      "side" -> technique.side,
      "status" -> technique.horizonStatus,
      "status_label" -> technique.statusLabel,
      "trigger_move" -> technique.triggerMove,
      "status_move" -> technique.triggerMove.map(move => Json.obj("uci" -> move)),
      "entry_ply_offset" -> technique.entryPlyOffset,
      "terminal_ply_offset" -> technique.terminalPlyOffset,
      "required_squares" -> technique.requiredSquares,
      "maintained_squares" -> technique.maintainedSquares,
      "broken_squares" -> technique.brokenSquares,
      "squares" -> Json.obj(
        "required" -> technique.requiredSquares,
        "maintained" -> technique.maintainedSquares,
        "broken" -> technique.brokenSquares
      ),
      "terminal_consequences" -> technique.terminalConsequences.map(publicCodeJson),
      "failure_reason" -> technique.failureReason.map(publicCodeJson)
    )

  private def publicComparisonJson(comparison: MoveMeaningSurfaceComparison): JsObject =
    Json.obj(
      "kind" -> comparison.kind,
      "relation" -> comparison.relation,
      "reference_move" -> comparison.referenceMove,
      "candidate_move" -> comparison.candidateMove,
      "second_move" -> comparison.secondMove,
      "moves" -> comparison.moves.map(publicMoveRefJson),
      "lost_ideas" -> comparison.lostIdeas.map(publicComparisonLossJson)
    )

  private def publicMoveRefJson(move: MoveMeaningSurfaceMoveRef): JsObject =
    Json.obj(
      "role" -> move.role,
      "uci" -> move.uci
    )

  private def publicComparisonLossJson(loss: MoveMeaningSurfaceComparisonLoss): JsObject =
    Json.obj(
      "side" -> loss.side,
      "code" -> loss.code,
      "label" -> loss.label
    )

  private def publicTargetJson(target: MoveMeaningSurfaceTarget): JsObject =
    Json.obj(
      "squares" -> target.squares,
      "files" -> target.files,
      "pieces" -> target.pieces
    )
