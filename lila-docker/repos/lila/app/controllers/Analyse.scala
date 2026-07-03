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
            val status = publicReviewStatus(validation.isValid)
            val renderable = status == "ready"
            JsonOk(
              Json.obj(
                "ok" -> true,
                "status" -> status,
                "availability" -> publicAvailabilityJson(validation.isValid),
                "move_review" -> packet.moveJudgmentView.map(moveJudgmentViewMeaningJson(_, renderable))
              )
            ).toFuccess
      )

  private def moveJudgmentViewMeaningJson(view: MoveJudgmentView, renderable: Boolean): JsObject =
    Json.obj("renderable" -> renderable) ++
      (if renderable then
         val verdict = view.verdict.map(frame => MoveMeaningSurface.verdict(frame))
         val surfaces = MoveMeaningSurface.from(view)
         Json.obj(
           "verdict" -> verdict.map(publicVerdictJson),
           "move_semantics" -> surfaces.map(publicMoveSemanticJson),
           "llm_payload" -> verdict.toList.flatMap(publicLlmPayload(_, surfaces))
         )
       else Json.obj())

  private def publicReviewStatus(valid: Boolean): String =
    if valid then "ready" else "withheld"

  private def publicAvailabilityJson(valid: Boolean): JsObject =
    Json.obj(
      "state" -> publicReviewStatus(valid),
      "reason" ->
        (if !valid then Some("validation_failed")
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
      "line_role" -> surface.lineRole,
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
      "terminal_consequences" -> surface.terminalConsequences.map(publicCodeJson),
      "endgame_technique" -> surface.endgameTechnique.map(publicEndgameTechniqueJson),
      "technique" -> surface.endgameTechnique.map(publicEndgameTechniqueJson),
      "comparison" -> surface.comparison.map(publicComparisonJson),
      "evidence" -> publicEvidenceJson(surface)
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

  private def publicEvidenceJson(surface: MoveMeaningSurface): JsObject =
    val evidence = surface.evidence
    Json.obj(
      "has_carrier" -> evidence.hasCarrier,
      "proof_level" -> evidence.proofLevel,
      "target_bound" -> evidence.targetBound,
      "cause_ids" -> evidence.causeIds,
      "source_ids" -> evidence.sourceIds,
      "board_carriers" -> evidence.boardCarriers.map(publicBoardCarrierJson(_, surface))
    )

  private def publicBoardCarrierJson(carrier: MoveMeaningSurfaceBoardCarrier, surface: MoveMeaningSurface): JsObject =
    Json.obj(
      "subject" -> surface.subject,
      "line_role" -> surface.lineRole,
      "move_uci" -> surface.moveUci,
      "role" -> carrier.role,
      "kind" -> carrier.kind,
      "value" -> carrier.value,
      "from" -> carrier.from,
      "to" -> carrier.to
    )

  private def publicLlmPayload(verdict: MoveMeaningSurfaceVerdict, surfaces: List[MoveMeaningSurface]): List[JsObject] =
    val problemMove = verdict.moveQuality == "bad" || verdict.verdictCode == "playable_loss"
    val subject = if problemMove then "reference_move" else "played_move"
    val subjectMove = if problemMove then verdict.referenceMove else verdict.playedMove
    val pvRolePrefix = if problemMove then "reference_pv_" else "played_pv_"
    val evidenceSurfaces = surfaces.filter(surface => surface.subject == subject && publicLlmSurfaceHasCarrier(surface))
    val pv = surfaces
      .filter(_.subject == subject)
      .flatMap(_.comparison.toList.flatMap(_.moves))
      .filter(move => move.uci.nonEmpty && move.role.startsWith(pvRolePrefix))
      .map(_.uci)
      .distinct
    val terminal = evidenceSurfaces.flatMap(_.terminalConsequences).distinct
    val technique = evidenceSurfaces.flatMap(_.endgameTechnique).distinct
    val consequenceCarriers = publicLlmCarrierPairs(evidenceSurfaces)
      .filter((carrier, _) => carrier.role == "target" && publicLlmConsequenceCarrierKind(carrier.kind))
    val concreteConsequence = consequenceCarriers.exists((carrier, _) => carrier.kind == "PlanSubject" || carrier.kind == "Pawn")
    if evidenceSurfaces.isEmpty || (terminal.isEmpty && technique.isEmpty && (pv.isEmpty || !concreteConsequence)) then Nil
    else
      val carriers = publicLlmCarrierPairs(evidenceSurfaces).filter((carrier, _) =>
        (carrier.role == "actor" && carrier.kind == "Move" && carrier.value == subjectMove) ||
          (carrier.role == "target" && publicLlmConsequenceCarrierKind(carrier.kind))
      )
      List(
        Json.obj(
          "key" -> "current-move-chain",
          "current_move" -> verdict.playedMove,
          "reference_move" -> verdict.referenceMove,
          "move_quality" -> verdict.moveQuality,
          "subject" -> subject,
          "proof_levels" -> evidenceSurfaces.map(_.evidence.proofLevel).distinct,
          "carriers" -> carriers.map((carrier, surface) => publicBoardCarrierJson(carrier, surface)),
          "pv" -> pv,
          "consequence_carriers" -> consequenceCarriers.map((carrier, surface) => publicBoardCarrierJson(carrier, surface)),
          "terminal_consequences" -> terminal.map(publicCodeJson),
          "technique" -> technique.map(publicEndgameTechniqueJson),
          "cause_ids" -> evidenceSurfaces.flatMap(_.evidence.causeIds).distinct,
          "source_ids" -> evidenceSurfaces.flatMap(_.evidence.sourceIds).distinct,
          "player_facing_reason_allowed" -> true
        )
      )

  private def publicLlmSurfaceHasCarrier(surface: MoveMeaningSurface): Boolean =
    surface.evidence.hasCarrier &&
      (surface.evidence.boardCarriers.nonEmpty || surface.terminalConsequences.nonEmpty || surface.endgameTechnique.nonEmpty)

  private def publicLlmConsequenceCarrierKind(kind: String): Boolean =
    kind == "PlanSubject" || kind == "Pawn" || kind == "Square" || kind == "File"

  private def publicLlmCarrierPairs(
      surfaces: List[MoveMeaningSurface]
  ): List[(MoveMeaningSurfaceBoardCarrier, MoveMeaningSurface)] =
    surfaces
      .flatMap(surface => surface.evidence.boardCarriers.map(carrier => carrier -> surface))
      .distinctBy((carrier, surface) => (surface.subject, surface.lineRole, surface.moveUci, carrier.role, carrier.kind, carrier.value, carrier.from, carrier.to))

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
