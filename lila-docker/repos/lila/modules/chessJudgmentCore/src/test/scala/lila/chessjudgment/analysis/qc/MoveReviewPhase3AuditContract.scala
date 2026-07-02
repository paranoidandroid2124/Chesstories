package lila.chessjudgment.analysis.qc

import lila.chessjudgment.analysis.assembly.RawMoveReviewInput
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

private[qc] object MoveReviewPhase3AuditContract:
  final case class AuditInputSample(
      sampleId: String,
      raw: RawMoveReviewInput,
      opening: Option[String],
      sliceKind: Option[String],
      targetPly: Option[Int],
      playedSan: Option[String],
      expectedSemanticSlots: List[ExpectedSemanticSlot] = Nil,
      expectedQuestionIds: List[String] = Nil
  )

  final case class ExpectedSemanticSlot(
      id: String,
      unit: PositionPlanTechniqueUnit,
      axisKey: Option[String] = None,
      questionId: Option[String] = None,
      description: Option[String] = None,
      requiredTerminalStage: Option[String] = None,
      requiredMechanismKinds: List[StrategicMechanismKind] = Nil,
      requiredCauseKinds: List[RelativeCauseKind] = Nil,
      requiredPrimaryRootCauseKinds: List[RelativeCauseKind] = Nil,
      requiredPrimaryRootArbitrationTiers: List[MoveJudgmentCauseRootArbitrationTier] = Nil,
      requiredTerminalConsequenceKinds: List[LineConsequenceKind] = Nil,
      requiredSemanticDetailTokens: List[String] = Nil,
      requiredCoLocatedSemanticDetailTokens: List[String] = Nil,
      requiredSemanticAnchorTokens: List[String] = Nil,
      requiredObjectBindingTokens: List[String] = Nil
  )

  def parseExpectedQuestionIds(json: JsValue): List[String] =
    (json \ "expectedQuestionIds").asOpt[List[String]].getOrElse(Nil).map(_.trim).filter(_.nonEmpty).distinct.sorted

  def parseExpectedSemanticSlots(json: JsValue): List[ExpectedSemanticSlot] =
    (json \ "expectedSemanticSlots").asOpt[List[JsValue]].getOrElse(Nil).flatMap(parseExpectedSemanticSlot)

  def expectedSemanticSlotsJson(slots: List[ExpectedSemanticSlot]): JsArray =
    JsArray(slots.map(expectedSemanticSlotJson))

  private def parseExpectedSemanticSlot(json: JsValue): Option[ExpectedSemanticSlot] =
    for
      id <- (json \ "id").asOpt[String]
      unit <- (json \ "unit").asOpt[String].flatMap(expectedSemanticUnit)
    yield
      ExpectedSemanticSlot(
        id = id,
        unit = unit,
        axisKey = (json \ "axisKey").asOpt[String],
        questionId = (json \ "questionId").asOpt[String],
        description = (json \ "description").asOpt[String],
        requiredTerminalStage = (json \ "requiredTerminalStage").asOpt[String],
        requiredMechanismKinds =
          (json \ "requiredMechanismKinds")
            .asOpt[List[String]]
            .getOrElse(Nil)
            .flatMap(raw => StrategicMechanismKind.values.find(_.toString == raw.trim)),
        requiredCauseKinds =
          (json \ "requiredCauseKinds")
            .asOpt[List[String]]
            .getOrElse(Nil)
            .flatMap(raw => RelativeCauseKind.values.find(_.toString == raw.trim)),
        requiredPrimaryRootCauseKinds =
          (json \ "requiredPrimaryRootCauseKinds")
            .asOpt[List[String]]
            .getOrElse(Nil)
            .flatMap(raw => RelativeCauseKind.values.find(_.toString == raw.trim)),
        requiredPrimaryRootArbitrationTiers =
          (json \ "requiredPrimaryRootArbitrationTiers")
            .asOpt[List[String]]
            .getOrElse(Nil)
            .flatMap(raw => MoveJudgmentCauseRootArbitrationTier.values.find(_.toString == raw.trim)),
        requiredTerminalConsequenceKinds =
          (json \ "requiredTerminalConsequenceKinds")
            .asOpt[List[String]]
            .getOrElse(Nil)
            .flatMap(raw => LineConsequenceKind.values.find(_.toString == raw.trim)),
        requiredSemanticDetailTokens = (json \ "requiredSemanticDetailTokens").asOpt[List[String]].getOrElse(Nil),
        requiredCoLocatedSemanticDetailTokens =
          (json \ "requiredCoLocatedSemanticDetailTokens").asOpt[List[String]].getOrElse(Nil),
        requiredSemanticAnchorTokens = (json \ "requiredSemanticAnchorTokens").asOpt[List[String]].getOrElse(Nil),
        requiredObjectBindingTokens = (json \ "requiredObjectBindingTokens").asOpt[List[String]].getOrElse(Nil)
      )

  private def expectedSemanticUnit(raw: String): Option[PositionPlanTechniqueUnit] =
    PositionPlanTechniqueUnit.values.find(_.toString == raw.trim)

  private def expectedSemanticSlotJson(slot: ExpectedSemanticSlot): JsObject =
    Json.obj(
      "id" -> slot.id,
      "unit" -> slot.unit.toString,
      "axisKey" -> slot.axisKey,
      "questionId" -> slot.questionId,
      "description" -> slot.description,
      "requiredTerminalStage" -> slot.requiredTerminalStage,
      "requiredMechanismKinds" -> slot.requiredMechanismKinds.map(_.toString),
      "requiredCauseKinds" -> slot.requiredCauseKinds.map(_.toString),
      "requiredPrimaryRootCauseKinds" -> slot.requiredPrimaryRootCauseKinds.map(_.toString),
      "requiredPrimaryRootArbitrationTiers" -> slot.requiredPrimaryRootArbitrationTiers.map(_.toString),
      "requiredTerminalConsequenceKinds" -> slot.requiredTerminalConsequenceKinds.map(_.toString),
      "requiredSemanticDetailTokens" -> slot.requiredSemanticDetailTokens,
      "requiredCoLocatedSemanticDetailTokens" -> slot.requiredCoLocatedSemanticDetailTokens,
      "requiredSemanticAnchorTokens" -> slot.requiredSemanticAnchorTokens,
      "requiredObjectBindingTokens" -> slot.requiredObjectBindingTokens
    )
