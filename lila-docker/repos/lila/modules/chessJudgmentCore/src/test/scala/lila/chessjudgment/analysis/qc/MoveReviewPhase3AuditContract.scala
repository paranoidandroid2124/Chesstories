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
      requiredSupportLevel: Option[String] = None
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
        requiredSupportLevel = (json \ "requiredSupportLevel").asOpt[String]
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
      "requiredSupportLevel" -> slot.requiredSupportLevel
    )
