package io.chesstory.evaluation.runtimeadapter

import lila.chessjudgment.model.judgment.{
  EvidenceBackedJudgmentPacket,
  RelationFactEvidence,
  TypedEvidenceGraph
}

/** Explicit public-state views for native runtime classes that are immutable
  * values but do not implement Product. No derived judgment is computed here.
  */
private[runtimeadapter] object RuntimeNativeViews:
  val adapters: List[NativeTreeEncoder.StructuralAdapter] =
    List(
      TypedEvidenceGraphAdapter,
      RelationFactEvidenceAdapter,
      EvidenceBackedJudgmentPacketAdapter
    )

  private object TypedEvidenceGraphAdapter extends NativeTreeEncoder.StructuralAdapter:
    def adapt(value: Any): Option[NativeTreeEncoder.StructuralProductView] =
      value match
        case graph: TypedEvidenceGraph =>
          Some(
            NativeTreeEncoder.StructuralProductView(
              scalaType = "lila.chessjudgment.model.judgment.TypedEvidenceGraph",
              constructor = "TypedEvidenceGraph",
              fields = List("records" -> graph.records)
            )
          )
        case _ => None

  private object RelationFactEvidenceAdapter extends NativeTreeEncoder.StructuralAdapter:
    def adapt(value: Any): Option[NativeTreeEncoder.StructuralProductView] =
      value match
        case relation: RelationFactEvidence =>
          Some(
            NativeTreeEncoder.StructuralProductView(
              scalaType = "lila.chessjudgment.model.judgment.RelationFactEvidence",
              constructor = "RelationFactEvidence",
              fields = List(
                "detail" -> relation.detail,
                "lineMoves" -> relation.lineMoves
              )
            )
          )
        case _ => None

  private object EvidenceBackedJudgmentPacketAdapter extends NativeTreeEncoder.StructuralAdapter:
    def adapt(value: Any): Option[NativeTreeEncoder.StructuralProductView] =
      value match
        case packet: EvidenceBackedJudgmentPacket =>
          Some(
            NativeTreeEncoder.StructuralProductView(
              scalaType = "lila.chessjudgment.model.judgment.EvidenceBackedJudgmentPacket",
              constructor = "EvidenceBackedJudgmentPacket",
              fields = List(
                "assembly" -> packet.assembly,
                "probeRequests" -> packet.probeRequests,
                "playerFacingClaimDecisions" -> packet.playerFacingClaimDecisions,
                "onlyMoveConstraintResolutions" -> packet.onlyMoveConstraintResolutions,
                "causeExposureResolution" -> packet.causeExposureResolution,
                "causeDispositionLedger" -> packet.causeDispositionLedger,
                "selectedContentClaimIds" -> packet.selectedContentClaimIds
              )
            )
          )
        case _ => None
