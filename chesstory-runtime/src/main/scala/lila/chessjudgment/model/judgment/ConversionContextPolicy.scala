package lila.chessjudgment.model.judgment

/** Shared evidence classification for conversion claim proposal and admission. */
object ConversionContextPolicy:

  def supports(records: List[EvidenceRecord]): Boolean =
    records.exists(record => supportsRecord(record, allowPassedPawnConcession = false))

  def supports(
      records: List[EvidenceRecord],
      causeKind: RelativeCauseKind
  ): Boolean =
    records.exists(record =>
      supportsRecord(
        record,
        allowPassedPawnConcession = causeKind == RelativeCauseKind.ConversionMiss
      )
    )

  private def supportsRecord(
      record: EvidenceRecord,
      allowPassedPawnConcession: Boolean
  ): Boolean =
    record match
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasConversionConsequence
      case EvidenceRecord(_, payload: BoardFactEvidence, _) =>
        payload.positionFeatures.exists(_.materialPhase.phase == "endgame")
      case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
        structuralContext(payload, allowPassedPawnConcession)
      case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
        payload.kind == RelationFactKind.BadPieceLiquidation && payload.hasConcreteRelationProof
      case _ =>
        false

  private def structuralContext(
      payload: StructuralDeltaEvidence,
      allowPassedPawnConcession: Boolean
  ): Boolean =
    import TransitionConsequenceKind.*
    payload.hasAnyConsequence(Set(PassedPawnProgress, PromotionPressureGain)) ||
      (allowPassedPawnConcession &&
        payload.hasAnyConsequence(Set(PassedPawnConcession, PromotionPressureConcession)))
