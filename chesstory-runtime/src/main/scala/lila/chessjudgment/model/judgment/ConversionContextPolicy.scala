package lila.chessjudgment.model.judgment

/** Shared evidence classification for conversion claim proposal and admission. */
object ConversionContextPolicy:

  def supports(records: List[EvidenceRecord]): Boolean =
    supports(records, allowPassedPawnConcession = false)

  def supports(
      records: List[EvidenceRecord],
      causeKind: RelativeCauseKind
  ): Boolean =
    supports(records, allowPassedPawnConcession = causeKind == RelativeCauseKind.ConversionMiss)

  private def supports(
      records: List[EvidenceRecord],
      allowPassedPawnConcession: Boolean
  ): Boolean =
    records.exists(record => supportsNonRelation(record, allowPassedPawnConcession))

  private def supportsNonRelation(
      record: EvidenceRecord,
      allowPassedPawnConcession: Boolean
  ): Boolean =
    record match
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasConversionConsequence
      case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
        structuralContext(payload, allowPassedPawnConcession)
      case _ =>
        false

  private def structuralContext(
      payload: StructuralDeltaEvidence,
      allowPassedPawnConcession: Boolean
  ): Boolean =
    import TransitionConsequenceKind.*
    payload.hasConsequence(PassedPawnProgress) ||
      (allowPassedPawnConcession &&
        payload.hasConsequence(PassedPawnConcession))
