package lila.chessjudgment.model.judgment

/** Shared evidence classification for conversion claim proposal and admission. */
object ConversionContextPolicy:

  def supports(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.directCauseProjectionEligibleConsequenceKinds.exists {
          case LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow =>
            true
          case _ =>
            false
        }
      case _ =>
        false
    }

  def supports(
      records: List[EvidenceRecord],
      causeKind: RelativeCauseKind
  ): Boolean =
    records.exists {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.directCauseProjectionEligibleConsequenceKinds.exists(
          RelativeCauseKind.acceptsLineConsequence(causeKind, _)
        )
      case _ =>
        false
    }
