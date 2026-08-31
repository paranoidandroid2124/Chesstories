package lila.chessjudgment.model.judgment

private[chessjudgment] object ExplicitCauseAdmissionTestSupport:

  def graph(records: List[EvidenceRecord]): TypedEvidenceGraph =
    val rawGraph = records
      .filterNot(_.payload.isInstanceOf[RelativeCauseFactEvidence])
      .foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    records
      .map {
        case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          val endpointObservations = EvidenceObjectBinding
            .rawDirectSentenceChannelsForProjection(cause, rawGraph)
            .map(_.exactOccurrenceFingerprint -> None)
            .toMap
          record.copy(
            payload = RelativeCauseFactEvidence(
              cause.copy(
                directEffectAdmission = DirectEffectAdmission.Restricted(endpointObservations)
              )
            )
          )
        case record => record
      }
      .foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))

  def registeredCause(
      ref: EvidenceRef,
      graph: TypedEvidenceGraph
  ): Option[RelativeCauseFact] =
    graph.record(ref).collect {
      case EvidenceRecord(registeredRef, RelativeCauseFactEvidence(cause), _)
          if registeredRef == ref =>
        cause
    }
