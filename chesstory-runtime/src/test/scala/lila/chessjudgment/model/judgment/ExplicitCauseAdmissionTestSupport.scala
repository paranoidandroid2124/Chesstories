package lila.chessjudgment.model.judgment

private[chessjudgment] object ExplicitCauseAdmissionTestSupport:

  def comparisonRecord(
      id: String,
      position: PositionNodeRef,
      comparison: CandidateComparisonFact
  ): EvidenceRecord =
    EvidenceRecord(
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.RelativeMoveProducer,
        layer = EvidenceLayer.CandidateComparison,
        position = position,
        line = Some(comparison.candidateLine),
        scope = comparison.candidateLine.role.scope,
        confidence = EvidenceConfidence.EngineBacked
      ),
      CandidateComparisonEvidence(comparison)
    )

  def graph(records: List[EvidenceRecord]): TypedEvidenceGraph =
    val rawGraph = records
      .filterNot(_.payload.isInstanceOf[RelativeCauseFactEvidence])
      .foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    records
      .map {
        case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          val signatures = EvidenceObjectBinding
            .rawDirectSentenceChannelsForProjection(cause, rawGraph)
            .map(_.causalSignature)
            .toSet
          record.copy(
            payload = RelativeCauseFactEvidence(
              cause.copy(
                directEffectAdmission = DirectEffectAdmission.Restricted(signatures)
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
