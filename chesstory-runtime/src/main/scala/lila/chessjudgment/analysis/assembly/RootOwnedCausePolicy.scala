package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

private[chessjudgment] object RootOwnedCausePolicy:
  def directProofRecords(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      attributionKind: CauseAttributionKind,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    if kind != RelativeCauseKind.MissedTacticalResource then records
    else if
      binding.sourceSide != RelativeCauseSourceSide.Reference ||
        binding.eventLine != fact.referenceLine ||
        attributionKind != CauseAttributionKind.ReferenceCreatesResource
    then Nil
    else
      val primitiveOwners =
        records.filter(record =>
          graph.record(record.ref).contains(record) &&
            primitiveResourceOwner(graph, record, binding.eventLine)
        )
      val mechanismOwners = records.filter(record =>
        tacticalMechanismOwnsResource(graph, record, binding.eventLine)
      )
      (primitiveOwners ++ mechanismOwners).distinctBy(_.ref.id)

  def forcingResourceRecords(
      graph: TypedEvidenceGraph,
      records: List[EvidenceRecord],
      eventLine: LineNodeRef
  ): List[EvidenceRecord] =
    records
      .filter(record => primitiveResourceOwner(graph, record, eventLine))
      .distinctBy(_.ref.id)

  private def primitiveResourceOwner(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      eventLine: LineNodeRef
  ): Boolean =
    record.payload match
      case line: LineFactEvidence =>
        RootOwnedEffectPolicy.lineRecordOwnsEventRoot(record.ref, line, eventLine) &&
          line.rootOwnedCausalEpisodes(eventLine.rootMove).exists(_.forcingTacticalResource(line))
      case relation: RelationFactEvidence =>
        RootOwnedEffectPolicy.relationRecordOwnsEventRoot(graph, record.ref, relation, eventLine) &&
          RootOwnedEffectPolicy.relationDirectlyProvesCause(
            relation,
            RelativeCauseKind.MissedTacticalResource
          )
      case _ =>
        false

  private def tacticalMechanismOwnsResource(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      eventLine: LineNodeRef
  ): Boolean =
    record.payload match
      case mechanism: TacticalMechanismEvidence =>
        RootOwnedEffectPolicy.tacticalCarrierOwnsEventRoot(graph, record.ref, mechanism, eventLine) &&
          mechanism.signals.exists(signal =>
            signal.source.exists(source =>
              graph.record(source).exists(primitiveResourceOwner(graph, _, eventLine))
            )
          )
      case _ =>
        false
