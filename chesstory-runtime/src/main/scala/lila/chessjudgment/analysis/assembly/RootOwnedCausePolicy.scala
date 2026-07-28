package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.analysis.tactical.TacticalMotifClassifier
import lila.chessjudgment.model.judgment.*

private[chessjudgment] object RootOwnedCausePolicy:
  private val forcingResourceRelations = Set(
    RelationFactKind.Overload,
    RelationFactKind.Deflection,
    RelationFactKind.DiscoveredAttack,
    RelationFactKind.DoubleCheck,
    RelationFactKind.BackRankMate,
    RelationFactKind.MateNet,
    RelationFactKind.Fork,
    RelationFactKind.Decoy,
    RelationFactKind.Interference,
    RelationFactKind.Clearance,
    RelationFactKind.XRay,
    RelationFactKind.Pin,
    RelationFactKind.Skewer,
    RelationFactKind.Zwischenzug,
    RelationFactKind.Domination,
    RelationFactKind.TrappedPiece,
    RelationFactKind.GreekGift
  )

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
            primitiveResourceOwner(record, binding.eventLine)
        )
      val mechanismOwners = records.filter(record =>
        tacticalMechanismOwnsResource(graph, record, binding.eventLine)
      )
      (primitiveOwners ++ mechanismOwners).distinctBy(_.ref.id)

  def forcingResourceRecords(
      records: List[EvidenceRecord],
      eventLine: LineNodeRef
  ): List[EvidenceRecord] =
    records
      .filter(record => primitiveResourceOwner(record, eventLine))
      .distinctBy(_.ref.id)

  private def primitiveResourceOwner(
      record: EvidenceRecord,
      eventLine: LineNodeRef
  ): Boolean =
    record.payload match
      case line: LineFactEvidence =>
        RootOwnedEffectPolicy.lineRecordOwnsEventRoot(record.ref, line, eventLine) &&
          line.rootOwnedCausalEpisodes(eventLine.rootMove).exists(_.forcingTacticalResource(line))
      case motif: MoveMotifEvidence =>
        RootOwnedEffectPolicy.motifRecordOwnsEventRoot(record.ref, motif, eventLine) &&
          TacticalMotifClassifier.isRootCauseEligible(motif) &&
          motif.geometry.focusSquares.nonEmpty
      case relation: RelationFactEvidence =>
        RootOwnedEffectPolicy.relationRecordOwnsEventRoot(record.ref, relation, eventLine) &&
          forcingResourceRelations(relation.kind)
      case _ =>
        false

  private def tacticalMechanismOwnsResource(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      eventLine: LineNodeRef
  ): Boolean =
    record.payload match
      case mechanism: TacticalMechanismEvidence =>
        RootOwnedEffectPolicy.tacticalCarrierOwnsEventRoot(record.ref, mechanism, eventLine) &&
          mechanism.signals.exists(signal =>
            signal.source.exists(source =>
              graph.record(source).exists(primitiveResourceOwner(_, eventLine))
            )
          )
      case _ =>
        false
