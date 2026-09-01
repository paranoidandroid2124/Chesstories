package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.analysis.structure.StructuralDeltaAnalyzer
import lila.chessjudgment.analysis.relation.ClosedRelationEvidence
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.judgment.*

object EvidenceFactAssembler:

  def assemble(raw: RawMoveReviewInput): Option[JudgmentAssemblyContext] =
    NodeLineTransitionAssembler.assemble(raw).map(enrich)

  def enrich(context: JudgmentAssemblyContext): JudgmentAssemblyContext =
    val input = context.input
    val allocator = JudgmentProvenanceAllocator.forInput(input)
    val closedRelationsByOccurrence = closedPositionRelations(context)
    val canonicalRelationDeltas = canonicalRelationDeltasFor(
      context,
      closedRelationsByOccurrence
    )
    val closedRelationRecords = relationRecords(
      context,
      allocator,
      closedRelationsByOccurrence,
      canonicalRelationDeltas
    )
    val baseRecords =
      List.concat(
        closedRelationRecords,
        structuralDeltaRecords(context, allocator, canonicalRelationDeltas, closedRelationRecords)
      )
    context.withEvidence(baseRecords)

  private def closedPositionRelations(
      context: JudgmentAssemblyContext
  ): Map[PositionNodeRef, CanonicalPositionRelationSnapshot] =
    val positionScopes = context.positions.map(node => node.ref -> node.role.scope).toMap
    require(
      positionScopes.size == context.positions.size,
      "every registered relation occurrence must own exactly one evidence scope"
    )
    context.transitions
      .flatMap(edge => List(edge.from, edge.to))
      .distinct
      .map { position =>
        val scope = positionScopes.getOrElse(
          position,
          throw IllegalArgumentException(
            s"relation occurrence '${position.id.getOrElse(position.fen)}' has no registered scope"
          )
        )
        val analysis = context.positionAnalysis(position).getOrElse(
          throw IllegalArgumentException(
            s"relation occurrence '${position.id.getOrElse(position.fen)}' has no position analysis"
          )
        )
        position -> context.evidenceGraph.relationGraph.closedPositionRelationSnapshot(
          position,
          scope,
          analysis.relationInventory
        )
      }
      .toMap

  /** One occurrence binding per admitted transition. Closed relation outputs
    * and structural deltas consume this same value instead of resolving the
    * identical changed relation nodes twice.
    */
  private def canonicalRelationDeltasFor(
      context: JudgmentAssemblyContext,
      closedRelations: Map[PositionNodeRef, CanonicalPositionRelationSnapshot]
  ): Map[String, CanonicalRelationDelta] =
    val deltas = context.transitions.map { edge =>
      val transition = context
        .transitionReplay(edge)
        .flatMap(_.onlyTransition)
        .getOrElse(
          throw IllegalArgumentException(
            s"transition '${edge.evidence.id}' has no exact canonical replay transition"
          )
        )
      edge.evidence.id -> CanonicalRelationDelta.bind(
        transition.declared,
        transition.relationDelta,
        closedRelations(edge.from),
        closedRelations(edge.to)
      )
    }
    require(
      deltas.map(_._1).distinct.size == deltas.size,
      "one transition evidence occurrence must own exactly one canonical relation delta"
    )
    deltas.toMap

  private def relationRecords(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      closedRelationsByOccurrence: Map[PositionNodeRef, CanonicalPositionRelationSnapshot],
      canonicalRelationDeltas: Map[String, CanonicalRelationDelta]
  ): List[EvidenceRecord] =
    context.transitions.flatMap { edge =>
      val lineOwner = lineForTransition(context, edge)
      val replay = context.transitionReplay(edge).getOrElse(
        throw IllegalArgumentException(
          s"transition '${edge.evidence.id}' has no exact canonical replay"
        )
      )
      val production = ClosedRelationEvidence.relationProduction(replay, edge.moveUci)
      val closedRelations = production.bindClosedOutput(
        closedRelationsByOccurrence(edge.from),
        closedRelationsByOccurrence(edge.to),
        canonicalRelationDeltas(edge.evidence.id)
      )
      val lineEvidence = lineOwner.map(line => requiredLineFactRecord(context, line.ref).ref)
      val relationScope = lineOwner.fold(edge.role.scope)(_ => EvidenceScope.LegalLine)
      val provisional = production.relations.map { relation =>
        val occurrenceOwner = lineOwner.map(_.ref.id).getOrElse(edge.evidence.id)
        RelationFactEvidence.record(
          id = allocator.evidenceId(
            s"relation:transition:${exactIdentityKey(List(occurrenceOwner, RelationFactKind.id(relation.kind), relation.semanticId))}"
          ),
          payload = relation,
          position = edge.from,
          line = lineOwner.map(_.ref),
          scope = relationScope,
          confidence = EvidenceConfidence.LegalReplayVerified
        )
      }
      closedRelations.materializeOccurrence(
        occurrenceId = allocator.evidenceId(s"closed-relation-occurrence:${edge.evidence.id}"),
        edge = edge,
        lineOwner = lineOwner.map(_.ref),
        lineEvidence = lineEvidence,
        outputRecords = provisional
      )
    }

  private def exactIdentityKey(parts: Iterable[String]): String =
    parts.iterator.map(value => s"${value.length}:$value").mkString("|")

  private def structuralDeltaRecords(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      canonicalRelationDeltas: Map[String, CanonicalRelationDelta],
      relationRecords: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    val occurrencesById = relationRecords.collect {
      case EvidenceRecord(ref, occurrence: ClosedRelationOccurrenceEvidence, _) => ref.id -> occurrence
    }.toMap
    val resultOutputsByTransitionId = relationRecords.flatMap {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, parents)
          if relation.proofStage != RelationProofStage.PositionFact =>
        parents.flatMap(parent =>
          occurrencesById.get(parent.id).flatMap(occurrence =>
            occurrence.outputFor(record.ref).map(_ =>
              occurrence.edge.evidence.id -> (DerivedRelationResultKey.from(relation) -> record)
            )
          )
        )
      case _ => Nil
    }.groupMap(_._1)(_._2)
    context.transitions.flatMap { edge =>
      for
        transitionReplay <- context.transitionReplay(edge)
        replayTransition <- transitionReplay.onlyTransition
        line = lineForTransition(context, edge)
        consequences = replayTransition.structuralConsequences
        delta = StructuralDeltaAnalyzer.bind(
          transition = replayTransition,
          canonicalRelations = canonicalRelationDeltas(edge.evidence.id)
        )
        consequenceKeys = consequences.flatMap(_.relationKeys).toSet
        relationSourceOccurrences = delta.canonicalRelations.changes
          .filter(change => consequenceKeys(change.key))
          .map(_.source)
        relationSourcesById = relationSourceOccurrences.groupBy(_.id)
        _ = require(
          relationSourcesById.values.forall(_.distinct.size == 1),
          "canonical relation changes cannot collide distinct evidence owners under one id"
        )
        // One exact carrier may certify several consumed change keys. Project
        // that proven shared edge once; equal ids with unequal owners failed above.
        relationSources = relationSourcesById.toList.sortBy(_._1).map(_._2.head)
        expectedResultKeys = consequences.flatMap(_.resultPremiseKeys).toSet
        resultOutputRecords = resultOutputsByTransitionId
          .getOrElse(edge.evidence.id, Nil)
          .filter { case (key, _) => expectedResultKeys(key) }
        _ = require(
          resultOutputRecords.map(_._1).distinct.size == resultOutputRecords.size &&
            resultOutputRecords.map(_._1).toSet == expectedResultKeys,
          "a structural consequence must bind every consumed transition result to one exact occurrence output"
        )
        resultPremiseSources = resultOutputRecords.sortBy(_._1.stableKey).map { case (key, output) =>
          TransitionResultPremiseSource(key, output.ref)
        }
        parentOccurrences =
          List(edge.evidence) ++
            line.toList.flatMap(lineParents(context, _)) ++
            evidenceRefs(context, EvidenceLayer.PositionOccurrence, Some(edge.from), None) ++
            evidenceRefs(context, EvidenceLayer.PositionOccurrence, Some(edge.to), None) ++
            relationSources ++
            resultPremiseSources.map(_.source)
        _ = require(
          parentOccurrences.map(_.id).distinct.size == parentOccurrences.size,
          "a structural delta cannot silently merge duplicate or colliding parent edges"
        )
        record = TransitionFactNormalizer.fromStructuralDelta(
          id = allocator.evidenceId(s"structural-delta:${edge.evidence.id}"),
          delta = delta,
          transition = edge,
          replay = transitionReplay,
          line = line.map(_.ref),
          resultPremiseSources = resultPremiseSources,
          parents = parentOccurrences
        )
      yield record
    }

  private def requiredLineFactRecord(
      context: JudgmentAssemblyContext,
      line: LineNodeRef
  ): EvidenceRecord =
    context.evidenceGraph.uniqueProofEligibleLineFactRecordFor(line) match
      case Some((record, _)) => record
      case None =>
        throw IllegalArgumentException(
          s"line '${line.id}' has no unique proof-eligible line-fact carrier"
        )

  private def lineParents(
      context: JudgmentAssemblyContext,
      line: LegalLineNode
  ): List[EvidenceRef] =
    evidenceRefs(context, EvidenceLayer.Line, None, Some(line.ref))

  private def evidenceRefs(
      context: JudgmentAssemblyContext,
      layer: EvidenceLayer,
      position: Option[PositionNodeRef],
      line: Option[LineNodeRef]
  ): List[EvidenceRef] =
    context.evidenceGraph.records.collect {
      case record
          if record.ref.layer == layer &&
            position.forall(_ == record.ref.position) &&
            line.forall(record.ref.line.contains) =>
        record.ref
    }

  private def lineForTransition(
      context: JudgmentAssemblyContext,
      edge: MoveTransitionEdge
  ): Option[LegalLineNode] =
    context.lineForRootTransition(edge)
