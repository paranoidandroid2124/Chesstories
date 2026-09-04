package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.analysis.structure.StructuralDeltaAnalyzer
import lila.chessjudgment.analysis.relation.ClosedRelationEvidence
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.judgment.*

object EvidenceFactAssembler:

  def enrich(context: JudgmentAssemblyContext): JudgmentAssemblyContext =
    val input = context.input
    val allocator = JudgmentProvenanceAllocator.forInput(input)
    val closedRelationsByOccurrence = closedPositionRelations(context)
    val canonicalRelationDeltas = canonicalRelationDeltasFor(
      context,
      closedRelationsByOccurrence
    )
    val lineOwnersByTransitionId = exactLineOwnersByTransitionId(context)
    val lineRecordsByLine = context.legalLines.map(line =>
      line.ref -> requiredLineFactRecord(context, line.ref)
    ).toMap
    val positionRecordsByPosition = context.positions.map(position =>
      position.ref -> requiredPositionOccurrenceRecord(context, position.ref)
    ).toMap
    val closedRelationRecords = relationRecords(
      context,
      allocator,
      closedRelationsByOccurrence,
      canonicalRelationDeltas,
      lineOwnersByTransitionId,
      lineRecordsByLine
    )
    val baseRecords =
      List.concat(
        closedRelationRecords,
        structuralDeltaRecords(
          context,
          allocator,
          canonicalRelationDeltas,
          closedRelationRecords,
          lineOwnersByTransitionId,
          lineRecordsByLine,
          positionRecordsByPosition
        )
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
      canonicalRelationDeltas: Map[String, CanonicalRelationDelta],
      lineOwnersByTransitionId: Map[String, LegalLineNode],
      lineRecordsByLine: Map[LineNodeRef, EvidenceRecord]
  ): List[EvidenceRecord] =
    context.transitions.flatMap { edge =>
      val lineOwner = lineOwnersByTransitionId(edge.evidence.id)
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
      val lineEvidence = lineRecordsByLine(lineOwner.ref).ref
      val provisional = production.relations.map { relation =>
        RelationFactEvidence.record(
          id = allocator.evidenceId(
            s"relation:transition:${exactIdentityKey(List(edge.evidence.id, RelationFactKind.id(relation.kind), relation.semanticId))}"
          ),
          payload = relation,
          position = edge.from,
          line = Some(lineOwner.ref),
          scope = EvidenceScope.LegalLine,
          confidence = EvidenceConfidence.LegalReplayVerified
        )
      }
      closedRelations.materializeOccurrence(
        occurrenceId = allocator.evidenceId(s"closed-relation-occurrence:${edge.evidence.id}"),
        edge = edge,
        lineOwner = lineOwner.ref,
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
      relationRecords: List[EvidenceRecord],
      lineOwnersByTransitionId: Map[String, LegalLineNode],
      lineRecordsByLine: Map[LineNodeRef, EvidenceRecord],
      positionRecordsByPosition: Map[PositionNodeRef, EvidenceRecord]
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
        line = lineOwnersByTransitionId(edge.evidence.id)
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
          List(
            edge.evidence,
            lineRecordsByLine(line.ref).ref,
            positionRecordsByPosition(edge.from).ref,
            positionRecordsByPosition(edge.to).ref
          ) ++
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
          line = Some(line.ref),
          resultPremiseSources = resultPremiseSources,
          parents = parentOccurrences
        )
      yield record
    }

  private def exactLineOwnersByTransitionId(
      context: JudgmentAssemblyContext
  ): Map[String, LegalLineNode] =
    val owners = context.transitions.map { edge =>
      edge.evidence.id -> context.lineForTransition(edge).getOrElse(
        throw IllegalArgumentException(
          s"transition '${edge.evidence.id}' has no exact LegalLine owner"
        )
      )
    }
    require(
      owners.map(_._1).distinct.size == owners.size,
      "one transition occurrence cannot have multiple LegalLine owners"
    )
    owners.toMap

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

  private def requiredPositionOccurrenceRecord(
      context: JudgmentAssemblyContext,
      position: PositionNodeRef
  ): EvidenceRecord =
    context.evidenceGraph.recordsFor(position).collect {
      case record @ EvidenceRecord(ref, _: PositionOccurrenceEvidence, _)
          if ref.producer == EvidenceProducer.PositionOccurrenceProducer &&
            ref.layer == EvidenceLayer.PositionOccurrence && ref.position == position &&
            context.evidenceGraph.proofEligible(record) =>
        record
    } match
      case record :: Nil => record
      case _ =>
        throw IllegalArgumentException(
          s"position '${position.id.getOrElse(position.fen)}' has no unique proof-eligible occurrence carrier"
        )
