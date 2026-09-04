package lila.chessjudgment.model.judgment

import scala.collection.immutable.VectorMap

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

/** Exact relation-output inventory for one admitted transition. `bind` adds
  * occurrence ownership to the direct transition results and later vertical
  * derivations.
  */
private[chessjudgment] final class ClosedRelationTransitionInventory private (
    val transition: LineReplayStep,
    private val controlSetDeltas: ControlSetDeltaResults,
    private val vertical: ClosedVerticalRelationResults,
    private val delta: RelationSemanticDelta
):
  val transitionRelations: List[RelationFactEvidence] =
    controlSetDeltas.relations

  require(
    transitionRelations.map(_.semanticId).distinct.size == transitionRelations.size,
    "a closed relation transition inventory cannot repeat transition semantics"
  )

  val verticalRelations: List[RelationFactEvidence] =
    vertical.byContract.iterator
      .flatMap { case (contract, results) =>
        results.iterator.map(relation => contract -> relation)
      }
      .toList
      .sortBy { case (contract, relation) =>
        VerticalRelationContractKind.id(contract) -> relation.semanticId
      }
      .map(_._2)

  require(
    verticalRelations.map(_.semanticId).distinct.size == verticalRelations.size,
    "a closed relation transition inventory cannot repeat a vertical derivation"
  )

  val allRelations: List[RelationFactEvidence] =
    (transitionRelations ++ verticalRelations)
      .sortBy(relation => RelationFactKind.id(relation.kind) -> relation.semanticId)

  private[judgment] def absenceCertificate(
      premise: ClosedRelationAbsencePremise
  ): Option[PositionRelationExtractor.ClosedRelationAbsenceCertificate] =
    vertical.absenceCertificate(premise)

  private[judgment] def stateCertificate(
      premise: ClosedPositionStatePremise
  ): Option[PositionRelationExtractor.ClosedPositionStateCertificate] =
    vertical.stateCertificate(premise)

  private[judgment] def ownsControlSetDelta(relation: RelationFactEvidence): Boolean =
    controlSetDeltas.ownsOutput(relation)

  require(
    allRelations.map(_.semanticId).distinct.size == allRelations.size,
    "a closed relation output inventory cannot repeat a derivation"
  )

  /** Bind the shareable semantic result to the exact graph occurrences that
    * own its before/after premises. No relation is recomputed here.
    */
  private[chessjudgment] def bind(
      before: CanonicalPositionRelationSnapshot,
      after: CanonicalPositionRelationSnapshot,
      canonicalDelta: CanonicalRelationDelta
  ): CanonicalRelationTransitionInventory =
    CanonicalRelationTransitionInventory.bind(this, delta, before, after, canonicalDelta)

private[chessjudgment] object ClosedRelationTransitionInventory:
  def close(
      transition: LineReplayStep,
      delta: RelationSemanticDelta,
      controlSetDeltas: ControlSetDeltaResults,
      vertical: ClosedVerticalRelationResults
  ): ClosedRelationTransitionInventory =
    new ClosedRelationTransitionInventory(
      transition,
      controlSetDeltas,
      vertical,
      delta
    )

/** Occurrence-owned view of one closed semantic transition inventory. This
  * binding cannot be shared across occurrences: every premise is an exact
  * canonical graph node at the declared transition origin or destination.
  */
private[chessjudgment] final case class CanonicalRelationSourceBinding(
    positionSources: List[CanonicalRelationNode],
    derivedSourceSemanticIds: List[String],
    absenceProofs: List[BoundClosedRelationAbsence],
    stateProofs: List[BoundClosedPositionState]
):
  require(
    positionSources.map(_.ref.id).distinct.size == positionSources.size,
    "a relation source binding cannot repeat a position occurrence"
  )
  require(
    derivedSourceSemanticIds.distinct.size == derivedSourceSemanticIds.size,
    "a relation source binding cannot repeat a derived result"
  )

private[chessjudgment] final class CanonicalRelationTransitionInventory private (
    val transition: LineReplayStep,
    val before: CanonicalPositionRelationSnapshot,
    val after: CanonicalPositionRelationSnapshot,
    val delta: CanonicalRelationDelta,
    val relations: List[RelationFactEvidence],
    private val sourceBindingsByResult: VectorMap[String, CanonicalRelationSourceBinding],
    private val transitionSemanticIds: Set[String],
    private val verticalSemanticIds: Set[String]
):
  private val relationsBySemanticId: Map[String, RelationFactEvidence] =
    relations.map(relation => relation.semanticId -> relation).toMap

  require(
    transitionSemanticIds.intersect(verticalSemanticIds).isEmpty &&
      transitionSemanticIds ++ verticalSemanticIds == relationsBySemanticId.keySet,
    "a canonical transition inventory needs an exact disjoint result partition"
  )

  /** Materializes one persistent transition occurrence. Chess meaning and
    * proof keys were already closed by the semantic producer and occurrence
    * binder; this method only binds exact evidence identities.
    */
  private[chessjudgment] def materializeOccurrence(
      occurrenceId: String,
      edge: MoveTransitionEdge,
      lineOwner: LineNodeRef,
      lineEvidence: EvidenceRef,
      outputRecords: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    require(
      edge.from == before.position && edge.to == after.position &&
        EvidenceRef.sameMove(edge.moveUci, transition.moveUci) &&
        PrincipalVariationEvidence.sameBoardState(edge.from.fen, transition.fenBefore) &&
        PrincipalVariationEvidence.sameBoardState(edge.to.fen, transition.fenAfter),
      "a relation occurrence must bind its exact canonical transition"
    )
    require(outputRecords.map(_.ref.id).distinct.size == outputRecords.size, "a relation occurrence cannot repeat an output")
    val relationOutputs = outputRecords.map {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _) => record -> relation
      case _ => throw IllegalArgumentException("a closed relation occurrence may materialize only relation facts")
    }
    val outputsBySemanticId = relationOutputs
      .map { case (record, relation) => relation.semanticId -> (record -> relation) }
      .groupMap(_._1)(_._2)
    require(
      outputsBySemanticId.keySet == relationsBySemanticId.keySet &&
        outputsBySemanticId.values.forall(_.size == 1) &&
        relations.forall(relation =>
          outputsBySemanticId(relation.semanticId).head._2 == relation
        ),
      "a relation occurrence must materialize every closed relation output exactly once"
    )
    val transitionOutputsBySemanticId = outputsBySemanticId.filter { case (semanticId, _) =>
      transitionSemanticIds(semanticId)
    }
    val verticalOutputsBySemanticId = outputsBySemanticId.filter { case (semanticId, _) =>
      verticalSemanticIds(semanticId)
    }
    require(
      transitionOutputsBySemanticId.keySet == transitionSemanticIds &&
        verticalOutputsBySemanticId.keySet == verticalSemanticIds,
      "a relation occurrence must preserve the exact closed transition and vertical result partitions"
    )
    val bindings = relationOutputs.map { case (record, relation) =>
      val binding = sourceBindingsByResult.getOrElse(
        relation.semanticId,
        throw IllegalArgumentException(s"relation '${relation.semanticId}' has no exact occurrence source binding")
      )
      val derivedSources = binding.derivedSourceSemanticIds.map { semanticId =>
        outputsBySemanticId.get(semanticId) match
          case Some((sourceRecord, sourceRelation) :: Nil)
              if RelationProofStage.rank(sourceRelation.proofStage) < RelationProofStage.rank(relation.proofStage) =>
            sourceRecord.ref
          case _ =>
            throw IllegalArgumentException(
              s"vertical relation '${relation.semanticId}' lost lower source '$semanticId'"
            )
      }
      val sources = (binding.positionSources.map(_.ref) ++ derivedSources).sortBy(_.id)
      ClosedRelationOutputBinding.certified(
        record.ref,
        relation,
        sources,
        binding.absenceProofs,
        binding.stateProofs
      )
    }
    val occurrence = ClosedRelationOccurrenceEvidence.certified(
      edge,
      lineOwner,
      lineEvidence,
      bindings
    )
    val carrier = ClosedRelationOccurrenceEvidence.record(occurrenceId, occurrence)
    val bindingsByResultId = bindings.map(binding => binding.result.id -> binding).toMap
    carrier :: outputRecords.map { record =>
      val sources = bindingsByResultId(record.ref.id).sources
      record.copy(parents = carrier.ref :: sources.sortBy(_.id))
    }

private[chessjudgment] object CanonicalRelationTransitionInventory:
  def bind(
      semantic: ClosedRelationTransitionInventory,
      semanticDelta: RelationSemanticDelta,
      before: CanonicalPositionRelationSnapshot,
      after: CanonicalPositionRelationSnapshot,
      canonicalDelta: CanonicalRelationDelta
  ): CanonicalRelationTransitionInventory =
    require(
      canonicalDelta.binds(semantic.transition, semanticDelta, before, after),
      "a closed transition inventory must reuse its exact canonical relation delta"
    )
    val changesByKey = canonicalDelta.changes.map { change =>
      (change.direction, change.kind, change.semanticId) -> change
    }.toMap
    require(
      changesByKey.size == canonicalDelta.changes.size,
      "a canonical relation delta cannot repeat a premise source"
    )
    val transitionBindings = semantic.transitionRelations.map { relation =>
      val proof = RelationWitnessDetail.combinationProof(relation.detail).getOrElse(
        throw IllegalArgumentException("a closed control-set delta needs its registered proof")
      )
      require(
        semantic.ownsControlSetDelta(relation),
        s"control-set delta '${relation.semanticId}' is not owned by its exact total producer"
      )
      val resolved = proof.premises.map { premise =>
        premise.occurrence match
          case RelationPremiseOccurrence.Before =>
            before.nodesBySemanticId
              .get(premise.semanticId)
              .filter(_.relation.kind == premise.kind)
              .getOrElse(
                throw IllegalArgumentException(
                  s"before premise '${premise.semanticId}' is absent from its exact occurrence"
                )
              )
          case RelationPremiseOccurrence.After =>
            after.nodesBySemanticId
              .get(premise.semanticId)
              .filter(_.relation.kind == premise.kind)
              .getOrElse(
                throw IllegalArgumentException(
                  s"after premise '${premise.semanticId}' is absent from its exact occurrence"
                )
              )
          case RelationPremiseOccurrence.Removed =>
            changesByKey
              .get((RelationChangeDirection.Removed, premise.kind, premise.semanticId))
              .map(_.sourceNode)
              .getOrElse(
                throw IllegalArgumentException(
                  s"removed premise '${premise.semanticId}' is absent from its exact transition"
                )
              )
          case RelationPremiseOccurrence.Established =>
            changesByKey
              .get((RelationChangeDirection.Established, premise.kind, premise.semanticId))
              .map(_.sourceNode)
              .getOrElse(
                throw IllegalArgumentException(
                  s"established premise '${premise.semanticId}' is absent from its exact transition"
                )
              )
      }
      require(
        resolved.map(_.ref.id).distinct.size == resolved.size,
        s"control-set delta '${relation.semanticId}' repeats one exact premise occurrence"
      )
      val changedRelationProofKeys = proof.premises.flatMap { premise =>
        premise.occurrence match
          case RelationPremiseOccurrence.Before | RelationPremiseOccurrence.After => Nil
          case RelationPremiseOccurrence.Removed =>
            changesByKey
              .get((RelationChangeDirection.Removed, premise.kind, premise.semanticId))
              .toList
              .flatMap(_.proofKeys)
          case RelationPremiseOccurrence.Established =>
            changesByKey
              .get((RelationChangeDirection.Established, premise.kind, premise.semanticId))
              .toList
              .flatMap(_.proofKeys)
      }
      val changedStateProofKeys = relation.detail match
        case RelationWitnessDetail.GeometricControlSetDelta(_, _, target, _, _, _, _, _, _, _) =>
          RelationProofKey.forDependencySquares(List(target), semanticDelta.transitionFootprint)
        case _ =>
          Nil
      val exactProofKeys = (changedRelationProofKeys ++ changedStateProofKeys).distinct.sortBy(_.stableKey)
      require(
        exactProofKeys.nonEmpty && exactProofKeys == proof.proofKeys,
        s"control-set delta '${relation.semanticId}' does not own its exact transition proof keys: " +
          s"changed=${exactProofKeys.map(_.stableKey).mkString("[", ",", "]")}, " +
          s"declared=${proof.proofKeys.map(_.stableKey).mkString("[", ",", "]")}, " +
          s"premises=${proof.premises.map(premise => s"${premise.occurrence}:${premise.kind}:${premise.semanticId}").mkString("[", ",", "]")}"
      )
      relation.semanticId -> CanonicalRelationSourceBinding(
        resolved.sortBy(_.ref.id),
        Nil,
        Nil,
        Nil
      )
    }
    val transitionRelationsBySemanticId = semantic.transitionRelations
      .map(relation => relation.semanticId -> relation)
      .toMap
    val verticalBindings = semantic.verticalRelations.map { relation =>
      val proof = VerticalRelationContracts.proofOf(relation.detail).getOrElse(
        throw IllegalArgumentException("a vertical relation result needs its typed derivation proof")
      )
      val resolvedSources = proof.sourcePremises.flatMap { premise =>
        def exactPositionSource(node: Option[CanonicalRelationNode]): Option[CanonicalRelationNode] =
          node.filter(source =>
            source.relation.kind == premise.kind && source.relation.assertionId == premise.assertionId &&
              source.relation.semanticId == premise.semanticId && source.relation.proofStage == premise.stage &&
              premise.stage == RelationProofStage.PositionFact
          )

        def required[A](source: Option[A]): A =
          source.getOrElse(
            throw IllegalArgumentException(
              s"vertical result '${relation.semanticId}' lost exact typed premise '${premise.stableKey}'"
            )
          )

        premise.source match
          case VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.Before) =>
            List(Left(required(exactPositionSource(before.nodesBySemanticId.get(premise.semanticId)))))
          case VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After) =>
            List(Left(required(exactPositionSource(after.nodesBySemanticId.get(premise.semanticId)))))
          case VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.Removed) =>
            List(Left(required(exactPositionSource(
              changesByKey
                .get((RelationChangeDirection.Removed, premise.kind, premise.semanticId))
                .map(_.sourceNode)
            ))))
          case VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.Established) =>
            List(Left(required(exactPositionSource(
              changesByKey
                .get((RelationChangeDirection.Established, premise.kind, premise.semanticId))
                .map(_.sourceNode)
            ))))
          case VerticalRelationPremiseSource.RootTransition =>
            require(
              premise.stage == RelationProofStage.TransitionFact &&
                semanticDelta.rootMove.fact.kind == premise.kind &&
                semanticDelta.rootMove.fact.assertionId == premise.assertionId &&
                semanticDelta.rootMove.fact.semanticId == premise.semanticId,
              s"vertical result '${relation.semanticId}' lost its exact root transition"
            )
            // The occurrence carrier owns the transition edge. Rebinding this
            // obligation to the before-position LegalMove node would give one
            // fact two occurrence roles and can duplicate a genuine position
            // premise such as the chosen check response.
            Nil
          case VerticalRelationPremiseSource.Derived =>
            List(Right(required(transitionRelationsBySemanticId.get(premise.semanticId).filter(source =>
              source.kind == premise.kind && source.assertionId == premise.assertionId &&
                source.proofStage == premise.stage &&
                RelationProofStage.rank(source.proofStage) < RelationProofStage.rank(relation.proofStage)
            )).semanticId))
      }
      val positionSources = resolvedSources.collect { case Left(source) => source }
        .foldLeft(VectorMap.empty[String, CanonicalRelationNode]) { (indexed, source) =>
          indexed.get(source.ref.id) match
            case Some(existing) =>
              require(
                existing.asInstanceOf[AnyRef].eq(source.asInstanceOf[AnyRef]),
                s"vertical result '${relation.semanticId}' changed one canonical premise owner"
              )
              indexed
            case None => indexed.updated(source.ref.id, source)
        }
        .values
        .toList
        .sortBy(_.ref.id)
      val sourceSemanticIds = resolvedSources.collect { case Right(semanticId) => semanticId }.sorted
      require(
        sourceSemanticIds.distinct.size == sourceSemanticIds.size,
        s"vertical result '${relation.semanticId}' repeated one derived premise occurrence"
      )
      val absenceProofs = proof.absences.map { absence =>
        val certificate = semantic.absenceCertificate(absence).getOrElse(
          throw IllegalArgumentException(
            s"vertical result '${relation.semanticId}' lost semantic absence '${absence.stableKey}'"
          )
        )
        BoundClosedRelationAbsence.bind(absence, certificate, before, after).getOrElse(
          throw IllegalArgumentException(
            s"vertical result '${relation.semanticId}' cannot bind '${absence.stableKey}' to this occurrence"
          )
        )
      }
      val stateProofs = proof.states.map { state =>
        val certificate = semantic.stateCertificate(state).getOrElse(
          throw IllegalArgumentException(
            s"vertical result '${relation.semanticId}' lost semantic state '${state.stableKey}'"
          )
        )
        BoundClosedPositionState.bind(state, certificate, before, after).getOrElse(
          throw IllegalArgumentException(
            s"vertical result '${relation.semanticId}' cannot bind '${state.stableKey}' to this occurrence"
          )
        )
      }
      relation.semanticId -> CanonicalRelationSourceBinding(
        positionSources,
        sourceSemanticIds,
        absenceProofs,
        stateProofs
      )
    }
    val bindings = transitionBindings ++ verticalBindings
    require(
      bindings.map(_._1).distinct.size == bindings.size,
      "a canonical relation output inventory cannot repeat result semantics"
    )
    new CanonicalRelationTransitionInventory(
      semantic.transition,
      before,
      after,
      canonicalDelta,
      semantic.allRelations,
      bindings.foldLeft(VectorMap.empty[String, CanonicalRelationSourceBinding]) {
        case (inventory, (semanticId, sources)) => inventory.updated(semanticId, sources)
      },
      semantic.transitionRelations.map(_.semanticId).toSet,
      semantic.verticalRelations.map(_.semanticId).toSet
    )
