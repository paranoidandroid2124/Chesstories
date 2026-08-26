package lila.chessjudgment.model.judgment

import scala.collection.immutable.VectorMap

import lila.chessjudgment.model.line.PrincipalVariationEvidence

/** Closed catalogue of the one-transition relation contracts currently owned
  * by the canonical relation graph. A contract name never proves its result;
  * it identifies which exact canonical producer slot owns it.
  */
private[chessjudgment] enum RelationCombinationContractKind:
  case GeometricControlSetDelta
  case GeometricSupporterCapture
  case GeometricSupportDelta
  case SliderControlInterference
  case GeometricLineControlAfterBlockerRemoval
  case CheckingEnemyControlBundle
  case DoubleCheck

private[chessjudgment] object RelationCombinationContractKind:
  def id(kind: RelationCombinationContractKind): String =
    kind match
      case GeometricControlSetDelta => "geometric_control_set_delta"
      case GeometricSupporterCapture => "geometric_supporter_capture"
      case GeometricSupportDelta => "geometric_support_delta"
      case SliderControlInterference  => "slider_control_interference"
      case GeometricLineControlAfterBlockerRemoval => "geometric_line_control_after_blocker_removal"
      case CheckingEnemyControlBundle => "checking_enemy_control_bundle"
      case DoubleCheck                => "double_check"

  def forDetail(detail: RelationWitnessDetail): Option[RelationCombinationContractKind] =
    detail match
      case _: RelationWitnessDetail.GeometricControlSetDelta => Some(GeometricControlSetDelta)
      case _: RelationWitnessDetail.GeometricSupporterCapture => Some(GeometricSupporterCapture)
      case _: RelationWitnessDetail.GeometricSupportDelta => Some(GeometricSupportDelta)
      case _: RelationWitnessDetail.SliderControlInterference => Some(SliderControlInterference)
      case _: RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval =>
        Some(GeometricLineControlAfterBlockerRemoval)
      case _: RelationWitnessDetail.CheckingEnemyControlBundle => Some(CheckingEnemyControlBundle)
      case _: RelationWitnessDetail.DoubleCheck                => Some(DoubleCheck)
      case _                                                   => None

/** The sole closed result of the seven registered combination producers.
  * Every field is mandatory, including an empty result, so completeness does
  * not need a second chess-semantic evaluator.
  */
private[chessjudgment] final class ClosedRelationCombinationResults private (
    private[judgment] val byContract: VectorMap[RelationCombinationContractKind, List[RelationFactEvidence]]
):
  val relations: List[RelationFactEvidence] = byContract.valuesIterator.flatten.toList

  require(
    byContract.keySet == RelationCombinationContractKind.values.toSet,
    "closed relation combinations must contain every registered contract"
  )
  require(
    byContract.forall { case (contract, results) =>
      results.forall(relation => RelationCombinationContractKind.forDetail(relation.detail).contains(contract))
    },
    "a relation-combination contract may emit only its own exact result kind"
  )
  require(
    relations.map(_.semanticId).distinct.size == relations.size,
    "closed relation combinations cannot repeat relation semantics"
  )

  private[chessjudgment] def replaceWithCertified(
      certified: List[RelationFactEvidence]
  ): ClosedRelationCombinationResults =
    val (remaining, rebuilt) = byContract.foldLeft(
      certified -> VectorMap.empty[RelationCombinationContractKind, List[RelationFactEvidence]]
    ) { case ((unclaimed, inventory), (contract, raw)) =>
      val (owned, rest) = unclaimed.splitAt(raw.size)
      require(
        owned.map(_.detail) == raw.map(_.detail),
        s"certified ${RelationCombinationContractKind.id(contract)} results changed their produced semantics"
      )
      rest -> inventory.updated(contract, owned)
    }
    require(remaining.isEmpty, "legal-replay certification returned an unowned combination result")
    new ClosedRelationCombinationResults(rebuilt)

private[chessjudgment] object ClosedRelationCombinationResults:
  def exact(
      geometricControlSetDeltas: List[RelationFactEvidence],
      geometricSupporterCaptures: List[RelationFactEvidence],
      geometricSupportDeltas: List[RelationFactEvidence],
      sliderControlInterferences: List[RelationFactEvidence],
      geometricLineControlsAfterBlockerRemoval: List[RelationFactEvidence],
      checkingEnemyControlBundles: List[RelationFactEvidence],
      doubleChecks: List[RelationFactEvidence]
  ): ClosedRelationCombinationResults =
    new ClosedRelationCombinationResults(
      VectorMap(
        RelationCombinationContractKind.GeometricControlSetDelta -> geometricControlSetDeltas.sortBy(_.semanticId),
        RelationCombinationContractKind.GeometricSupporterCapture -> geometricSupporterCaptures.sortBy(_.semanticId),
        RelationCombinationContractKind.GeometricSupportDelta -> geometricSupportDeltas.sortBy(_.semanticId),
        RelationCombinationContractKind.SliderControlInterference -> sliderControlInterferences.sortBy(_.semanticId),
        RelationCombinationContractKind.GeometricLineControlAfterBlockerRemoval ->
          geometricLineControlsAfterBlockerRemoval.sortBy(_.semanticId),
        RelationCombinationContractKind.CheckingEnemyControlBundle -> checkingEnemyControlBundles.sortBy(_.semanticId),
        RelationCombinationContractKind.DoubleCheck -> doubleChecks.sortBy(_.semanticId)
      )
    )

/** Exact relation-output inventory for one admitted transition. Every
  * combination contract is produced once, including empty results, and every
  * canonical named-ray projection is present. `bind` adds occurrence ownership.
  */
private[chessjudgment] final class ClosedRelationTransitionInventory private (
    val transition: LineReplayStep,
    private val combinations: ClosedRelationCombinationResults,
    val projections: List[RelationFactEvidence],
    private val delta: RelationSemanticDelta
):
  private[judgment] val resultSemanticIdsByContract: VectorMap[RelationCombinationContractKind, List[String]] =
    combinations.byContract.foldLeft(VectorMap.empty[RelationCombinationContractKind, List[String]]) {
      case (inventory, (contract, results)) =>
        inventory.updated(contract, results.map(_.semanticId).sorted)
    }

  val combinationRelations: List[RelationFactEvidence] =
    combinations.byContract.iterator
      .flatMap { case (contract, results) =>
        results.iterator.map(relation => contract -> relation)
      }
      .toList
      .sortBy { case (contract, relation) =>
        RelationCombinationContractKind.id(contract) -> relation.semanticId
      }
      .map(_._2)

  require(
    combinationRelations.map(_.semanticId).distinct.size == combinationRelations.size,
    "a closed relation transition inventory cannot repeat combination semantics"
  )

  val allRelations: List[RelationFactEvidence] =
    (combinationRelations ++ projections).sortBy(relation => RelationFactKind.id(relation.kind) -> relation.semanticId)

  require(
    allRelations.map(_.semanticId).distinct.size == allRelations.size,
    "a closed relation output inventory cannot repeat combination or projection semantics"
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
      combinations: ClosedRelationCombinationResults,
      projections: List[RelationFactEvidence]
  ): ClosedRelationTransitionInventory =
    val expectedProjectionDetails = delta.newlyEstablishedNamedRays.map(_.detail).sortBy(
      RelationWitnessDetail.stableKey
    )
    val actualProjectionDetails = projections.map(_.detail).sortBy(RelationWitnessDetail.stableKey)
    require(
      projections.forall(relation =>
        relation.kind == RelationFactKind.RayBarrier &&
          relation.lineMoves == List(EvidenceRef.normalizeMove(transition.moveUci))
      ) && actualProjectionDetails == expectedProjectionDetails,
      "a closed relation output inventory must contain every exact named-ray projection"
    )
    new ClosedRelationTransitionInventory(
      transition,
      combinations,
      projections.sortBy(_.semanticId),
      delta
    )

/** Occurrence-owned view of one closed semantic transition inventory. This
  * binding cannot be shared across occurrences: every premise is an exact
  * canonical graph node at the declared transition origin or destination.
  */
private[chessjudgment] final class CanonicalRelationTransitionInventory private (
    val transition: LineReplayStep,
    val before: CanonicalPositionRelationSnapshot,
    val after: CanonicalPositionRelationSnapshot,
    val delta: CanonicalRelationDelta,
    val relations: List[RelationFactEvidence],
    private val sourceNodesByResult: VectorMap[String, List[CanonicalRelationNode]],
    private val resultSemanticIdsByContract: VectorMap[RelationCombinationContractKind, List[String]]
):
  private val relationsBySemanticId: Map[String, RelationFactEvidence] =
    relations.map(relation => relation.semanticId -> relation).toMap

  private val combinationSemanticIds: Set[String] =
    resultSemanticIdsByContract.valuesIterator.flatten.toSet

  private[chessjudgment] def sourceNodesFor(
      relation: RelationFactEvidence
  ): Option[List[CanonicalRelationNode]] =
    sourceNodesByResult
      .get(relation.semanticId)
      .filter(_ => relationsBySemanticId.get(relation.semanticId).contains(relation))

  /** Materializes one persistent transition occurrence. Chess meaning and
    * proof keys were already closed by the semantic producer and occurrence
    * binder; this method only binds exact evidence identities.
    */
  private[chessjudgment] def materializeOccurrence(
      occurrenceId: String,
      edge: MoveTransitionEdge,
      lineOwner: Option[LineNodeRef],
      lineEvidence: Option[EvidenceRef],
      outputRecords: List[EvidenceRecord],
      sourceNodesByOutputId: Map[String, List[CanonicalRelationNode]]
  ): List[EvidenceRecord] =
    require(
      edge.from == before.position && edge.to == after.position &&
        EvidenceRef.sameMove(edge.moveUci, transition.moveUci) &&
        PrincipalVariationEvidence.sameBoardState(edge.from.fen, transition.fenBefore) &&
        PrincipalVariationEvidence.sameBoardState(edge.to.fen, transition.fenAfter),
      "a relation occurrence must bind its exact canonical transition"
    )
    require(
      outputRecords.map(_.ref.id).distinct.size == outputRecords.size &&
        sourceNodesByOutputId.keySet == outputRecords.map(_.ref.id).toSet,
      "a relation occurrence needs one exact source binding per unique output"
    )
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
    val combinationOutputsBySemanticId = outputsBySemanticId.filter { case (semanticId, _) =>
      combinationSemanticIds(semanticId)
    }
    val closedResults = resultSemanticIdsByContract.foldLeft(
      VectorMap.empty[RelationCombinationContractKind, List[EvidenceRef]]
    ) { case (inventory, (contract, semanticIds)) =>
      val refs = semanticIds.map(semanticId => combinationOutputsBySemanticId(semanticId).head._1.ref)
      inventory.updated(contract, refs.sortBy(_.id))
    }
    val bindings = relationOutputs.map { case (record, relation) =>
      val sources = sourceNodesByOutputId(record.ref.id)
      sourceNodesFor(relation).foreach(expected =>
        require(
          sources.map(_.ref).toSet == expected.map(_.ref).toSet && sources.size == expected.size,
          s"combined relation '${relation.semanticId}' must preserve its exact premise occurrences"
        )
      )
      ClosedRelationOutputBinding.certified(record.ref, relation, sources.map(_.ref))
    }
    val occurrence = ClosedRelationOccurrenceEvidence.certified(
      edge,
      lineOwner,
      lineEvidence,
      closedResults,
      bindings
    )
    val carrier = ClosedRelationOccurrenceEvidence.record(occurrenceId, occurrence)
    carrier :: outputRecords.map { record =>
      val sources = sourceNodesByOutputId(record.ref.id).map(_.ref).sortBy(_.id)
      record.copy(parents = carrier.ref :: sources)
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
    val combinationBindings = semantic.combinationRelations.map { relation =>
      val proof = RelationWitnessDetail.combinationProof(relation.detail).getOrElse(
        throw IllegalArgumentException("a closed combination result needs its registered proof")
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
        s"combination result '${relation.semanticId}' repeats one exact premise occurrence"
      )
      val changedProofKeys = proof.premises.flatMap { premise =>
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
      }.distinct.sortBy(_.stableKey)
      require(
        changedProofKeys.nonEmpty && changedProofKeys == proof.proofKeys,
        s"combination result '${relation.semanticId}' does not own its exact transition proof keys"
      )
      relation.semanticId -> resolved.sortBy(_.ref.id)
    }
    val projectionBindings = semantic.projections.map { projection =>
      val expected = semanticDelta.newlyEstablishedNamedRays.filter(
        _.detail == projection.detail
      )
      require(
        expected.size == 1,
        s"named-ray projection '${projection.semanticId}' must match one exact canonical change"
      )
      val source = changesByKey
        .get((RelationChangeDirection.Established, RelationFactKind.RayBarrier, expected.head.semanticId))
        .map(_.sourceNode)
        .getOrElse(
          throw IllegalArgumentException(
            s"named-ray projection '${projection.semanticId}' lost its exact after-position source"
          )
        )
      projection.semanticId -> List(source)
    }
    val bindings = combinationBindings ++ projectionBindings
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
      bindings.foldLeft(VectorMap.empty[String, List[CanonicalRelationNode]]) {
        case (inventory, (semanticId, sources)) => inventory.updated(semanticId, sources)
      },
      semantic.resultSemanticIdsByContract
    )
