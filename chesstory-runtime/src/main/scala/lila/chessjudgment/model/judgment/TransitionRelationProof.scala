package lila.chessjudgment.model.judgment

/** Opaque output of the sole L0.5 transition-contract producer. Callers can
  * consume or certify this batch, but cannot assemble arbitrary details and
  * attach transition authority afterward.
  */
private[chessjudgment] final class TransitionRelationContractBatch private (
    private[judgment] val results: ClosedRelationCombinationResults,
    private val owner: RelationSemanticDelta
):
  private[judgment] def owns(delta: RelationSemanticDelta): Boolean =
    owner.asInstanceOf[AnyRef] eq delta.asInstanceOf[AnyRef]

private[chessjudgment] object TransitionRelationContractBatch:
  private[judgment] def derive(delta: RelationSemanticDelta): TransitionRelationContractBatch =
    val rootMove = delta.rootMove
    val results = ClosedRelationCombinationResults.demand(
      activeContracts = activeContracts(delta),
      expectedSourceKeys = contract => sourceKeys(delta, contract),
      derive = {
        case RelationCombinationContractKind.GeometricControlSetDelta =>
          geometricControlSetDeltas(delta, rootMove)
      }
    )
    new TransitionRelationContractBatch(results, delta)

  private def sourceKeys(
      delta: RelationSemanticDelta,
      contract: RelationCombinationContractKind
  ): List[String] =
    contract match
      case RelationCombinationContractKind.GeometricControlSetDelta =>
        delta.geometricControlTransition.targetSetChanges.map(_.stableKey)

  private def activeContracts(
      delta: RelationSemanticDelta
  ): List[RelationCombinationContractKind] =
    val changedControlDependency =
      delta.transitionFootprint.changedSquares.nonEmpty ||
        delta.ofKind(RelationFactKind.GeometricControl).nonEmpty
    Option.when(changedControlDependency)(RelationCombinationContractKind.GeometricControlSetDelta).toList

  private def relation(
      detail: RelationWitnessDetail,
      rootMove: CanonicalRootLegalMove
  ): RelationFactEvidence =
    RelationFactEvidence.from(detail, List(rootMove.moveUci))

  private def emission(
      sourceKey: String,
      detail: RelationWitnessDetail,
      rootMove: CanonicalRootLegalMove
  ): RelationCombinationEmission =
    RelationCombinationEmission(sourceKey, relation(detail, rootMove))

  private def combinationProof(
      delta: RelationSemanticDelta,
      contract: RelationCombinationContractKind,
      premises: List[(RelationPremiseOccurrence, RelationFactEvidence)],
      changedStateSquares: List[EvidenceSquare] = Nil
  ): RelationCombinationProof =
    val changedIds = premises.collect {
      case (RelationPremiseOccurrence.Removed | RelationPremiseOccurrence.Established, source) =>
        source.semanticId
    }
    require(
      changedIds.distinct.size == changedIds.size,
      "a transition proof cannot repeat one changed relation premise"
    )
    val changed = changedIds.map(semanticId =>
      delta.changeBySemanticId(semanticId).getOrElse(
        throw IllegalArgumentException(
          s"combined transition premise '$semanticId' is not a canonical relation change"
        )
      )
    )
    val proofKeys =
      (changed.flatMap(_.proofKeys) ++
        RelationProofKey.forDependencySquares(changedStateSquares, delta.transitionFootprint))
        .toSet
        .toList
        .sortBy(_.stableKey)
    require(
      proofKeys.nonEmpty,
      "combined transition premises must belong to one exact canonical relation transition"
    )
    RelationCombinationProof.from(contract, premises, proofKeys)

  private def beforeOccurrence(
      delta: RelationSemanticDelta,
      fact: RelationFactEvidence
  ): RelationPremiseOccurrence =
    delta.changeBySemanticId(fact.semanticId) match
      case Some(change) if change.direction == RelationChangeDirection.Removed =>
        RelationPremiseOccurrence.Removed
      case _ => RelationPremiseOccurrence.Before

  private def afterOccurrence(
      delta: RelationSemanticDelta,
      fact: RelationFactEvidence
  ): RelationPremiseOccurrence =
    delta.changeBySemanticId(fact.semanticId) match
      case Some(change) if change.direction == RelationChangeDirection.Established =>
        RelationPremiseOccurrence.Established
      case _ => RelationPremiseOccurrence.After

  private def geometricControlSetDeltas(
      delta: RelationSemanticDelta,
      rootMove: CanonicalRootLegalMove
  ): List[RelationCombinationEmission] =
    delta.geometricControlTransition.targetSetChanges.map { change =>
      val proof = combinationProof(
        delta,
        RelationCombinationContractKind.GeometricControlSetDelta,
        (RelationPremiseOccurrence.Before -> rootMove.fact) ::
          (change.before.map(edge => beforeOccurrence(delta, edge.fact) -> edge.fact) ++
            change.after.map(edge => afterOccurrence(delta, edge.fact) -> edge.fact)),
        List(change.target)
      )
      emission(change.stableKey, RelationWitnessDetail.GeometricControlSetDelta(
        mover = rootMove.witness,
        controllingSide = change.side,
        targetSquare = change.target,
        beforeTarget = change.beforeState,
        afterTarget = change.afterState,
        beforeControllers = change.before.map(_.controller),
        afterControllers = change.after.map(_.controller),
        removedControllers = change.removedControllers,
        establishedControllers = change.establishedControllers,
        proof = proof
      ), rootMove)
    }
