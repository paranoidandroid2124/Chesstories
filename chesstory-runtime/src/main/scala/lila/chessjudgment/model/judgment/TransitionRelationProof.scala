package lila.chessjudgment.model.judgment

/** Exact geometric-control target-set changes produced by one canonical
  * transition. The result owns its source delta and, after replay
  * certification, the exact relation instances consumed downstream.
  */
private[chessjudgment] final class ControlSetDeltaResults private (
    rawRelations: List[RelationFactEvidence],
    private val deltaOwner: RelationSemanticDelta,
    certify: RelationFactEvidence => RelationFactEvidence,
    certificationOwner: Option[RelationReplayCertificationOwner]
):
  lazy val relations: List[RelationFactEvidence] = rawRelations.map(certify)

  private[judgment] def owns(delta: RelationSemanticDelta): Boolean =
    deltaOwner.asInstanceOf[AnyRef].eq(delta.asInstanceOf[AnyRef])

  private[judgment] def ownsOutput(relation: RelationFactEvidence): Boolean =
    relations.exists(expected => expected.asInstanceOf[AnyRef].eq(relation.asInstanceOf[AnyRef]))

  private[judgment] def certifiedBy(
      owner: RelationReplayCertificationOwner,
      certification: RelationFactEvidence => RelationFactEvidence
  ): ControlSetDeltaResults =
    require(certificationOwner.isEmpty, "control-set deltas cannot replace replay certification authority")
    new ControlSetDeltaResults(rawRelations, deltaOwner, certification, Some(owner))

  private[judgment] def certifiedFor(owner: RelationReplayCertificationOwner): Boolean =
    certificationOwner.contains(owner)

private[chessjudgment] object ControlSetDeltaResults:
  private[judgment] def derive(delta: RelationSemanticDelta): ControlSetDeltaResults =
    val expectedSourceKeys = delta.geometricControlTransition.targetSetChanges.map(_.stableKey).sorted
    require(
      expectedSourceKeys.distinct.size == expectedSourceKeys.size,
      "geometric-control target-set changes must have unique source keys"
    )
    val emissions = delta.geometricControlTransition.targetSetChanges.map { change =>
      change.stableKey -> relation(
        RelationWitnessDetail.GeometricControlSetDelta(
          mover = delta.rootMove.witness,
          controllingSide = change.side,
          targetSquare = change.target,
          beforeTarget = change.beforeState,
          afterTarget = change.afterState,
          beforeControllers = change.before.map(_.controller),
          afterControllers = change.after.map(_.controller),
          removedControllers = change.removedControllers,
          establishedControllers = change.establishedControllers,
          proof = combinationProof(
            delta,
            (RelationPremiseOccurrence.Before -> delta.rootMove.fact) ::
              (change.before.map(edge => beforeOccurrence(delta, edge.fact) -> edge.fact) ++
                change.after.map(edge => afterOccurrence(delta, edge.fact) -> edge.fact)),
            List(change.target)
          )
        ),
        delta.rootMove
      )
    }.sortBy(_._1)
    val emittedSourceKeys = emissions.map(_._1)
    require(
      emittedSourceKeys.distinct.size == emittedSourceKeys.size && emittedSourceKeys == expectedSourceKeys,
      "geometric-control target-set changes were not consumed exactly once"
    )
    val results = emissions.map(_._2).sortBy(_.semanticId)
    require(
      results.forall(_.detail.isInstanceOf[RelationWitnessDetail.GeometricControlSetDelta]),
      "control-set delta results may contain only geometric-control target-set deltas"
    )
    require(
      results.map(_.semanticId).distinct.size == results.size,
      "control-set deltas cannot repeat relation semantics"
    )
    new ControlSetDeltaResults(results, delta, identity, None)

  private def relation(
      detail: RelationWitnessDetail,
      rootMove: CanonicalRootLegalMove
  ): RelationFactEvidence =
    RelationFactEvidence.from(detail, List(rootMove.moveUci))

  private def combinationProof(
      delta: RelationSemanticDelta,
      premises: List[(RelationPremiseOccurrence, RelationFactEvidence)],
      changedStateSquares: List[EvidenceSquare]
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
    RelationCombinationProof.from(premises, proofKeys)

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
