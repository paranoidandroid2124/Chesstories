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
        case RelationCombinationContractKind.GeometricSupporterCapture =>
          geometricSupporterCaptures(delta, rootMove)
        case RelationCombinationContractKind.GeometricSupportDelta =>
          geometricSupportDeltas(delta, rootMove)
        case RelationCombinationContractKind.SliderLineInterruption =>
          sliderLineInterruptions(delta, rootMove)
        case RelationCombinationContractKind.GeometricLineControlAfterBlockerRemoval =>
          geometricLineControlsAfterBlockerRemoval(delta, rootMove)
        case RelationCombinationContractKind.NamedRayTransition =>
          namedRayTransitions(delta, rootMove)
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
      case RelationCombinationContractKind.GeometricSupporterCapture =>
        delta.geometricSupporterCaptures.map(_.stableKey)
      case RelationCombinationContractKind.GeometricSupportDelta =>
        delta.geometricSupportTransition.changes.map(_.stableKey)
      case RelationCombinationContractKind.SliderLineInterruption =>
        delta.sliderLineInterruptions.map(_.stableKey)
      case RelationCombinationContractKind.GeometricLineControlAfterBlockerRemoval =>
        delta.geometricLineOpenings.map(_.stableKey)
      case RelationCombinationContractKind.NamedRayTransition =>
        delta.changedNamedRays.map(_.stableKey)

  private def activeContracts(
      delta: RelationSemanticDelta
  ): List[RelationCombinationContractKind] =
    val changedControlDependency =
      delta.transitionFootprint.changedSquares.nonEmpty ||
        delta.ofKind(RelationFactKind.GeometricControl).nonEmpty
    val removedControls = delta.removedOf(RelationFactKind.GeometricControl).nonEmpty
    val establishedControls = delta.establishedOf(RelationFactKind.GeometricControl).nonEmpty
    val removedRays = delta.removedOf(RelationFactKind.RayBarrier).nonEmpty
    val establishedRays = delta.establishedOf(RelationFactKind.RayBarrier).nonEmpty
    List(
      Option.when(changedControlDependency)(RelationCombinationContractKind.GeometricControlSetDelta),
      Option.when(delta.rootMove.capture.nonEmpty)(RelationCombinationContractKind.GeometricSupporterCapture),
      Option.when(changedControlDependency)(RelationCombinationContractKind.GeometricSupportDelta),
      Option.when(establishedRays && removedControls)(RelationCombinationContractKind.SliderLineInterruption),
      Option.when(removedRays && establishedControls)(
        RelationCombinationContractKind.GeometricLineControlAfterBlockerRemoval
      ),
      Option.when(removedRays || establishedRays)(RelationCombinationContractKind.NamedRayTransition)
    ).flatten

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

  private def geometricSupportDeltas(
      delta: RelationSemanticDelta,
      rootMove: CanonicalRootLegalMove
  ): List[RelationCombinationEmission] =
    def supporters(edges: List[GeometricSupportEdge]): List[RelationPieceWitness] =
      edges
        .map(edge => RelationPieceWitness(edge.supporter.square, edge.supporter.role))
        .sortBy(piece => piece.square.key -> piece.role.name)

    delta.geometricSupportTransition.changes.map { change =>
      val beforePremises = change.before.map(edge => beforeOccurrence(delta, edge.fact) -> edge.fact)
      val afterPremises = change.after.map(edge => afterOccurrence(delta, edge.fact) -> edge.fact)
      val proof = combinationProof(
        delta,
        RelationCombinationContractKind.GeometricSupportDelta,
        (RelationPremiseOccurrence.Before -> rootMove.fact) :: (beforePremises ++ afterPremises),
        List(change.supportedBefore.square, change.supportedAfter.square)
      )
      emission(change.stableKey, RelationWitnessDetail.GeometricSupportDelta(
        mover = rootMove.witness,
        supportedSide = change.supportedBefore.side,
        supportedBeforeSquare = change.supportedBefore.square,
        supportedBeforeRole = change.supportedBefore.role,
        supportedAfterSquare = change.supportedAfter.square,
        supportedAfterRole = change.supportedAfter.role,
        beforeSupporters = supporters(change.before),
        afterSupporters = supporters(change.after),
        removedSupporters = supporters(change.removed),
        establishedSupporters = supporters(change.established),
        proof = proof
      ), rootMove)
    }

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

  private def geometricSupporterCaptures(
      delta: RelationSemanticDelta,
      rootMove: CanonicalRootLegalMove
  ): List[RelationCombinationEmission] =
    delta.geometricSupporterCaptures.map { change =>
      val support = change.removedSupport
      val proof = combinationProof(
        delta,
        RelationCombinationContractKind.GeometricSupporterCapture,
        List(
          RelationPremiseOccurrence.Before -> rootMove.fact,
          RelationPremiseOccurrence.Removed -> support.fact
        )
      )
      emission(change.stableKey, RelationWitnessDetail.GeometricSupporterCapture(
        capturer = rootMove.witness,
        supporterSquare = support.supporter.square,
        supporterRole = support.supporter.role,
        supportedSquare = support.supported.square,
        supportedRole = support.supported.role,
        proof = proof
      ), rootMove)
    }

  private def sliderLineInterruptions(
      delta: RelationSemanticDelta,
      rootMove: CanonicalRootLegalMove
  ): List[RelationCombinationEmission] =
    delta.sliderLineInterruptions.map { change =>
      val proof = combinationProof(
        delta,
        RelationCombinationContractKind.SliderLineInterruption,
        List(
          RelationPremiseOccurrence.Before -> rootMove.fact,
          RelationPremiseOccurrence.Established -> change.establishedBarrier
        ) ++ change.removedControls.map(control =>
          RelationPremiseOccurrence.Removed -> control.fact
        )
      )
      emission(change.stableKey, RelationWitnessDetail.SliderLineInterruption(
        interposer = change.interposer,
        controllerSide = change.controllerSide,
        controllerBeforeSquare = change.controllerBefore.square,
        controllerAfterSquare = change.controllerAfter.square,
        controllerRole = change.controllerAfter.role,
        interruptedTargets = change.removedControls.map(control =>
          RelationControlReachWitness(control.target, control.targetState)
        ),
        proof = proof
      ), rootMove)
    }

  private def geometricLineControlsAfterBlockerRemoval(
      delta: RelationSemanticDelta,
      rootMove: CanonicalRootLegalMove
  ): List[RelationCombinationEmission] =
    delta.geometricLineOpenings.map { change =>
      val proof = combinationProof(
        delta,
        RelationCombinationContractKind.GeometricLineControlAfterBlockerRemoval,
        List(
          RelationPremiseOccurrence.Before -> rootMove.fact,
          RelationPremiseOccurrence.Removed -> change.removedBarrier
        ) ++ change.openedControls.map(control =>
          RelationPremiseOccurrence.Established -> control.fact
        )
      )
      emission(change.stableKey, RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(
        mover = change.removingMove,
        controllerSide = change.controllerSide,
        controllerBeforeSquare = change.controllerBefore,
        controllerAfterSquare = change.controllerAfter,
        controllerRole = change.controllerRole,
        blockerSquare = change.blocker.square,
        blockerRole = change.blocker.role,
        openedTargets = change.openedControls.map(control =>
          RelationControlReachWitness(control.target, control.targetState)
        ),
        barrierPattern = change.barrierPattern,
        removalMode = change.removalMode,
        proof = proof
      ), rootMove)
    }

  private def namedRayTransitions(
      delta: RelationSemanticDelta,
      rootMove: CanonicalRootLegalMove
  ): List[RelationCombinationEmission] =
    delta.changedNamedRays.map { named =>
      val change = named.change
      val projection = named.projection
      val proof = combinationProof(
        delta,
        RelationCombinationContractKind.NamedRayTransition,
        List(
          RelationPremiseOccurrence.Before -> rootMove.fact,
          (change.direction match
            case RelationChangeDirection.Established => RelationPremiseOccurrence.Established
            case RelationChangeDirection.Removed     => RelationPremiseOccurrence.Removed
          ) -> change.relation
        )
      )
      emission(named.stableKey, RelationWitnessDetail.NamedRayTransition(
        mover = rootMove.witness,
        side = projection.side,
        attackerSquare = projection.attackerSquare,
        attackerRole = projection.attackerRole,
        barrier = projection.barrier,
        immediateTarget = projection.immediateTarget,
        axis = projection.axis,
        pattern = projection.pattern,
        direction = change.direction,
        proof = proof
      ), rootMove)
    }
