package lila.chessjudgment.analysis.tactical

import _root_.chess.{ Color, King }

import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.position.BoardTransitionFootprint

private[chessjudgment] object TacticalRelationEvidence:

  private[chessjudgment] final class RelationProduction private (
      val closed: ClosedRelationTransitionInventory,
      val projections: List[RelationFactEvidence],
      val transition: LineReplayStep,
      val delta: RelationSemanticDelta
  ):
    val relations: List[RelationFactEvidence] = closed.allRelations
    require(
      projections.sortBy(_.semanticId) == closed.projections,
      "relation production must expose the exact closed named-ray projection inventory"
    )
    require(
      relations.map(_.semanticId).distinct.size == relations.size,
      "one tactical relation production cannot repeat relation semantics"
    )

    def bindClosedOutput(
        before: CanonicalPositionRelationSnapshot,
        after: CanonicalPositionRelationSnapshot,
        canonicalDelta: CanonicalRelationDelta
    ): CanonicalRelationTransitionInventory =
      closed.bind(before, after, canonicalDelta)

  private object RelationProduction:
    def certified(
        closed: ClosedRelationTransitionInventory,
        projections: List[RelationFactEvidence],
        transition: LineReplayStep,
        delta: RelationSemanticDelta
    ): RelationProduction =
      new RelationProduction(closed, projections, transition, delta)

  private final case class EstablishedEnemyControl(
      fact: RelationFactEvidence,
      side: Color,
      witness: RelationEnemyControlWitness
  )

  private final case class TacticalTransitionContext(
      replayTransition: CanonicalReplayTransition
  ):
    val before = replayTransition.beforeAnalysis
    val after = replayTransition.afterAnalysis

    val boardFootprint: BoardTransitionFootprint = replayTransition.boardFootprint

    val relationDelta = replayTransition.relationDelta

    private val geometricControlTransition = relationDelta.geometricControlTransition

    val establishedGeometricControls: List[RelationFactEvidence] =
      geometricControlTransition.newlyEstablished

    val geometricSupportChanges: List[GeometricSupportChange] =
      relationDelta.geometricSupportTransition.changes

    val geometricControlSetChanges: List[GeometricControlTargetSetChange] =
      geometricControlTransition.targetSetChanges

    val establishedEnemyControls: List[EstablishedEnemyControl] =
      establishedGeometricControls.flatMap { fact =>
        fact.detail match
          case RelationWitnessDetail.GeometricControl(
                side,
                controller,
                controllerRole,
                target,
                RelationControlTarget.Enemy(targetRole)
              ) =>
            List(
              EstablishedEnemyControl(
                fact,
                side,
                RelationEnemyControlWitness(controller, controllerRole, target, targetRole)
              )
            )
          case _ => Nil
      }.sortBy(control =>
        (
          control.side.toString,
          control.witness.controllerSquare.key,
          control.witness.controllerRole.name,
          control.witness.targetSquare.key,
          control.witness.targetRole.name
        )
      )

    val newlyEstablishedNamedRayBarriers: List[RelationFactEvidence] =
      relationDelta.newlyEstablishedNamedRays.map(_.relation)

    def combinationProof(
        contract: RelationCombinationContractKind,
        premises: List[(RelationPremiseOccurrence, RelationFactEvidence)]
    ): RelationCombinationProof =
      val changedIds = premises.collect {
        case (RelationPremiseOccurrence.Removed | RelationPremiseOccurrence.Established, relation) =>
          relation.semanticId
      }.distinct
      val changed = changedIds.map(semanticId =>
        relationDelta.changeBySemanticId(semanticId).getOrElse(
          throw IllegalArgumentException(
            s"combined tactical premise '$semanticId' is not a canonical relation change"
          )
        )
      )
      val combinationProofKeys = changed.flatMap(_.proofKeys).distinct.sortBy(_.stableKey)
      require(
        changed.nonEmpty && combinationProofKeys.nonEmpty,
        "combined tactical premises must belong to one exact canonical relation transition"
      )
      RelationCombinationProof.from(contract, premises, combinationProofKeys)

    def beforeOccurrence(fact: RelationFactEvidence): RelationPremiseOccurrence =
      relationDelta.changeBySemanticId(fact.semanticId) match
        case Some(change) if change.direction == RelationChangeDirection.Removed =>
          RelationPremiseOccurrence.Removed
        case _ => RelationPremiseOccurrence.Before

    def afterOccurrence(fact: RelationFactEvidence): RelationPremiseOccurrence =
      relationDelta.changeBySemanticId(fact.semanticId) match
        case Some(change) if change.direction == RelationChangeDirection.Established =>
          RelationPremiseOccurrence.Established
        case _ => RelationPremiseOccurrence.After

    def rootLegalMove(moveUci: String): Option[CanonicalRootLegalMove] =
      Option.when(EvidenceRef.sameMove(relationDelta.rootMove.moveUci, moveUci))(
        relationDelta.rootMove
      )

  private def relation(
      detail: RelationWitnessDetail,
      lineMoves: List[String]
  ): RelationFactEvidence =
    RelationFactEvidence.from(detail, lineMoves)

  private def combinedTransitionResults(
      transition: TacticalTransitionContext,
      rootMove: CanonicalRootLegalMove
  ): ClosedRelationCombinationResults =
    ClosedRelationCombinationResults.exact(
      geometricControlSetDeltas = geometricControlSetDeltas(transition, rootMove),
      geometricSupporterCaptures = geometricSupporterCaptures(transition, rootMove),
      geometricSupportDeltas = geometricSupportDeltas(transition, rootMove),
      sliderControlInterferences = sliderControlInterferences(transition, rootMove),
      geometricLineControlsAfterBlockerRemoval = geometricLineControlsAfterBlockerRemoval(transition, rootMove),
      checkingEnemyControlBundles = checkingEnemyControlBundle(transition, rootMove).toList,
      doubleChecks = doubleCheckWitness(transition, rootMove).toList
    )

  private def geometricSupportDeltas(
      transition: TacticalTransitionContext,
      rootMove: CanonicalRootLegalMove
  ): List[RelationFactEvidence] =
    transition.geometricSupportChanges.map { change =>
      val beforePremises = change.before.map { edge =>
        transition.beforeOccurrence(edge.fact) -> edge.fact
      }
      val afterPremises = change.after.map { edge =>
        transition.afterOccurrence(edge.fact) -> edge.fact
      }
      val proof = transition.combinationProof(
        RelationCombinationContractKind.GeometricSupportDelta,
        (RelationPremiseOccurrence.Before -> rootMove.fact) :: (beforePremises ++ afterPremises)
      )
      relation(
        RelationWitnessDetail.GeometricSupportDelta(
          mover = rootMove.witness,
          supportedSide = change.supportedBefore.side,
          supportedBeforeSquare = change.supportedBefore.square,
          supportedBeforeRole = change.supportedBefore.role,
          supportedAfterSquare = change.supportedAfter.square,
          supportedAfterRole = change.supportedAfter.role,
          beforeSupporters = change.before.map(edge =>
            RelationPieceWitness(edge.supporter.square, edge.supporter.role)
          ),
          afterSupporters = change.after.map(edge =>
            RelationPieceWitness(edge.supporter.square, edge.supporter.role)
          ),
          removedSupporters = change.removed.map(edge =>
            RelationPieceWitness(edge.supporter.square, edge.supporter.role)
          ),
          establishedSupporters = change.established.map(edge =>
            RelationPieceWitness(edge.supporter.square, edge.supporter.role)
          ),
          proof = proof
        ),
        Nil
      )
    }.sortBy(_.semanticId)

  private def geometricControlSetDeltas(
      transition: TacticalTransitionContext,
      rootMove: CanonicalRootLegalMove
  ): List[RelationFactEvidence] =
    transition.geometricControlSetChanges.map { change =>
      val proof = transition.combinationProof(
        RelationCombinationContractKind.GeometricControlSetDelta,
        (RelationPremiseOccurrence.Before -> rootMove.fact) ::
          (change.before.map(edge => transition.beforeOccurrence(edge.fact) -> edge.fact) ++
            change.after.map(edge => transition.afterOccurrence(edge.fact) -> edge.fact))
      )
      relation(
        RelationWitnessDetail.GeometricControlSetDelta(
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
        ),
        Nil
      )
    }.sortBy(_.semanticId)

  private def geometricSupporterCaptures(
      transition: TacticalTransitionContext,
      rootMove: CanonicalRootLegalMove
  ): List[RelationFactEvidence] =
    transition.relationDelta.geometricCombinationTransition.supporterCaptures.map { change =>
      val support = change.removedSupport
      val proof = transition.combinationProof(
        RelationCombinationContractKind.GeometricSupporterCapture,
        List(
          RelationPremiseOccurrence.Before -> rootMove.fact,
          RelationPremiseOccurrence.Removed -> support.fact
        )
      )
      relation(
        RelationWitnessDetail.GeometricSupporterCapture(
          capturer = rootMove.witness,
          supporterSquare = support.supporter.square,
          supporterRole = support.supporter.role,
          supportedSquare = support.supported.square,
          supportedRole = support.supported.role,
          proof = proof
        ),
        Nil
      )
    }

  private def sliderControlInterferences(
      transition: TacticalTransitionContext,
      rootMove: CanonicalRootLegalMove
  ): List[RelationFactEvidence] =
    transition.relationDelta.geometricCombinationTransition.controlInterferences.map { change =>
      val control = change.removedControl
      val proof = transition.combinationProof(
        RelationCombinationContractKind.SliderControlInterference,
        List(
          RelationPremiseOccurrence.Before -> rootMove.fact,
          RelationPremiseOccurrence.Removed -> control.fact,
          RelationPremiseOccurrence.Established -> change.establishedBarrier
        )
      )
      relation(
        RelationWitnessDetail.SliderControlInterference(
          interposer = change.interposer,
          controllerSide = control.side,
          controllerSquare = control.controller.square,
          controllerRole = control.controller.role,
          targetSquare = control.target,
          target = control.targetState,
          proof = proof
        ),
        Nil
      )
    }

  private def geometricLineControlsAfterBlockerRemoval(
      transition: TacticalTransitionContext,
      rootMove: CanonicalRootLegalMove
  ): List[RelationFactEvidence] =
    transition.relationDelta.geometricCombinationTransition.lineOpenings.map { change =>
      val proof = transition.combinationProof(
        RelationCombinationContractKind.GeometricLineControlAfterBlockerRemoval,
        List(
          RelationPremiseOccurrence.Before -> rootMove.fact,
          RelationPremiseOccurrence.Removed -> change.removedBarrier,
          RelationPremiseOccurrence.Established -> change.establishedControl
        )
      )
      relation(
        RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(
          mover = change.removingMove,
          controllerSide = change.controllerSide,
          controllerBeforeSquare = change.controllerBefore,
          controllerAfterSquare = change.controllerAfter,
          controllerRole = change.controllerRole,
          blockerSquare = change.blocker.square,
          blockerRole = change.blocker.role,
          targetSquare = change.target,
          target = change.targetState,
          barrierPattern = change.barrierPattern,
          removalMode = change.removalMode,
          proof = proof
        ),
        Nil
      )
    }

  private def checkingEnemyControlBundle(
      transition: TacticalTransitionContext,
      rootMove: CanonicalRootLegalMove
  ): Option[RelationFactEvidence] =
    val controls = transition.establishedEnemyControls.filter(_.side == rootMove.side)
    val kingControls = controls.filter(_.witness.targetRole.name.equalsIgnoreCase(King.name))
    val otherEnemyControls = controls.filterNot(_.witness.targetRole.name.equalsIgnoreCase(King.name))
    Option.when(
      kingControls.nonEmpty && otherEnemyControls.nonEmpty &&
        kingControls.map(_.witness.targetSquare).distinct.size == 1
    ) {
      val proof = transition.combinationProof(
        RelationCombinationContractKind.CheckingEnemyControlBundle,
        (RelationPremiseOccurrence.Before -> rootMove.fact) ::
          controls.map(control => RelationPremiseOccurrence.Established -> control.fact)
      )
      relation(
        RelationWitnessDetail.CheckingEnemyControlBundle(
          mover = rootMove.witness,
          kingControls = kingControls.map(_.witness),
          otherEnemyControls = otherEnemyControls.map(_.witness),
          proof = proof
        ),
        Nil
      )
    }

  def relationProduction(
      replay: CanonicalLineReplay,
      playedMove: String
  ): RelationProduction =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    val firstReplayStep = replay.replaySteps.headOption.getOrElse(
      throw IllegalArgumentException("a tactical relation production needs one replay transition")
    )
    val replayTransition = replay.transition(firstReplayStep).getOrElse(
      throw IllegalArgumentException("the tactical replay root has no canonical transition")
    )
    val first = replayTransition.legal
    if first.uci != normalizedPlayed then
      throw IllegalArgumentException(s"'$playedMove' is not the canonical replay root move")
    val transition = TacticalTransitionContext(replayTransition)
    val rootMove = transition.rootLegalMove(first.uci).getOrElse(
      throw IllegalArgumentException(s"'$playedMove' is absent from its closed legal-move inventory")
    )
    val unverified = combinedTransitionResults(transition, rootMove)
    val certifiedRelations = RelationFactEvidence
      .certifiedTacticalBatch(
        unverified.relations,
        replayTransition
      )
      .getOrElse(
        throw IllegalArgumentException("a closed tactical relation batch failed legal-replay certification")
      )
    val combinations = unverified.replaceWithCertified(certifiedRelations)
    val projections = rootRayWitnesses(
      transition.newlyEstablishedNamedRayBarriers,
      replayTransition
    )
    val closed = ClosedRelationTransitionInventory.close(
      replayTransition.declared,
      transition.relationDelta,
      combinations,
      projections
    )
    RelationProduction.certified(
      closed,
      projections,
      replayTransition.declared,
      transition.relationDelta
    )

  private def rootRayWitnesses(
      newlyNamedRays: List[RelationFactEvidence],
      transition: CanonicalReplayTransition
  ): List[RelationFactEvidence] =
    RelationFactEvidence
      .certifiedRootAfterProjections(newlyNamedRays, transition)
      .getOrElse(
        throw IllegalArgumentException("root-after ray projections lost their exact closed source occurrence")
      )

  private def doubleCheckWitness(
      transition: TacticalTransitionContext,
      rootMove: CanonicalRootLegalMove
  ): Option[RelationFactEvidence] =
    val kingChecks = transition.establishedEnemyControls.collect {
      case control
          if control.side == rootMove.side && control.witness.targetRole.name.equalsIgnoreCase(King.name) =>
        (
          control.witness.targetSquare,
          control.fact,
          RelationPieceWitness(control.witness.controllerSquare, control.witness.controllerRole)
        )
    }
    val kingGroups = kingChecks
      .groupMap(_._1) { case (_, fact, checker) => fact -> checker }
      .toList
      .sortBy(_._1.key)
    require(
      kingGroups.size <= 1,
      "one legal destination inventory cannot expose more than one enemy king target"
    )
    kingGroups match
      case (king, rawCheckers) :: Nil if rawCheckers.map(_._2).distinct.size >= 2 =>
        val checkers = rawCheckers.sortBy { case (_, checker) =>
          (checker.square.key, checker.role.name)
        }
        val proof = transition.combinationProof(
          RelationCombinationContractKind.DoubleCheck,
          (RelationPremiseOccurrence.Before -> rootMove.fact) ::
            checkers.map { case (fact, _) => RelationPremiseOccurrence.Established -> fact }
        )
        Some(
          relation(
            RelationWitnessDetail.DoubleCheck(
              mover = rootMove.witness,
              kingSquare = king,
              checkers = checkers.map(_._2),
              proof = proof
            ),
            Nil
          )
        )
      case _ => None
