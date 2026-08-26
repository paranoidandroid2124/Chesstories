package lila.chessjudgment.model.judgment

import chess.{ Bitboard, Color, King, Rank, Rook, Square }

import lila.chessjudgment.analysis.position.PositionRelationExtractor.PositionRelationInventoryCertificate
import lila.chessjudgment.model.line.{ LegalReplayStep, PrincipalVariationEvidence }
import lila.chessjudgment.model.position.{ BoardGeometry, BoardPieceTransition, BoardTransitionFootprint }

private[chessjudgment] enum RelationChangeDirection:
  case Established
  case Removed

private[chessjudgment] final case class RelationChangeKey(
    direction: RelationChangeDirection,
    kind: RelationFactKind,
    semanticId: String
):
  def stableKey: String =
    s"${direction.toString.toLowerCase}:${RelationFactKind.id(kind)}:$semanticId"

/** Exact occurrence of one canonical relation change in an admitted line.
  *
  * The semantic relation id alone is position-agnostic. Keeping the replay
  * step prevents a later proof from silently treating the same-shaped edge in
  * another position as the occurrence that established or removed it.
  */
private[chessjudgment] final case class ReplayRelationChangeWitness private (
    step: LineReplayStep,
    changeKey: RelationChangeKey
):
  def stableKey: String =
    ReplayRelationChangeWitness.product(
      "replay-relation-change",
      List(
        step.ply.toString,
        PrincipalVariationEvidence.normalizeUci(step.moveUci),
        Option(step.fenBefore).getOrElse("").trim,
        Option(step.fenAfter).getOrElse("").trim,
        changeKey.stableKey
      )
    )

  def resolve(replay: CanonicalLineReplay): Option[RelationSemanticChange] =
    replay.transition(step).flatMap { transition =>
      transition.relationDelta.changes.filter(_.key == changeKey) match
        case change :: Nil => Some(change)
        case _             => None
    }

private[chessjudgment] object ReplayRelationChangeWitness:
  def certify(
      replay: CanonicalLineReplay,
      step: LineReplayStep,
      change: RelationSemanticChange
  ): Option[ReplayRelationChangeWitness] =
    val witness = ReplayRelationChangeWitness(step, change.key)
    Option.when(witness.resolve(replay).contains(change))(witness)

  private def product(label: String, values: List[String]): String =
    (label :: values).map(value => s"${value.length}:$value").mkString

private[chessjudgment] enum RelationProofKey:
  case ChangedSquare(square: EvidenceSquare)
  case MovedPiece(
      side: Color,
      beforeRole: EvidencePieceRole,
      afterRole: EvidencePieceRole,
      from: EvidenceSquare,
      to: EvidenceSquare
  )

  def stableKey: String =
    this match
      case ChangedSquare(square) =>
        s"square:${square.key.toLowerCase}"
      case MovedPiece(side, beforeRole, afterRole, from, to) =>
        s"mover:${side.toString.toLowerCase}:${beforeRole.name.toLowerCase}>${afterRole.name.toLowerCase}:${from.key.toLowerCase}-${to.key.toLowerCase}"

private[chessjudgment] object RelationProofKey:
  /** The only owner of transition proof-key derivation. Both semantic delta
    * production and graph ancestry verification consume this exact function.
    */
  def forDependencySquares(
      dependencies: List[EvidenceSquare],
      footprint: BoardTransitionFootprint
  ): List[RelationProofKey] =
    val dependencyKeys = dependencies.map(_.key.toLowerCase).toSet
    val changed = footprint.changedSquares
      .filter(square => dependencyKeys(square.key.toLowerCase))
      .map(square => RelationProofKey.ChangedSquare(EvidenceSquare(square.key)))
    val movers = footprint.pieceTransitions.collect {
      case transition
          if dependencyKeys(transition.from.key.toLowerCase) ||
            dependencyKeys(transition.to.key.toLowerCase) =>
        RelationProofKey.MovedPiece(
          transition.side,
          EvidencePieceRole(transition.beforeRole.name),
          EvidencePieceRole(transition.afterRole.name),
          EvidenceSquare(transition.from.key),
          EvidenceSquare(transition.to.key)
        )
    }
    (changed ++ movers).distinct.sortBy(_.stableKey)

private[chessjudgment] final case class RelationSemanticChange(
    direction: RelationChangeDirection,
    relation: RelationFactEvidence,
    dependencySquares: List[EvidenceSquare],
    proofKeys: List[RelationProofKey]
):
  def kind: RelationFactKind = relation.kind
  def detail: RelationWitnessDetail = relation.detail
  def semanticId: String = relation.semanticId
  def participants: List[RelationParticipant] = relation.participants
  def targetSquares: List[EvidenceSquare] = relation.targetSquares
  def files: List[EvidenceFile] = relation.files
  def key: RelationChangeKey = RelationChangeKey(direction, kind, semanticId)
  def stableKey: String = key.stableKey

private[chessjudgment] final case class RelationSemanticDelta(
    moveUci: String,
    moverSide: Color,
    changedSquares: List[EvidenceSquare],
    changes: List[RelationSemanticChange],
    rootMove: CanonicalRootLegalMove,
    beforeInventory: PositionRelationInventoryCertificate,
    afterInventory: PositionRelationInventoryCertificate,
    geometricControlTransition: GeometricControlRelationTransition,
    geometricSupportTransition: GeometricSupportRelationTransition,
    geometricCombinationTransition: GeometricRelationCombinationTransition,
    newlyEstablishedNamedRays: List[RelationSemanticChange]
):
  require(changes.map(_.stableKey).distinct.size == changes.size, "duplicate semantic relation changes")
  require(changes.map(_.semanticId).distinct.size == changes.size, "one relation semantic cannot change twice")

  private lazy val byDirectionAndKind: Map[(RelationChangeDirection, RelationFactKind), List[RelationSemanticChange]] =
    changes.groupMap(change => change.direction -> change.kind)(identity)

  private lazy val bySemanticId: Map[String, RelationSemanticChange] =
    changes.map(change => change.semanticId -> change).toMap

  lazy val established: List[RelationSemanticChange] =
    changes.filter(_.direction == RelationChangeDirection.Established)

  lazy val removed: List[RelationSemanticChange] =
    changes.filter(_.direction == RelationChangeDirection.Removed)

  def establishedOf(kind: RelationFactKind): List[RelationSemanticChange] =
    byDirectionAndKind.getOrElse(RelationChangeDirection.Established -> kind, Nil)

  def removedOf(kind: RelationFactKind): List[RelationSemanticChange] =
    byDirectionAndKind.getOrElse(RelationChangeDirection.Removed -> kind, Nil)

  def ofKind(kind: RelationFactKind): List[RelationSemanticChange] =
    removedOf(kind) ++ establishedOf(kind)

  def changeBySemanticId(semanticId: String): Option[RelationSemanticChange] =
    bySemanticId.get(semanticId)

private[chessjudgment] final case class CanonicalRootLegalMove(
    fact: RelationFactEvidence,
    movement: BoardPieceTransition,
    moveUci: String,
    capture: Option[RelationLegalCaptureWitness]
):
  def side: Color = movement.side
  def from: EvidenceSquare = EvidenceSquare(movement.from.key)
  def to: EvidenceSquare = EvidenceSquare(movement.to.key)
  def beforeRole: EvidencePieceRole = EvidencePieceRole(movement.beforeRole.name)
  def afterRole: EvidencePieceRole = EvidencePieceRole(movement.afterRole.name)
  def witness: RelationMoveTransitionWitness =
    RelationMoveTransitionWitness(side, from, to, beforeRole, afterRole)

private[chessjudgment] final case class GeometricControlKey(
    side: Color,
    controller: EvidenceSquare,
    controllerRole: EvidencePieceRole,
    target: EvidenceSquare
)

private[chessjudgment] final case class GeometricControlTargetStateTransition(
    key: GeometricControlKey,
    before: RelationControlTarget,
    after: RelationControlTarget,
    removedFact: RelationFactEvidence,
    establishedFact: RelationFactEvidence
):
  require(before != after, "a geometric-control target-state transition must change occupancy state")

private[chessjudgment] final case class GeometricControlRelationTransition(
    targetStateChanges: List[GeometricControlTargetStateTransition],
    newlyEstablished: List[RelationFactEvidence],
    targetSetChanges: List[GeometricControlTargetSetChange]
)

private[chessjudgment] final case class GeometricControlEdge(
    fact: RelationFactEvidence,
    side: Color,
    controller: RelationPieceWitness,
    target: EvidenceSquare,
    targetState: RelationControlTarget
)

private[chessjudgment] final case class GeometricControlTargetSetChange(
    side: Color,
    target: EvidenceSquare,
    beforeState: RelationControlTarget,
    afterState: RelationControlTarget,
    before: List[GeometricControlEdge],
    after: List[GeometricControlEdge],
    removedControllers: List[RelationPieceWitness],
    establishedControllers: List[RelationPieceWitness]
):
  lazy val removed: List[GeometricControlEdge] =
    before.filter(edge => removedControllers.contains(edge.controller)).sortBy(_.fact.semanticId)

  lazy val established: List[GeometricControlEdge] =
    after.filter(edge => establishedControllers.contains(edge.controller)).sortBy(_.fact.semanticId)

private[chessjudgment] final case class GeometricSupportEdge(
    fact: RelationFactEvidence,
    supporter: RelationColoredPieceWitness,
    supported: RelationColoredPieceWitness
)

private[chessjudgment] final case class GeometricSupportChange(
    supportedBefore: RelationColoredPieceWitness,
    supportedAfter: RelationColoredPieceWitness,
    before: List[GeometricSupportEdge],
    after: List[GeometricSupportEdge],
    removed: List[GeometricSupportEdge],
    established: List[GeometricSupportEdge]
):
  require(removed.nonEmpty || established.nonEmpty, "a support change needs an exact changed edge")
  require(
    (removed ++ established).forall(edge => edge.supported.side == supportedBefore.side),
    "one support change cannot mix beneficiary sides"
  )
  require(
    before.forall(_.supported == supportedBefore) && after.forall(_.supported == supportedAfter),
    "closed support sets must belong to their exact beneficiary occurrence"
  )

private[chessjudgment] final case class GeometricSupportRelationTransition(
    changes: List[GeometricSupportChange]
):
  val removed: List[GeometricSupportEdge] = changes.flatMap(_.removed)
  val established: List[GeometricSupportEdge] = changes.flatMap(_.established)

private[chessjudgment] final case class GeometricSupporterCaptureChange(
    removedSupport: GeometricSupportEdge
):
  def stableKey: String = removedSupport.fact.semanticId

private[chessjudgment] final case class SliderControlInterferenceChange(
    interposer: RelationMoveTransitionWitness,
    removedControl: GeometricControlEdge,
    establishedBarrier: RelationFactEvidence
):
  def stableKey: String =
    s"${removedControl.fact.semanticId}:${establishedBarrier.semanticId}:${interposer.from.key}-${interposer.to.key}:${interposer.afterRole.name}"

private[chessjudgment] final case class GeometricLineOpeningChange(
    removingMove: RelationMoveTransitionWitness,
    controllerSide: Color,
    controllerBefore: EvidenceSquare,
    controllerAfter: EvidenceSquare,
    controllerRole: EvidencePieceRole,
    blocker: RelationColoredPieceWitness,
    target: EvidenceSquare,
    targetState: RelationControlTarget,
    barrierPattern: RelationRayPattern,
    removalMode: RelationBlockerRemovalMode,
    removedBarrier: RelationFactEvidence,
    establishedControl: RelationFactEvidence
):
  def stableKey: String =
    s"${removedBarrier.semanticId}:${establishedControl.semanticId}:${removingMove.from.key}-${removingMove.to.key}:${removalMode.toString}"

private[chessjudgment] final case class GeometricRelationCombinationTransition(
    supporterCaptures: List[GeometricSupporterCaptureChange],
    controlInterferences: List[SliderControlInterferenceChange],
    lineOpenings: List[GeometricLineOpeningChange]
):
  require(
    supporterCaptures.map(_.stableKey).distinct.size == supporterCaptures.size &&
      controlInterferences.map(_.stableKey).distinct.size == controlInterferences.size &&
      lineOpenings.map(_.stableKey).distinct.size == lineOpenings.size,
    "a geometric combination transition cannot repeat an exact change"
  )

private final case class GeometricRelationTransition(
    control: GeometricControlRelationTransition,
    support: GeometricSupportRelationTransition
)

/** Exact relation facts changed by the canonical position producer for one
  * admitted legal transition. Unchanged position facts are deliberately not
  * copied here: transition proof consumes this sparse inventory instead of
  * diffing both complete position snapshots again.
  */
private[chessjudgment] final case class RelationInventoryTransition(
    comparableRemoved: List[RelationFactEvidence],
    comparableEstablished: List[RelationFactEvidence],
    turnScopedBefore: List[RelationFactEvidence],
    turnScopedAfter: List[RelationFactEvidence],
    beforeInventory: PositionRelationInventoryCertificate,
    afterInventory: PositionRelationInventoryCertificate
):
  private def requireUnique(label: String, relations: List[RelationFactEvidence]): Unit =
    require(
      relations.map(relation => RelationFactKind.id(relation.kind) -> relation.semanticId).distinct.size == relations.size,
      s"a relation inventory transition must contain each $label fact exactly once"
    )
  requireUnique("removed comparable", comparableRemoved)
  requireUnique("established comparable", comparableEstablished)
  requireUnique("before turn-scoped", turnScopedBefore)
  requireUnique("after turn-scoped", turnScopedAfter)

  def removed: List[RelationFactEvidence] = comparableRemoved ++ turnScopedBefore
  def established: List[RelationFactEvidence] = comparableEstablished ++ turnScopedAfter

  private lazy val removedGeometricControlsByKey: Map[GeometricControlKey, (RelationControlTarget, RelationFactEvidence)] =
    geometricControlsByKey(comparableRemoved, "removed")

  private lazy val establishedGeometricControlsByKey: Map[GeometricControlKey, (RelationControlTarget, RelationFactEvidence)] =
    geometricControlsByKey(comparableEstablished, "established")

  def geometricTransition(
      boardFootprint: BoardTransitionFootprint
  ): GeometricRelationTransition =
    val established = establishedGeometricControlsByKey.toList.sortBy { case (key, _) =>
      (key.side.toString, key.controller.key, key.target.key, key.controllerRole.name)
    }
    val classified = established.map { case (afterKey, (afterTarget, afterFact)) =>
      val beforeKey = priorControlKey(afterKey, boardFootprint)
      removedGeometricControlsByKey.get(beforeKey) match
        case Some((beforeTarget, beforeFact)) if beforeTarget != afterTarget =>
          Some(
            GeometricControlTargetStateTransition(
              afterKey,
              beforeTarget,
              afterTarget,
              beforeFact,
              afterFact
            )
          ) -> None
        case Some(_) =>
          None -> None
        case None =>
          None -> Some(afterFact)
    }
    val targetSetChanges = geometricControlTargetSetChanges(boardFootprint)
    GeometricRelationTransition(
      control = GeometricControlRelationTransition(
        targetStateChanges = classified.flatMap(_._1),
        newlyEstablished = classified.flatMap(_._2).sortBy(_.semanticId),
        targetSetChanges = targetSetChanges
      ),
      support = geometricSupportTransition(boardFootprint, targetSetChanges)
    )

  def geometricCombinationTransition(
      boardFootprint: BoardTransitionFootprint,
      rootMove: CanonicalRootLegalMove,
      geometric: GeometricRelationTransition
  ): GeometricRelationCombinationTransition =
    val supporterCaptures = rootMove.capture.toList.flatMap { capture =>
      geometric.support.removed.collect {
        case support
            if rootMove.side != support.supporter.side &&
              capture.capturedSide == support.supporter.side &&
              capture.capturedSquare == support.supporter.square &&
              capture.capturedRole == support.supporter.role =>
          GeometricSupporterCaptureChange(support)
      }
    }.sortBy(_.stableKey)

    val establishedRays = comparableEstablished
      .filter(_.kind == RelationFactKind.RayBarrier)
      .sortBy(_.semanticId)
    val removedControls = geometric.control.targetSetChanges.flatMap(_.removed).sortBy(_.fact.semanticId)
    val controlInterferences = removedControls.flatMap { control =>
      val controllerBefore = RelationColoredPieceWitness(
        control.controller.square,
        control.controller.role,
        control.side
      )
      pieceAfter(controllerBefore, boardFootprint).toList.flatMap { controllerAfter =>
        boardFootprint.pieceTransitions.flatMap { movement =>
          val interposer = movementWitness(movement)
          val liesBetween = BoardGeometry.liesStrictlyBetween(
            boardSquare(controllerAfter.square),
            boardSquare(control.target),
            movement.to
          )
          Option.when(liesBetween)(interposer).toList.flatMap { exactInterposer =>
            establishedRays.collect {
              case barrier
                  if (barrier.detail match
                    case RelationWitnessDetail.RayBarrier(side, controller, role, occupants, _) =>
                      side == controllerAfter.side && controller == controllerAfter.square &&
                        role == controllerAfter.role &&
                        occupants.headOption.exists(occupant =>
                          occupant.side == movement.side &&
                            occupant.square.key.equalsIgnoreCase(movement.to.key) &&
                            occupant.role.name.equalsIgnoreCase(movement.afterRole.name)
                        )
                    case _ => false) =>
                SliderControlInterferenceChange(exactInterposer, control, barrier)
            }
          }
        }
      }
    }.sortBy(_.stableKey)

    val removedRays = comparableRemoved.filter(_.kind == RelationFactKind.RayBarrier).sortBy(_.semanticId)
    val establishedControls = geometric.control.targetSetChanges.flatMap(_.established).sortBy(_.fact.semanticId)
    val lineOpenings = removedRays.flatMap { removed =>
      removed.detail match
        case ray @ RelationWitnessDetail.RayBarrier(
              controllerSide,
              controllerBefore,
              controllerBeforeRole,
              occupants,
              _
            ) =>
          val blocker = occupants.head
          val movedBlocker = boardFootprint.pieceTransitions.find(movement =>
            movement.side == blocker.side &&
              movement.from.key.equalsIgnoreCase(blocker.square.key) &&
              movement.beforeRole.name.equalsIgnoreCase(blocker.role.name)
          )
          val capturedBlocker = rootMove.capture.filter(capture =>
            capture.capturedSide == blocker.side && capture.capturedSquare == blocker.square &&
              capture.capturedRole == blocker.role
          )
          val removal = movedBlocker
            .map(movement => RelationBlockerRemovalMode.Moved -> movementWitness(movement))
            .orElse(capturedBlocker.map(_ => RelationBlockerRemovalMode.Captured -> rootMove.witness))
          removal.toList.flatMap { case (removalMode, removingMove) =>
            val controllerMovement = boardFootprint.pieceTransitions.find(movement =>
              movement.side == controllerSide &&
                movement.from.key.equalsIgnoreCase(controllerBefore.key) &&
                movement.beforeRole.name.equalsIgnoreCase(controllerBeforeRole.name)
            )
            val controllerAfter = controllerMovement
              .map(movement => EvidenceSquare(movement.to.key))
              .getOrElse(controllerBefore)
            val controllerAfterRole = controllerMovement
              .map(movement => EvidencePieceRole(movement.afterRole.name))
              .getOrElse(controllerBeforeRole)
            establishedControls.collect {
              case control
                  if control.side == controllerSide && control.controller.square == controllerAfter &&
                    control.controller.role == controllerAfterRole &&
                    BoardGeometry.liesStrictlyBetween(
                      boardSquare(controllerBefore),
                      boardSquare(control.target),
                      boardSquare(blocker.square)
                    ) =>
                GeometricLineOpeningChange(
                  removingMove = removingMove,
                  controllerSide = controllerSide,
                  controllerBefore = controllerBefore,
                  controllerAfter = controllerAfter,
                  controllerRole = controllerAfterRole,
                  blocker = blocker,
                  target = control.target,
                  targetState = control.targetState,
                  barrierPattern = RelationRayProjection.pattern(ray),
                  removalMode = removalMode,
                  removedBarrier = removed,
                  establishedControl = control.fact
                )
            }
          }
        case _ => Nil
    }.sortBy(_.stableKey)

    GeometricRelationCombinationTransition(
      supporterCaptures = supporterCaptures,
      controlInterferences = controlInterferences,
      lineOpenings = lineOpenings
    )

  private def movementWitness(movement: BoardPieceTransition): RelationMoveTransitionWitness =
    RelationMoveTransitionWitness(
      movement.side,
      EvidenceSquare(movement.from.key),
      EvidenceSquare(movement.to.key),
      EvidencePieceRole(movement.beforeRole.name),
      EvidencePieceRole(movement.afterRole.name)
    )

  private def geometricControlTargetSetChanges(
      footprint: BoardTransitionFootprint
  ): List[GeometricControlTargetSetChange] =
    val removed = geometricControlEdges(removedGeometricControlsByKey)
    val established = geometricControlEdges(establishedGeometricControlsByKey)
    val keys = (removed.map(edge => edge.side -> edge.target) ++ established.map(edge => edge.side -> edge.target))
      .distinct
      .sortBy { case (side, target) => side.toString -> target.key }
    keys.map { case (side, target) =>
      val before = fullControlsAt(beforeInventory, side, target)
      val after = fullControlsAt(afterInventory, side, target)
      val afterByController = after.map(edge => edge.controller -> edge).toMap
      val maintainedAfterIds = before.flatMap { edge =>
        val controller = RelationColoredPieceWitness(edge.controller.square, edge.controller.role, side)
        pieceAfter(controller, footprint).flatMap(afterController =>
          afterByController
            .get(RelationPieceWitness(afterController.square, afterController.role))
            .map(_.fact.semanticId)
        )
      }.toSet
      val removedControllers = before.flatMap { edge =>
        val controller = RelationColoredPieceWitness(edge.controller.square, edge.controller.role, side)
        val maintained = pieceAfter(controller, footprint).exists(afterController =>
          afterByController.contains(RelationPieceWitness(afterController.square, afterController.role))
        )
        Option.when(!maintained)(edge.controller)
      }
      GeometricControlTargetSetChange(
        side = side,
        target = target,
        beforeState = controlTargetAt(beforeInventory, side, target),
        afterState = controlTargetAt(afterInventory, side, target),
        before = before,
        after = after,
        removedControllers = removedControllers.sortBy(value => (value.square.key, value.role.name)),
        establishedControllers = after
          .filterNot(edge => maintainedAfterIds(edge.fact.semanticId))
          .map(_.controller)
          .sortBy(value => (value.square.key, value.role.name))
      )
    }

  private def geometricControlEdges(
      controls: Map[GeometricControlKey, (RelationControlTarget, RelationFactEvidence)]
  ): List[GeometricControlEdge] =
    controls.values.map { case (_, fact) => geometricControl(fact) }.toList.sortBy(_.fact.semanticId)

  private def fullControlsAt(
      inventory: PositionRelationInventoryCertificate,
      side: Color,
      target: EvidenceSquare
  ): List[GeometricControlEdge] =
    val square = boardSquare(target)
    inventory
      .relationsFor(RelationDependencyKey.AttackTarget(square))
      .map(geometricControl)
      .filter(_.side == side)
      .sortBy(_.fact.semanticId)

  private def geometricControl(fact: RelationFactEvidence): GeometricControlEdge =
    fact.detail match
      case RelationWitnessDetail.GeometricControl(side, controller, controllerRole, target, targetState) =>
        GeometricControlEdge(
          fact,
          side,
          RelationPieceWitness(controller, controllerRole),
          target,
          targetState
        )
      case _ =>
        throw IllegalArgumentException(s"relation '${fact.semanticId}' is not a geometric control")

  private def controlTargetAt(
      inventory: PositionRelationInventoryCertificate,
      side: Color,
      target: EvidenceSquare
  ): RelationControlTarget =
    inventory.occupantAt(target) match
      case None => RelationControlTarget.Empty
      case Some(piece) if piece.side == side => RelationControlTarget.Friendly(piece.role)
      case Some(piece) => RelationControlTarget.Enemy(piece.role)

  private def boardSquare(square: EvidenceSquare): Square =
    Square.fromKey(square.key).getOrElse(
      throw IllegalArgumentException(s"invalid relation square '${square.key}'")
    )

  private def geometricSupportTransition(
      boardFootprint: BoardTransitionFootprint,
      targetSetChanges: List[GeometricControlTargetSetChange]
  ): GeometricSupportRelationTransition =
    final case class SupportPair(
        supporter: RelationColoredPieceWitness,
        supported: RelationColoredPieceWitness
    )
    final case class SupportedIdentity(
        before: RelationColoredPieceWitness,
        after: RelationColoredPieceWitness
    )

    val removed = geometricSupports(removedGeometricControlsByKey)
    val established = geometricSupports(establishedGeometricControlsByKey)
    val establishedByPair = established.map(edge => SupportPair(edge.supporter, edge.supported) -> edge).toMap
    require(
      establishedByPair.size == established.size,
      "one position cannot repeat a geometric support edge"
    )
    val maintainedEstablished = removed.flatMap { edge =>
      for
        supporter <- pieceAfter(edge.supporter, boardFootprint)
        supported <- pieceAfter(edge.supported, boardFootprint)
        maintained <- establishedByPair.get(SupportPair(supporter, supported))
      yield maintained.fact.semanticId
    }.toSet
    val removedChanges = removed.flatMap { edge =>
      pieceAfter(edge.supported, boardFootprint).flatMap { supportedAfter =>
        val maintained = for
          supporterAfter <- pieceAfter(edge.supporter, boardFootprint)
          exact <- establishedByPair.get(SupportPair(supporterAfter, supportedAfter))
        yield exact
        Option.when(maintained.isEmpty)(
          SupportedIdentity(edge.supported, supportedAfter) -> edge
        )
      }
    }
    val establishedChanges = established
      .filterNot(edge => maintainedEstablished(edge.fact.semanticId))
      .map { edge =>
        val supportedBefore = pieceBefore(edge.supported, boardFootprint).getOrElse(
          throw IllegalArgumentException(
            s"established support beneficiary '${edge.supported.square.key}' has no exact pre-move identity"
          )
        )
        SupportedIdentity(supportedBefore, edge.supported) -> edge
      }
    val identities = (removedChanges.map(_._1) ++ establishedChanges.map(_._1)).distinct.sortBy(identity =>
      (
        identity.before.side.toString,
        identity.before.square.key,
        identity.before.role.name,
        identity.after.square.key,
        identity.after.role.name
      )
    )
    GeometricSupportRelationTransition(
      identities.map { identity =>
        GeometricSupportChange(
          supportedBefore = identity.before,
          supportedAfter = identity.after,
          before = fullSupportsAt(targetSetChanges, identity.before, before = true),
          after = fullSupportsAt(targetSetChanges, identity.after, before = false),
          removed = removedChanges.collect { case (`identity`, edge) => edge }.sortBy(_.fact.semanticId),
          established = establishedChanges.collect { case (`identity`, edge) => edge }.sortBy(_.fact.semanticId)
        )
      }
    )

  private def geometricSupports(
      controls: Map[GeometricControlKey, (RelationControlTarget, RelationFactEvidence)]
  ): List[GeometricSupportEdge] =
    controls.values.flatMap { case (_, fact) => geometricSupport(fact) }.toList.sortBy(_.fact.semanticId)

  private def fullSupportsAt(
      targetSetChanges: List[GeometricControlTargetSetChange],
      supported: RelationColoredPieceWitness,
      before: Boolean
  ): List[GeometricSupportEdge] =
    targetSetChanges
      .find(change => change.side == supported.side && change.target == supported.square)
      .toList
      .flatMap(change => if before then change.before else change.after)
      .flatMap(edge => geometricSupport(edge.fact))
      .filter(_.supported == supported)
      .sortBy(_.fact.semanticId)

  private def geometricSupport(
      fact: RelationFactEvidence
  ): Option[GeometricSupportEdge] =
    fact.detail match
      case RelationWitnessDetail.GeometricControl(
            side,
            supporter,
            supporterRole,
            supported,
            RelationControlTarget.Friendly(supportedRole)
          ) =>
        Some(
          GeometricSupportEdge(
            fact,
            RelationColoredPieceWitness(supporter, supporterRole, side),
            RelationColoredPieceWitness(supported, supportedRole, side)
          )
        )
      case _ => None

  private def pieceAfter(
      before: RelationColoredPieceWitness,
      footprint: BoardTransitionFootprint
  ): Option[RelationColoredPieceWitness] =
    footprint.pieceTransitions
      .find(transition =>
        transition.side == before.side &&
          transition.from.key.equalsIgnoreCase(before.square.key) &&
          transition.beforeRole.name.equalsIgnoreCase(before.role.name)
      )
      .map(transition =>
        RelationColoredPieceWitness(
          EvidenceSquare(transition.to.key),
          EvidencePieceRole(transition.afterRole.name),
          transition.side
        )
      )
      .orElse(Option.when(!footprint.changedSquares.exists(_.key.equalsIgnoreCase(before.square.key)))(before))

  private def pieceBefore(
      after: RelationColoredPieceWitness,
      footprint: BoardTransitionFootprint
  ): Option[RelationColoredPieceWitness] =
    footprint.pieceTransitions
      .find(transition =>
        transition.side == after.side &&
          transition.to.key.equalsIgnoreCase(after.square.key) &&
          transition.afterRole.name.equalsIgnoreCase(after.role.name)
      )
      .map(transition =>
        RelationColoredPieceWitness(
          EvidenceSquare(transition.from.key),
          EvidencePieceRole(transition.beforeRole.name),
          transition.side
        )
      )
      .orElse(Option.when(!footprint.changedSquares.exists(_.key.equalsIgnoreCase(after.square.key)))(after))

  private def priorControlKey(
      after: GeometricControlKey,
      boardFootprint: BoardTransitionFootprint
  ): GeometricControlKey =
    boardFootprint.pieceTransitions
      .find(transition =>
        transition.side == after.side &&
          transition.beforeRole == transition.afterRole &&
          transition.to.key.equalsIgnoreCase(after.controller.key) &&
          transition.afterRole.name.equalsIgnoreCase(after.controllerRole.name)
      )
      .map(transition =>
        after.copy(
          controller = EvidenceSquare(transition.from.key),
          controllerRole = EvidencePieceRole(transition.beforeRole.name)
        )
      )
      .getOrElse(after)

  private def geometricControlsByKey(
      relations: List[RelationFactEvidence],
      label: String
  ): Map[GeometricControlKey, (RelationControlTarget, RelationFactEvidence)] =
    val values = relations.flatMap { relation =>
      relation.detail match
        case RelationWitnessDetail.GeometricControl(side, controller, role, target, targetState) =>
          List(GeometricControlKey(side, controller, role, target) -> (targetState -> relation))
        case _ => Nil
    }
    require(
      values.map(_._1).distinct.size == values.size,
      s"a relation transition must contain each $label geometric-control edge exactly once"
    )
    values.toMap

private[chessjudgment] object RelationInventoryTransition:
  def fromBoardTransition(
      boardRemoved: List[RelationFactEvidence],
      boardEstablished: List[RelationFactEvidence],
      beforeInventory: PositionRelationInventoryCertificate,
      afterInventory: PositionRelationInventoryCertificate
  ): RelationInventoryTransition =
    val (turnBoardBefore, comparableRemoved) = boardRemoved.partition(isTurnScoped)
    val (turnBoardAfter, comparableEstablished) = boardEstablished.partition(isTurnScoped)
    RelationInventoryTransition(
      comparableRemoved = comparableRemoved,
      comparableEstablished = comparableEstablished,
      turnScopedBefore = turnBoardBefore.sortBy(_.semanticId),
      turnScopedAfter = turnBoardAfter.sortBy(_.semanticId),
      beforeInventory = beforeInventory,
      afterInventory = afterInventory
    )

  private[judgment] def isTurnScoped(relation: RelationFactEvidence): Boolean =
    relation.detail match
      case _: RelationWitnessDetail.LegalMove => true
      case _                                  => false

private[chessjudgment] final case class CanonicalRelationChange(
    direction: RelationChangeDirection,
    sourceNode: CanonicalRelationNode,
    dependencySquares: List[EvidenceSquare],
    proofKeys: List[RelationProofKey]
):
  def source: EvidenceRef = sourceNode.ref
  def relation: RelationFactEvidence = sourceNode.relation
  def kind: RelationFactKind = relation.kind
  def detail: RelationWitnessDetail = relation.detail
  def semanticId: String = relation.semanticId
  def participants: List[RelationParticipant] = relation.participants
  def targetSquares: List[EvidenceSquare] = relation.targetSquares
  def files: List[EvidenceFile] = relation.files

  def key: RelationChangeKey = RelationChangeKey(direction, kind, semanticId)
  def stableKey: String = key.stableKey

private[chessjudgment] final case class CanonicalRelationDelta private (
    semantic: RelationSemanticDelta,
    changes: List[CanonicalRelationChange]
):
  require(changes.map(_.stableKey).distinct.size == changes.size, "duplicate canonical relation changes")
  require(changes.map(_.source.id).distinct.size == changes.size, "canonical relation changes must own distinct sources")

  def changedSquares: List[EvidenceSquare] = semantic.changedSquares

  lazy val established: List[CanonicalRelationChange] =
    changes.filter(_.direction == RelationChangeDirection.Established)

  lazy val removed: List[CanonicalRelationChange] =
    changes.filter(_.direction == RelationChangeDirection.Removed)

  def sourceRefs: List[EvidenceRef] =
    changes.map(_.source)

  private[chessjudgment] def binds(
      declared: LineReplayStep,
      exactSemantic: RelationSemanticDelta,
      before: CanonicalPositionRelationSnapshot,
      after: CanonicalPositionRelationSnapshot
  ): Boolean =
    semantic == exactSemantic && EvidenceRef.sameMove(semantic.moveUci, declared.moveUci) &&
      PrincipalVariationEvidence.normalizeFen(before.position.fen) ==
        PrincipalVariationEvidence.normalizeFen(declared.fenBefore) &&
      PrincipalVariationEvidence.normalizeFen(after.position.fen) ==
        PrincipalVariationEvidence.normalizeFen(declared.fenAfter) &&
      semantic.beforeInventory.sameOwner(before.inventory) &&
      semantic.afterInventory.sameOwner(after.inventory) &&
      changes.forall { change =>
        val owner = change.direction match
          case RelationChangeDirection.Removed     => before.position
          case RelationChangeDirection.Established => after.position
        change.source.position == owner
      }

  private[chessjudgment] def owns(exactSemantic: RelationSemanticDelta): Boolean =
    semantic == exactSemantic

private[chessjudgment] object CanonicalRelationDelta:
  /** Bind one already-computed semantic transition to the exact canonical
    * relation occurrences that own its removed and established facts. This
    * step does not reproduce board relations or reinterpret the move.
    */
  def bind(
      declared: LineReplayStep,
      semantic: RelationSemanticDelta,
      before: CanonicalPositionRelationSnapshot,
      after: CanonicalPositionRelationSnapshot
  ): CanonicalRelationDelta =
    require(
      EvidenceRef.sameMove(semantic.moveUci, declared.moveUci),
      "a relation delta must bind its exact admitted move"
    )
    require(
      PrincipalVariationEvidence.normalizeFen(before.position.fen) ==
        PrincipalVariationEvidence.normalizeFen(declared.fenBefore),
      "the before relation snapshot must own the legal transition origin"
    )
    require(
      PrincipalVariationEvidence.normalizeFen(after.position.fen) ==
        PrincipalVariationEvidence.normalizeFen(declared.fenAfter),
      "the after relation snapshot must own the legal transition destination"
    )
    require(
      semantic.beforeInventory.sameOwner(before.inventory) &&
        semantic.afterInventory.sameOwner(after.inventory),
      "a relation delta must bind the same closed inventories that produced its transition"
    )
    val changes = semantic.changes.map { change =>
      val sourceNode =
        change.direction match
          case RelationChangeDirection.Removed =>
            before.nodeFor(change.relation).getOrElse(
              throw IllegalArgumentException(
                s"removed relation '${change.semanticId}' is absent from its certified before snapshot"
              )
            )
          case RelationChangeDirection.Established =>
            after.nodeFor(change.relation).getOrElse(
              throw IllegalArgumentException(
                s"established relation '${change.semanticId}' is absent from its certified after snapshot"
              )
            )
      CanonicalRelationChange(
        direction = change.direction,
        sourceNode = sourceNode,
        dependencySquares = change.dependencySquares,
        proofKeys = change.proofKeys
      )
    }
    CanonicalRelationDelta(semantic, changes)

  def semanticFrom(
      legalStep: LegalReplayStep,
      boardFootprint: BoardTransitionFootprint,
      relationTransition: RelationInventoryTransition
  ): RelationSemanticDelta =
    val beforeFen = chess.format.Fen.write(legalStep.before).value
    val afterFen = chess.format.Fen.write(legalStep.after).value
    val removedBySemanticId = uniquePositionInventory(
      relationTransition.comparableRemoved,
      beforeFen,
      "removed comparable"
    )
    val establishedBySemanticId = uniquePositionInventory(
      relationTransition.comparableEstablished,
      afterFen,
      "established comparable"
    )
    val turnScopedBefore = uniquePositionInventory(relationTransition.turnScopedBefore, beforeFen, "before turn-scoped")
    val turnScopedAfter = uniquePositionInventory(relationTransition.turnScopedAfter, afterFen, "after turn-scoped")
    require(
      removedBySemanticId.values.forall(relation => !RelationInventoryTransition.isTurnScoped(relation)) &&
        establishedBySemanticId.values.forall(relation => !RelationInventoryTransition.isTurnScoped(relation)) &&
        turnScopedBefore.values.forall(RelationInventoryTransition.isTurnScoped) &&
        turnScopedAfter.values.forall(RelationInventoryTransition.isTurnScoped),
      "comparable and turn-scoped relation transitions must remain disjoint"
    )
    require(
      removedBySemanticId.keySet.intersect(establishedBySemanticId.keySet).isEmpty,
      "an exact relation transition cannot remove and establish the same semantic fact"
    )
    val changedSquares = boardFootprint.changedSquares.map(square => EvidenceSquare(square.key))
    val removed = removedBySemanticId.toList.sortBy(_._1).map { case (_, relation) =>
      semanticChange(RelationChangeDirection.Removed, relation, boardFootprint, legalStep.uci)
    }
    val established = establishedBySemanticId.toList.sortBy(_._1).map { case (_, relation) =>
      semanticChange(RelationChangeDirection.Established, relation, boardFootprint, legalStep.uci)
    }
    val rootMove = canonicalRootLegalMove(legalStep, boardFootprint, relationTransition)
    val geometricTransition = relationTransition.geometricTransition(boardFootprint)
    val geometricCombinations = relationTransition.geometricCombinationTransition(
      boardFootprint,
      rootMove,
      geometricTransition
    )
    val namedRays = newlyEstablishedNamedRays(removed, established)
    RelationSemanticDelta(
      moveUci = EvidenceRef.normalizeMove(legalStep.uci),
      moverSide = legalStep.before.color,
      changedSquares = changedSquares,
      changes = (removed ++ established).sortBy(_.stableKey),
      rootMove = rootMove,
      beforeInventory = relationTransition.beforeInventory,
      afterInventory = relationTransition.afterInventory,
      geometricControlTransition = geometricTransition.control,
      geometricSupportTransition = geometricTransition.support,
      geometricCombinationTransition = geometricCombinations,
      newlyEstablishedNamedRays = namedRays
    )

  private def newlyEstablishedNamedRays(
      removed: List[RelationSemanticChange],
      established: List[RelationSemanticChange]
  ): List[RelationSemanticChange] =
    val removedProjections = removed.flatMap(change =>
      change.detail match
        case ray: RelationWitnessDetail.RayBarrier => RelationRayProjection.named(ray)
        case _                                      => None
    ).toSet
    established.filter(change =>
      change.detail match
        case ray: RelationWitnessDetail.RayBarrier =>
          RelationRayProjection.named(ray).exists(projection => !removedProjections(projection))
        case _ => false
    ).sortBy(_.stableKey)

  private def canonicalRootLegalMove(
      legalStep: LegalReplayStep,
      footprint: BoardTransitionFootprint,
      transition: RelationInventoryTransition
  ): CanonicalRootLegalMove =
    val matches = transition.turnScopedBefore.filter(_.detail match
      case RelationWitnessDetail.LegalMove(_, _, _, _, moveUci, _) =>
        EvidenceRef.sameMove(moveUci, legalStep.uci)
      case _ => false
    )
    matches match
      case fact :: Nil =>
        fact.detail match
          case RelationWitnessDetail.LegalMove(side, from, role, to, moveUci, capture) =>
            val movements = footprint.pieceTransitions.filter(movement =>
              movement.side == side && movement.from.key.equalsIgnoreCase(from.key) &&
                movement.to.key.equalsIgnoreCase(to.key) &&
                movement.beforeRole.name.equalsIgnoreCase(role.name)
            )
            require(movements.size == 1, s"'${legalStep.uci}' must own one exact primary piece transition")
            val movement = movements.head
            val promotionRole = PrincipalVariationEvidence.normalizeUci(legalStep.uci).lift(4).flatMap {
              case 'q' => Some("queen")
              case 'r' => Some("rook")
              case 'b' => Some("bishop")
              case 'n' => Some("knight")
              case _   => None
            }
            require(
              promotionRole.fold(movement.beforeRole == movement.afterRole)(
                _.equalsIgnoreCase(movement.afterRole.name)
              ),
              s"'${legalStep.uci}' role transition must agree with its exact legal move"
            )
            if footprint.pieceTransitions.size > 1 then
              require(
                footprint.pieceTransitions.size == 2 &&
                  footprint.pieceTransitions.forall(_.side == side) &&
                  footprint.pieceTransitions.map(_.beforeRole).toSet == Set(King, Rook),
                s"'${legalStep.uci}' may have multiple piece transitions only for castling"
              )
            CanonicalRootLegalMove(fact, movement, moveUci, capture)
          case _ => throw IllegalStateException("a selected legal-move relation changed kind")
      case Nil =>
        throw IllegalArgumentException(s"'${legalStep.uci}' is absent from its closed legal-move inventory")
      case _ =>
        throw IllegalArgumentException(s"canonical legal-move inventory repeated '${legalStep.uci}'")

  private def uniquePositionInventory(
      relations: List[RelationFactEvidence],
      fen: String,
      positionLabel: String
  ): Map[String, RelationFactEvidence] =
    require(
      relations.forall(relation =>
        relation.isPositionRelation &&
          (relation.origin match
            case RelationEvidenceOrigin.PositionSnapshot(semanticFen) =>
              PrincipalVariationEvidence.sameBoardState(semanticFen, fen)
            case _ => false)
      ),
      s"$positionLabel semantic relation inventory must be certified at its exact board snapshot"
    )
    uniqueRelationBySemanticId(relations, positionLabel)

  private def uniqueRelationBySemanticId(
      relations: List[RelationFactEvidence],
      positionLabel: String
  ): Map[String, RelationFactEvidence] =
    val grouped = relations.groupBy(_.semanticId)
    val duplicates = grouped.collect { case (semanticId, values) if values.size > 1 => semanticId }.toList.sorted
    require(duplicates.isEmpty, s"duplicate $positionLabel relation semantics: ${duplicates.mkString(",")}")
    grouped.view.mapValues(_.head).toMap

  private def semanticChange(
      direction: RelationChangeDirection,
      relation: RelationFactEvidence,
      footprint: BoardTransitionFootprint,
      moveUci: String
  ): RelationSemanticChange =
    val dependencies = relation.canonicalFact.dependencyFootprint.map(_.squares).getOrElse(
      throw IllegalArgumentException(
        s"relation change '${relation.semanticId}' (${relation.detail.detailName}) has no canonical board dependency footprint"
      )
    )
    val proofKeys = RelationProofKey.forDependencySquares(dependencies, footprint)
    require(
      proofKeys.nonEmpty,
      s"relation change '${relation.semanticId}' (${relation.detail.detailName}) after '$moveUci' has no exact transition proof key; changed=${footprint.changedSquares.map(_.key).mkString(",")}; dependencies=${dependencies.map(_.key).mkString(",")}"
    )

    RelationSemanticChange(
      direction = direction,
      relation = relation,
      dependencySquares = dependencies,
      proofKeys = proofKeys
    )
