package lila.chessjudgment.model.judgment

import chess.{ Bishop, Color, King, Queen, Rook, Square }

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.analysis.position.PositionRelationExtractor.PositionRelationInventoryCertificate
import lila.chessjudgment.model.line.{ LegalReplayStep, PrincipalVariationEvidence }
import lila.chessjudgment.model.position.BoardTransitionFootprint

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
    val keys = changed ++ movers
    require(
      keys.distinct.size == keys.size,
      "one canonical transition footprint cannot repeat a relation proof key"
    )
    keys.sortBy(_.stableKey)

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

private[chessjudgment] final case class RelationNamedRayChange(
    projection: RelationNamedRayProjection,
    change: RelationSemanticChange
):
  require(
    change.detail.isInstanceOf[RelationWitnessDetail.RayBarrier] &&
      projection.pattern != RelationRayPattern.Ordinary,
    "a named-ray change needs one exact non-ordinary ray projection"
  )

  def stableKey: String = change.stableKey

private[chessjudgment] final class RelationSemanticDelta(
    val moveUci: String,
    val changes: List[RelationSemanticChange],
    val rootMove: CanonicalRootLegalMove,
    val beforeInventory: PositionRelationInventoryCertificate,
    val afterInventory: PositionRelationInventoryCertificate,
    val transitionFootprint: BoardTransitionFootprint,
    private val relationTransition: RelationInventoryTransition
):
  require(
    relationTransition.beforeInventory.sameOwner(beforeInventory) &&
      relationTransition.afterInventory.sameOwner(afterInventory),
    "a semantic delta must retain the closed transition inventory that owns its positions"
  )
  require(changes.map(_.stableKey).distinct.size == changes.size, "duplicate semantic relation changes")
  require(changes.map(_.semanticId).distinct.size == changes.size, "one relation semantic cannot change twice")

  lazy val geometricControlTransition: GeometricControlRelationTransition =
    relationTransition.geometricControlTransition(transitionFootprint)

  lazy val geometricSupportTransition: GeometricSupportRelationTransition =
    relationTransition.geometricSupportTransition(
      transitionFootprint,
      geometricControlTransition
    )

  lazy val sliderReachTransition: ClosedSliderReachTransitionInventory =
    relationTransition.sliderReachTransition(
      transitionFootprint,
      geometricControlTransition
    )

  lazy val geometricSupporterCaptures: List[GeometricSupporterCaptureChange] =
    relationTransition.geometricSupporterCaptures(rootMove, geometricSupportTransition)

  lazy val sliderLineInterruptions: List[SliderLineInterruptionChange] =
    relationTransition.sliderLineInterruptions(sliderReachTransition)

  lazy val geometricLineOpenings: List[GeometricLineOpeningChange] =
    relationTransition.geometricLineOpenings(rootMove, sliderReachTransition)

  lazy val changedNamedRays: List[RelationNamedRayChange] =
    CanonicalRelationDelta.changedNamedRays(removed, established)

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
    witness: RelationMoveTransitionWitness,
    moveUci: String,
    capture: Option[RelationLegalCaptureWitness]
):
  def side: Color = witness.side
  def from: EvidenceSquare = witness.from
  def to: EvidenceSquare = witness.to
  def beforeRole: EvidencePieceRole = witness.beforeRole
  def afterRole: EvidencePieceRole = witness.afterRole

private[chessjudgment] final case class GeometricControlKey(
    side: Color,
    controller: EvidenceSquare,
    controllerRole: EvidencePieceRole,
    target: EvidenceSquare
)

private[chessjudgment] final case class GeometricControlRelationTransition(
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

  def stableKey: String =
    List(
      side.toString.toLowerCase,
      target.key.toLowerCase,
      beforeState.toString,
      afterState.toString,
      before.map(_.fact.semanticId).sorted.mkString(","),
      after.map(_.fact.semanticId).sorted.mkString(",")
    ).mkString(":")

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

  def stableKey: String =
    List(
      supportedBefore.side.toString.toLowerCase,
      supportedBefore.square.key.toLowerCase,
      supportedBefore.role.name.toLowerCase,
      supportedAfter.square.key.toLowerCase,
      supportedAfter.role.name.toLowerCase,
      before.map(_.fact.semanticId).sorted.mkString(","),
      after.map(_.fact.semanticId).sorted.mkString(",")
    ).mkString(":")

private[chessjudgment] final case class GeometricSupportRelationTransition(
    changes: List[GeometricSupportChange]
):
  val removed: List[GeometricSupportEdge] = changes.flatMap(_.removed)
  val established: List[GeometricSupportEdge] = changes.flatMap(_.established)

/** One exact directional reach change for a slider identity. Both the generic
  * L1 reach fact and the more specific opening/interruption facts consume this
  * inventory; neither reconstructs slider pairing or ray state independently.
  */
private[chessjudgment] final case class ClosedSliderReachChange(
    side: Color,
    sliderBefore: Option[RelationPieceWitness],
    sliderAfter: Option[RelationPieceWitness],
    direction: RelationRayDirection,
    before: Option[PositionRelationExtractor.ClosedSliderReach],
    after: Option[PositionRelationExtractor.ClosedSliderReach],
    removedControls: List[GeometricControlEdge],
    establishedControls: List[GeometricControlEdge]
):
  require(sliderBefore.nonEmpty || sliderAfter.nonEmpty, "a slider-reach change needs one slider occurrence")
  require(
    before.map(_.witness) != after.map(_.witness),
    "a slider-reach change must alter its exact directional reach"
  )
  require(
    before.forall(reach => reach.side == side && sliderBefore.contains(reach.slider)) &&
      after.forall(reach => reach.side == side && sliderAfter.contains(reach.slider)),
    "a slider-reach change must retain its exact before and after slider occurrences"
  )
  require(
    removedControls.forall(edge => sliderBefore.contains(edge.controller)) &&
      establishedControls.forall(edge => sliderAfter.contains(edge.controller)),
    "changed controls must belong to the paired slider occurrence"
  )

  def stableKey: String =
    def piece(value: Option[RelationPieceWitness]): String =
      value.map(item => s"${item.role.name.toLowerCase}@${item.square.key.toLowerCase}").getOrElse("-")
    s"${side.toString.toLowerCase}:${piece(sliderBefore)}:${piece(sliderAfter)}:${direction.stableKey}"

private[chessjudgment] final case class ClosedSliderReachTransitionInventory(
    changes: List[ClosedSliderReachChange],
    pieceMovements: List[RelationMoveTransitionWitness]
):
  require(
    changes.map(_.stableKey).distinct.size == changes.size,
    "one slider-reach transition cannot repeat a directional change"
  )
  require(
    pieceMovements.map(_.stableKey).distinct.size == pieceMovements.size,
    "one slider-reach transition cannot repeat a causal piece movement"
  )

private[chessjudgment] final case class GeometricSupporterCaptureChange(
    removedSupport: GeometricSupportEdge
):
  def stableKey: String = removedSupport.fact.semanticId

private[chessjudgment] final case class SliderLineInterruptionChange(
    interposer: RelationMoveTransitionWitness,
    controllerSide: Color,
    controllerBefore: RelationPieceWitness,
    controllerAfter: RelationPieceWitness,
    removedControls: List[GeometricControlEdge],
    establishedBarrier: RelationFactEvidence
):
  require(removedControls.nonEmpty, "a line interruption needs a non-empty affected corridor")
  require(
    removedControls.map(_.fact.semanticId).distinct.size == removedControls.size &&
      removedControls.map(_.target).distinct.size == removedControls.size,
    "a line interruption cannot repeat one removed control"
  )
  require(
    removedControls.forall(control =>
      control.side == controllerSide && control.controller == controllerBefore
    ),
    "one line interruption may describe only one exact slider occurrence"
  )
  def stableKey: String =
    s"${removedControls.map(_.fact.semanticId).mkString(",")}:${establishedBarrier.semanticId}:${interposer.from.key}-${interposer.to.key}:${interposer.afterRole.name}"

private[chessjudgment] final case class GeometricLineOpeningChange(
    removingMove: RelationMoveTransitionWitness,
    controllerSide: Color,
    controllerBefore: EvidenceSquare,
    controllerAfter: EvidenceSquare,
    controllerRole: EvidencePieceRole,
    blocker: RelationColoredPieceWitness,
    openedControls: List[GeometricControlEdge],
    barrierPattern: RelationRayPattern,
    removalMode: RelationBlockerRemovalMode,
    removedBarrier: RelationFactEvidence
):
  require(openedControls.nonEmpty, "a geometric line opening must establish a non-empty corridor")
  require(
    openedControls.map(_.target).distinct.size == openedControls.size,
    "a geometric line-opening corridor cannot repeat a target square"
  )
  def stableKey: String =
    s"${removedBarrier.semanticId}:${openedControls.map(_.fact.semanticId).mkString(",")}:${removingMove.from.key}-${removingMove.to.key}:${removalMode.toString}"

/** Exact relation facts changed by the canonical position producer for one
  * admitted legal transition. Unchanged position facts are deliberately not
  * copied here: transition proof consumes this sparse inventory instead of
  * diffing both complete position snapshots again.
  */
private[chessjudgment] final case class RelationInventoryTransition(
    comparableRemoved: List[RelationFactEvidence],
    comparableEstablished: List[RelationFactEvidence],
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
  require(
    (comparableRemoved ++ comparableEstablished).forall(_.kind != RelationFactKind.LegalMove),
    "a sparse relation transition cannot carry position-owned legal-move inventories"
  )

  private lazy val removedGeometricControlsByKey: Map[GeometricControlKey, (RelationControlTarget, RelationFactEvidence)] =
    geometricControlsByKey(comparableRemoved, beforeInventory, "removed")

  private lazy val establishedGeometricControlsByKey: Map[GeometricControlKey, (RelationControlTarget, RelationFactEvidence)] =
    geometricControlsByKey(comparableEstablished, afterInventory, "established")

  def geometricControlTransition(
      boardFootprint: BoardTransitionFootprint
  ): GeometricControlRelationTransition =
    val established = establishedGeometricControlsByKey.toList.sortBy { case (key, _) =>
      (key.side.toString, key.controller.key, key.target.key, key.controllerRole.name)
    }
    val newlyEstablished = established.flatMap { case (afterKey, (_, afterFact)) =>
      val beforeKey = priorControlKey(afterKey, boardFootprint)
      Option.unless(removedGeometricControlsByKey.contains(beforeKey))(afterFact)
    }
    val targetSetChanges = geometricControlTargetSetChanges(boardFootprint)
    GeometricControlRelationTransition(
      newlyEstablished = newlyEstablished.sortBy(_.semanticId),
      targetSetChanges = targetSetChanges
    )

  def geometricSupportTransition(
      boardFootprint: BoardTransitionFootprint,
      geometricControl: GeometricControlRelationTransition
  ): GeometricSupportRelationTransition =
    geometricSupportTransition(boardFootprint, geometricControl.targetSetChanges)

  def sliderReachTransition(
      boardFootprint: BoardTransitionFootprint,
      geometricControl: GeometricControlRelationTransition
  ): ClosedSliderReachTransitionInventory =
    def isSlider(role: EvidencePieceRole): Boolean =
      List(Bishop.name, Rook.name, Queen.name).exists(_.equalsIgnoreCase(role.name))

    def sliderAt(
        inventory: PositionRelationInventoryCertificate,
        square: EvidenceSquare
    ): Option[(Color, RelationPieceWitness)] =
      inventory.stateView.occupantAt(square).filter(piece => isSlider(piece.role)).map(piece =>
        piece.side -> RelationPieceWitness(piece.square, piece.role)
      )

    def reachesByDirection(
        reaches: List[PositionRelationExtractor.ClosedSliderReach]
    ): Map[RelationRayDirection, PositionRelationExtractor.ClosedSliderReach] =
      reaches.map(reach => reach.direction -> reach).toMap

    val affectedSquares = (
      boardFootprint.geometricControlOriginsToRefresh ++ boardFootprint.affectedOccupiedRayOrigins
    ).toList.sortBy(_.key).map(square => EvidenceSquare(square.key))
    val beforeSliders = affectedSquares.flatMap(square => sliderAt(beforeInventory, square))
    val afterSliders = affectedSquares.flatMap(square => sliderAt(afterInventory, square))
    val pairedFromBefore = beforeSliders.map { case before @ (side, slider) =>
      val movement = boardFootprint.pieceTransitions.find(candidate =>
        candidate.side == side && candidate.from.key.equalsIgnoreCase(slider.square.key) &&
          candidate.beforeRole.name.equalsIgnoreCase(slider.role.name)
      )
      val movedAfter = movement.filter(candidate =>
        isSlider(EvidencePieceRole(candidate.afterRole.name))
      ).flatMap(candidate =>
        sliderAt(afterInventory, EvidenceSquare(candidate.to.key)).filter { case (afterSide, afterSlider) =>
          afterSide == side && afterSlider.role.name.equalsIgnoreCase(candidate.afterRole.name)
        }
      )
      val stationaryAfter = Option.when(movement.isEmpty)(()).flatMap(_ =>
        sliderAt(afterInventory, slider.square).filter(_ == before)
      )
      Some(before) -> movedAfter.orElse(stationaryAfter)
    }
    val usedAfter = pairedFromBefore.flatMap(_._2).toSet
    val pairs = pairedFromBefore ++ afterSliders.filterNot(usedAfter).map(after => None -> Some(after))
    val removedById = geometricControl.targetSetChanges.flatMap(_.removed).map(edge => edge.fact.semanticId -> edge).toMap
    val establishedById =
      geometricControl.targetSetChanges.flatMap(_.established).map(edge => edge.fact.semanticId -> edge).toMap

    val changes = pairs.flatMap { case (beforeSlider, afterSlider) =>
      val side = beforeSlider.orElse(afterSlider).map(_._1).getOrElse(
        throw IllegalArgumentException("a slider-reach transition lost both slider occurrences")
      )
      val beforePiece = beforeSlider.map(_._2)
      val afterPiece = afterSlider.map(_._2)
      val beforeByDirection = reachesByDirection(
        beforePiece.toList.flatMap(piece => beforeInventory.sliderReachesFrom(piece.square))
      )
      val afterByDirection = reachesByDirection(
        afterPiece.toList.flatMap(piece => afterInventory.sliderReachesFrom(piece.square))
      )
      (beforeByDirection.keySet ++ afterByDirection.keySet).toList.sortBy(_.stableKey).flatMap { direction =>
        val beforeReach = beforeByDirection.get(direction)
        val afterReach = afterByDirection.get(direction)
        Option.when(beforeReach.map(_.witness) != afterReach.map(_.witness))(
          ClosedSliderReachChange(
            side = side,
            sliderBefore = beforePiece,
            sliderAfter = afterPiece,
            direction = direction,
            before = beforeReach,
            after = afterReach,
            removedControls = beforeReach.toList.flatMap(_.controls).flatMap(control =>
              removedById.get(control.source.semanticId)
            ),
            establishedControls = afterReach.toList.flatMap(_.controls).flatMap(control =>
              establishedById.get(control.source.semanticId)
            )
          )
        ).toList
      }
    }.sortBy(_.stableKey)

    ClosedSliderReachTransitionInventory(
      changes,
      boardFootprint.pieceTransitions.map(RelationMoveTransitionWitness.from).sortBy(_.stableKey)
    )

  def geometricSupporterCaptures(
      rootMove: CanonicalRootLegalMove,
      geometricSupport: GeometricSupportRelationTransition
  ): List[GeometricSupporterCaptureChange] =
    val supporterCaptures = rootMove.capture.toList.flatMap { capture =>
      geometricSupport.removed.collect {
        case support
            if rootMove.side != support.supporter.side &&
              capture.capturedSide == support.supporter.side &&
              capture.capturedSquare == support.supporter.square &&
              capture.capturedRole == support.supporter.role =>
          GeometricSupporterCaptureChange(support)
      }
    }.sortBy(_.stableKey)
    require(
      supporterCaptures.map(_.stableKey).distinct.size == supporterCaptures.size,
      "a supporter-capture transition cannot repeat an exact change"
    )
    supporterCaptures

  def sliderLineInterruptions(
      sliderReach: ClosedSliderReachTransitionInventory
  ): List[SliderLineInterruptionChange] =
    val establishedBarrierIds = comparableEstablished.collect {
      case relation if relation.kind == RelationFactKind.RayBarrier => relation.semanticId
    }.toSet

    val lineInterruptions = sliderReach.changes.flatMap { change =>
      for
        controllerBefore <- change.sliderBefore.toList
        controllerAfter <- change.sliderAfter.toList
        afterReach <- change.after.toList
        barrier <- Option(afterReach.raySource).filter(source => establishedBarrierIds(source.semanticId)).toList
        occupant <- afterReach.firstOccupant.toList
        geometry <- (barrier.detail match
          case RelationWitnessDetail.RayBarrier(_, _, _, _, exact) => Some(exact)
          case _ => None).toList
        interposer <- sliderReach.pieceMovements.filter(movement =>
          movement.side == occupant.side && movement.to == occupant.square && movement.afterRole == occupant.role
        )
        interrupted = change.removedControls
          .filter(control => geometry.strictlyBetween(control.target, interposer.to))
          .sortBy(control => geometry.distance(control.target) -> control.target.key)
        if interrupted.nonEmpty
      yield SliderLineInterruptionChange(
        interposer,
        change.side,
        controllerBefore,
        controllerAfter,
        interrupted,
        barrier
      )
    }.sortBy(_.stableKey)
    require(
      lineInterruptions.map(_.stableKey).distinct.size == lineInterruptions.size,
      "a line-interruption transition cannot repeat an exact change"
    )
    lineInterruptions

  def geometricLineOpenings(
      rootMove: CanonicalRootLegalMove,
      sliderReach: ClosedSliderReachTransitionInventory
  ): List[GeometricLineOpeningChange] =
    val removedBarrierIds = comparableRemoved.collect {
      case relation if relation.kind == RelationFactKind.RayBarrier => relation.semanticId
    }.toSet
    val lineOpenings = sliderReach.changes.flatMap { change =>
      for
        controllerBefore <- change.sliderBefore.toList
        controllerAfter <- change.sliderAfter.toList
        beforeReach <- change.before.toList
        afterReach <- change.after.toList
        removedBarrier <- Option(beforeReach.raySource).filter(source => removedBarrierIds(source.semanticId)).toList
        geometry <- (removedBarrier.detail match
          case RelationWitnessDetail.RayBarrier(_, _, _, _, exact) => Some(exact)
          case _ => None).toList
        blocker <- beforeReach.firstOccupant.toList
        barrierPattern <- (removedBarrier.detail match
          case ray: RelationWitnessDetail.RayBarrier => Some(RelationRayProjection.pattern(ray))
          case _                                      => None).toList
        removal <- (
          sliderReach.pieceMovements.filter(movement =>
            movement.side == blocker.side && movement.from == blocker.square && movement.beforeRole == blocker.role
          ).map(movement => RelationBlockerRemovalMode.Moved -> movement) ++
            rootMove.capture.filter(capture =>
              capture.capturedSide == blocker.side && capture.capturedSquare == blocker.square &&
                capture.capturedRole == blocker.role
            ).map(_ => RelationBlockerRemovalMode.Captured -> rootMove.witness)
        )
        openedIds = change.establishedControls
          .filter(control => geometry.strictlyBetween(control.target, blocker.square))
          .map(_.fact.semanticId)
          .toSet
        opened = afterReach.controls
          .flatMap(control => change.establishedControls.find(_.fact.semanticId == control.source.semanticId))
          .filter(control => openedIds(control.fact.semanticId))
        if opened.nonEmpty
      yield GeometricLineOpeningChange(
        removingMove = removal._2,
        controllerSide = change.side,
        controllerBefore = controllerBefore.square,
        controllerAfter = controllerAfter.square,
        controllerRole = controllerAfter.role,
        blocker = blocker,
        openedControls = opened,
        barrierPattern = barrierPattern,
        removalMode = removal._1,
        removedBarrier = removedBarrier
      )
    }.sortBy(_.stableKey)
    require(
      lineOpenings.map(_.stableKey).distinct.size == lineOpenings.size,
      "a line-opening transition cannot repeat an exact change"
    )
    lineOpenings

  private def geometricControlTargetSetChanges(
      footprint: BoardTransitionFootprint
  ): List[GeometricControlTargetSetChange] =
    val removed = geometricControlEdges(removedGeometricControlsByKey)
    val established = geometricControlEdges(establishedGeometricControlsByKey)
    val changedTargetKeys = footprint.changedSquares.flatMap { square =>
      val target = EvidenceSquare(square.key)
      List(Color.White -> target, Color.Black -> target)
    }
    val keys = (
      removed.map(edge => edge.side -> edge.target) ++
        established.map(edge => edge.side -> edge.target) ++
        changedTargetKeys
    )
      .distinct
      .sortBy { case (side, target) => side.toString -> target.key }
    keys.flatMap { case (side, target) =>
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
      val change = GeometricControlTargetSetChange(
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
      Option.when(
        (before.nonEmpty || after.nonEmpty) &&
          (
            change.beforeState != change.afterState ||
              change.removedControllers.nonEmpty ||
              change.establishedControllers.nonEmpty
          )
      )(change)
    }

  private def geometricControlEdges(
      controls: Map[GeometricControlKey, (RelationControlTarget, RelationFactEvidence)]
  ): List[GeometricControlEdge] =
    controls.values.map { case (targetState, fact) => geometricControl(fact, targetState) }.toList.sortBy(_.fact.semanticId)

  private def fullControlsAt(
      inventory: PositionRelationInventoryCertificate,
      side: Color,
      target: EvidenceSquare
  ): List[GeometricControlEdge] =
    val square = boardSquare(target)
    inventory
      .relationsFor(RelationDependencyKey.AttackTarget(square))
      .map(fact => geometricControl(fact, controlTargetAt(inventory, side, target)))
      .filter(_.side == side)
      .sortBy(_.fact.semanticId)

  private def geometricControl(
      fact: RelationFactEvidence,
      targetState: RelationControlTarget
  ): GeometricControlEdge =
    fact.detail match
      case RelationWitnessDetail.GeometricControl(side, controller, controllerRole, target) =>
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

    val removed = targetSetChanges.flatMap(_.before.flatMap(geometricSupport)).sortBy(_.fact.semanticId)
    val established = targetSetChanges.flatMap(_.after.flatMap(geometricSupport)).sortBy(_.fact.semanticId)
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
    controls.values.flatMap { case (targetState, fact) =>
      geometricSupport(geometricControl(fact, targetState))
    }.toList.sortBy(_.fact.semanticId)

  private def fullSupportsAt(
      targetSetChanges: List[GeometricControlTargetSetChange],
      supported: RelationColoredPieceWitness,
      before: Boolean
  ): List[GeometricSupportEdge] =
    targetSetChanges
      .find(change => change.side == supported.side && change.target == supported.square)
      .toList
      .flatMap(change => if before then change.before else change.after)
      .flatMap(geometricSupport)
      .filter(_.supported == supported)
      .sortBy(_.fact.semanticId)

  private def geometricSupport(
      edge: GeometricControlEdge
  ): Option[GeometricSupportEdge] =
    edge.targetState match
      case RelationControlTarget.Friendly(supportedRole) =>
        Some(
          GeometricSupportEdge(
            edge.fact,
            RelationColoredPieceWitness(edge.controller.square, edge.controller.role, edge.side),
            RelationColoredPieceWitness(edge.target, supportedRole, edge.side)
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
      inventory: PositionRelationInventoryCertificate,
      label: String
  ): Map[GeometricControlKey, (RelationControlTarget, RelationFactEvidence)] =
    val values = relations.flatMap { relation =>
      relation.detail match
        case RelationWitnessDetail.GeometricControl(side, controller, role, target) =>
          List(
            GeometricControlKey(side, controller, role, target) ->
              (controlTargetAt(inventory, side, target) -> relation)
          )
        case _ => Nil
    }
    require(
      values.map(_._1).distinct.size == values.size,
      s"a relation transition must contain each $label geometric-control edge exactly once"
    )
    values.toMap

private[chessjudgment] object RelationInventoryTransition:
  def fromStaticTransition(
      boardRemoved: List[RelationFactEvidence],
      boardEstablished: List[RelationFactEvidence],
      beforeInventory: PositionRelationInventoryCertificate,
      afterInventory: PositionRelationInventoryCertificate
  ): RelationInventoryTransition =
    RelationInventoryTransition(
      comparableRemoved = boardRemoved,
      comparableEstablished = boardEstablished,
      beforeInventory = beforeInventory,
      afterInventory = afterInventory
    )

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
    require(
      (removedBySemanticId.values ++ establishedBySemanticId.values).forall(_.kind != RelationFactKind.LegalMove),
      "a sparse semantic transition cannot own position-scoped legal moves"
    )
    require(
      removedBySemanticId.keySet.intersect(establishedBySemanticId.keySet).isEmpty,
      "an exact relation transition cannot remove and establish the same semantic fact"
    )
    val removed = removedBySemanticId.toList.sortBy(_._1).map { case (_, relation) =>
      semanticChange(RelationChangeDirection.Removed, relation, boardFootprint, legalStep.uci)
    }
    val established = establishedBySemanticId.toList.sortBy(_._1).map { case (_, relation) =>
      semanticChange(RelationChangeDirection.Established, relation, boardFootprint, legalStep.uci)
    }
    val rootMove = canonicalRootLegalMove(legalStep, boardFootprint, relationTransition)
    new RelationSemanticDelta(
      moveUci = EvidenceRef.normalizeMove(legalStep.uci),
      changes = (removed ++ established).sortBy(_.stableKey),
      rootMove = rootMove,
      beforeInventory = relationTransition.beforeInventory,
      afterInventory = relationTransition.afterInventory,
      transitionFootprint = boardFootprint,
      relationTransition = relationTransition
    )

  private[judgment] def changedNamedRays(
      removed: List[RelationSemanticChange],
      established: List[RelationSemanticChange]
  ): List[RelationNamedRayChange] =
    def namedChanges(
        changes: List[RelationSemanticChange]
    ): List[(RelationNamedRayProjection, RelationSemanticChange)] =
      changes.flatMap(change =>
        change.detail match
          case ray: RelationWitnessDetail.RayBarrier =>
            RelationRayProjection.named(ray).map(_ -> change)
          case _ => None
      )
    val removedNamed = namedChanges(removed)
    val establishedNamed = namedChanges(established)

    def projectionOrder(projection: RelationNamedRayProjection): String =
      List(
        projection.side.toString.toLowerCase,
        projection.attackerSquare.key.toLowerCase,
        projection.attackerRole.name.toLowerCase,
        projection.barrier.square.key.toLowerCase,
        projection.barrier.role.name.toLowerCase,
        projection.immediateTarget.map(target =>
          s"${target.side.toString.toLowerCase}:${target.role.name.toLowerCase}@${target.square.key.toLowerCase}"
        ).getOrElse("-"),
        projection.axis.toString.toLowerCase,
        projection.pattern.toString.toLowerCase
      ).mkString(":")

    def batteryIdentity(projection: RelationNamedRayProjection): RelationBatteryFormationWitness =
      val sliders = List(
        RelationColoredPieceWitness(
          projection.attackerSquare,
          projection.attackerRole,
          projection.side
        ),
        projection.barrier
      ).sortBy(piece => piece.square.key.toLowerCase -> piece.role.name.toLowerCase)
      RelationBatteryFormationWitness(
        projection.side,
        sliders.head,
        sliders.last,
        projection.axis
      )

    def changedBatteryRays(
        from: List[(RelationNamedRayProjection, RelationSemanticChange)],
        to: List[(RelationNamedRayProjection, RelationSemanticChange)]
    ): List[(RelationNamedRayProjection, RelationSemanticChange)] =
      val toIdentities = to.map(value => batteryIdentity(value._1)).toSet
      from.groupBy(value => batteryIdentity(value._1)).toList.sortBy(_._1.stableKey).flatMap { case (identity, values) =>
        if toIdentities(identity) then Nil
        else
          val ordered = values.sortBy(value => projectionOrder(value._1))
          val targetBearing = ordered.filter(_._1.immediateTarget.nonEmpty)
          if targetBearing.nonEmpty then targetBearing else ordered.take(1)
      }

    val (removedBatteries, removedOther) = removedNamed.partition(_._1.pattern == RelationRayPattern.Battery)
    val (establishedBatteries, establishedOther) =
      establishedNamed.partition(_._1.pattern == RelationRayPattern.Battery)
    val removedOtherProjections = removedOther.map(_._1).toSet
    val establishedOtherProjections = establishedOther.map(_._1).toSet
    (removedOther.filterNot { case (projection, _) => establishedOtherProjections(projection) } ++
      establishedOther.filterNot { case (projection, _) => removedOtherProjections(projection) } ++
      changedBatteryRays(removedBatteries, establishedBatteries) ++
      changedBatteryRays(establishedBatteries, removedBatteries))
      .map(RelationNamedRayChange.apply)
      .sortBy(_.stableKey)

  private def canonicalRootLegalMove(
      legalStep: LegalReplayStep,
      footprint: BoardTransitionFootprint,
      transition: RelationInventoryTransition
  ): CanonicalRootLegalMove =
    val legalMove = transition.beforeInventory.legalMove(legalStep.uci).getOrElse(
      throw IllegalArgumentException(s"'${legalStep.uci}' is absent from its closed legal-move inventory")
    )
    require(
      footprint.moveEffect.asInstanceOf[AnyRef].eq(legalMove.boardEffect.asInstanceOf[AnyRef]),
      s"'${legalStep.uci}' must reuse one canonical move effect across legal resources and transition geometry"
    )
    val movement = legalMove.movement
    val footprintMovements = footprint.pieceTransitions.filter(exact =>
      exact.side == movement.side && exact.from.key.equalsIgnoreCase(movement.from.key) &&
        exact.to.key.equalsIgnoreCase(movement.to.key) &&
        exact.beforeRole.name.equalsIgnoreCase(movement.beforeRole.name) &&
        exact.afterRole.name.equalsIgnoreCase(movement.afterRole.name)
    )
    require(footprintMovements.size == 1, s"'${legalStep.uci}' must own one exact primary piece transition")
    if footprint.pieceTransitions.size > 1 then
      require(
        footprint.pieceTransitions.size == 2 &&
          footprint.pieceTransitions.forall(_.side == movement.side) &&
          footprint.pieceTransitions.map(_.beforeRole).toSet == Set(King, Rook),
        s"'${legalStep.uci}' may have multiple piece transitions only for castling"
      )
    CanonicalRootLegalMove(legalMove.source, movement, legalMove.moveUci, legalMove.capture)

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
