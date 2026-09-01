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
  val key: RelationChangeKey = RelationChangeKey(direction, kind, semanticId)
  val stableKey: String = key.stableKey

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

  lazy val sliderReachTransition: ClosedSliderReachTransitionInventory =
    relationTransition.sliderReachTransition(transitionFootprint)

  private lazy val byDirectionAndKind: Map[(RelationChangeDirection, RelationFactKind), List[RelationSemanticChange]] =
    changes.groupMap(change => change.direction -> change.kind)(identity)

  private lazy val bySemanticId: Map[String, RelationSemanticChange] =
    changes.map(change => change.semanticId -> change).toMap

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
    capture: Option[RelationLegalCaptureWitness],
    mode: PositionRelationExtractor.ClosedLegalMovementMode
):
  def side: Color = witness.side
  def from: EvidenceSquare = witness.from
  def to: EvidenceSquare = witness.to
  def beforeRole: EvidencePieceRole = witness.beforeRole
  def afterRole: EvidencePieceRole = witness.afterRole
  def isCastling: Boolean = mode == PositionRelationExtractor.ClosedLegalMovementMode.Castling

private[chessjudgment] final case class GeometricControlKey(
    side: Color,
    controller: EvidenceSquare,
    controllerRole: EvidencePieceRole,
    target: EvidenceSquare
)

private[chessjudgment] final case class GeometricControlRelationTransition(
    targetSetChanges: List[GeometricControlTargetSetChange]
)

private[chessjudgment] final case class GeometricControlEdge(
    fact: RelationFactEvidence,
    side: Color,
    controller: RelationPieceWitness,
    target: EvidenceSquare
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
  def stableKey: String =
    List(
      side.toString.toLowerCase,
      target.key.toLowerCase,
      beforeState.toString,
      afterState.toString,
      before.map(_.fact.semanticId).sorted.mkString(","),
      after.map(_.fact.semanticId).sorted.mkString(",")
    ).mkString(":")

/** One exact directional reach change for a slider identity. The generic
  * L1 reach fact consumes this inventory without reconstructing slider pairing
  * or ray state independently.
  */
private[chessjudgment] final case class ClosedSliderReachChange(
    side: Color,
    sliderBefore: Option[RelationPieceWitness],
    sliderAfter: Option[RelationPieceWitness],
    direction: RelationRayDirection,
    before: Option[PositionRelationExtractor.ClosedSliderReach],
    after: Option[PositionRelationExtractor.ClosedSliderReach]
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
  def stableKey: String =
    def piece(value: Option[RelationPieceWitness]): String =
      value.map(item => s"${item.role.name.toLowerCase}@${item.square.key.toLowerCase}").getOrElse("-")
    s"${side.toString.toLowerCase}:${piece(sliderBefore)}:${piece(sliderAfter)}:${direction.stableKey}"

private[chessjudgment] final case class ClosedSliderReachTransitionInventory(
    changes: List[ClosedSliderReachChange]
):
  require(
    changes.map(_.stableKey).distinct.size == changes.size,
    "one slider-reach transition cannot repeat a directional change"
  )
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

  private lazy val removedGeometricControlsByKey: Map[GeometricControlKey, RelationFactEvidence] =
    geometricControlsByKey(comparableRemoved, "removed")

  private lazy val establishedGeometricControlsByKey: Map[GeometricControlKey, RelationFactEvidence] =
    geometricControlsByKey(comparableEstablished, "established")

  def geometricControlTransition(
      boardFootprint: BoardTransitionFootprint
  ): GeometricControlRelationTransition =
    val targetSetChanges = geometricControlTargetSetChanges(boardFootprint)
    GeometricControlRelationTransition(targetSetChanges)

  def sliderReachTransition(
      boardFootprint: BoardTransitionFootprint
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
            after = afterReach
          )
        ).toList
      }
    }.sortBy(_.stableKey)

    ClosedSliderReachTransitionInventory(changes)

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
      controls: Map[GeometricControlKey, RelationFactEvidence]
  ): List[GeometricControlEdge] =
    controls.values.map(geometricControl).toList.sortBy(_.fact.semanticId)

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
      case RelationWitnessDetail.GeometricControl(side, controller, controllerRole, target) =>
        GeometricControlEdge(
          fact,
          side,
          RelationPieceWitness(controller, controllerRole),
          target
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

  private def geometricControlsByKey(
      relations: List[RelationFactEvidence],
      label: String
  ): Map[GeometricControlKey, RelationFactEvidence] =
    val values = relations.flatMap { relation =>
      relation.detail match
        case RelationWitnessDetail.GeometricControl(side, controller, role, target) =>
          List(
            GeometricControlKey(side, controller, role, target) -> relation
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

/** Occurrence binding for one already-produced semantic change. It owns only
  * the canonical graph source; chess meaning remains on the exact semantic
  * change object and cannot diverge through copied fields.
  */
private[chessjudgment] final class CanonicalRelationChange private (
    private[chessjudgment] val semanticChange: RelationSemanticChange,
    val sourceNode: CanonicalRelationNode
):
  def direction: RelationChangeDirection = semanticChange.direction
  def source: EvidenceRef = sourceNode.ref
  def relation: RelationFactEvidence = sourceNode.relation
  def kind: RelationFactKind = semanticChange.kind
  def semanticId: String = semanticChange.semanticId
  def dependencySquares: List[EvidenceSquare] = semanticChange.dependencySquares
  def proofKeys: List[RelationProofKey] = semanticChange.proofKeys
  def key: RelationChangeKey = semanticChange.key
  def stableKey: String = semanticChange.stableKey

private[chessjudgment] object CanonicalRelationChange:
  def bind(
      semanticChange: RelationSemanticChange,
      sourceNode: CanonicalRelationNode
  ): CanonicalRelationChange =
    require(
      sourceNode.relation == semanticChange.relation,
      "a canonical relation occurrence must bind its exact semantic relation"
    )
    new CanonicalRelationChange(semanticChange, sourceNode)

private[chessjudgment] final case class CanonicalRelationDelta private (
    semantic: RelationSemanticDelta,
    changes: List[CanonicalRelationChange]
):
  require(changes.map(_.stableKey).distinct.size == changes.size, "duplicate canonical relation changes")
  require(changes.map(_.source.id).distinct.size == changes.size, "canonical relation changes must own distinct sources")

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
      CanonicalRelationChange.bind(change, sourceNode)
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
    CanonicalRootLegalMove(legalMove.source, movement, legalMove.moveUci, legalMove.capture, legalMove.mode)

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
