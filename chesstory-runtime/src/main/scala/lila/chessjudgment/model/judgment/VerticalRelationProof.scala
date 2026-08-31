package lila.chessjudgment.model.judgment

import _root_.chess.{ Bishop, King, Pawn, Queen, Rook, Square }
import scala.collection.immutable.VectorMap

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.position.{ BoardPieceTransition, BoardTransitionFootprint }

/** Explicit proof strata. The numeric order is proof authority, not a score:
  * a derivation may consume only strictly lower strata.
  */
private[chessjudgment] enum RelationProofStage:
  case PositionFact
  case TransitionFact
  case Vertical(level: Int)

private[chessjudgment] object RelationProofStage:
  def rank(stage: RelationProofStage): Int =
    stage match
      case RelationProofStage.PositionFact   => 0
      case RelationProofStage.TransitionFact => 1
      case RelationProofStage.Vertical(level) =>
        require(level > 0, "a vertical proof level must be positive")
        level + 1

  def id(stage: RelationProofStage): String =
    stage match
      case RelationProofStage.PositionFact   => "position_fact"
      case RelationProofStage.TransitionFact => "transition_fact"
      case RelationProofStage.Vertical(level) => s"vertical_l$level"

private[chessjudgment] enum VerticalRelationContractKind:
  case CaptureRecaptureInventory
  case CreatedCheckResponseInventory
  case RootCheckResponse
  case SliderReachDelta
  case PawnTopologyTransition
  case StalemateTransition

private[chessjudgment] object VerticalRelationContractKind:
  def id(kind: VerticalRelationContractKind): String =
    kind match
      case VerticalRelationContractKind.CaptureRecaptureInventory =>
        "capture_recapture_inventory"
      case VerticalRelationContractKind.CreatedCheckResponseInventory =>
        "created_check_response_inventory"
      case VerticalRelationContractKind.RootCheckResponse =>
        "root_check_response"
      case VerticalRelationContractKind.SliderReachDelta =>
        "slider_reach_delta"
      case VerticalRelationContractKind.PawnTopologyTransition =>
        "pawn_topology_transition"
      case VerticalRelationContractKind.StalemateTransition =>
        "stalemate_transition"

  def outputStage(kind: VerticalRelationContractKind): RelationProofStage =
    kind match
      case VerticalRelationContractKind.CaptureRecaptureInventory |
          VerticalRelationContractKind.CreatedCheckResponseInventory |
          VerticalRelationContractKind.RootCheckResponse |
          VerticalRelationContractKind.SliderReachDelta |
          VerticalRelationContractKind.PawnTopologyTransition |
          VerticalRelationContractKind.StalemateTransition =>
        RelationProofStage.Vertical(1)

  def rank(kind: VerticalRelationContractKind): Int =
    RelationProofStage.rank(outputStage(kind))

  def forDetail(detail: RelationWitnessDetail): Option[VerticalRelationContractKind] =
    detail match
      case _: RelationWitnessDetail.CaptureRecaptureInventory =>
        Some(VerticalRelationContractKind.CaptureRecaptureInventory)
      case _: RelationWitnessDetail.CreatedCheckResponseInventory =>
        Some(VerticalRelationContractKind.CreatedCheckResponseInventory)
      case _: RelationWitnessDetail.RootCheckResponse =>
        Some(VerticalRelationContractKind.RootCheckResponse)
      case _: RelationWitnessDetail.SliderReachDelta =>
        Some(VerticalRelationContractKind.SliderReachDelta)
      case _: RelationWitnessDetail.PawnTopologyTransition =>
        Some(VerticalRelationContractKind.PawnTopologyTransition)
      case _: RelationWitnessDetail.StalemateTransition =>
        Some(VerticalRelationContractKind.StalemateTransition)
      case _ => None

final case class RelationLegalMoveResourceWitness(
    movement: RelationMoveTransitionWitness,
    moveUci: String,
    capture: Option[RelationLegalCaptureWitness]
):
  require(
    EvidenceRef.normalizeMove(moveUci).nonEmpty,
    "a legal resource witness needs one normalized UCI move"
  )
  require(
    EvidenceRef.sameMove(
      moveUci,
      s"${movement.from.key}${movement.to.key}${EvidencePieceRole.promotionSuffix(movement.beforeRole, movement.afterRole)}"
    ),
    "a legal resource witness must retain its exact movement"
  )

  private[chessjudgment] def stableKey: String =
    s"${movement.stableKey}:${EvidenceRef.normalizeMove(moveUci)}:${capture.map(value => s"${value.capturedSide.toString.toLowerCase}:${value.capturedRole.name.toLowerCase}@${value.capturedSquare.key.toLowerCase}").getOrElse("quiet")}" 

enum RelationCheckResponseMode:
  case KingMove
  case CaptureChecker
  case Interpose

  private[chessjudgment] def stableKey: String =
    toString.toLowerCase

final case class RelationCheckResponseWitness(
    resource: RelationLegalMoveResourceWitness,
    modes: List[RelationCheckResponseMode]
):
  require(modes.nonEmpty, "a check response needs one exact response mode")
  require(
    modes.distinct.size == modes.size && modes == modes.sortBy(_.stableKey),
    "check response modes must be unique and canonically ordered"
  )

  private[chessjudgment] def stableKey: String =
    s"${resource.stableKey}:${modes.map(_.stableKey).mkString(",")}"

final case class RelationControlledKingDestinationWitness(
    resource: RelationMovementResourceWitness,
    controllers: List[RelationPieceWitness]
):
  require(
    resource.mode == RelationMovementResourceMode.ControlledDestination,
    "a controlled king destination needs one exact geometric movement resource"
  )
  require(
    controllers.nonEmpty && controllers.distinct.size == controllers.size &&
      controllers == controllers.sortBy(value => value.square.key -> value.role.name),
    "a controlled king destination needs unique canonical opponent controllers"
  )

  private[chessjudgment] def stableKey: String =
    s"${resource.stableKey}:post-move-enemy-king:${controllers.map(value => s"${value.role.name.toLowerCase}@${value.square.key.toLowerCase}").mkString(",")}"

enum RelationCheckTerminalState:
  case Ongoing
  case Checkmate

enum RelationMovementResourceMode:
  case ControlledDestination
  case PawnAdvance
  case PawnDoubleAdvance
  case EnPassantCapture(capturedPawn: RelationColoredPieceWitness)

  private[chessjudgment] def stableKey: String =
    this match
      case ControlledDestination => "controlled_destination"
      case PawnAdvance           => "pawn_advance"
      case PawnDoubleAdvance     => "pawn_double_advance"
      case EnPassantCapture(capturedPawn) =>
        s"en_passant:${capturedPawn.side.toString.toLowerCase}:${capturedPawn.role.name.toLowerCase}@${capturedPawn.square.key.toLowerCase}"

final case class RelationMovementResourceWitness(
    destination: EvidenceSquare,
    target: RelationControlTarget,
    mode: RelationMovementResourceMode = RelationMovementResourceMode.ControlledDestination
):
  private[chessjudgment] def stableKey: String =
    val targetKey = target match
      case RelationControlTarget.Empty          => "empty"
      case RelationControlTarget.Friendly(role) => s"friendly:${role.name.toLowerCase}"
      case RelationControlTarget.Enemy(role)    => s"enemy:${role.name.toLowerCase}"
    s"${destination.key.toLowerCase}:$targetKey:${mode.stableKey}"

final case class RelationAbsolutePinPathWitness(
    pinner: RelationPieceWitness,
    pinned: RelationPieceWitness,
    kingSquare: EvidenceSquare,
    geometry: RelationRayGeometry
):
  require(pinner != pinned, "an absolute-pin path needs distinct pinner and pinned piece")
  require(pinned.square != kingSquare, "an absolute-pin path needs the king behind the pinned piece")

  private[chessjudgment] def stableKey: String =
    s"${pinner.role.name.toLowerCase}@${pinner.square.key.toLowerCase}:${pinned.role.name.toLowerCase}@${pinned.square.key.toLowerCase}:${kingSquare.key.toLowerCase}:${geometry.direction.stableKey}:${geometry.squares.map(_.key.toLowerCase).mkString(",")}"

final case class RelationRestrictedResourceWitness(
    piece: RelationPieceWitness,
    resource: RelationMovementResourceWitness,
    kingSquare: EvidenceSquare,
    postMoveControllers: List[RelationPieceWitness],
    absolutePinPaths: List[RelationAbsolutePinPathWitness]
):
  require(
    postMoveControllers.nonEmpty && postMoveControllers.distinct.size == postMoveControllers.size &&
      postMoveControllers == postMoveControllers.sortBy(value => value.square.key -> value.role.name),
    "a restricted resource needs unique canonical post-move king controllers"
  )
  require(
    absolutePinPaths.distinct.size == absolutePinPaths.size &&
      absolutePinPaths == absolutePinPaths.sortBy(_.stableKey) &&
      absolutePinPaths.forall(path =>
        path.pinned == piece && path.kingSquare == kingSquare && postMoveControllers.contains(path.pinner)
      ),
    "absolute-pin explanations must match the restricted piece, king, and actual controller"
  )

  private[chessjudgment] def stableKey: String =
    s"${piece.role.name.toLowerCase}@${piece.square.key.toLowerCase}:${resource.stableKey}:${kingSquare.key.toLowerCase}:${postMoveControllers.map(value => s"${value.role.name.toLowerCase}@${value.square.key.toLowerCase}").mkString(",")}:${absolutePinPaths.map(_.stableKey).mkString("[", ",", "]")}"

final case class RelationSliderReachWitness(
    segment: List[RelationControlReachWitness],
    firstOccupant: Option[RelationColoredPieceWitness]
):
  require(segment.nonEmpty, "a slider-reach witness needs one controlled square")
  require(
    segment.map(_.square).distinct.size == segment.size,
    "a slider-reach witness cannot repeat a controlled square"
  )
  require(
    segment.dropRight(1).forall(_.target == RelationControlTarget.Empty),
    "a slider-reach witness cannot continue beyond its first occupant"
  )
  require(
    firstOccupant match
      case Some(occupant) =>
        segment.last.square == occupant.square && (segment.last.target match
          case RelationControlTarget.Friendly(role) => role == occupant.role
          case RelationControlTarget.Enemy(role)    => role == occupant.role
          case RelationControlTarget.Empty => false)
      case None => segment.forall(_.target == RelationControlTarget.Empty),
    "a slider-reach witness must close its exact first occupant"
  )

  private[chessjudgment] def stableKey: String =
    s"${segment.map(_.stableKey).mkString("[", ",", "]")}:${firstOccupant.map(piece => s"${piece.side.toString.toLowerCase}:${piece.role.name.toLowerCase}@${piece.square.key.toLowerCase}").getOrElse("edge")}" 

enum RelationPawnConnectionKind:
  case GeometricSupport
  case Phalanx

  private[chessjudgment] def stableKey: String = toString.toLowerCase

enum RelationPawnConnectionDirection:
  case Supports
  case SupportedBy
  case Peer

  private[chessjudgment] def stableKey: String = toString.toLowerCase

final case class RelationPawnConnectionWitness(
    peer: EvidenceSquare,
    kind: RelationPawnConnectionKind,
    direction: RelationPawnConnectionDirection
):
  require(
    (kind == RelationPawnConnectionKind.Phalanx) ==
      (direction == RelationPawnConnectionDirection.Peer),
    "only a phalanx is an undirected pawn peer relation"
  )
  private[chessjudgment] def stableKey: String =
    s"${peer.key.toLowerCase}:${kind.stableKey}:${direction.stableKey}"

final case class RelationPawnTopologyStateWitness(
    side: _root_.chess.Color,
    square: EvidenceSquare,
    doubled: Boolean,
    isolated: Boolean,
    passed: Boolean,
    geometricallyProtectedPasser: Boolean,
    frontSquare: Option[EvidenceSquare],
    frontOccupant: Option[RelationColoredPieceWitness],
    componentPawns: List[EvidenceSquare],
    connections: List[RelationPawnConnectionWitness],
    enemyPawnContacts: List[EvidenceSquare]
):
  require(!geometricallyProtectedPasser || passed, "a protected passer state must first be passed")
  require(
    componentPawns.nonEmpty && componentPawns.contains(square) &&
      componentPawns.distinct.size == componentPawns.size && componentPawns == componentPawns.sortBy(_.key),
    "a pawn topology state needs one canonical connected component containing the pawn"
  )
  require(
    connections.distinct.size == connections.size && connections == connections.sortBy(_.stableKey),
    "pawn connections must be unique and canonically ordered"
  )
  require(
    enemyPawnContacts.distinct.size == enemyPawnContacts.size && enemyPawnContacts == enemyPawnContacts.sortBy(_.key),
    "opposing pawn contacts must be unique and canonically ordered"
  )
  require(frontOccupant.isEmpty || frontSquare.nonEmpty, "a front occupant needs its exact front square")

  private[chessjudgment] def stableKey: String =
    def squareKey(value: EvidenceSquare): String = value.key.toLowerCase
    def coloredPieceKey(value: RelationColoredPieceWitness): String =
      s"${value.side.toString.toLowerCase}:${squareKey(value.square)}:${value.role.name.toLowerCase}"
    s"${side.toString.toLowerCase}:${squareKey(square)}:$doubled:$isolated:$passed:$geometricallyProtectedPasser:${frontSquare.map(squareKey).getOrElse("-")}:${frontOccupant.map(coloredPieceKey).getOrElse("-")}:${componentPawns.map(squareKey).mkString("[", ",", "]")}:${connections.map(_.stableKey).mkString("[", ",", "]")}:${enemyPawnContacts.map(squareKey).mkString("[", ",", "]")}"

enum RelationPawnTopologyFacet:
  case Doubled
  case Isolated
  case Passed
  case GeometricallyProtectedPasser
  case FrontState
  case Component
  case Connections
  case EnemyPawnContacts
  case Removed

  private[chessjudgment] def stableKey: String = toString.toLowerCase

private[chessjudgment] object RelationPawnTopologyFacet:
  def changed(
      before: Option[RelationPawnTopologyStateWitness],
      after: Option[RelationPawnTopologyStateWitness]
  ): List[RelationPawnTopologyFacet] =
    val values = (before, after) match
      case (Some(previous), Some(next)) =>
        require(previous.side == next.side, "one pawn identity cannot change side")
        List(
          Option.when(previous.doubled != next.doubled)(RelationPawnTopologyFacet.Doubled),
          Option.when(previous.isolated != next.isolated)(RelationPawnTopologyFacet.Isolated),
          Option.when(previous.passed != next.passed)(RelationPawnTopologyFacet.Passed),
          Option.when(previous.geometricallyProtectedPasser != next.geometricallyProtectedPasser)(
            RelationPawnTopologyFacet.GeometricallyProtectedPasser
          ),
          Option.when(
            previous.frontSquare != next.frontSquare || previous.frontOccupant != next.frontOccupant
          )(RelationPawnTopologyFacet.FrontState),
          Option.when(previous.componentPawns != next.componentPawns)(RelationPawnTopologyFacet.Component),
          Option.when(previous.connections != next.connections)(RelationPawnTopologyFacet.Connections),
          Option.when(previous.enemyPawnContacts != next.enemyPawnContacts)(
            RelationPawnTopologyFacet.EnemyPawnContacts
          )
        ).flatten
      case (Some(_), None) => List(RelationPawnTopologyFacet.Removed)
      case (None, Some(_)) =>
        throw IllegalArgumentException("a legal chess transition cannot create a new pawn identity")
      case (None, None) =>
        throw IllegalArgumentException("a pawn topology transition needs one pawn occurrence")
    values.sortBy(_.stableKey)

private[chessjudgment] enum VerticalRelationPremiseRole:
  case RootMove
  case RootCapture
  case ChosenCheckResponse(response: RelationCheckResponseWitness)
  case CheckControlDelta(
      checkedSide: _root_.chess.Color,
      kingSquare: EvidenceSquare
  )
  case CheckerControl(
      occurrence: RelationSnapshotOccurrence,
      controllingSide: _root_.chess.Color,
      checker: RelationPieceWitness,
      kingSquare: EvidenceSquare
  )
  case CheckerRay(
      occurrence: RelationSnapshotOccurrence,
      checker: RelationPieceWitness,
      kingSquare: EvidenceSquare
  )
  case CheckResponse(response: RelationCheckResponseWitness)
  case GeometricRecaptureCandidate(
      recapturingSide: _root_.chess.Color,
      recapturer: RelationPieceWitness,
      target: RelationPieceWitness
  )
  case LegalRecapture(resource: RelationLegalMoveResourceWitness)
  case AbsolutePinBarrier(
      occurrence: RelationSnapshotOccurrence,
      restrictedSide: _root_.chess.Color,
      pinner: RelationPieceWitness,
      pinned: RelationPieceWitness,
      kingSquare: EvidenceSquare,
      axis: RelationAxisSignal
  )
  case MovementResource(
      pieceSide: _root_.chess.Color,
      piece: RelationPieceWitness,
      resource: RelationMovementResourceWitness
  )
  case LegalMoveResource(
      occurrence: RelationSnapshotOccurrence,
      resource: RelationLegalMoveResourceWitness
  )
  case PawnTopologyFacetSource(
      occurrence: RelationSnapshotOccurrence,
      side: _root_.chess.Color,
      pawn: EvidenceSquare,
      facet: RelationPawnTopologyFacet,
      componentPawns: List[EvidenceSquare],
      semanticId: String
  )
  case PawnPassageStateSource(
      occurrence: RelationSnapshotOccurrence,
      side: _root_.chess.Color,
      pawn: EvidenceSquare,
      semanticId: String
  )
  case SliderReachControl(
      occurrence: RelationSnapshotOccurrence,
      side: _root_.chess.Color,
      slider: RelationPieceWitness,
      direction: RelationRayDirection,
      control: RelationControlReachWitness
  )
  case SliderReachBarrier(
      occurrence: RelationSnapshotOccurrence,
      side: _root_.chess.Color,
      slider: RelationPieceWitness,
      direction: RelationRayDirection,
      firstOccupant: Option[RelationColoredPieceWitness]
  )

private[chessjudgment] object VerticalRelationPremiseRole:
  def id(role: VerticalRelationPremiseRole): String =
    role match
      case VerticalRelationPremiseRole.RootMove => "root_move"
      case VerticalRelationPremiseRole.RootCapture => "root_capture"
      case VerticalRelationPremiseRole.ChosenCheckResponse(response) =>
        s"chosen_check_response:${response.stableKey}"
      case VerticalRelationPremiseRole.CheckControlDelta(checkedSide, kingSquare) =>
        s"check_control_delta:${checkedSide.toString.toLowerCase}:${kingSquare.key.toLowerCase}"
      case VerticalRelationPremiseRole.CheckerControl(occurrence, controllingSide, checker, kingSquare) =>
        s"checker_control:${occurrence.toString.toLowerCase}:${controllingSide.toString.toLowerCase}:${checker.square.key.toLowerCase}:${checker.role.name.toLowerCase}:${kingSquare.key.toLowerCase}"
      case VerticalRelationPremiseRole.CheckerRay(occurrence, checker, kingSquare) =>
        s"checker_ray:${occurrence.toString.toLowerCase}:${checker.square.key.toLowerCase}:${checker.role.name.toLowerCase}:${kingSquare.key.toLowerCase}"
      case VerticalRelationPremiseRole.CheckResponse(response) =>
        s"check_response:${response.stableKey}"
      case VerticalRelationPremiseRole.GeometricRecaptureCandidate(recapturingSide, recapturer, target) =>
        s"geometric_recapture:${recapturingSide.toString.toLowerCase}:${recapturer.square.key.toLowerCase}:${recapturer.role.name.toLowerCase}:${target.square.key.toLowerCase}:${target.role.name.toLowerCase}"
      case VerticalRelationPremiseRole.LegalRecapture(resource) =>
        s"legal_recapture:${resource.stableKey}"
      case VerticalRelationPremiseRole.AbsolutePinBarrier(
            occurrence,
            restrictedSide,
            pinner,
            pinned,
            kingSquare,
            axis
          ) =>
        s"absolute_pin:${occurrence.toString.toLowerCase}:${restrictedSide.toString.toLowerCase}:${pinner.square.key.toLowerCase}:${pinner.role.name.toLowerCase}:${pinned.square.key.toLowerCase}:${pinned.role.name.toLowerCase}:${kingSquare.key.toLowerCase}:${axis.toString.toLowerCase}"
      case VerticalRelationPremiseRole.MovementResource(pieceSide, piece, resource) =>
        s"movement_resource:${pieceSide.toString.toLowerCase}:${piece.square.key.toLowerCase}:${piece.role.name.toLowerCase}:${resource.stableKey}"
      case VerticalRelationPremiseRole.LegalMoveResource(occurrence, resource) =>
        s"legal_move_resource:${occurrence.toString.toLowerCase}:${resource.stableKey}"
      case VerticalRelationPremiseRole.PawnTopologyFacetSource(
            occurrence,
            side,
            pawn,
            facet,
            componentPawns,
            semanticId
          ) =>
        s"pawn_topology_facet_source:${occurrence.toString.toLowerCase}:${side.toString.toLowerCase}:${pawn.key.toLowerCase}:${facet.stableKey}:${componentPawns.map(_.key.toLowerCase).mkString("[", ",", "]")}:$semanticId"
      case VerticalRelationPremiseRole.PawnPassageStateSource(occurrence, side, pawn, semanticId) =>
        s"pawn_passage_state_source:${occurrence.toString.toLowerCase}:${side.toString.toLowerCase}:${pawn.key.toLowerCase}:$semanticId"
      case VerticalRelationPremiseRole.SliderReachControl(occurrence, side, slider, direction, control) =>
        s"slider_reach_control:${occurrence.toString.toLowerCase}:${side.toString.toLowerCase}:${slider.role.name.toLowerCase}@${slider.square.key.toLowerCase}:${direction.stableKey}:${control.stableKey}"
      case VerticalRelationPremiseRole.SliderReachBarrier(occurrence, side, slider, direction, occupant) =>
        s"slider_reach_barrier:${occurrence.toString.toLowerCase}:${side.toString.toLowerCase}:${slider.role.name.toLowerCase}@${slider.square.key.toLowerCase}:${direction.stableKey}:${occupant.map(value => s"${value.side.toString.toLowerCase}:${value.role.name.toLowerCase}@${value.square.key.toLowerCase}").getOrElse("open")}"

  def accepts(
      role: VerticalRelationPremiseRole,
      source: VerticalRelationPremiseSource,
      relation: RelationFactEvidence,
      rootFootprint: BoardTransitionFootprint
  ): Boolean =
    def positionAt(occurrences: RelationPremiseOccurrence*): Boolean =
      source match
        case VerticalRelationPremiseSource.Position(occurrence) => occurrences.contains(occurrence)
        case _                                                  => false
    def rootTransition: Boolean = source == VerticalRelationPremiseSource.RootTransition
    def derived: Boolean = source == VerticalRelationPremiseSource.Derived
    def legalMove(moveUci: String): Boolean =
      relation.detail match
        case RelationWitnessDetail.LegalMove(_, _, _, _, exact, _) =>
          EvidenceRef.sameMove(exact, moveUci)
        case _ => false
    def legalResource(resource: RelationLegalMoveResourceWitness): Boolean =
      relation.detail match
        case RelationWitnessDetail.LegalMove(side, from, beforeRole, to, moveUci, capture) =>
          side == resource.movement.side && from == resource.movement.from &&
            beforeRole == resource.movement.beforeRole && to == resource.movement.to &&
            EvidenceRef.sameMove(moveUci, resource.moveUci) && capture == resource.capture
        case _ => false
    def exactRootPieceTransition(movement: RelationMoveTransitionWitness): Boolean =
      rootFootprint.pieceTransitions.count(transition =>
        transition.side == movement.side &&
          transition.from.key.equalsIgnoreCase(movement.from.key) &&
          transition.to.key.equalsIgnoreCase(movement.to.key) &&
          transition.beforeRole.name.equalsIgnoreCase(movement.beforeRole.name) &&
          transition.afterRole.name.equalsIgnoreCase(movement.afterRole.name)
      ) == 1
    def legalMovement(movement: RelationMoveTransitionWitness): Boolean =
      rootTransition && relation.detail.isInstanceOf[RelationWitnessDetail.LegalMove] &&
        exactRootPieceTransition(movement)
    def enemyControl(
        controllingSide: _root_.chess.Color,
        controller: RelationPieceWitness,
        target: RelationPieceWitness
    ): Boolean =
      relation.detail match
        case RelationWitnessDetail.GeometricControl(
              side,
              attacker,
              attackerRole,
              targetSquare
            ) =>
          side == controllingSide && attacker == controller.square && attackerRole == controller.role &&
            targetSquare == target.square
        case _ => false
    role match
      case RootMove =>
        rootTransition && (relation.detail match
          case _: RelationWitnessDetail.LegalMove => true
          case _                                  => false)
      case RootCapture =>
        rootTransition && (relation.detail match
          case RelationWitnessDetail.LegalMove(_, _, _, _, _, Some(_)) => true
          case _                                                        => false)
      case ChosenCheckResponse(response) =>
        positionAt(RelationPremiseOccurrence.Before) && legalResource(response.resource)
      case CheckControlDelta(checkedSide, kingSquare) =>
        derived && (relation.detail match
          case RelationWitnessDetail.GeometricControlSetDelta(
                _,
                controllingSide,
                target,
                _,
                RelationControlTarget.Enemy(role),
                _,
                afterControllers,
                _,
                establishedControllers,
                _
              ) =>
            controllingSide != checkedSide && target == kingSquare &&
              role.name.equalsIgnoreCase(King.name) && afterControllers.nonEmpty &&
              establishedControllers.nonEmpty
          case _ => false)
      case CheckerControl(occurrence, controllingSide, checker, kingSquare) =>
        val exactOccurrence = occurrence match
          case RelationSnapshotOccurrence.Before =>
            positionAt(RelationPremiseOccurrence.Before, RelationPremiseOccurrence.Removed)
          case RelationSnapshotOccurrence.After =>
            positionAt(RelationPremiseOccurrence.After, RelationPremiseOccurrence.Established)
        exactOccurrence && enemyControl(
          controllingSide,
          checker,
          RelationPieceWitness(kingSquare, EvidencePieceRole(King.name))
        )
      case CheckerRay(occurrence, checker, kingSquare) =>
        val exactOccurrence = occurrence match
          case RelationSnapshotOccurrence.Before => positionAt(RelationPremiseOccurrence.Before)
          case RelationSnapshotOccurrence.After  => positionAt(RelationPremiseOccurrence.After)
        exactOccurrence &&
          (relation.detail match
            case RelationWitnessDetail.RayBarrier(_, attacker, attackerRole, occupants, _) =>
              attacker == checker.square && attackerRole == checker.role &&
                occupants.headOption.exists(piece =>
                  piece.square == kingSquare && piece.role.name.equalsIgnoreCase(King.name)
                )
            case _ => false)
      case CheckResponse(response) =>
        positionAt(RelationPremiseOccurrence.After) && legalResource(response.resource)
      case GeometricRecaptureCandidate(recapturingSide, recapturer, target) =>
        positionAt(RelationPremiseOccurrence.After) && enemyControl(recapturingSide, recapturer, target)
      case LegalRecapture(resource) =>
        positionAt(RelationPremiseOccurrence.After) && legalResource(resource) && (relation.detail match
          case RelationWitnessDetail.LegalMove(_, _, _, _, _, Some(_)) => true
          case _                                                        => false)
      case AbsolutePinBarrier(occurrence, restrictedSide, pinner, pinned, kingSquare, axis) =>
        val exactOccurrence = occurrence match
          case RelationSnapshotOccurrence.Before =>
            positionAt(RelationPremiseOccurrence.Before, RelationPremiseOccurrence.Removed)
          case RelationSnapshotOccurrence.After =>
            positionAt(RelationPremiseOccurrence.After, RelationPremiseOccurrence.Established)
        exactOccurrence && (relation.detail match
          case ray @ RelationWitnessDetail.RayBarrier(owner, attacker, attackerRole, occupants, geometry) =>
            RelationRayProjection.isAbsoluteKingPinGeometry(ray) &&
              owner == !restrictedSide && attacker == pinner.square && attackerRole == pinner.role &&
              geometry.axis == axis &&
              occupants.headOption.exists(piece =>
                piece.side == restrictedSide && piece.square == pinned.square && piece.role == pinned.role
              ) && occupants.lift(1).exists(piece =>
                piece.side == restrictedSide && piece.square == kingSquare &&
                  piece.role.name.equalsIgnoreCase(King.name)
              )
          case _ => false)
      case MovementResource(pieceSide, piece, resource) =>
        positionAt(RelationPremiseOccurrence.After) && (relation.detail match
          case RelationWitnessDetail.GeometricControl(side, attacker, attackerRole, target) =>
            val exactMode = resource.mode match
              case RelationMovementResourceMode.ControlledDestination =>
                resource.target match
                  case RelationControlTarget.Friendly(_) => false
                  case RelationControlTarget.Empty => !piece.role.name.equalsIgnoreCase(Pawn.name)
                  case RelationControlTarget.Enemy(_) => true
              case RelationMovementResourceMode.EnPassantCapture(capturedPawn) =>
                val exactCapturedSquare = for
                  origin <- Square.fromKey(piece.square.key)
                  destination <- Square.fromKey(resource.destination.key)
                yield EvidenceSquare(Square(destination.file, origin.rank).key)
                piece.role.name.equalsIgnoreCase(Pawn.name) && resource.target == RelationControlTarget.Empty &&
                  capturedPawn.side == !pieceSide && capturedPawn.role.name.equalsIgnoreCase(Pawn.name) &&
                  exactCapturedSquare.contains(capturedPawn.square)
              case RelationMovementResourceMode.PawnAdvance |
                  RelationMovementResourceMode.PawnDoubleAdvance => false
            side == pieceSide && attacker == piece.square && attackerRole == piece.role &&
              target == resource.destination && exactMode
          case RelationWitnessDetail.PawnAdvanceAffordance(side, pawn, target, traversed) =>
            val mode =
              if traversed.size == 2 then RelationMovementResourceMode.PawnDoubleAdvance
              else RelationMovementResourceMode.PawnAdvance
            side == pieceSide && pawn == piece.square && piece.role.name.equalsIgnoreCase(Pawn.name) &&
              target == resource.destination && resource.target == RelationControlTarget.Empty &&
              resource.mode == mode
          case _ => false)
      case LegalMoveResource(occurrence, resource) =>
        val exactOccurrence = occurrence match
          case RelationSnapshotOccurrence.Before => positionAt(RelationPremiseOccurrence.Before)
          case RelationSnapshotOccurrence.After  => positionAt(RelationPremiseOccurrence.After)
        exactOccurrence && legalResource(resource)
      case PawnTopologyFacetSource(occurrence, side, pawn, facet, componentPawns, semanticId) =>
        val exactOccurrence = (occurrence, source) match
          case (
                RelationSnapshotOccurrence.Before,
                VerticalRelationPremiseSource.Position(
                  RelationPremiseOccurrence.Before | RelationPremiseOccurrence.Removed
                )
              ) => true
          case (
                RelationSnapshotOccurrence.After,
                VerticalRelationPremiseSource.Position(
                  RelationPremiseOccurrence.After | RelationPremiseOccurrence.Established
                )
              ) => true
          case _ => false
        def pawnFileDistance(file: EvidenceFile): Int =
          file.key.toLowerCase.headOption
            .map(value => (value.toInt - pawn.key.toLowerCase.head.toInt).abs)
            .getOrElse(Int.MaxValue)
        def ownOrAdjacentPawnGroup(detail: RelationWitnessDetail): Boolean =
          detail match
            case RelationWitnessDetail.PawnFileGroup(owner, file, _) =>
              owner == side && pawnFileDistance(file) <= 1
            case _ => false
        val componentSet = componentPawns.toSet
        val exactComponentSource = relation.detail match
          case RelationWitnessDetail.PawnFileGroup(owner, _, pawns) =>
            owner == side && pawns.exists(componentSet)
          case RelationWitnessDetail.GeometricControl(
                owner,
                controller,
                controllerRole,
                target
              ) =>
            owner == side && controllerRole.name.equalsIgnoreCase(Pawn.name) &&
              componentSet(controller) && componentSet(target)
          case _ => false
        def exactPawnPassage(detail: RelationWitnessDetail): Boolean =
          detail match
            case RelationWitnessDetail.PawnPassage(owner, exactPawn, _) =>
              owner == side && exactPawn == pawn
            case _ => false
        def exactPawnSupport(detail: RelationWitnessDetail): Boolean =
          detail match
            case RelationWitnessDetail.GeometricControl(
                  owner,
                  controller,
                  controllerRole,
                  target
                ) =>
              owner == side && controllerRole.name.equalsIgnoreCase("pawn") &&
                (controller == pawn || target == pawn)
            case _ => false
        def exactEnemyPawnContact(detail: RelationWitnessDetail): Boolean =
          detail match
            case RelationWitnessDetail.GeometricControl(
                  chess.Color.White,
                  whitePawn,
                  whiteRole,
                  blackPawn
                ) =>
              whiteRole.name.equalsIgnoreCase(Pawn.name) &&
                (if side.white then whitePawn == pawn else blackPawn == pawn)
            case _ => false
        val exactFacetSource = facet match
          case RelationPawnTopologyFacet.Doubled =>
            relation.detail match
              case RelationWitnessDetail.PawnFileGroup(owner, file, pawns) =>
                owner == side && pawnFileDistance(file) == 0 && pawns.contains(pawn)
              case _ => false
          case RelationPawnTopologyFacet.Isolated =>
            ownOrAdjacentPawnGroup(relation.detail)
          case RelationPawnTopologyFacet.Passed =>
            exactPawnPassage(relation.detail)
          case RelationPawnTopologyFacet.GeometricallyProtectedPasser =>
            exactPawnPassage(relation.detail) || exactPawnSupport(relation.detail)
          case RelationPawnTopologyFacet.FrontState =>
            relation.detail match
              case RelationWitnessDetail.PawnFrontOccupancy(owner, exactPawn, _, _) =>
                owner == side && exactPawn == pawn
              case _ => false
          case RelationPawnTopologyFacet.Connections =>
            ownOrAdjacentPawnGroup(relation.detail) || exactPawnSupport(relation.detail)
          case RelationPawnTopologyFacet.Component =>
            componentSet(pawn) && exactComponentSource
          case RelationPawnTopologyFacet.EnemyPawnContacts =>
            exactEnemyPawnContact(relation.detail) || exactPawnPassage(relation.detail)
          case RelationPawnTopologyFacet.Removed =>
            ownOrAdjacentPawnGroup(relation.detail) || exactPawnPassage(relation.detail) ||
              exactPawnSupport(relation.detail) || exactEnemyPawnContact(relation.detail) ||
              (relation.detail match
                case RelationWitnessDetail.PawnFrontOccupancy(owner, exactPawn, _, _) =>
                  owner == side && exactPawn == pawn
                case RelationWitnessDetail.PawnFileGroup(owner, file, pawns) =>
                  owner == side && (pawnFileDistance(file) <= 1 || pawns.exists(componentSet))
                case RelationWitnessDetail.GeometricControl(
                      owner,
                      controller,
                      controllerRole,
                      target
                    ) =>
                  owner == side && controllerRole.name.equalsIgnoreCase("pawn") &&
                    (controller == pawn || target == pawn || componentSet(controller) && componentSet(target))
                case _ => false)
        exactOccurrence && semanticId == relation.semanticId && exactFacetSource
      case PawnPassageStateSource(occurrence, side, pawn, semanticId) =>
        val exactOccurrence = (occurrence, source) match
          case (
                RelationSnapshotOccurrence.Before,
                VerticalRelationPremiseSource.Position(
                  RelationPremiseOccurrence.Before | RelationPremiseOccurrence.Removed
                )
              ) => true
          case (
                RelationSnapshotOccurrence.After,
                VerticalRelationPremiseSource.Position(
                  RelationPremiseOccurrence.After | RelationPremiseOccurrence.Established
                )
              ) => true
          case _ => false
        exactOccurrence && semanticId == relation.semanticId && (relation.detail match
          case RelationWitnessDetail.PawnPassage(owner, exactPawn, _) =>
            owner == side && exactPawn == pawn
          case _ => false)
      case SliderReachControl(occurrence, side, slider, exactDirection, control) =>
        val exactOccurrence = occurrence match
          case RelationSnapshotOccurrence.Before =>
            positionAt(RelationPremiseOccurrence.Before, RelationPremiseOccurrence.Removed)
          case RelationSnapshotOccurrence.After =>
            positionAt(RelationPremiseOccurrence.After, RelationPremiseOccurrence.Established)
        exactOccurrence && (relation.detail match
          case RelationWitnessDetail.GeometricControl(
                exactSide,
                attacker,
                attackerRole,
                target
              ) =>
            exactSide == side && attacker == slider.square && attackerRole == slider.role &&
              target == control.square
          case _ => false)
      case SliderReachBarrier(occurrence, side, slider, exactDirection, firstOccupant) =>
        val exactOccurrence = occurrence match
          case RelationSnapshotOccurrence.Before =>
            positionAt(RelationPremiseOccurrence.Before, RelationPremiseOccurrence.Removed)
          case RelationSnapshotOccurrence.After =>
            positionAt(RelationPremiseOccurrence.After, RelationPremiseOccurrence.Established)
        exactOccurrence && (relation.detail match
          case RelationWitnessDetail.RayBarrier(exactSide, attacker, attackerRole, occupants, geometry) =>
            exactSide == side && attacker == slider.square && attackerRole == slider.role &&
              occupants.headOption == firstOccupant && geometry.direction == exactDirection
          case _ => false)

private[chessjudgment] enum RelationSnapshotOccurrence:
  case Before
  case After

/** Exact source domain of one vertical premise. Position facts retain their
  * before/after or sparse-change occurrence; derived facts are resolved from
  * the already-closed lower ledger. This is proof identity, not a search hint.
  */
private[chessjudgment] enum VerticalRelationPremiseSource:
  case Position(occurrence: RelationPremiseOccurrence)
  case RootTransition
  case Derived

private[chessjudgment] object VerticalRelationPremiseSource:
  def id(source: VerticalRelationPremiseSource): String =
    source match
      case VerticalRelationPremiseSource.Position(occurrence) =>
        s"position:${occurrence.toString.toLowerCase}"
      case VerticalRelationPremiseSource.RootTransition => "root_transition"
      case VerticalRelationPremiseSource.Derived => "derived"

  def transitionAnchored(
      source: VerticalRelationPremiseSource,
      stage: RelationProofStage
  ): Boolean =
    source match
      case VerticalRelationPremiseSource.Position(
            RelationPremiseOccurrence.Removed | RelationPremiseOccurrence.Established
          ) => true
      case VerticalRelationPremiseSource.RootTransition => true
      case VerticalRelationPremiseSource.Derived =>
        RelationProofStage.rank(stage) >= RelationProofStage.rank(RelationProofStage.TransitionFact)
      case _ => false

private[chessjudgment] final case class ClosedRelationAbsencePremise(
    occurrence: RelationSnapshotOccurrence,
    query: PositionRelationExtractor.ClosedRelationAbsenceQuery
):
  private[judgment] def stableKey: String =
    s"${occurrence.toString.toLowerCase}:${query.stableKey}"

private[chessjudgment] final case class ClosedPositionStatePremise(
    occurrence: RelationSnapshotOccurrence,
    query: PositionRelationExtractor.ClosedPositionStateQuery
):
  private[judgment] def stableKey: String =
    s"${occurrence.toString.toLowerCase}:${query.stableKey}"

private[chessjudgment] final case class VerticalRelationPremise private[chessjudgment] (
    role: VerticalRelationPremiseRole,
    source: VerticalRelationPremiseSource,
    stage: RelationProofStage,
    kind: RelationFactKind,
    assertionId: String,
    semanticId: String
):
  require(assertionId.matches("[0-9a-f]{64}"), "a vertical premise needs a canonical assertion id")
  require(semanticId.matches("[0-9a-f]{64}"), "a vertical premise needs a canonical derivation id")

  private[judgment] def stableKey: String =
    List(
      VerticalRelationPremiseRole.id(role),
      VerticalRelationPremiseSource.id(source),
      RelationProofStage.id(stage),
      RelationFactKind.id(kind),
      assertionId,
      semanticId
    ).mkString(":")

/** Semantic derivation identity. It contains exact lower-result identities and
  * closed absence queries, but never occurrence-owned evidence references.
  */
final case class VerticalRelationDerivationProof private[chessjudgment] (
    private[chessjudgment] val contract: VerticalRelationContractKind,
    private val obligations: List[VerticalRelationPremise],
    private val alternativeRoutes: List[VerticalRelationPremise],
    private[chessjudgment] val absences: List[ClosedRelationAbsencePremise],
    private[chessjudgment] val states: List[ClosedPositionStatePremise]
):
  require(obligations.nonEmpty, "a vertical relation derivation needs a lower proof obligation")
  require(
    obligations.map(_.role).distinct.size == obligations.size,
    "a vertical relation derivation cannot repeat an exact obligation"
  )
  require(obligations == obligations.sortBy(_.stableKey), "vertical relation obligations must use canonical order")
  require(
    alternativeRoutes.distinct.size == alternativeRoutes.size &&
      alternativeRoutes == alternativeRoutes.sortBy(_.stableKey) &&
      alternativeRoutes.forall(route => obligations.exists(_.role == route.role)),
    "vertical relation alternatives must be unique canonical routes for an existing obligation"
  )
  require(absences.distinct.size == absences.size, "a vertical relation derivation cannot repeat an absence premise")
  require(absences == absences.sortBy(_.stableKey), "vertical absence premises must use canonical order")
  require(states.distinct.size == states.size, "a vertical relation derivation cannot repeat a state premise")
  require(states == states.sortBy(_.stableKey), "vertical state premises must use canonical order")
  require(
    (obligations ++ alternativeRoutes).forall(premise =>
      RelationProofStage.rank(premise.stage) < VerticalRelationContractKind.rank(contract)
    ),
    "a vertical relation derivation may consume only a strictly lower proof stratum"
  )
  require(
    (obligations ++ alternativeRoutes).exists(premise =>
      VerticalRelationPremiseSource.transitionAnchored(premise.source, premise.stage)
    ),
    "a transition-scoped vertical derivation needs one exact changed or derived transition premise"
  )

  private[chessjudgment] def premises: List[VerticalRelationPremise] =
    (obligations ++ alternativeRoutes).sortBy(_.stableKey)

  private[chessjudgment] lazy val sourcePremises: List[VerticalRelationPremise] =
    premises
      .groupBy(premise => VerticalRelationPremiseSource.id(premise.source) -> premise.semanticId)
      .toList
      .map { case ((_, semanticId), occurrences) =>
        require(
          occurrences.forall(premise =>
              premise.kind == occurrences.head.kind && premise.assertionId == occurrences.head.assertionId &&
              premise.stage == occurrences.head.stage && premise.source == occurrences.head.source
          ),
          s"one lower relation '$semanticId' cannot change identity across proof obligations"
        )
        occurrences.head
      }
      .sortBy(_.stableKey)

  private[chessjudgment] lazy val sourceSemanticIds: Set[String] =
    sourcePremises.map(_.semanticId).toSet

  private def exactRoles(expected: List[VerticalRelationPremiseRole]): Boolean =
    val actual = obligations.map(_.role)
    expected.distinct.size == expected.size && actual.size == expected.size && actual.toSet == expected.toSet

  private def exactAbsences(expected: List[ClosedRelationAbsencePremise]): Boolean =
    expected.distinct.size == expected.size && absences == expected.sortBy(_.stableKey)

  private def exactStates(expected: List[ClosedPositionStatePremise]): Boolean =
    expected.distinct.size == expected.size && states == expected.sortBy(_.stableKey)

  /** Closes the semantic boundary between lower proof obligations and the
    * emitted L1 assertion. Resolving a correctly typed source is necessary,
    * but it is not sufficient: every output witness must be named by the
    * exact premise set, and unrelated lower facts are rejected.
    */
  private[chessjudgment] def proves(detail: RelationWitnessDetail): Boolean =
    if !VerticalRelationContractKind.forDetail(detail).contains(contract) then false
    else if states.nonEmpty &&
        !detail.isInstanceOf[RelationWitnessDetail.CaptureRecaptureInventory] &&
        !detail.isInstanceOf[RelationWitnessDetail.CreatedCheckResponseInventory] &&
        !detail.isInstanceOf[RelationWitnessDetail.PawnTopologyTransition] &&
        !detail.isInstanceOf[RelationWitnessDetail.SliderReachDelta]
    then false
    else
      detail match
        case RelationWitnessDetail.CaptureRecaptureInventory(
              mover,
              captured,
              geometricRecapturers,
              legalRecaptures,
              restrictedRecaptures,
              _
            ) =>
          val target = RelationPieceWitness(mover.to, mover.afterRole)
          val expectedRoles =
            VerticalRelationPremiseRole.RootCapture ::
              geometricRecapturers.map(recapturer =>
                VerticalRelationPremiseRole.GeometricRecaptureCandidate(captured.side, recapturer, target)
              ) ++ legalRecaptures.map(resource =>
                VerticalRelationPremiseRole.LegalRecapture(resource)
              ) ++ restrictedRecaptures.map(restriction =>
                VerticalRelationPremiseRole.MovementResource(
                  captured.side,
                  restriction.piece,
                  restriction.resource
                )
              ) ++ restrictedRecaptures.flatMap(_.absolutePinPaths.map(path =>
                VerticalRelationPremiseRole.AbsolutePinBarrier(
                  RelationSnapshotOccurrence.After,
                  captured.side,
                  path.pinner,
                  path.pinned,
                  path.kingSquare,
                  path.geometry.axis
                )
              )).distinct
          val expectedAbsences =
            Option.when(geometricRecapturers.isEmpty)(
              ClosedRelationAbsencePremise(
                RelationSnapshotOccurrence.After,
                PositionRelationExtractor.ClosedRelationAbsenceQuery.GeometricControlOf(
                  captured.side,
                  mover.to
                )
              )
            ).toList ++ Option.when(legalRecaptures.isEmpty)(
              ClosedRelationAbsencePremise(
                RelationSnapshotOccurrence.After,
                PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
                  captured.side,
                  mover.to
                )
              )
            ).toList ++ restrictedRecaptures.map(restriction =>
              ClosedRelationAbsencePremise(
                RelationSnapshotOccurrence.After,
                PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
                  captured.side,
                  restriction.piece.square,
                  restriction.resource.destination
                )
              )
            )
          val expectedStates = restrictedRecaptures.map(restriction =>
            ClosedPositionStatePremise(
              RelationSnapshotOccurrence.After,
              PositionRelationExtractor.ClosedPositionStateQuery.OwnKingExposure(
                captured.side,
                restriction.piece,
                restriction.resource,
                restriction.kingSquare,
                restriction.postMoveControllers
              )
            )
          )
          exactRoles(expectedRoles) && exactAbsences(expectedAbsences) && exactStates(expectedStates)

        case RelationWitnessDetail.CreatedCheckResponseInventory(
              _,
              checkedSide,
              kingSquare,
              checkers,
              responses,
              controlledKingDestinations,
              _,
              _
            ) =>
          val king = RelationPieceWitness(kingSquare, EvidencePieceRole(King.name))
          val checkerRay = Option.when(responses.exists(_.modes.contains(RelationCheckResponseMode.Interpose)))(
            VerticalRelationPremiseRole.CheckerRay(RelationSnapshotOccurrence.After, checkers.head, kingSquare)
          ).toList
          val expectedRoles =
            VerticalRelationPremiseRole.CheckControlDelta(checkedSide, kingSquare) ::
              checkers.map(checker =>
                VerticalRelationPremiseRole.CheckerControl(
                  RelationSnapshotOccurrence.After,
                  !checkedSide,
                  checker,
                  kingSquare
                )
              ) ++
              checkerRay ++ responses.map(response =>
                VerticalRelationPremiseRole.CheckResponse(response)
              ) ++ controlledKingDestinations.flatMap(destination =>
                List(VerticalRelationPremiseRole.MovementResource(
                  checkedSide,
                  king,
                  destination.resource
                ))
              )
          val expectedAbsences = controlledKingDestinations.map(destination =>
            ClosedRelationAbsencePremise(
              RelationSnapshotOccurrence.After,
              PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
                checkedSide,
                kingSquare,
                destination.resource.destination
              )
            )
          ) ++ Option.when(responses.isEmpty)(
            ClosedRelationAbsencePremise(
              RelationSnapshotOccurrence.After,
              PositionRelationExtractor.ClosedRelationAbsenceQuery.AnyLegalMove(checkedSide)
            )
          ).toList
          val expectedStates = controlledKingDestinations.map(destination =>
            ClosedPositionStatePremise(
              RelationSnapshotOccurrence.After,
              PositionRelationExtractor.ClosedPositionStateQuery.OwnKingExposure(
                checkedSide,
                king,
                destination.resource,
                destination.resource.destination,
                destination.controllers
              )
            )
          )
          exactRoles(expectedRoles) && exactAbsences(expectedAbsences) && exactStates(expectedStates)

        case RelationWitnessDetail.RootCheckResponse(
              _,
              respondingSide,
              kingSquare,
              checkers,
              response,
              _
            ) =>
          val checkerRay = Option.when(response.modes.contains(RelationCheckResponseMode.Interpose))(
            VerticalRelationPremiseRole.CheckerRay(RelationSnapshotOccurrence.Before, checkers.head, kingSquare)
          ).toList
          exactRoles(
            VerticalRelationPremiseRole.RootMove ::
              VerticalRelationPremiseRole.ChosenCheckResponse(response) ::
              checkers.map(checker =>
                VerticalRelationPremiseRole.CheckerControl(
                  RelationSnapshotOccurrence.Before,
                  !respondingSide,
                  checker,
                  kingSquare
                )
              ) ++
              checkerRay
          ) && exactAbsences(Nil)

        case RelationWitnessDetail.SliderReachDelta(
              _,
              side,
              sliderBefore,
              sliderAfter,
              direction,
              before,
              after,
              _
            ) =>
          val beforeStatePiece = sliderBefore.orElse(sliderAfter)
          val afterStatePiece = sliderAfter.orElse(sliderBefore)
          val expectedStates = (beforeStatePiece, afterStatePiece) match
            case (Some(beforePiece), Some(afterPiece)) =>
              List(
                ClosedPositionStatePremise(
                  RelationSnapshotOccurrence.Before,
                  PositionRelationExtractor.ClosedPositionStateQuery.SliderReach(
                    side,
                    beforePiece,
                    direction,
                    before
                  )
                ),
                ClosedPositionStatePremise(
                  RelationSnapshotOccurrence.After,
                  PositionRelationExtractor.ClosedPositionStateQuery.SliderReach(
                    side,
                    afterPiece,
                    direction,
                    after
                  )
                )
              )
            case _ => Nil
          before != after && expectedStates.size == 2 &&
            exactRoles(
              VerticalRelationPremiseRole.RootMove ::
                (VerticalSliderReachProofProjection.roles(
                  RelationSnapshotOccurrence.Before,
                  side,
                  sliderBefore,
                  direction,
                  before
                ) ++ VerticalSliderReachProofProjection.roles(
                  RelationSnapshotOccurrence.After,
                  side,
                  sliderAfter,
                  direction,
                  after
                ))
            ) && exactAbsences(Nil) && exactStates(expectedStates)

        case RelationWitnessDetail.PawnTopologyTransition(_, before, after, changed, _) =>
          provesPawnTopologyFacets(before, after, changed)

        case RelationWitnessDetail.StalemateTransition(_, stalledSide, kingSquare, _) =>
          exactRoles(List(VerticalRelationPremiseRole.RootMove)) && exactAbsences(List(
            ClosedRelationAbsencePremise(
              RelationSnapshotOccurrence.After,
              PositionRelationExtractor.ClosedRelationAbsenceQuery.AnyLegalMove(stalledSide)
            ),
            ClosedRelationAbsencePremise(
              RelationSnapshotOccurrence.After,
              PositionRelationExtractor.ClosedRelationAbsenceQuery.KingCheck(stalledSide, kingSquare)
            )
          ))

        case _ => false

  private[chessjudgment] def provesPawnTopologyFacets(
      before: Option[RelationPawnTopologyStateWitness],
      after: Option[RelationPawnTopologyStateWitness],
      changed: List[RelationPawnTopologyFacet]
  ): Boolean =
    if contract != VerticalRelationContractKind.PawnTopologyTransition then false
    else
      val rootPremises = obligations.filter(_.role == VerticalRelationPremiseRole.RootMove)
      val facetPremises = obligations.flatMap { premise =>
        premise.role match
          case role @ VerticalRelationPremiseRole.PawnTopologyFacetSource(
                occurrence,
                side,
                pawn,
                facet,
                componentPawns,
                semanticId
              ) =>
            Some((premise, role, occurrence, side, pawn, facet, componentPawns, semanticId))
          case VerticalRelationPremiseRole.RootMove => None
          case _                                    => None
      }
      val passageStatePremises = obligations.flatMap { premise =>
        premise.role match
          case VerticalRelationPremiseRole.PawnPassageStateSource(
                occurrence,
                side,
                pawn,
                semanticId
              ) => Some((premise, occurrence, side, pawn, semanticId))
          case _ => None
      }
      val onlyTopologyRoles =
        obligations.size == rootPremises.size + facetPremises.size + passageStatePremises.size
      val exactFacetStates = facetPremises.forall {
        case (premise, _, occurrence, side, pawn, facet, componentPawns, semanticId) =>
          val state = occurrence match
            case RelationSnapshotOccurrence.Before => before
            case RelationSnapshotOccurrence.After  => after
          state.exists(value =>
            value.side == side && value.square == pawn && value.componentPawns == componentPawns
          ) &&
            changed.contains(facet) && premise.semanticId == semanticId
      }
      val exactFacets = facetPremises.map(_._6).toSet == changed.toSet &&
        changed.forall(facet => facetPremises.exists(_._6 == facet))
      val expectedPassageOccurrences = (before, after) match
        case (Some(previous), Some(next)) if previous.square != next.square =>
          Set(
            (RelationSnapshotOccurrence.Before, previous.side, previous.square),
            (RelationSnapshotOccurrence.After, next.side, next.square)
          )
        case _ => Set.empty[(RelationSnapshotOccurrence, _root_.chess.Color, EvidenceSquare)]
      val exactPassageStates = passageStatePremises.forall {
        case (premise, occurrence, side, pawn, semanticId) =>
          expectedPassageOccurrences((occurrence, side, pawn)) && premise.semanticId == semanticId
      } && passageStatePremises.map { case (_, occurrence, side, pawn, _) =>
        (occurrence, side, pawn)
      }.toSet == expectedPassageOccurrences
      val expectedProtectedPasserAbsences = Option.when(
        changed.contains(RelationPawnTopologyFacet.GeometricallyProtectedPasser)
      )(
        List(
          RelationSnapshotOccurrence.Before -> before,
          RelationSnapshotOccurrence.After -> after
        ).flatMap { case (occurrence, state) =>
          state.filter(value => value.passed && !value.geometricallyProtectedPasser).map(value =>
            ClosedRelationAbsencePremise(
              occurrence,
              PositionRelationExtractor.ClosedRelationAbsenceQuery.GeometricPawnSupportOf(
                value.side,
                value.square
              )
            )
          )
        }
      ).toList.flatten.sortBy(_.stableKey)
      val expectedPositionStates = before.map(state =>
        ClosedPositionStatePremise(
          RelationSnapshotOccurrence.Before,
          PositionRelationExtractor.ClosedPositionStateQuery.PawnTopology(state)
        )
      ).toList ++ after.map(state =>
        ClosedPositionStatePremise(
          RelationSnapshotOccurrence.After,
          PositionRelationExtractor.ClosedPositionStateQuery.PawnTopology(state)
        )
      )
      rootPremises.size == 1 && onlyTopologyRoles && exactFacetStates && exactFacets && exactPassageStates &&
        absences == expectedProtectedPasserAbsences && exactStates(expectedPositionStates)

  private[judgment] def stableKey: String =
    val sourceKey = obligations.map(_.stableKey).mkString("[", ",", "]")
    val alternativeKey = alternativeRoutes.map(_.stableKey).mkString("[", ",", "]")
    val absenceKey = absences.map(_.stableKey).mkString("[", ",", "]")
    val stateKey = states.map(_.stableKey).mkString("[", ",", "]")
    s"contract:${VerticalRelationContractKind.id(contract)}:obligations:$sourceKey:alternatives:$alternativeKey:absences:$absenceKey:states:$stateKey"

private[chessjudgment] object VerticalRelationDerivationProof:
  private[judgment] def fromRootAndSources(
      contract: VerticalRelationContractKind,
      rootRole: VerticalRelationPremiseRole,
      root: CanonicalRootLegalMove,
      premises: List[(VerticalRelationPremiseRole, VerticalRelationPremiseSource, RelationFactEvidence)],
      absences: List[ClosedRelationAbsencePremise] = Nil,
      states: List[ClosedPositionStatePremise] = Nil
  ): VerticalRelationDerivationProof =
    val rootPremise = VerticalRelationPremise(
      rootRole,
      VerticalRelationPremiseSource.RootTransition,
      RelationProofStage.TransitionFact,
      root.fact.kind,
      root.fact.assertionId,
      root.fact.semanticId
    )
    val exact = premises.map { case (role, source, relation) =>
          VerticalRelationPremise(
            role,
            source,
            source match
              case VerticalRelationPremiseSource.RootTransition => RelationProofStage.TransitionFact
              case _                                            => relation.proofStage,
            relation.kind,
        relation.assertionId,
        relation.semanticId
      )
    }
    fromPremises(contract, rootPremise :: exact, absences, states)

  private[judgment] def from(
      contract: VerticalRelationContractKind,
      premises: List[(VerticalRelationPremiseRole, RelationFactEvidence)],
      absences: List[ClosedRelationAbsencePremise] = Nil,
      states: List[ClosedPositionStatePremise] = Nil
  ): VerticalRelationDerivationProof =
    fromSources(
      contract,
      premises.map { case (role, relation) =>
        (role, VerticalRelationPremiseSource.Derived, relation)
      },
      absences,
      states
    )

  private[judgment] def fromSources(
      contract: VerticalRelationContractKind,
      premises: List[(VerticalRelationPremiseRole, VerticalRelationPremiseSource, RelationFactEvidence)],
      absences: List[ClosedRelationAbsencePremise] = Nil,
      states: List[ClosedPositionStatePremise] = Nil
  ): VerticalRelationDerivationProof =
    fromPremises(
      contract,
      premises.map { case (role, source, relation) =>
        VerticalRelationPremise(
          role,
          source,
          source match
            case VerticalRelationPremiseSource.RootTransition => RelationProofStage.TransitionFact
            case _                                            => relation.proofStage,
          relation.kind,
          relation.assertionId,
          relation.semanticId
        )
      },
      absences,
      states
    )

  private[judgment] def fromPremises(
      contract: VerticalRelationContractKind,
      premises: List[VerticalRelationPremise],
      absences: List[ClosedRelationAbsencePremise],
      states: List[ClosedPositionStatePremise]
  ): VerticalRelationDerivationProof =
    require(
      premises.distinct.size == premises.size,
      "a vertical derivation cannot silently deduplicate one exact proof route"
    )
    val routesByRole = premises
      .groupMap(_.role)(identity)
      .toList
      .sortBy { case (role, _) => VerticalRelationPremiseRole.id(role) }
      .map { case (_, routes) => routes.sortBy(_.stableKey) }
    VerticalRelationDerivationProof(
      contract,
      routesByRole.map(_.head).sortBy(_.stableKey),
      routesByRole.flatMap(_.drop(1)).sortBy(_.stableKey),
      absences.sortBy(_.stableKey),
      states.sortBy(_.stableKey)
    )

/** The closed L1 ledger for one transition. Only contracts named by an exact
  * changed dependency are run. An absent contract key certifies non-execution;
  * an empty stored result certifies that an activated contract found no fact.
  */
private[chessjudgment] final class ClosedVerticalRelationResults private (
    engine: ClosedVerticalRelationEngine,
    private val lowerOwner: ClosedRelationCombinationResults,
    private val deltaOwner: RelationSemanticDelta,
    certify: RelationFactEvidence => RelationFactEvidence
):
  private val certifiedByContract = scala.collection.mutable.Map.empty[
    VerticalRelationContractKind,
    List[RelationFactEvidence]
  ]

  private[chessjudgment] def resultsFor(
      contract: VerticalRelationContractKind
  ): List[RelationFactEvidence] = synchronized {
    certifiedByContract.getOrElseUpdate(
      contract,
      engine.resultsFor(contract).map(certify)
    )
  }

  private[judgment] lazy val byContract: VectorMap[
    VerticalRelationContractKind,
    List[RelationFactEvidence]
  ] =
    engine.activeContracts
      .sortBy(contract => VerticalRelationContractKind.rank(contract) -> VerticalRelationContractKind.id(contract))
      .foldLeft(VectorMap.empty[VerticalRelationContractKind, List[RelationFactEvidence]]) {
        case (closed, contract) => closed.updated(contract, resultsFor(contract))
      }

  lazy val relations: List[RelationFactEvidence] =
    val exact = byContract.valuesIterator.flatten.toList
    require(
      exact.map(_.semanticId).distinct.size == exact.size,
      "closed vertical relations cannot repeat one derivation path"
    )
    exact

  private[judgment] def owns(
      lowerResults: ClosedRelationCombinationResults,
      delta: RelationSemanticDelta
  ): Boolean =
    lowerOwner.asInstanceOf[AnyRef].eq(lowerResults.asInstanceOf[AnyRef]) &&
      deltaOwner.asInstanceOf[AnyRef].eq(delta.asInstanceOf[AnyRef])

  private[judgment] def absenceCertificate(
      premise: ClosedRelationAbsencePremise
  ): Option[PositionRelationExtractor.ClosedRelationAbsenceCertificate] =
    engine.absenceCertificate(premise)

  private[judgment] def stateCertificate(
      premise: ClosedPositionStatePremise
  ): Option[PositionRelationExtractor.ClosedPositionStateCertificate] =
    engine.stateCertificate(premise)

  private[judgment] def certifiedBy(
      certification: RelationFactEvidence => RelationFactEvidence
  ): ClosedVerticalRelationResults =
    new ClosedVerticalRelationResults(
      engine,
      lowerOwner,
      deltaOwner,
      certification
    )

private final class ClosedVerticalRelationEngine(
    combinations: ClosedRelationCombinationResults,
    delta: RelationSemanticDelta
):
  private val input = VerticalRelationInput(delta, combinations)
  private val rawByContract = scala.collection.mutable.Map.empty[
    VerticalRelationContractKind,
    List[RelationFactEvidence]
  ]

  def activeContracts: List[VerticalRelationContractKind] = input.activeContracts

  def resultsFor(contract: VerticalRelationContractKind): List[RelationFactEvidence] = synchronized {
    rawByContract.getOrElseUpdate(
      contract,
      {
        val results =
          if input.activates(contract) then VerticalRelationContracts.derive(contract, input).sortBy(_.semanticId)
          else Nil
        val validation = results.map { relation =>
          val proof = VerticalRelationContracts.proofOf(relation.detail)
          val unresolved = proof.toList.flatMap(exact =>
            exact.premises.filterNot(input.resolves).map(_.stableKey) ++
              exact.absences.filterNot(input.resolves).map(_.stableKey) ++
              exact.states.filterNot(input.resolves).map(_.stableKey)
          )
          val valid =
            relation.proofStage == VerticalRelationContractKind.outputStage(contract) &&
              input.ownsOutput(relation.detail) &&
              proof.exists(exact =>
                exact.contract == contract && exact.proves(relation.detail) && unresolved.isEmpty
              )
          relation.semanticId -> (valid -> unresolved)
        }
        val invalid = validation.flatMap { case (semanticId, (_, unresolved)) =>
          unresolved.map(semanticId -> _)
        }
        require(
          validation.forall { case (_, (valid, _)) => valid },
          s"vertical contract '${VerticalRelationContractKind.id(contract)}' emitted an unresolved derivation: ${invalid.mkString(",")}"
        )
        require(
          results.map(_.semanticId).distinct.size == results.size,
          "one vertical contract cannot repeat a derivation path"
        )
        input.admit(results)
        results
      }
    )
  }

  def absenceCertificate(
      premise: ClosedRelationAbsencePremise
  ): Option[PositionRelationExtractor.ClosedRelationAbsenceCertificate] = synchronized {
    input.absenceCertificate(premise)
  }

  def stateCertificate(
      premise: ClosedPositionStatePremise
  ): Option[PositionRelationExtractor.ClosedPositionStateCertificate] = synchronized {
    input.stateCertificate(premise)
  }

private[chessjudgment] object ClosedVerticalRelationResults:
  def derive(
      combinations: ClosedRelationCombinationResults,
      delta: RelationSemanticDelta
  ): ClosedVerticalRelationResults =
    new ClosedVerticalRelationResults(
      new ClosedVerticalRelationEngine(combinations, delta),
      combinations,
      delta,
      identity
    )

private[chessjudgment] object VerticalRelationWitnessProjection:
  def movementResource(
      movement: PositionRelationExtractor.ClosedMovementAffordance
  ): RelationMovementResourceWitness =
    RelationMovementResourceWitness(movement.destination, movement.target, movement.mode)

  def legalResource(
      move: PositionRelationExtractor.ClosedLegalMove
  ): RelationLegalMoveResourceWitness =
    RelationLegalMoveResourceWitness(move.movement, move.moveUci, move.capture)

  def checkResponse(
      response: PositionRelationExtractor.ClosedKingResponse
  ): RelationCheckResponseWitness =
    RelationCheckResponseWitness(
      legalResource(response.move),
      response.modes.toList.map {
        case PositionRelationExtractor.ClosedKingResponseMode.KingMove =>
          RelationCheckResponseMode.KingMove
        case PositionRelationExtractor.ClosedKingResponseMode.CaptureChecker =>
          RelationCheckResponseMode.CaptureChecker
        case PositionRelationExtractor.ClosedKingResponseMode.Interpose =>
          RelationCheckResponseMode.Interpose
      }.sortBy(_.stableKey)
    )

  def controlledKingDestination(
      destination: PositionRelationExtractor.ClosedLegalResourceRestriction
  ): RelationControlledKingDestinationWitness =
    RelationControlledKingDestinationWitness(
      movementResource(destination.movement),
      destination.postMoveControllers
    )

  def restrictedResource(
      clause: VerticalLegalRestrictionClause
  ): RelationRestrictedResourceWitness =
    val restriction = clause.restriction
    RelationRestrictedResourceWitness(
      restriction.movement.piece,
      movementResource(restriction.movement),
      restriction.kingSquare,
      restriction.postMoveControllers,
      clause.absolutePinCauses.map(_.witness)
    )

private[chessjudgment] object VerticalSliderReachProofProjection:
  def roles(
      occurrence: RelationSnapshotOccurrence,
      side: _root_.chess.Color,
      slider: Option[RelationPieceWitness],
      direction: RelationRayDirection,
      reach: Option[RelationSliderReachWitness]
  ): List[VerticalRelationPremiseRole] =
    require(reach.isEmpty || slider.nonEmpty, "a slider reach cannot lose its controller identity")
    (slider, reach) match
      case (Some(exactSlider), Some(exactReach)) =>
        exactReach.segment.map(control =>
          VerticalRelationPremiseRole.SliderReachControl(
            occurrence,
            side,
            exactSlider,
            direction,
            control
          )
        ) ++ List(
          VerticalRelationPremiseRole.SliderReachBarrier(
            occurrence,
            side,
            exactSlider,
            direction,
            exactReach.firstOccupant
          )
        )
      case _ => Nil

private[chessjudgment] final class VerticalRelationInput private (
    delta: RelationSemanticDelta,
    combinations: ClosedRelationCombinationResults
):
  import VerticalRelationWitnessProjection.*

  private var derivedBySemanticId = Map.empty[String, RelationFactEvidence]

  private var retainedTransitionContracts = Set.empty[RelationCombinationContractKind]

  def transitionResultsFor(contract: RelationCombinationContractKind): List[RelationFactEvidence] =
    val results = combinations.resultsFor(contract)
    if !retainedTransitionContracts(contract) then
      retainDerived(results, allowExisting = true)
      retainedTransitionContracts += contract
    results

  private lazy val controlSetChangeInput: List[VerticalControlSetChange] =
    transitionResultsFor(RelationCombinationContractKind.GeometricControlSetDelta).map { relation =>
      relation.detail match
        case RelationWitnessDetail.GeometricControlSetDelta(
              mover,
              controllingSide,
              target,
              beforeTarget,
              afterTarget,
              beforeControllers,
              afterControllers,
              removedControllers,
              establishedControllers,
              _
            ) =>
          VerticalControlSetChange(
            relation,
            mover,
            controllingSide,
            target,
            beforeTarget,
            afterTarget,
            beforeControllers,
            afterControllers,
            removedControllers,
            establishedControllers
          )
        case _ => throw IllegalArgumentException("control-set contract changed relation kind")
    }

  private[judgment] def controlSetChanges: List[VerticalControlSetChange] =
    controlSetChangeInput

  def establishedPositionRelations(kind: RelationFactKind): List[RelationFactEvidence] =
    delta.establishedOf(kind).map(_.relation)

  private[judgment] def exactPositionSource(
      occurrence: RelationSnapshotOccurrence,
      relation: RelationFactEvidence
  ): VerticalRelationPremiseSource =
    val premiseOccurrence = (occurrence, delta.changeBySemanticId(relation.semanticId)) match
      case (RelationSnapshotOccurrence.Before, Some(change))
          if change.direction == RelationChangeDirection.Removed =>
        RelationPremiseOccurrence.Removed
      case (RelationSnapshotOccurrence.After, Some(change))
          if change.direction == RelationChangeDirection.Established =>
        RelationPremiseOccurrence.Established
      case (RelationSnapshotOccurrence.Before, _) => RelationPremiseOccurrence.Before
      case (RelationSnapshotOccurrence.After, _)  => RelationPremiseOccurrence.After
    VerticalRelationPremiseSource.Position(premiseOccurrence)

  /** Exact dependency dispatch. These predicates inspect only the already
    * closed delta/state keys; they never materialize an L1 result to decide
    * whether that same contract should run.
    */
  private lazy val activeContractSet: Set[VerticalRelationContractKind] =
    val sliderReachSourceChanged =
      delta.ofKind(RelationFactKind.RayBarrier).nonEmpty ||
        delta.ofKind(RelationFactKind.GeometricControl).exists(_.detail match
          case RelationWitnessDetail.GeometricControl(_, _, role, _) =>
            List(Bishop.name, Rook.name, Queen.name).exists(_.equalsIgnoreCase(role.name))
          case _ => false
        )
    val createdCheckChanged =
      delta.establishedOf(RelationFactKind.GeometricControl).exists(_.detail match
        case RelationWitnessDetail.GeometricControl(side, _, _, target) =>
          side == rootMove.side && delta.afterInventory.stateView.occupantAt(target).exists(piece =>
            piece.side == !side && piece.role.name.equalsIgnoreCase(King.name)
          )
        case _ => false
      )
    val pawnTopologyChanged =
      List(
        RelationFactKind.PawnFileGroup,
        RelationFactKind.PawnFrontOccupancy,
        RelationFactKind.PawnPassage
      ).exists(kind => delta.ofKind(kind).nonEmpty) ||
        delta.ofKind(RelationFactKind.GeometricControl).exists(change => change.detail match
          case RelationWitnessDetail.GeometricControl(_, _, controllerRole, target) =>
            val inventory =
              if change.direction == RelationChangeDirection.Established then delta.afterInventory
              else delta.beforeInventory
            controllerRole.name.equalsIgnoreCase(Pawn.name) &&
              inventory.stateView.occupantAt(target).exists(_.role.name.equalsIgnoreCase(Pawn.name))
          case _ => false)
    List(
      Option.when(rootMove.capture.nonEmpty)(VerticalRelationContractKind.CaptureRecaptureInventory),
      Option.when(createdCheckChanged)(VerticalRelationContractKind.CreatedCheckResponseInventory),
      Option.when(delta.beforeInventory.stateView.inCheck(rootMove.side))(
        VerticalRelationContractKind.RootCheckResponse
      ),
      Option.when(sliderReachSourceChanged)(VerticalRelationContractKind.SliderReachDelta),
      Option.when(pawnTopologyChanged)(VerticalRelationContractKind.PawnTopologyTransition),
      Option.when(
        delta.afterInventory.kingTerminalState ==
          PositionRelationExtractor.ClosedKingTerminalState.Stalemate
      )(VerticalRelationContractKind.StalemateTransition)
    ).flatten.toSet

  private[judgment] def activeContracts: List[VerticalRelationContractKind] =
    activeContractSet.toList.sortBy(VerticalRelationContractKind.id)

  private[judgment] def activates(contract: VerticalRelationContractKind): Boolean =
    activeContractSet(contract)

  private[judgment] def rootMove: CanonicalRootLegalMove =
    delta.rootMove

  private[judgment] def legalRestrictionClauseFor(
      restriction: PositionRelationExtractor.ClosedLegalResourceRestriction
  ): VerticalLegalRestrictionClause =
    val after = delta.afterInventory
    val pinned = restriction.movement.piece
    val pinCauses = after.rayBarriersWithFirstOccupant(pinned.square).flatMap { source =>
      source.detail match
        case ray @ RelationWitnessDetail.RayBarrier(owner, attacker, attackerRole, occupants, geometry)
            if RelationRayProjection.isAbsoluteKingPinGeometry(ray) &&
              owner == !after.sideToMove &&
              restriction.postMoveControllers.contains(RelationPieceWitness(attacker, attackerRole)) &&
              occupants.headOption.exists(piece =>
                piece.side == after.sideToMove && piece.square == pinned.square && piece.role == pinned.role
              ) && occupants.lift(1).exists(piece =>
                piece.side == after.sideToMove && piece.square == restriction.kingSquare &&
                  piece.role.name.equalsIgnoreCase(King.name)
              ) =>
          List(VerticalAbsolutePinCause(
            RelationAbsolutePinPathWitness(
              RelationPieceWitness(attacker, attackerRole),
              pinned,
              restriction.kingSquare,
              geometry
            ),
            source
          ))
        case _ => Nil
    }.sortBy(_.witness.stableKey)
    VerticalLegalRestrictionClause(restriction, pinCauses)

  /** One shared, demand-created view for the L1 contracts that explain a
    * root capture. The exact position inventory owns both source lists; this
    * view prevents sibling contracts from rebuilding the same join.
    */
  private lazy val captureRecaptureInput: Option[VerticalCaptureRecaptureInput] =
    delta.rootMove.capture.map { captured =>
      val after = delta.afterInventory
      require(
        after.sideToMove == captured.capturedSide && captured.capturedSide != delta.rootMove.side,
        "the captured side must own the immediate response position"
      )
      val geometric = after.controlsAt(captured.capturedSide, delta.rootMove.to).filter(_.target match
        case RelationControlTarget.Enemy(role) => role == delta.rootMove.afterRole
        case _                                 => false
      ).sortBy(control => control.controller.square.key -> control.controller.role.name)
      val legal = after.legalCapturesOf(delta.rootMove.to).filter(move =>
        move.capture.exists(value =>
          value.capturedSquare == delta.rootMove.to && value.capturedSide == delta.rootMove.side &&
            value.capturedRole == delta.rootMove.afterRole
        )
      ).sortBy(_.moveUci)
      val geometricPieces = geometric.map(_.controller).toSet
      require(
        legal.forall(move => geometricPieces(RelationPieceWitness(move.movement.from, move.movement.beforeRole))),
        "every legal recapture must retain its canonical geometric control"
      )
      val legalPieces = legal.map(move =>
        RelationPieceWitness(move.movement.from, move.movement.beforeRole)
      ).toSet
      val restrictions = geometric.filterNot(control => legalPieces(control.controller)).map { control =>
        val resources = after.movementAffordancesFrom(control.controller.square).filter(resource =>
          resource.piece == control.controller && resource.destination == delta.rootMove.to &&
            resource.target == control.target &&
            resource.mode == RelationMovementResourceMode.ControlledDestination
        )
        require(resources.size == 1, "one geometric recapture needs one exact movement resource")
        after.legalRestrictionFor(resources.head).map(legalRestrictionClauseFor).getOrElse(
          throw IllegalArgumentException(
            s"geometric recapture '${control.controller.role.name}@${control.controller.square.key}' is neither legal nor an exact own-king exposure"
          )
        )
      }.sortBy(_.stableKey)
      VerticalCaptureRecaptureInput(delta.rootMove, captured, geometric, legal, restrictions)
    }

  private[judgment] def captureRecapture: Option[VerticalCaptureRecaptureInput] =
    captureRecaptureInput

  /** Shared interpretation of one newly established check. Double-check and
    * response contracts consume this exact view instead of parsing the same
    * control-set delta independently.
    */
  private lazy val newlyEstablishedCheckInput: List[VerticalCheckTransitionSubject] =
    controlSetChanges.flatMap { change =>
      (change.beforeTarget, change.afterTarget) match
        case (RelationControlTarget.Enemy(beforeRole), RelationControlTarget.Enemy(afterRole))
            if change.controllingSide == change.mover.side &&
              beforeRole.name.equalsIgnoreCase(King.name) && afterRole.name.equalsIgnoreCase(King.name) &&
              change.afterControllers.nonEmpty && change.establishedControllers.nonEmpty =>
          Some(VerticalCheckTransitionSubject(
            change.relation,
            change.mover,
            !change.controllingSide,
            change.target,
            change.afterControllers.sortBy(value => value.square.key -> value.role.name)
          ))
        case _ => None
    }.sortBy(_.relation.semanticId)

  private[judgment] def newlyEstablishedChecks: List[VerticalCheckTransitionSubject] =
    newlyEstablishedCheckInput

  private lazy val sliderReachDeltaInput: List[VerticalSliderReachDeltaInput] =
    delta.sliderReachTransition.changes.map { change =>
      val beforeStatePiece = change.sliderBefore.orElse(change.sliderAfter).getOrElse(
        throw IllegalArgumentException("a slider-reach transition lost its before-state identity")
      )
      val afterStatePiece = change.sliderAfter.orElse(change.sliderBefore).getOrElse(
        throw IllegalArgumentException("a slider-reach transition lost its after-state identity")
      )
      VerticalSliderReachDeltaInput(
        change.side,
        change.sliderBefore,
        change.sliderAfter,
        change.direction,
        change.before,
        change.after,
        List(
          ClosedPositionStatePremise(
            RelationSnapshotOccurrence.Before,
            PositionRelationExtractor.ClosedPositionStateQuery.SliderReach(
              change.side,
              beforeStatePiece,
              change.direction,
              change.before.map(_.witness)
            )
          ),
          ClosedPositionStatePremise(
            RelationSnapshotOccurrence.After,
            PositionRelationExtractor.ClosedPositionStateQuery.SliderReach(
              change.side,
              afterStatePiece,
              change.direction,
              change.after.map(_.witness)
            )
          )
        ).sortBy(_.stableKey)
      )
    }.sortBy(_.stableKey)

  private[judgment] def sliderReachDeltas: List[VerticalSliderReachDeltaInput] =
    sliderReachDeltaInput

  private lazy val sliderReachDeltaRelations: List[RelationFactEvidence] =
    VerticalRelationContracts.computeSliderReachDeltas(this)

  private[judgment] def sliderReachDeltaResults: List[RelationFactEvidence] =
    sliderReachDeltaRelations

  /** Exact closed positive projections for a position occurrence. Consumers
    * query this owner for controls, legal moves, king responses, ray
    * corridors, or pawn topology and retain each projection's existing
    * relation sources; they do not rebuild facts from a FEN.
    */
  private[judgment] def positionInventory(
      occurrence: RelationSnapshotOccurrence
  ): PositionRelationExtractor.PositionRelationInventoryCertificate =
    occurrence match
      case RelationSnapshotOccurrence.Before => delta.beforeInventory
      case RelationSnapshotOccurrence.After  => delta.afterInventory

  /** The proof shape closes lower facts against the output assertion; this
    * check separately closes the occurrence against this exact root replay.
    */
  private[judgment] def ownsOutput(detail: RelationWitnessDetail): Boolean =
    def rootOwned(mover: RelationMoveTransitionWitness): Boolean =
      mover == rootMove.witness
    def checkTerminal(
        terminal: PositionRelationExtractor.ClosedKingTerminalState
    ): Option[RelationCheckTerminalState] =
      terminal match
        case PositionRelationExtractor.ClosedKingTerminalState.Ongoing =>
          Some(RelationCheckTerminalState.Ongoing)
        case PositionRelationExtractor.ClosedKingTerminalState.Checkmate =>
          Some(RelationCheckTerminalState.Checkmate)
        case PositionRelationExtractor.ClosedKingTerminalState.Stalemate =>
          None
    detail match
      case RelationWitnessDetail.CaptureRecaptureInventory(
            mover,
            captured,
            geometricRecapturers,
            legalRecaptures,
            restrictedRecaptures,
            _
          ) =>
        rootOwned(mover) && captureRecapture.exists(view =>
          captured == RelationColoredPieceWitness(
            view.captured.capturedSquare,
            view.captured.capturedRole,
            view.captured.capturedSide
          ) && geometricRecapturers == view.geometric.map(_.controller) &&
            legalRecaptures == view.legal.map(legalResource).sortBy(_.stableKey) &&
            restrictedRecaptures == view.restrictions.map(restrictedResource).sortBy(_.stableKey)
        )
      case RelationWitnessDetail.CreatedCheckResponseInventory(
            mover,
            checkedSide,
            kingSquare,
            checkers,
            responses,
            controlledKingDestinations,
            terminal,
            _
          ) =>
        val inventory = positionInventory(RelationSnapshotOccurrence.After).kingResponses
        val exactCheckers = inventory.checkers.map(_.controller).sortBy(value => value.square.key -> value.role.name)
        rootOwned(mover) && newlyEstablishedChecks.exists(subject =>
          subject.mover == mover && subject.checkedSide == checkedSide && subject.kingSquare == kingSquare &&
            subject.afterCheckers == checkers
        ) && inventory.side == checkedSide && inventory.inCheck && inventory.kingSquare.contains(kingSquare) &&
          checkers == exactCheckers && responses == inventory.responses.map(checkResponse).sortBy(_.stableKey) &&
          controlledKingDestinations == inventory.controlledKingDestinations
            .map(controlledKingDestination).sortBy(_.stableKey) &&
          checkTerminal(inventory.terminal).contains(terminal)
      case RelationWitnessDetail.RootCheckResponse(
            mover,
            respondingSide,
            kingSquare,
            checkers,
            response,
            _
          ) =>
        val inventory = positionInventory(RelationSnapshotOccurrence.Before).kingResponses
        val selected = inventory.responses.filter(candidate =>
          EvidenceRef.sameMove(candidate.move.moveUci, rootMove.moveUci)
        )
        rootOwned(mover) && respondingSide == rootMove.side && inventory.side == respondingSide &&
          inventory.inCheck && inventory.kingSquare.contains(kingSquare) &&
          checkers == inventory.checkers.map(_.controller).sortBy(value => value.square.key -> value.role.name) &&
          selected.size == 1 && response == checkResponse(selected.head)
      case detail @ RelationWitnessDetail.SliderReachDelta(mover, _, _, _, _, _, _, _) =>
        rootOwned(mover) && sliderReachDeltaResults.exists(_.detail == detail)
      case RelationWitnessDetail.PawnTopologyTransition(mover, before, after, changedFacets, _) =>
        val beforeTopology = positionInventory(RelationSnapshotOccurrence.Before).pawnTopologyView
        val afterTopology = positionInventory(RelationSnapshotOccurrence.After).pawnTopologyView
        val expectedAfter = before.flatMap { previous =>
          if rootMove.beforeRole.name.equalsIgnoreCase(Pawn.name) && previous.side == rootMove.side &&
              previous.square == rootMove.from
          then
            Option.when(rootMove.afterRole.name.equalsIgnoreCase(Pawn.name))(
              afterTopology.stateWitness(rootMove.side, rootMove.to)
            ).flatten
          else if rootMove.capture.exists(capture =>
              capture.capturedRole.name.equalsIgnoreCase(Pawn.name) &&
                capture.capturedSide == previous.side && capture.capturedSquare == previous.square
            )
          then None
          else afterTopology.stateWitness(previous.side, previous.square)
        }
        rootOwned(mover) && before.nonEmpty && before.forall(previous =>
          beforeTopology.stateWitness(previous.side, previous.square).contains(previous)
        ) && after == expectedAfter && changedFacets == RelationPawnTopologyFacet.changed(before, expectedAfter)
      case RelationWitnessDetail.StalemateTransition(mover, _, _, _) =>
        rootOwned(mover)
      case _ => false

  def resolves(premise: VerticalRelationPremise): Boolean =
    val candidate = premise.source match
      case VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.Before) =>
        delta.beforeInventory.relationBySemanticId(premise.semanticId)
      case VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After) =>
        delta.afterInventory.relationBySemanticId(premise.semanticId)
      case VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.Removed) =>
        delta.changeBySemanticId(premise.semanticId)
          .filter(_.direction == RelationChangeDirection.Removed)
          .map(_.relation)
      case VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.Established) =>
        delta.changeBySemanticId(premise.semanticId)
          .filter(_.direction == RelationChangeDirection.Established)
          .map(_.relation)
      case VerticalRelationPremiseSource.RootTransition =>
        Option.when(
          premise.stage == RelationProofStage.TransitionFact &&
            delta.rootMove.fact.kind == premise.kind &&
            delta.rootMove.fact.assertionId == premise.assertionId &&
            delta.rootMove.fact.semanticId == premise.semanticId
        )(delta.rootMove.fact)
      case VerticalRelationPremiseSource.Derived =>
        derivedBySemanticId.get(premise.semanticId)
    candidate.exists { relation =>
      val exactStage = premise.source match
        case VerticalRelationPremiseSource.RootTransition =>
          relation.proofStage == RelationProofStage.PositionFact && premise.stage == RelationProofStage.TransitionFact
        case _ => relation.proofStage == premise.stage
      relation.kind == premise.kind && relation.assertionId == premise.assertionId &&
      relation.semanticId == premise.semanticId && exactStage &&
        VerticalRelationPremiseRole.accepts(
          premise.role,
          premise.source,
          relation,
          delta.transitionFootprint
        )
    }

  def resolves(absence: ClosedRelationAbsencePremise): Boolean =
    absenceCertificate(absence).nonEmpty

  def resolves(state: ClosedPositionStatePremise): Boolean =
    stateCertificate(state).nonEmpty

  private[judgment] def absenceCertificate(
      absence: ClosedRelationAbsencePremise
  ): Option[PositionRelationExtractor.ClosedRelationAbsenceCertificate] =
    val inventory = absence.occurrence match
      case RelationSnapshotOccurrence.Before => delta.beforeInventory
      case RelationSnapshotOccurrence.After  => delta.afterInventory
    inventory.certifyAbsence(absence.query)

  private[judgment] def stateCertificate(
      state: ClosedPositionStatePremise
  ): Option[PositionRelationExtractor.ClosedPositionStateCertificate] =
    val inventory = state.occurrence match
      case RelationSnapshotOccurrence.Before => delta.beforeInventory
      case RelationSnapshotOccurrence.After  => delta.afterInventory
    inventory.certifyState(state.query)

  private[judgment] def admit(relations: List[RelationFactEvidence]): Unit =
    retainDerived(relations, allowExisting = false)

  private def retainDerived(
      relations: List[RelationFactEvidence],
      allowExisting: Boolean
  ): Unit =
    val entries = relations.map(relation => relation.semanticId -> relation)
    require(
      entries.map(_._1).distinct.size == entries.size,
      "one lower contract cannot repeat a derivation identity"
    )
    entries.foreach { case (semanticId, relation) =>
      derivedBySemanticId.get(semanticId) match
        case Some(existing) =>
          require(
            allowExisting && existing.asInstanceOf[AnyRef].eq(relation.asInstanceOf[AnyRef]),
            "a vertical derivation cannot replace or repeat a lower result"
          )
        case None =>
          derivedBySemanticId = derivedBySemanticId.updated(semanticId, relation)
    }

private[chessjudgment] object VerticalRelationInput:
  def apply(
      delta: RelationSemanticDelta,
      combinations: ClosedRelationCombinationResults
  ): VerticalRelationInput =
    new VerticalRelationInput(delta, combinations)

private[chessjudgment] final case class VerticalCaptureRecaptureInput(
    rootMove: CanonicalRootLegalMove,
    captured: RelationLegalCaptureWitness,
    geometric: List[PositionRelationExtractor.ClosedGeometricControl],
    legal: List[PositionRelationExtractor.ClosedLegalMove],
    restrictions: List[VerticalLegalRestrictionClause]
)

private[chessjudgment] final case class VerticalAbsolutePinCause(
    witness: RelationAbsolutePinPathWitness,
    source: RelationFactEvidence
)

private[chessjudgment] final case class VerticalLegalRestrictionClause(
    restriction: PositionRelationExtractor.ClosedLegalResourceRestriction,
    absolutePinCauses: List[VerticalAbsolutePinCause]
):
  require(
    absolutePinCauses.distinct.size == absolutePinCauses.size &&
      absolutePinCauses == absolutePinCauses.sortBy(_.witness.stableKey),
    "absolute-pin causes must be unique and canonically ordered"
  )

  def stableKey: String =
    s"${restriction.stableKey}:${absolutePinCauses.map(_.witness.stableKey).mkString("[", ",", "]")}"

private[chessjudgment] final case class VerticalSliderReachDeltaInput(
    side: _root_.chess.Color,
    sliderBefore: Option[RelationPieceWitness],
    sliderAfter: Option[RelationPieceWitness],
    direction: RelationRayDirection,
    before: Option[PositionRelationExtractor.ClosedSliderReach],
    after: Option[PositionRelationExtractor.ClosedSliderReach],
    states: List[ClosedPositionStatePremise]
):
  require(sliderBefore.nonEmpty || sliderAfter.nonEmpty, "a slider-reach delta needs one slider identity")
  require(before.map(_.witness) != after.map(_.witness), "a slider-reach delta must change")
  require(before.forall(reach => sliderBefore.contains(reach.slider) && reach.side == side))
  require(after.forall(reach => sliderAfter.contains(reach.slider) && reach.side == side))
  require(states.size == 2 && states.distinct.size == states.size && states == states.sortBy(_.stableKey))

  def stableKey: String =
    s"${side.toString.toLowerCase}:${sliderBefore.map(piece => s"${piece.role.name}@${piece.square.key}").getOrElse("-")}:${sliderAfter.map(piece => s"${piece.role.name}@${piece.square.key}").getOrElse("-")}:${direction.stableKey}"

private[chessjudgment] final case class VerticalCheckTransitionSubject(
    relation: RelationFactEvidence,
    mover: RelationMoveTransitionWitness,
    checkedSide: _root_.chess.Color,
    kingSquare: EvidenceSquare,
    afterCheckers: List[RelationPieceWitness]
)

private[chessjudgment] final case class VerticalControlSetChange(
    relation: RelationFactEvidence,
    mover: RelationMoveTransitionWitness,
    controllingSide: _root_.chess.Color,
    target: EvidenceSquare,
    beforeTarget: RelationControlTarget,
    afterTarget: RelationControlTarget,
    beforeControllers: List[RelationPieceWitness],
    afterControllers: List[RelationPieceWitness],
    removedControllers: List[RelationPieceWitness],
    establishedControllers: List[RelationPieceWitness]
)

/** One occurrence-owned absence capability. The opaque proof can only be
  * minted by the exact closed position inventory.
  */
private[chessjudgment] final class BoundClosedRelationAbsence private (
    val premise: ClosedRelationAbsencePremise,
    proof: PositionRelationExtractor.ClosedRelationAbsenceProof,
    positionScope: EvidenceScope,
    owner: PositionRelationExtractor.PositionRelationInventoryCertificate
):
  def ownedBy(edge: MoveTransitionEdge): Boolean =
    val expectedPosition = premise.occurrence match
      case RelationSnapshotOccurrence.Before => edge.from
      case RelationSnapshotOccurrence.After  => edge.to
    proof.query == premise.query &&
      proof.sameOwner(expectedPosition, positionScope, owner)

private[chessjudgment] object BoundClosedRelationAbsence:
  def bind(
      premise: ClosedRelationAbsencePremise,
      certificate: PositionRelationExtractor.ClosedRelationAbsenceCertificate,
      before: CanonicalPositionRelationSnapshot,
      after: CanonicalPositionRelationSnapshot
  ): Option[BoundClosedRelationAbsence] =
    val snapshot = premise.occurrence match
      case RelationSnapshotOccurrence.Before => before
      case RelationSnapshotOccurrence.After  => after
    Option.when(certificate.query == premise.query)(certificate).flatMap(snapshot.bindAbsence).map(proof =>
      new BoundClosedRelationAbsence(premise, proof, snapshot.scope, snapshot.inventory)
    )

/** Occurrence-owned positive state capability. Like closed absence, it is
  * demand-created and never persists a duplicate square-state relation.
  */
private[chessjudgment] final class BoundClosedPositionState private (
    val premise: ClosedPositionStatePremise,
    proof: PositionRelationExtractor.ClosedPositionStateProof,
    positionScope: EvidenceScope,
    owner: PositionRelationExtractor.PositionRelationInventoryCertificate
):
  def ownedBy(edge: MoveTransitionEdge): Boolean =
    val expectedPosition = premise.occurrence match
      case RelationSnapshotOccurrence.Before => edge.from
      case RelationSnapshotOccurrence.After  => edge.to
    proof.query == premise.query &&
      proof.sameOwner(expectedPosition, positionScope, owner)

private[chessjudgment] object BoundClosedPositionState:
  def bind(
      premise: ClosedPositionStatePremise,
      certificate: PositionRelationExtractor.ClosedPositionStateCertificate,
      before: CanonicalPositionRelationSnapshot,
      after: CanonicalPositionRelationSnapshot
  ): Option[BoundClosedPositionState] =
    val snapshot = premise.occurrence match
      case RelationSnapshotOccurrence.Before => before
      case RelationSnapshotOccurrence.After  => after
    Option.when(certificate.query == premise.query)(certificate).flatMap(snapshot.bindState).map(proof =>
      new BoundClosedPositionState(premise, proof, snapshot.scope, snapshot.inventory)
    )

/** Single semantic owner of L1 chess implications. Creation and certification
  * share these exact predicates; binders only resolve occurrence identities.
  */
private[chessjudgment] object VerticalRelationContracts:
  import VerticalRelationWitnessProjection.*

  def derive(
      contract: VerticalRelationContractKind,
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    contract match
      case VerticalRelationContractKind.CaptureRecaptureInventory =>
        deriveCaptureRecaptureInventory(input)
      case VerticalRelationContractKind.CreatedCheckResponseInventory =>
        deriveCreatedCheckResponseInventory(input)
      case VerticalRelationContractKind.RootCheckResponse =>
        deriveRootCheckResponse(input)
      case VerticalRelationContractKind.SliderReachDelta =>
        input.sliderReachDeltaResults
      case VerticalRelationContractKind.PawnTopologyTransition =>
        derivePawnTopologyTransitions(input)
      case VerticalRelationContractKind.StalemateTransition =>
        deriveStalemateTransition(input)

  def proofOf(detail: RelationWitnessDetail): Option[VerticalRelationDerivationProof] =
    detail match
      case RelationWitnessDetail.CaptureRecaptureInventory(_, _, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.CreatedCheckResponseInventory(_, _, _, _, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.RootCheckResponse(_, _, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.SliderReachDelta(_, _, _, _, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.PawnTopologyTransition(_, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.StalemateTransition(_, _, _, proof) => Some(proof)
      case _                                                                            => None

  private def deriveCaptureRecaptureInventory(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    input.captureRecapture.toList.map { view =>
      val root = view.rootMove
      val captured = RelationColoredPieceWitness(
        view.captured.capturedSquare,
        view.captured.capturedRole,
        view.captured.capturedSide
      )
      val recapturers = view.geometric.map(_.controller)
      val legal = view.legal.map(legalResource).sortBy(_.stableKey)
      val restricted = view.restrictions.map(restrictedResource).sortBy(_.stableKey)
      val recaptureTarget = RelationPieceWitness(root.to, root.afterRole)
      val premises = view.geometric.map(control =>
          (
            VerticalRelationPremiseRole.GeometricRecaptureCandidate(
              view.captured.capturedSide,
              control.controller,
              recaptureTarget
            ),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            control.source
          )
        ) ++ view.legal.map(move =>
          (
            VerticalRelationPremiseRole.LegalRecapture(legalResource(move)),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            move.source
          )
        ) ++ view.restrictions.flatMap { clause =>
          val restriction = clause.restriction
          (
            VerticalRelationPremiseRole.MovementResource(
              view.captured.capturedSide,
              restriction.movement.piece,
              movementResource(restriction.movement)
            ),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            restriction.movement.source
          ) :: clause.absolutePinCauses.map(cause =>
            (
              VerticalRelationPremiseRole.AbsolutePinBarrier(
                RelationSnapshotOccurrence.After,
                view.captured.capturedSide,
                cause.witness.pinner,
                cause.witness.pinned,
                cause.witness.kingSquare,
                cause.witness.geometry.axis
              ),
              VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
              cause.source
            )
          )
        }.distinctBy { case (role, source, relation) =>
          (role, source, relation.semanticId)
        }
      val absences =
        Option.when(recapturers.isEmpty)(
          ClosedRelationAbsencePremise(
            RelationSnapshotOccurrence.After,
            PositionRelationExtractor.ClosedRelationAbsenceQuery.GeometricControlOf(
              captured.side,
              root.to
            )
          )
        ).toList ++ Option.when(legal.isEmpty)(
          ClosedRelationAbsencePremise(
            RelationSnapshotOccurrence.After,
            PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
              captured.side,
              root.to
            )
          )
        ).toList ++ view.restrictions.map { clause =>
          val restriction = clause.restriction
          ClosedRelationAbsencePremise(
            RelationSnapshotOccurrence.After,
            PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
              captured.side,
              restriction.movement.piece.square,
              restriction.movement.destination
            )
          )
        }
      val states = view.restrictions.map { clause =>
        val restriction = clause.restriction
        ClosedPositionStatePremise(
          RelationSnapshotOccurrence.After,
          PositionRelationExtractor.ClosedPositionStateQuery.OwnKingExposure(
            captured.side,
            restriction.movement.piece,
            movementResource(restriction.movement),
            restriction.kingSquare,
            restriction.postMoveControllers
          )
        )
      }
      val proof = VerticalRelationDerivationProof.fromRootAndSources(
        VerticalRelationContractKind.CaptureRecaptureInventory,
        VerticalRelationPremiseRole.RootCapture,
        root,
        premises,
        absences,
        states
      )
      RelationFactEvidence.from(
        RelationWitnessDetail.CaptureRecaptureInventory(
          root.witness,
          captured,
          recapturers,
          legal,
          restricted,
          proof
        ),
        List(root.moveUci)
      )
    }

  private def deriveCreatedCheckResponseInventory(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    val subjects = input.newlyEstablishedChecks
    if subjects.isEmpty then Nil
    else
      val after = input.positionInventory(RelationSnapshotOccurrence.After)
      subjects.flatMap(subject => checkResponseInventory(subject, after).toList).sortBy(_.semanticId)

  private def checkResponseInventory(
      subject: VerticalCheckTransitionSubject,
      after: PositionRelationExtractor.PositionRelationInventoryCertificate
  ): Option[RelationFactEvidence] =
    val inventory = after.kingResponses
    val exactCheckers = inventory.checkers.map(_.controller).sortBy(value => value.square.key -> value.role.name)
    val exactState = inventory.side == subject.checkedSide && inventory.inCheck &&
      inventory.kingSquare.contains(subject.kingSquare) && exactCheckers == subject.afterCheckers
    Option.when(exactState) {
      val responses = inventory.responses.map(checkResponse).sortBy(_.stableKey)
      val controlledKingDestinations = inventory.controlledKingDestinations
        .map(controlledKingDestination).sortBy(_.stableKey)
      val terminal = inventory.terminal match
        case PositionRelationExtractor.ClosedKingTerminalState.Ongoing =>
          RelationCheckTerminalState.Ongoing
        case PositionRelationExtractor.ClosedKingTerminalState.Checkmate =>
          RelationCheckTerminalState.Checkmate
        case PositionRelationExtractor.ClosedKingTerminalState.Stalemate =>
          throw IllegalArgumentException("a checked response inventory cannot be stalemate")
      val premises =
        List(
          (
            VerticalRelationPremiseRole.CheckControlDelta(subject.checkedSide, subject.kingSquare),
            VerticalRelationPremiseSource.Derived,
            subject.relation
          )
        ) ++ inventory.checkers.map(checker =>
          (
            VerticalRelationPremiseRole.CheckerControl(
              RelationSnapshotOccurrence.After,
              !subject.checkedSide,
              checker.controller,
              subject.kingSquare
            ),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            checker.source
          )
        ) ++ inventory.interpositionRay.toList.map(ray =>
          (
            VerticalRelationPremiseRole.CheckerRay(
              RelationSnapshotOccurrence.After,
              exactCheckers.head,
              subject.kingSquare
            ),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            ray
          )
        ) ++ inventory.responses.map(response =>
          (
            VerticalRelationPremiseRole.CheckResponse(checkResponse(response)),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            response.move.source
          )
        ) ++ inventory.controlledKingDestinations.flatMap { destination =>
          val resource = movementResource(destination.movement)
          List((
            VerticalRelationPremiseRole.MovementResource(
              subject.checkedSide,
              RelationPieceWitness(subject.kingSquare, EvidencePieceRole(King.name)),
              resource
            ),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            destination.movement.source
          ))
        }
      val absences = controlledKingDestinations.map(destination =>
        ClosedRelationAbsencePremise(
          RelationSnapshotOccurrence.After,
          PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
            subject.checkedSide,
            subject.kingSquare,
            destination.resource.destination
          )
        )
      ) ++ Option.when(responses.isEmpty)(
        ClosedRelationAbsencePremise(
          RelationSnapshotOccurrence.After,
          PositionRelationExtractor.ClosedRelationAbsenceQuery.AnyLegalMove(subject.checkedSide)
        )
      ).toList
      val states = inventory.controlledKingDestinations.map(destination =>
        ClosedPositionStatePremise(
          RelationSnapshotOccurrence.After,
          PositionRelationExtractor.ClosedPositionStateQuery.OwnKingExposure(
            subject.checkedSide,
            destination.movement.piece,
            movementResource(destination.movement),
            destination.kingSquare,
            destination.postMoveControllers
          )
        )
      )
      val proof = VerticalRelationDerivationProof.fromSources(
        VerticalRelationContractKind.CreatedCheckResponseInventory,
        premises,
        absences,
        states = states
      )
      RelationFactEvidence.from(
        RelationWitnessDetail.CreatedCheckResponseInventory(
          subject.mover,
          subject.checkedSide,
          subject.kingSquare,
          exactCheckers,
          responses,
          controlledKingDestinations,
          terminal,
          proof
        ),
        subject.relation.lineMoves
      )
    }

  private def deriveRootCheckResponse(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    val before = input.positionInventory(RelationSnapshotOccurrence.Before)
    val inventory = before.kingResponses
    require(
      inventory.side == input.rootMove.side && inventory.inCheck,
      "a root check-response contract requires the checked side's exact before-position inventory"
    )
    val selected = inventory.responses.filter(response =>
      EvidenceRef.sameMove(response.move.moveUci, input.rootMove.moveUci)
    )
    require(selected.size == 1, "the admitted root move must occur exactly once in the closed check responses")
    val response = selected.head
    val checkers = inventory.checkers.map(_.controller).sortBy(value => value.square.key -> value.role.name)
    val kingSquare = inventory.kingSquare.getOrElse(
      throw IllegalArgumentException("a checked legal position needs one exact king square")
    )
    val responseWitness = checkResponse(response)
    val sources =
      (
        VerticalRelationPremiseRole.ChosenCheckResponse(responseWitness),
        VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.Before),
        response.move.source
      ) :: inventory.checkers.map(checker =>
        (
          VerticalRelationPremiseRole.CheckerControl(
            RelationSnapshotOccurrence.Before,
            !input.rootMove.side,
            checker.controller,
            kingSquare
          ),
          VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.Before),
          checker.source
        )
      ) ++ Option.when(response.modes(PositionRelationExtractor.ClosedKingResponseMode.Interpose))(
        inventory.interpositionRay.map(ray =>
          (
            VerticalRelationPremiseRole.CheckerRay(
              RelationSnapshotOccurrence.Before,
              checkers.head,
              kingSquare
            ),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.Before),
            ray
          )
        )
      ).flatten.toList
    val proof = VerticalRelationDerivationProof.fromRootAndSources(
      VerticalRelationContractKind.RootCheckResponse,
      VerticalRelationPremiseRole.RootMove,
      input.rootMove,
      sources
    )
    List(RelationFactEvidence.from(
      RelationWitnessDetail.RootCheckResponse(
        input.rootMove.witness,
        input.rootMove.side,
        kingSquare,
        checkers,
        responseWitness,
        proof
      ),
      List(input.rootMove.moveUci)
    ))

  private def sliderReachSources(
      input: VerticalRelationInput,
      occurrence: RelationSnapshotOccurrence,
      side: _root_.chess.Color,
      direction: RelationRayDirection,
      reach: Option[PositionRelationExtractor.ClosedSliderReach]
  ): List[(VerticalRelationPremiseRole, VerticalRelationPremiseSource, RelationFactEvidence)] =
    reach.toList.flatMap { exact =>
      require(
        exact.side == side && exact.direction == direction,
        "a slider-reach source changed owner or direction"
      )
      exact.controls.map { control =>
        (
          VerticalRelationPremiseRole.SliderReachControl(
            occurrence,
            side,
            exact.slider,
            direction,
            RelationControlReachWitness(control.targetSquare, control.target)
          ),
          input.exactPositionSource(occurrence, control.source),
          control.source
        )
      } ++ List {
        val ray = exact.raySource
        (
          VerticalRelationPremiseRole.SliderReachBarrier(
            occurrence,
            side,
            exact.slider,
            direction,
            exact.firstOccupant
          ),
          input.exactPositionSource(occurrence, ray),
          ray
        )
      }
    }

  private[judgment] def computeSliderReachDeltas(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    input.sliderReachDeltas.map { delta =>
      val proof = VerticalRelationDerivationProof.fromRootAndSources(
        VerticalRelationContractKind.SliderReachDelta,
        VerticalRelationPremiseRole.RootMove,
        input.rootMove,
        sliderReachSources(
          input,
          RelationSnapshotOccurrence.Before,
          delta.side,
          delta.direction,
          delta.before
        ) ++ sliderReachSources(
          input,
          RelationSnapshotOccurrence.After,
          delta.side,
          delta.direction,
          delta.after
        ),
        states = delta.states
      )
      RelationFactEvidence.from(
        RelationWitnessDetail.SliderReachDelta(
          input.rootMove.witness,
          delta.side,
          delta.sliderBefore,
          delta.sliderAfter,
          delta.direction,
          delta.before.map(_.witness),
          delta.after.map(_.witness),
          proof
        ),
        List(input.rootMove.moveUci)
      )
    }.sortBy(_.semanticId)

  private final case class PawnTopologyOccurrence(
      witness: RelationPawnTopologyStateWitness,
      sourcesByFacet: Map[RelationPawnTopologyFacet, List[RelationFactEvidence]]
  ):
    def sourcesFor(facet: RelationPawnTopologyFacet): List[RelationFactEvidence] =
      sourcesByFacet.getOrElse(facet, Nil)

  private def derivePawnTopologyTransitions(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    val before = pawnTopologyOccurrences(
      input.positionInventory(RelationSnapshotOccurrence.Before).pawnTopologyView
    )
    val after = pawnTopologyOccurrences(
      input.positionInventory(RelationSnapshotOccurrence.After).pawnTopologyView
    )
    val root = input.rootMove
    val moverWasPawn = root.beforeRole.name.equalsIgnoreCase("pawn")
    val moverRemainsPawn = root.afterRole.name.equalsIgnoreCase("pawn")
    val capturedPawn = root.capture.filter(_.capturedRole.name.equalsIgnoreCase("pawn"))
    val pairs = before.toList.sortBy { case ((side, square), _) => side.toString -> square.key }.map {
      case (key @ (side, square), previous) =>
        val nextKey =
          if moverWasPawn && side == root.side && square == root.from then
            Option.when(moverRemainsPawn)(side -> root.to)
          else if capturedPawn.exists(capture => capture.capturedSide == side && capture.capturedSquare == square) then
            None
          else Some(key)
        previous -> nextKey.flatMap(after.get)
    }
    val matchedAfter = pairs.flatMap(_._2.map(next => next.witness.side -> next.witness.square)).toSet
    require(
      after.keySet == matchedAfter,
      "a legal transition cannot create an unpaired pawn topology identity"
    )
    pairs.flatMap { case (previous, next) =>
      val changed = RelationPawnTopologyFacet.changed(Some(previous.witness), next.map(_.witness))
      Option.when(changed.nonEmpty) {
        def sourcePremises(
            occurrence: RelationSnapshotOccurrence,
            side: _root_.chess.Color,
            pawn: EvidenceSquare,
            facet: RelationPawnTopologyFacet,
            componentPawns: List[EvidenceSquare],
            sources: List[RelationFactEvidence]
        ): List[(VerticalRelationPremiseRole, VerticalRelationPremiseSource, RelationFactEvidence)] =
          require(
            sources.map(_.semanticId).distinct.size == sources.size,
            "one pawn topology facet cannot repeat a canonical source"
          )
          sources.sortBy(_.semanticId).map { source =>
            (
              VerticalRelationPremiseRole.PawnTopologyFacetSource(
                occurrence,
                side,
                pawn,
                facet,
                componentPawns,
                source.semanticId
              ),
              input.exactPositionSource(occurrence, source),
              source
            )
          }
        val changedPremises = changed.flatMap { facet =>
          sourcePremises(
            RelationSnapshotOccurrence.Before,
            previous.witness.side,
            previous.witness.square,
            facet,
            previous.witness.componentPawns,
            previous.sourcesFor(facet)
          ) ++ next.toList.flatMap(value =>
            sourcePremises(
              RelationSnapshotOccurrence.After,
              value.witness.side,
              value.witness.square,
              facet,
              value.witness.componentPawns,
              value.sourcesFor(facet)
            )
          )
        }
        val passageStatePremises = next.toList.flatMap { value =>
          Option.when(previous.witness.square != value.witness.square) {
            val beforePassage = previous.sourcesFor(RelationPawnTopologyFacet.Passed) match
              case only :: Nil => only
              case found =>
                throw IllegalArgumentException(
                  s"a before pawn occurrence needs one passage source, found ${found.size}"
                )
            val afterPassage = value.sourcesFor(RelationPawnTopologyFacet.Passed) match
              case only :: Nil => only
              case found =>
                throw IllegalArgumentException(
                  s"an after pawn occurrence needs one passage source, found ${found.size}"
                )
            List(
              (
                VerticalRelationPremiseRole.PawnPassageStateSource(
                  RelationSnapshotOccurrence.Before,
                  previous.witness.side,
                  previous.witness.square,
                  beforePassage.semanticId
                ),
                input.exactPositionSource(
                  RelationSnapshotOccurrence.Before,
                  beforePassage
                ),
                beforePassage
              ),
              (
                VerticalRelationPremiseRole.PawnPassageStateSource(
                  RelationSnapshotOccurrence.After,
                  value.witness.side,
                  value.witness.square,
                  afterPassage.semanticId
                ),
                input.exactPositionSource(
                  RelationSnapshotOccurrence.After,
                  afterPassage
                ),
                afterPassage
              )
            )
          }.toList.flatten
        }
        val premises = changedPremises ++ passageStatePremises
        val protectedPasserAbsences = Option.when(
          changed.contains(RelationPawnTopologyFacet.GeometricallyProtectedPasser)
        )(
          List(
            RelationSnapshotOccurrence.Before -> Some(previous.witness),
            RelationSnapshotOccurrence.After -> next.map(_.witness)
          ).flatMap { case (occurrence, state) =>
            state.filter(value => value.passed && !value.geometricallyProtectedPasser).map(value =>
              ClosedRelationAbsencePremise(
                occurrence,
                PositionRelationExtractor.ClosedRelationAbsenceQuery.GeometricPawnSupportOf(
                  value.side,
                  value.square
                )
              )
            )
          }
        ).toList.flatten
        val states = ClosedPositionStatePremise(
          RelationSnapshotOccurrence.Before,
          PositionRelationExtractor.ClosedPositionStateQuery.PawnTopology(previous.witness)
        ) :: next.map(value =>
          ClosedPositionStatePremise(
            RelationSnapshotOccurrence.After,
            PositionRelationExtractor.ClosedPositionStateQuery.PawnTopology(value.witness)
          )
        ).toList
        val proof = VerticalRelationDerivationProof.fromRootAndSources(
          VerticalRelationContractKind.PawnTopologyTransition,
          VerticalRelationPremiseRole.RootMove,
          root,
          premises,
          protectedPasserAbsences,
          states
        )
        RelationFactEvidence.from(
          RelationWitnessDetail.PawnTopologyTransition(
            root.witness,
            Some(previous.witness),
            next.map(_.witness),
            changed,
            proof
          ),
          List(root.moveUci)
        )
      }.toList
    }.sortBy(_.semanticId)

  private def pawnTopologyOccurrences(
      topology: PositionRelationExtractor.ClosedPawnTopology
  ): Map[(_root_.chess.Color, EvidenceSquare), PawnTopologyOccurrence] =
    val statesByPawn = topology.stateWitnesses.map(state => (state.side -> state.square) -> state).toMap
    require(
      statesByPawn.size == topology.stateWitnesses.size,
      "one closed pawn topology projection must own one witness per pawn identity"
    )
    val connectionsByPawn = topology.connections.flatMap { connection =>
      connection.kind match
        case PositionRelationExtractor.ClosedPawnConnectionKind.GeometricSupport =>
          List(
            (connection.side -> connection.controller) -> (
              RelationPawnConnectionWitness(
                connection.target,
                RelationPawnConnectionKind.GeometricSupport,
                RelationPawnConnectionDirection.Supports
              ) -> connection
            ),
            (connection.side -> connection.target) -> (
              RelationPawnConnectionWitness(
                connection.controller,
                RelationPawnConnectionKind.GeometricSupport,
                RelationPawnConnectionDirection.SupportedBy
              ) -> connection
            )
          )
        case PositionRelationExtractor.ClosedPawnConnectionKind.Phalanx =>
          List(
            (connection.side -> connection.controller) -> (
              RelationPawnConnectionWitness(
                connection.target,
                RelationPawnConnectionKind.Phalanx,
                RelationPawnConnectionDirection.Peer
              ) -> connection
            ),
            (connection.side -> connection.target) -> (
              RelationPawnConnectionWitness(
                connection.controller,
                RelationPawnConnectionKind.Phalanx,
                RelationPawnConnectionDirection.Peer
              ) -> connection
            )
          )
    }.groupMap(_._1)(_._2)
    val contactsByPawn = topology.enemyPawnContacts.flatMap(contact =>
      List(
        (chess.Color.White -> contact.whitePawn) -> (contact.blackPawn -> contact.source),
        (chess.Color.Black -> contact.blackPawn) -> (contact.whitePawn -> contact.source)
      )
    ).groupMap(_._1)(_._2)
    val entries = topology.pawns.map { pawn =>
      val connections = connectionsByPawn.getOrElse(pawn.side -> pawn.square, Nil)
      val contacts = contactsByPawn.getOrElse(pawn.side -> pawn.square, Nil)
      val state = statesByPawn.getOrElse(
        pawn.side -> pawn.square,
        throw IllegalArgumentException("a closed pawn state lost its canonical topology witness")
      )

      /** Several topology edges may intentionally share one closed L0 fact
        * (for example one PawnFileGroup can witness two phalanx edges). The
        * proof domain is the set of canonical facts, so collapse only those
        * explicit shared owners and reject conflicting owners.
        */
      def sourceSet(
          label: String,
          sources: List[RelationFactEvidence]
      ): List[RelationFactEvidence] =
        sources
          .groupBy(_.semanticId)
          .toList
          .sortBy(_._1)
          .map { case (semanticId, owners) =>
            require(
              owners.tail.forall(owner => owner.asInstanceOf[AnyRef].eq(owners.head.asInstanceOf[AnyRef])),
              s"pawn topology $label source '$semanticId' changed canonical owner"
            )
            owners.head
          }

      val localConnectionSources = sourceSet(
        "connection",
        connections.flatMap(_._2.sources)
      )
      val componentSources = sourceSet(
        "component",
        pawn.componentPawnGroupSources ++ pawn.componentConnections.flatMap(_.sources)
      )
      val incomingPawnSupportSources = sourceSet(
        "protected-passer",
        connections.collect {
          case (witness, connection)
              if witness.direction == RelationPawnConnectionDirection.SupportedBy &&
                witness.kind == RelationPawnConnectionKind.GeometricSupport =>
            connection.sources
        }.flatten
      )
      val facetSources = Map(
        RelationPawnTopologyFacet.Doubled -> List(pawn.ownFileSource),
        RelationPawnTopologyFacet.Isolated ->
          sourceSet("isolation", pawn.ownFileSource :: pawn.adjacentFileSources),
        RelationPawnTopologyFacet.Passed -> List(pawn.passageSource),
        RelationPawnTopologyFacet.GeometricallyProtectedPasser ->
          sourceSet("protected-passer", pawn.passageSource :: incomingPawnSupportSources),
        RelationPawnTopologyFacet.FrontState -> List(pawn.frontSource),
        RelationPawnTopologyFacet.Component -> componentSources,
        RelationPawnTopologyFacet.Connections -> localConnectionSources,
        RelationPawnTopologyFacet.EnemyPawnContacts -> sourceSet(
          "enemy-pawn-contact",
          pawn.passageSource :: contacts.map(_._2)
        ),
        RelationPawnTopologyFacet.Removed -> sourceSet(
          "removed-state",
          List(pawn.ownFileSource, pawn.frontSource, pawn.passageSource) ++
            pawn.adjacentFileSources ++ componentSources ++ localConnectionSources ++ contacts.map(_._2)
        )
      )
      (pawn.side -> pawn.square) -> PawnTopologyOccurrence(
        state,
        facetSources
      )
    }
    require(entries.map(_._1).distinct.size == entries.size, "one closed pawn state must own one identity")
    entries.toMap

  private def deriveStalemateTransition(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    val after = input.positionInventory(RelationSnapshotOccurrence.After)
    val terminal = after.kingResponses
    require(
      terminal.terminal == PositionRelationExtractor.ClosedKingTerminalState.Stalemate,
      "a stalemate transition requires the closed after-position terminal state"
    )
    val stalledSide = terminal.side
    val kingSquare = terminal.kingSquare.getOrElse(
      throw IllegalArgumentException("a stalemate position needs one exact king square")
    )
    val proof = VerticalRelationDerivationProof.fromRootAndSources(
      VerticalRelationContractKind.StalemateTransition,
      VerticalRelationPremiseRole.RootMove,
      input.rootMove,
      Nil,
      List(
        ClosedRelationAbsencePremise(
          RelationSnapshotOccurrence.After,
          PositionRelationExtractor.ClosedRelationAbsenceQuery.AnyLegalMove(stalledSide)
        ),
        ClosedRelationAbsencePremise(
          RelationSnapshotOccurrence.After,
          PositionRelationExtractor.ClosedRelationAbsenceQuery.KingCheck(stalledSide, kingSquare)
        )
      )
    )
    List(RelationFactEvidence.from(
      RelationWitnessDetail.StalemateTransition(
        input.rootMove.witness,
        stalledSide,
        kingSquare,
        proof
      ),
      List(input.rootMove.moveUci)
    ))
