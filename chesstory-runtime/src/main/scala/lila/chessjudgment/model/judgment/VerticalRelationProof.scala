package lila.chessjudgment.model.judgment

import _root_.chess.{ File, King, Pawn, Queen, Rook, Side, Square }
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
  case MovementAffordanceLegalRestriction
  case AbsolutePinMovementRestriction
  case SliderReachDelta
  case GeometricMultiTargetContact
  case GeometricEnemyContactWithoutFriendlySupport
  case SharedGeometricSupportOfEnemyControlledTargets
  case GeometricSupportCausalTransition
  case PawnTopologyTransition
  case PawnOccupiedFilePartitionTransition
  case MajorPiecePawnFileCorridorTransition
  case CastlingRightRemoved
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
      case VerticalRelationContractKind.MovementAffordanceLegalRestriction =>
        "movement_affordance_legal_restriction"
      case VerticalRelationContractKind.AbsolutePinMovementRestriction =>
        "absolute_pin_movement_restriction"
      case VerticalRelationContractKind.SliderReachDelta =>
        "slider_reach_delta"
      case VerticalRelationContractKind.GeometricMultiTargetContact =>
        "geometric_multi_target_contact"
      case VerticalRelationContractKind.GeometricEnemyContactWithoutFriendlySupport =>
        "geometric_enemy_contact_without_friendly_support"
      case VerticalRelationContractKind.SharedGeometricSupportOfEnemyControlledTargets =>
        "shared_geometric_support_of_enemy_controlled_targets"
      case VerticalRelationContractKind.GeometricSupportCausalTransition =>
        "geometric_support_causal_transition"
      case VerticalRelationContractKind.PawnTopologyTransition =>
        "pawn_topology_transition"
      case VerticalRelationContractKind.PawnOccupiedFilePartitionTransition =>
        "pawn_occupied_file_partition_transition"
      case VerticalRelationContractKind.MajorPiecePawnFileCorridorTransition =>
        "major_piece_pawn_file_corridor_transition"
      case VerticalRelationContractKind.CastlingRightRemoved =>
        "castling_right_removed"
      case VerticalRelationContractKind.StalemateTransition =>
        "stalemate_transition"

  def outputStage(kind: VerticalRelationContractKind): RelationProofStage =
    kind match
      case VerticalRelationContractKind.CaptureRecaptureInventory |
          VerticalRelationContractKind.CreatedCheckResponseInventory |
          VerticalRelationContractKind.RootCheckResponse |
          VerticalRelationContractKind.MovementAffordanceLegalRestriction |
          VerticalRelationContractKind.AbsolutePinMovementRestriction |
          VerticalRelationContractKind.SliderReachDelta |
          VerticalRelationContractKind.GeometricMultiTargetContact |
          VerticalRelationContractKind.GeometricEnemyContactWithoutFriendlySupport |
          VerticalRelationContractKind.SharedGeometricSupportOfEnemyControlledTargets |
          VerticalRelationContractKind.GeometricSupportCausalTransition |
          VerticalRelationContractKind.PawnTopologyTransition |
          VerticalRelationContractKind.PawnOccupiedFilePartitionTransition |
          VerticalRelationContractKind.MajorPiecePawnFileCorridorTransition |
          VerticalRelationContractKind.CastlingRightRemoved |
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
      case _: RelationWitnessDetail.MovementAffordanceLegalRestriction =>
        Some(VerticalRelationContractKind.MovementAffordanceLegalRestriction)
      case _: RelationWitnessDetail.AbsolutePinMovementRestriction =>
        Some(VerticalRelationContractKind.AbsolutePinMovementRestriction)
      case _: RelationWitnessDetail.SliderReachDelta =>
        Some(VerticalRelationContractKind.SliderReachDelta)
      case _: RelationWitnessDetail.GeometricMultiTargetContact =>
        Some(VerticalRelationContractKind.GeometricMultiTargetContact)
      case _: RelationWitnessDetail.GeometricEnemyContactWithoutFriendlySupport =>
        Some(VerticalRelationContractKind.GeometricEnemyContactWithoutFriendlySupport)
      case _: RelationWitnessDetail.SharedGeometricSupportOfEnemyControlledTargets =>
        Some(VerticalRelationContractKind.SharedGeometricSupportOfEnemyControlledTargets)
      case _: RelationWitnessDetail.GeometricSupportCausalTransition =>
        Some(VerticalRelationContractKind.GeometricSupportCausalTransition)
      case _: RelationWitnessDetail.PawnTopologyTransition =>
        Some(VerticalRelationContractKind.PawnTopologyTransition)
      case _: RelationWitnessDetail.PawnOccupiedFilePartitionTransition =>
        Some(VerticalRelationContractKind.PawnOccupiedFilePartitionTransition)
      case _: RelationWitnessDetail.MajorPiecePawnFileCorridorTransition =>
        Some(VerticalRelationContractKind.MajorPiecePawnFileCorridorTransition)
      case _: RelationWitnessDetail.CastlingRightRemoved =>
        Some(VerticalRelationContractKind.CastlingRightRemoved)
      case _: RelationWitnessDetail.StalemateTransition =>
        Some(VerticalRelationContractKind.StalemateTransition)
      case _ => None

enum RelationCastlingFlank:
  case KingSide
  case QueenSide

  private[chessjudgment] def castleSide: Side =
    this match
      case KingSide  => Side.KingSide
      case QueenSide => Side.QueenSide

  private[chessjudgment] def stableKey: String = toString.toLowerCase

enum RelationSupportChangeCause:
  case SupporterCaptured
  case SliderLineInterrupted
  case SliderLineOpened
  case SupporterRelocated(movement: RelationMoveTransitionWitness)
  case SupportedPieceRelocated(movement: RelationMoveTransitionWitness)

  def id: String =
    this match
      case SupporterCaptured    => "supporter_captured"
      case SliderLineInterrupted => "slider_line_interrupted"
      case SliderLineOpened      => "slider_line_opened"
      case SupporterRelocated(_) => "supporter_relocated"
      case SupportedPieceRelocated(_) => "supported_piece_relocated"

  private[chessjudgment] def stableKey: String =
    this match
      case SupporterRelocated(movement) => s"$id:${movement.stableKey}"
      case SupportedPieceRelocated(movement) => s"$id:${movement.stableKey}"
      case _                            => id

final case class RelationSupportRemovalWitness(
    supporter: RelationPieceWitness,
    causes: List[RelationSupportChangeCause]
):
  require(causes.nonEmpty, "a removed support relation needs at least one exact cause")
  require(
    causes.map(_.stableKey).distinct.size == causes.size && causes == causes.sortBy(_.stableKey),
    "support-removal causes must be unique and canonically ordered"
  )
  causes.foreach {
    case RelationSupportChangeCause.SupporterRelocated(movement) =>
      require(
        movement.from == supporter.square && movement.beforeRole == supporter.role,
        "a relocated support witness must retain the exact supporter origin"
      )
    case RelationSupportChangeCause.SupportedPieceRelocated(_) => ()
    case RelationSupportChangeCause.SliderLineOpened =>
      throw IllegalArgumentException("a removed support relation cannot be caused by a line opening")
    case _ => ()
  }

final case class RelationSupportEstablishmentWitness(
    supporter: RelationPieceWitness,
    causes: List[RelationSupportChangeCause]
):
  require(causes.nonEmpty, "an established support relation needs at least one exact cause")
  require(
    causes.map(_.stableKey).distinct.size == causes.size && causes == causes.sortBy(_.stableKey),
    "support-establishment causes must be unique and canonically ordered"
  )
  causes.foreach {
    case RelationSupportChangeCause.SupporterRelocated(movement) =>
      require(
        movement.to == supporter.square && movement.afterRole == supporter.role,
        "a relocated support witness must retain the exact supporter destination"
      )
    case RelationSupportChangeCause.SupportedPieceRelocated(_) | RelationSupportChangeCause.SliderLineOpened => ()
    case RelationSupportChangeCause.SupporterCaptured | RelationSupportChangeCause.SliderLineInterrupted =>
      throw IllegalArgumentException("an established support relation needs an establishment cause")
  }

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
    opponentTarget: RelationControlTarget,
    controllers: List[RelationPieceWitness]
):
  require(
    resource.mode == RelationMovementResourceMode.ControlledDestination,
    "a controlled king destination needs one exact geometric movement resource"
  )
  require(
    (resource.target, opponentTarget) match
      case (RelationControlTarget.Empty, RelationControlTarget.Empty) => true
      case (RelationControlTarget.Enemy(resourceRole), RelationControlTarget.Friendly(opponentRole)) =>
        resourceRole == opponentRole
      case _ => false,
    "checked-side movement and opponent control must describe the same destination occupancy"
  )
  require(
    controllers.nonEmpty && controllers.distinct.size == controllers.size &&
      controllers == controllers.sortBy(value => value.square.key -> value.role.name),
    "a controlled king destination needs unique canonical opponent controllers"
  )

  private[chessjudgment] def stableKey: String =
    val control = RelationControlReachWitness(resource.destination, opponentTarget)
    s"${resource.stableKey}:${control.stableKey}:${controllers.map(value => s"${value.role.name.toLowerCase}@${value.square.key.toLowerCase}").mkString(",")}"

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

final case class RelationPawnOccupiedFileRunWitness(
    files: List[EvidenceFile],
    pawns: List[EvidenceSquare]
):
  private val fileKeys = files.map(_.key.toLowerCase)
  private val pawnKeys = pawns.map(_.key.toLowerCase)

  require(files.nonEmpty && pawns.nonEmpty, "an occupied-file run needs occupied files and pawns")
  require(files.distinct.size == files.size && pawns.distinct.size == pawns.size)
  require(
    fileKeys == fileKeys.sorted && pawnKeys == pawnKeys.sorted,
    "an occupied-file run witness must use canonical file and pawn order"
  )
  require(
    fileKeys.forall(key => key.length == 1 && key.head >= 'a' && key.head <= 'h') &&
      pawnKeys.forall(key => Square.fromKey(key).nonEmpty),
    "an occupied-file run witness must name valid board files and squares"
  )
  require(
    fileKeys.map(_.head.toInt).sliding(2).forall {
      case List(first, second) => second == first + 1
      case _                   => true
    },
    "an occupied-file run must contain consecutive occupied files"
  )
  require(
    pawnKeys.map(_.head.toString).toSet == fileKeys.toSet,
    "every run file must contain a listed pawn and every listed pawn must belong to the run"
  )

  private[chessjudgment] def stableKey: String =
    s"${fileKeys.mkString}:${pawnKeys.mkString(",")}" 

object RelationPawnOccupiedFileRunWitness:
  private[chessjudgment] def canonicalPartition(values: List[RelationPawnOccupiedFileRunWitness]): Boolean =
    val ordered = values.sortBy(_.files.head.key.toLowerCase)
    val allFiles = values.flatMap(_.files.map(_.key.toLowerCase))
    val allPawns = values.flatMap(_.pawns.map(_.key.toLowerCase))
    values == ordered && allFiles.distinct.size == allFiles.size &&
      allPawns.distinct.size == allPawns.size &&
      values.sliding(2).forall {
        case List(first, second) =>
          second.files.head.key.toLowerCase.head.toInt > first.files.last.key.toLowerCase.head.toInt + 1
        case _ => true
      }

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

enum RelationPawnFileOpenness:
  case OwnPawnPresent
  case SemiOpen
  case Open

  def ownPawnAbsent: Boolean = this != RelationPawnFileOpenness.OwnPawnPresent

  private[chessjudgment] def stableKey: String = toString.toLowerCase

final case class RelationPawnFileStateWitness(
    file: EvidenceFile,
    whitePawns: List[EvidenceSquare],
    blackPawns: List[EvidenceSquare]
):
  private def canonical(pawns: List[EvidenceSquare]): Boolean =
    pawns.distinct.size == pawns.size && pawns == pawns.sortBy(_.key) &&
      pawns.forall(EvidenceFile.contains(file, _))

  require(canonical(whitePawns) && canonical(blackPawns), "a pawn-file state needs canonical pawns on its file")

  def opennessFor(side: _root_.chess.Color): RelationPawnFileOpenness =
    val own = if side.white then whitePawns else blackPawns
    val opposing = if side.white then blackPawns else whitePawns
    if own.nonEmpty then RelationPawnFileOpenness.OwnPawnPresent
    else if opposing.nonEmpty then RelationPawnFileOpenness.SemiOpen
    else RelationPawnFileOpenness.Open

  private[chessjudgment] def stableKey: String =
    s"${file.key.toLowerCase}:white:${whitePawns.map(_.key.toLowerCase).mkString(",")}:black:${blackPawns.map(_.key.toLowerCase).mkString(",")}"

final case class RelationFileRayReachWitness(
    direction: RelationRayDirection,
    reach: Option[RelationSliderReachWitness]
):
  require(direction.axis == RelationAxisSignal.File, "a file corridor can contain only file rays")
  private[chessjudgment] def stableKey: String =
    s"${direction.stableKey}:${reach.map(_.stableKey).getOrElse("edge")}"

final case class RelationFileCorridorWitness(rays: List[RelationFileRayReachWitness]):
  private val expectedDirections = Set(RelationRayDirection(0, -1), RelationRayDirection(0, 1))
  require(
    rays.size == expectedDirections.size && rays.map(_.direction).toSet == expectedDirections &&
      rays == rays.sortBy(_.stableKey) && rays.exists(_.reach.nonEmpty),
    "a file corridor must close both file directions and retain one reachable segment"
  )

  private[chessjudgment] def stableKey: String = rays.map(_.stableKey).mkString("[", ",", "]")

enum RelationSliderOccurrenceChange:
  case PieceTransition(movement: RelationMoveTransitionWitness)
  case Captured(piece: RelationColoredPieceWitness)

  private[chessjudgment] def stableKey: String =
    this match
      case PieceTransition(movement) => s"piece_transition:${movement.stableKey}"
      case Captured(piece) =>
        s"captured:${piece.side.toString.toLowerCase}:${piece.role.name.toLowerCase}@${piece.square.key.toLowerCase}"

final case class RelationSharedGeometricSupportTargetWitness(
    target: RelationPieceWitness,
    enemyControllers: List[RelationPieceWitness],
    friendlySupporters: List[RelationPieceWitness]
):
  require(enemyControllers.nonEmpty, "a shared-support target needs one opposing geometric controller")
  require(friendlySupporters.nonEmpty, "a shared-support target needs one friendly geometric supporter")
  require(
    enemyControllers.distinct.size == enemyControllers.size &&
      enemyControllers == enemyControllers.sortBy(value => value.square.key -> value.role.name),
    "opposing geometric controllers must be unique and canonically ordered"
  )
  require(
    friendlySupporters.distinct.size == friendlySupporters.size &&
      friendlySupporters == friendlySupporters.sortBy(value => value.square.key -> value.role.name),
    "friendly geometric supporters must be unique and canonically ordered"
  )

  private[chessjudgment] def stableKey: String =
    val enemyControlKey = enemyControllers.map(value => s"${value.role.name.toLowerCase}@${value.square.key.toLowerCase}").mkString(",")
    val friendlySupportKey = friendlySupporters.map(value => s"${value.role.name.toLowerCase}@${value.square.key.toLowerCase}").mkString(",")
    s"${target.role.name.toLowerCase}@${target.square.key.toLowerCase}:enemy_controllers:$enemyControlKey:friendly_supporters:$friendlySupportKey"

private[chessjudgment] enum VerticalRelationPremiseRole:
  case RootMove
  case RootCapture
  case SliderOccurrenceChange(change: RelationSliderOccurrenceChange)
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
  case KingDestinationControl(
      checkedSide: _root_.chess.Color,
      resource: RelationMovementResourceWitness,
      controller: RelationPieceWitness,
      opponentTarget: RelationControlTarget
  )
  case LegalRestrictionTrigger(
      pieceSide: _root_.chess.Color,
      piece: RelationPieceWitness,
      resource: RelationMovementResourceWitness,
      semanticId: String
  )
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
  case PinEstablishmentTrigger(
      restrictedSide: _root_.chess.Color,
      pinner: RelationPieceWitness,
      pinned: RelationPieceWitness,
      kingSquare: EvidenceSquare,
      axis: RelationAxisSignal,
      semanticId: String
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
  case EnemyGeometricControlChange(
      controllingSide: _root_.chess.Color,
      target: RelationPieceWitness
  )
  case NewEnemyControl(
      controllingSide: _root_.chess.Color,
      target: RelationPieceWitness,
      controllerBefore: RelationPieceWitness,
      controllerAfter: RelationPieceWitness
  )
  case AfterEnemyControl(
      controllingSide: _root_.chess.Color,
      target: RelationPieceWitness,
      controller: RelationPieceWitness
  )
  case BeforeEnemyControl(
      controllingSide: _root_.chess.Color,
      target: RelationPieceWitness,
      controller: RelationPieceWitness
  )
  case ContinuedEnemyControlChange(
      controllingSide: _root_.chess.Color,
      target: RelationPieceWitness,
      beforeController: RelationPieceWitness,
      afterController: RelationPieceWitness
  )
  case FriendlyGeometricSupport(
      supportedSide: _root_.chess.Color,
      target: RelationPieceWitness,
      supporter: RelationPieceWitness
  )
  case EnemyGeometricControl(
      controllingSide: _root_.chess.Color,
      target: RelationPieceWitness,
      controller: RelationPieceWitness
  )
  case SupportSetDelta(
      supportedSide: _root_.chess.Color,
      supportedBefore: RelationPieceWitness,
      supportedAfter: RelationPieceWitness,
      beforeSupporters: List[RelationPieceWitness],
      afterSupporters: List[RelationPieceWitness],
      removedSupporters: List[RelationPieceWitness],
      establishedSupporters: List[RelationPieceWitness]
  )
  case SupportRemovalCause(
      supported: RelationPieceWitness,
      supporter: RelationPieceWitness,
      cause: RelationSupportChangeCause
  )
  case SupportEstablishmentCause(
      supported: RelationPieceWitness,
      supporter: RelationPieceWitness,
      cause: RelationSupportChangeCause
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
  case FilePawnGroup(
      occurrence: RelationSnapshotOccurrence,
      side: _root_.chess.Color,
      file: EvidenceFile,
      pawns: List[EvidenceSquare]
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
      firstOccupant: RelationColoredPieceWitness
  )

private[chessjudgment] object VerticalRelationPremiseRole:
  def id(role: VerticalRelationPremiseRole): String =
    role match
      case VerticalRelationPremiseRole.RootMove => "root_move"
      case VerticalRelationPremiseRole.RootCapture => "root_capture"
      case VerticalRelationPremiseRole.SliderOccurrenceChange(change) =>
        s"slider_occurrence_change:${change.stableKey}"
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
      case VerticalRelationPremiseRole.KingDestinationControl(
            checkedSide,
            resource,
            controller,
            opponentTarget
          ) =>
        val control = RelationControlReachWitness(resource.destination, opponentTarget)
        s"king_destination_control:${checkedSide.toString.toLowerCase}:${resource.stableKey}:${controller.role.name.toLowerCase}@${controller.square.key.toLowerCase}:${control.stableKey}"
      case VerticalRelationPremiseRole.LegalRestrictionTrigger(pieceSide, piece, resource, semanticId) =>
        s"legal_restriction_trigger:${pieceSide.toString.toLowerCase}:${piece.square.key.toLowerCase}:${piece.role.name.toLowerCase}:${resource.stableKey}:$semanticId"
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
      case VerticalRelationPremiseRole.PinEstablishmentTrigger(
            restrictedSide,
            pinner,
            pinned,
            kingSquare,
            axis,
            semanticId
          ) =>
        s"pin_establishment_trigger:${restrictedSide.toString.toLowerCase}:${pinner.square.key.toLowerCase}:${pinner.role.name.toLowerCase}:${pinned.square.key.toLowerCase}:${pinned.role.name.toLowerCase}:${kingSquare.key.toLowerCase}:${axis.toString.toLowerCase}:$semanticId"
      case VerticalRelationPremiseRole.MovementResource(pieceSide, piece, resource) =>
        s"movement_resource:${pieceSide.toString.toLowerCase}:${piece.square.key.toLowerCase}:${piece.role.name.toLowerCase}:${resource.stableKey}"
      case VerticalRelationPremiseRole.LegalMoveResource(occurrence, resource) =>
        s"legal_move_resource:${occurrence.toString.toLowerCase}:${resource.stableKey}"
      case VerticalRelationPremiseRole.EnemyGeometricControlChange(controllingSide, target) =>
        s"enemy_geometric_control_change:${controllingSide.toString.toLowerCase}:${target.square.key.toLowerCase}:${target.role.name.toLowerCase}"
      case VerticalRelationPremiseRole.NewEnemyControl(controllingSide, target, controllerBefore, controllerAfter) =>
        s"new_enemy_control:${controllingSide.toString.toLowerCase}:${controllerBefore.square.key.toLowerCase}:${controllerBefore.role.name.toLowerCase}:${controllerAfter.square.key.toLowerCase}:${controllerAfter.role.name.toLowerCase}:${target.square.key.toLowerCase}:${target.role.name.toLowerCase}"
      case VerticalRelationPremiseRole.AfterEnemyControl(controllingSide, target, controller) =>
        s"after_enemy_control:${controllingSide.toString.toLowerCase}:${controller.square.key.toLowerCase}:${controller.role.name.toLowerCase}:${target.square.key.toLowerCase}:${target.role.name.toLowerCase}"
      case VerticalRelationPremiseRole.BeforeEnemyControl(controllingSide, target, controller) =>
        s"before_enemy_control:${controllingSide.toString.toLowerCase}:${controller.square.key.toLowerCase}:${controller.role.name.toLowerCase}:${target.square.key.toLowerCase}:${target.role.name.toLowerCase}"
      case VerticalRelationPremiseRole.ContinuedEnemyControlChange(controllingSide, target, beforeController, afterController) =>
        s"continued_enemy_control_change:${controllingSide.toString.toLowerCase}:${beforeController.square.key.toLowerCase}:${beforeController.role.name.toLowerCase}:${afterController.square.key.toLowerCase}:${afterController.role.name.toLowerCase}:${target.square.key.toLowerCase}:${target.role.name.toLowerCase}"
      case VerticalRelationPremiseRole.FriendlyGeometricSupport(supportedSide, target, supporter) =>
        s"friendly_geometric_support:${supportedSide.toString.toLowerCase}:${target.square.key.toLowerCase}:${target.role.name.toLowerCase}:${supporter.square.key.toLowerCase}:${supporter.role.name.toLowerCase}"
      case VerticalRelationPremiseRole.EnemyGeometricControl(controllingSide, target, controller) =>
        s"enemy_geometric_control:${controllingSide.toString.toLowerCase}:${target.square.key.toLowerCase}:${target.role.name.toLowerCase}:${controller.square.key.toLowerCase}:${controller.role.name.toLowerCase}"
      case VerticalRelationPremiseRole.SupportSetDelta(
            supportedSide,
            supportedBefore,
            supportedAfter,
            beforeSupporters,
            afterSupporters,
            removedSupporters,
            establishedSupporters
          ) =>
        def pieces(values: List[RelationPieceWitness]): String =
          values.map(value => s"${value.square.key.toLowerCase}:${value.role.name.toLowerCase}").mkString("[", ",", "]")
        s"support_set_delta:${supportedSide.toString.toLowerCase}:${supportedBefore.square.key.toLowerCase}:${supportedBefore.role.name.toLowerCase}:${supportedAfter.square.key.toLowerCase}:${supportedAfter.role.name.toLowerCase}:${pieces(beforeSupporters)}:${pieces(afterSupporters)}:${pieces(removedSupporters)}:${pieces(establishedSupporters)}"
      case VerticalRelationPremiseRole.SupportRemovalCause(supported, supporter, cause) =>
        s"support_removal_cause:${supported.square.key.toLowerCase}:${supported.role.name.toLowerCase}:${supporter.square.key.toLowerCase}:${supporter.role.name.toLowerCase}:${cause.stableKey}"
      case VerticalRelationPremiseRole.SupportEstablishmentCause(supported, supporter, cause) =>
        s"support_establishment_cause:${supported.square.key.toLowerCase}:${supported.role.name.toLowerCase}:${supporter.square.key.toLowerCase}:${supporter.role.name.toLowerCase}:${cause.stableKey}"
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
      case VerticalRelationPremiseRole.FilePawnGroup(occurrence, side, file, pawns) =>
        s"file_pawn_group:${occurrence.toString.toLowerCase}:${side.toString.toLowerCase}:${file.key.toLowerCase}:${pawns.map(_.key.toLowerCase).mkString("[", ",", "]")}"
      case VerticalRelationPremiseRole.SliderReachControl(occurrence, side, slider, direction, control) =>
        s"slider_reach_control:${occurrence.toString.toLowerCase}:${side.toString.toLowerCase}:${slider.role.name.toLowerCase}@${slider.square.key.toLowerCase}:${direction.stableKey}:${control.stableKey}"
      case VerticalRelationPremiseRole.SliderReachBarrier(occurrence, side, slider, direction, occupant) =>
        s"slider_reach_barrier:${occurrence.toString.toLowerCase}:${side.toString.toLowerCase}:${slider.role.name.toLowerCase}@${slider.square.key.toLowerCase}:${direction.stableKey}:${occupant.side.toString.toLowerCase}:${occupant.role.name.toLowerCase}@${occupant.square.key.toLowerCase}"

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
              targetSquare,
              RelationControlTarget.Enemy(targetRole)
            ) =>
          side == controllingSide && attacker == controller.square && attackerRole == controller.role &&
            targetSquare == target.square && targetRole == target.role
        case _ => false
    def friendlySupport(
        supporter: RelationPieceWitness,
        supported: RelationPieceWitness
    ): Boolean =
      relation.detail match
        case RelationWitnessDetail.GeometricControl(
              _,
              attacker,
              attackerRole,
              targetSquare,
              RelationControlTarget.Friendly(targetRole)
            ) =>
          attacker == supporter.square && attackerRole == supporter.role &&
            targetSquare == supported.square && targetRole == supported.role
        case _ => false
    def controllerPredecessor(
        controllingSide: _root_.chess.Color,
        controllerAfter: RelationPieceWitness
    ): Option[RelationPieceWitness] =
      val transitions = relation.rootTransitionFootprint.toList.flatMap(_.pieceTransitions).filter(transition =>
        transition.side == controllingSide &&
          transition.to.key.equalsIgnoreCase(controllerAfter.square.key) &&
          transition.afterRole.name.equalsIgnoreCase(controllerAfter.role.name)
      )
      if transitions.size > 1 then None
      else
        transitions.headOption
          .map(transition =>
            RelationPieceWitness(
              EvidenceSquare(transition.from.key),
              EvidencePieceRole(transition.beforeRole.name)
            )
          )
          .orElse(Some(controllerAfter))
    def direction(from: EvidenceSquare, to: EvidenceSquare): Option[RelationRayDirection] =
      for
        origin <- Square.fromKey(from.key)
        target <- Square.fromKey(to.key)
      yield RelationRayDirection(
        Integer.signum(target.file.value - origin.file.value),
        Integer.signum(target.rank.value - origin.rank.value)
      )
    role match
      case RootMove =>
        rootTransition && (relation.detail match
          case _: RelationWitnessDetail.LegalMove => true
          case _                                  => false)
      case RootCapture =>
        rootTransition && (relation.detail match
          case RelationWitnessDetail.LegalMove(_, _, _, _, _, Some(_)) => true
          case _                                                        => false)
      case SliderOccurrenceChange(change) =>
        rootTransition && (change match
          case RelationSliderOccurrenceChange.PieceTransition(movement) =>
            exactRootPieceTransition(movement)
          case RelationSliderOccurrenceChange.Captured(piece) =>
            relation.detail match
              case RelationWitnessDetail.LegalMove(_, _, _, _, _, Some(capture)) =>
                capture.capturedSide == piece.side && capture.capturedSquare == piece.square &&
                  capture.capturedRole == piece.role
              case _ => false)
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
      case KingDestinationControl(checkedSide, resource, controller, opponentTarget) =>
        positionAt(RelationPremiseOccurrence.After) && (relation.detail match
          case RelationWitnessDetail.GeometricControl(
                controllingSide,
                attacker,
                attackerRole,
                target,
                exactTarget
              ) =>
            controllingSide == !checkedSide && attacker == controller.square &&
              attackerRole == controller.role && target == resource.destination && exactTarget == opponentTarget
          case _ => false)
      case LegalRestrictionTrigger(pieceSide, piece, resource, semanticId) =>
        semanticId == relation.semanticId && (relation.detail match
          case RelationWitnessDetail.GeometricControlSetDelta(
                _,
                controllingSide,
                target,
                _,
                afterTarget,
                _,
                afterControllers,
                _,
                establishedControllers,
                _
              ) =>
            val ownResourceChanged = controllingSide == pieceSide && afterTarget == resource.target &&
              (afterControllers.contains(piece) || establishedControllers.contains(piece))
            val opposingKingControlChanged = controllingSide != pieceSide &&
              piece.role.name.equalsIgnoreCase(King.name) && establishedControllers.nonEmpty
            derived && target == resource.destination &&
              resource.mode == RelationMovementResourceMode.ControlledDestination &&
              (ownResourceChanged || opposingKingControlChanged)
          case RelationWitnessDetail.PawnAdvanceAffordance(side, pawn, target, traversed) =>
            val mode =
              if traversed.size == 2 then RelationMovementResourceMode.PawnDoubleAdvance
              else RelationMovementResourceMode.PawnAdvance
            positionAt(RelationPremiseOccurrence.Established) && side == pieceSide &&
              pawn == piece.square && piece.role.name.equalsIgnoreCase(Pawn.name) &&
              target == resource.destination && resource.target == RelationControlTarget.Empty &&
              resource.mode == mode
          case _ => false)
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
          case ray @ RelationWitnessDetail.RayBarrier(owner, attacker, attackerRole, occupants, exactAxis) =>
            RelationRayProjection.pattern(ray) == RelationRayPattern.AbsoluteKingPin &&
              owner == !restrictedSide && attacker == pinner.square && attackerRole == pinner.role &&
              exactAxis == axis &&
              occupants.headOption.exists(piece =>
                piece.side == restrictedSide && piece.square == pinned.square && piece.role == pinned.role
              ) && occupants.lift(1).exists(piece =>
                piece.side == restrictedSide && piece.square == kingSquare &&
                  piece.role.name.equalsIgnoreCase(King.name)
              )
          case _ => false)
      case PinEstablishmentTrigger(restrictedSide, pinner, pinned, kingSquare, axis, semanticId) =>
        semanticId == relation.semanticId && (relation.detail match
          case RelationWitnessDetail.NamedRayTransition(
                _, owner, attacker, attackerRole, barrier, immediateTarget, exactAxis,
                RelationRayPattern.AbsoluteKingPin,
                RelationChangeDirection.Established, _
              ) =>
            derived && owner == !restrictedSide && attacker == pinner.square &&
              attackerRole == pinner.role && barrier.side == restrictedSide &&
              barrier.square == pinned.square && barrier.role == pinned.role && exactAxis == axis &&
              immediateTarget.exists(target =>
                target.side == restrictedSide && target.square == kingSquare &&
                  target.role.name.equalsIgnoreCase(King.name)
              )
          case _ => false)
      case MovementResource(pieceSide, piece, resource) =>
        positionAt(RelationPremiseOccurrence.After) && (relation.detail match
          case RelationWitnessDetail.GeometricControl(side, attacker, attackerRole, target, controlTarget) =>
            val exactMode = resource.mode match
              case RelationMovementResourceMode.ControlledDestination =>
                controlTarget match
                  case RelationControlTarget.Friendly(_) => false
                  case RelationControlTarget.Empty => !piece.role.name.equalsIgnoreCase(Pawn.name)
                  case RelationControlTarget.Enemy(_) => true
              case RelationMovementResourceMode.EnPassantCapture(capturedPawn) =>
                val exactCapturedSquare = for
                  origin <- Square.fromKey(piece.square.key)
                  destination <- Square.fromKey(resource.destination.key)
                yield EvidenceSquare(Square(destination.file, origin.rank).key)
                piece.role.name.equalsIgnoreCase(Pawn.name) && controlTarget == RelationControlTarget.Empty &&
                  capturedPawn.side == !pieceSide && capturedPawn.role.name.equalsIgnoreCase(Pawn.name) &&
                  exactCapturedSquare.contains(capturedPawn.square)
              case RelationMovementResourceMode.PawnAdvance |
                  RelationMovementResourceMode.PawnDoubleAdvance => false
            side == pieceSide && attacker == piece.square && attackerRole == piece.role &&
              target == resource.destination && controlTarget == resource.target && exactMode
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
      case EnemyGeometricControlChange(controllingSide, target) =>
        derived && (relation.detail match
          case RelationWitnessDetail.GeometricControlSetDelta(
                _, exactSide, square, _, RelationControlTarget.Enemy(role), _, afterControllers, _,
                establishedControllers, _
              ) =>
            exactSide == controllingSide && square == target.square && role == target.role &&
              afterControllers.nonEmpty && establishedControllers.nonEmpty
          case _ => false)
      case NewEnemyControl(controllingSide, target, controllerBefore, controllerAfter) =>
        derived && (relation.detail match
          case RelationWitnessDetail.GeometricControlSetDelta(
                _, exactSide, square, _, RelationControlTarget.Enemy(role), _, _, _, establishedControllers, _
              ) =>
            exactSide == controllingSide && square == target.square && role == target.role &&
              establishedControllers.contains(controllerAfter) &&
              controllerPredecessor(controllingSide, controllerAfter).contains(controllerBefore)
          case _ => false)
      case AfterEnemyControl(controllingSide, target, controller) =>
        positionAt(RelationPremiseOccurrence.After, RelationPremiseOccurrence.Established) &&
          enemyControl(controllingSide, controller, target)
      case BeforeEnemyControl(controllingSide, target, controller) =>
        positionAt(RelationPremiseOccurrence.Before, RelationPremiseOccurrence.Removed) &&
          enemyControl(controllingSide, controller, target)
      case ContinuedEnemyControlChange(controllingSide, target, beforeController, afterController) =>
        derived && (relation.detail match
          case RelationWitnessDetail.GeometricControlSetDelta(
                _, exactSide, square, _, RelationControlTarget.Enemy(role), beforeControllers,
                afterControllers, _, _, _
              ) =>
            exactSide == controllingSide && square == target.square && role == target.role &&
              beforeControllers.contains(beforeController) &&
              afterControllers.contains(afterController)
          case _ => false)
      case FriendlyGeometricSupport(supportedSide, target, supporter) =>
        positionAt(RelationPremiseOccurrence.After, RelationPremiseOccurrence.Established) &&
          friendlySupport(supporter, target) && (relation.detail match
            case RelationWitnessDetail.GeometricControl(side, _, _, _, _) => side == supportedSide
            case _ => false)
      case EnemyGeometricControl(controllingSide, target, controller) =>
        positionAt(RelationPremiseOccurrence.After, RelationPremiseOccurrence.Established) &&
          enemyControl(controllingSide, controller, target)
      case SupportSetDelta(
            supportedSide,
            supportedBefore,
            supportedAfter,
            beforeSupporters,
            afterSupporters,
            removedSupporters,
            establishedSupporters
          ) =>
        derived && (relation.detail match
          case RelationWitnessDetail.GeometricSupportDelta(
                _, exactSide, beforeSquare, beforeRole, afterSquare, afterRole,
                exactBefore, exactAfter, exactRemoved, exactEstablished, _
              ) =>
            exactSide == supportedSide && beforeSquare == supportedBefore.square &&
              beforeRole == supportedBefore.role && afterSquare == supportedAfter.square &&
              afterRole == supportedAfter.role && exactBefore == beforeSupporters &&
              exactAfter == afterSupporters && exactRemoved == removedSupporters &&
              exactEstablished == establishedSupporters
          case _ => false)
      case SupportRemovalCause(supported, supporter, cause) =>
        cause match
          case RelationSupportChangeCause.SupporterCaptured =>
            derived && (relation.detail match
              case RelationWitnessDetail.GeometricSupporterCapture(
                    _, supporterSquare, supporterRole, supportedSquare, supportedRole, _
                  ) =>
                supporterSquare == supporter.square && supporterRole == supporter.role &&
                  supportedSquare == supported.square && supportedRole == supported.role
              case _ => false)
          case RelationSupportChangeCause.SliderLineInterrupted =>
            derived && (relation.detail match
              case RelationWitnessDetail.SliderLineInterruption(
                    _, _, controllerBefore, _, controllerRole, targets, _
                  ) =>
                controllerBefore == supporter.square && controllerRole == supporter.role &&
                  targets.exists {
                    case RelationControlReachWitness(square, RelationControlTarget.Friendly(role)) =>
                      square == supported.square && role == supported.role
                    case _ => false
                  }
              case _ => false)
          case RelationSupportChangeCause.SupporterRelocated(movement) =>
            rootTransition && legalMovement(movement) && movement.from == supporter.square &&
              movement.beforeRole == supporter.role
          case RelationSupportChangeCause.SupportedPieceRelocated(movement) =>
            rootTransition && legalMovement(movement) && movement.from == supported.square &&
              movement.beforeRole == supported.role
          case RelationSupportChangeCause.SliderLineOpened => false
      case SupportEstablishmentCause(supported, supporter, cause) =>
        cause match
          case RelationSupportChangeCause.SliderLineOpened =>
            derived && (relation.detail match
              case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(
                    _, _, _, controllerAfter, controllerRole, _, _, targets, _, _, _
                  ) =>
                controllerAfter == supporter.square && controllerRole == supporter.role &&
                  targets.exists {
                    case RelationControlReachWitness(square, RelationControlTarget.Friendly(role)) =>
                      square == supported.square && role == supported.role
                    case _ => false
                  }
              case _ => false)
          case RelationSupportChangeCause.SupporterRelocated(movement) =>
            rootTransition && legalMovement(movement) && movement.to == supporter.square &&
              movement.afterRole == supporter.role
          case RelationSupportChangeCause.SupportedPieceRelocated(movement) =>
            rootTransition && legalMovement(movement) && movement.to == supported.square &&
              movement.afterRole == supported.role
          case RelationSupportChangeCause.SupporterCaptured |
              RelationSupportChangeCause.SliderLineInterrupted => false
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
                target,
                RelationControlTarget.Friendly(targetRole)
              ) =>
            owner == side && controllerRole.name.equalsIgnoreCase(Pawn.name) &&
              targetRole.name.equalsIgnoreCase(Pawn.name) && componentSet(controller) && componentSet(target)
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
                  target,
                  RelationControlTarget.Friendly(targetRole)
                ) =>
              owner == side && controllerRole.name.equalsIgnoreCase("pawn") &&
                targetRole.name.equalsIgnoreCase("pawn") &&
                (controller == pawn || target == pawn)
            case _ => false
        def exactEnemyPawnContact(detail: RelationWitnessDetail): Boolean =
          detail match
            case RelationWitnessDetail.GeometricControl(
                  chess.Color.White,
                  whitePawn,
                  whiteRole,
                  blackPawn,
                  RelationControlTarget.Enemy(blackRole)
                ) =>
              whiteRole.name.equalsIgnoreCase(Pawn.name) && blackRole.name.equalsIgnoreCase(Pawn.name) &&
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
            exactEnemyPawnContact(relation.detail)
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
                      target,
                      RelationControlTarget.Friendly(targetRole)
                    ) =>
                  owner == side && controllerRole.name.equalsIgnoreCase("pawn") &&
                    targetRole.name.equalsIgnoreCase("pawn") &&
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
      case FilePawnGroup(occurrence, side, file, pawns) =>
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
        exactOccurrence && (relation.detail match
          case RelationWitnessDetail.PawnFileGroup(owner, exactFile, exactPawns) =>
            owner == side && exactFile.key.equalsIgnoreCase(file.key) && exactPawns == pawns
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
                target,
                targetState
              ) =>
            exactSide == side && attacker == slider.square && attackerRole == slider.role &&
              target == control.square && targetState == control.target &&
              direction(attacker, target).contains(exactDirection)
          case _ => false)
      case SliderReachBarrier(occurrence, side, slider, exactDirection, firstOccupant) =>
        val exactOccurrence = occurrence match
          case RelationSnapshotOccurrence.Before =>
            positionAt(RelationPremiseOccurrence.Before, RelationPremiseOccurrence.Removed)
          case RelationSnapshotOccurrence.After =>
            positionAt(RelationPremiseOccurrence.After, RelationPremiseOccurrence.Established)
        exactOccurrence && (relation.detail match
          case RelationWitnessDetail.RayBarrier(exactSide, attacker, attackerRole, occupants, axis) =>
            exactSide == side && attacker == slider.square && attackerRole == slider.role &&
              occupants.headOption.contains(firstOccupant) && axis == exactDirection.axis &&
              direction(attacker, firstOccupant.square).contains(exactDirection)
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
        !detail.isInstanceOf[RelationWitnessDetail.MovementAffordanceLegalRestriction] &&
        !detail.isInstanceOf[RelationWitnessDetail.MajorPiecePawnFileCorridorTransition] &&
        !detail.isInstanceOf[RelationWitnessDetail.PawnTopologyTransition] &&
        !detail.isInstanceOf[RelationWitnessDetail.PawnOccupiedFilePartitionTransition] &&
        !detail.isInstanceOf[RelationWitnessDetail.CastlingRightRemoved] &&
        !detail.isInstanceOf[RelationWitnessDetail.SliderReachDelta] &&
        !detail.isInstanceOf[RelationWitnessDetail.AbsolutePinMovementRestriction]
    then false
    else
      detail match
        case RelationWitnessDetail.CaptureRecaptureInventory(
              mover,
              captured,
              geometricRecapturers,
              legalRecaptures,
              _
            ) =>
          val target = RelationPieceWitness(mover.to, mover.afterRole)
          val expectedRoles =
            VerticalRelationPremiseRole.RootCapture ::
              geometricRecapturers.map(recapturer =>
                VerticalRelationPremiseRole.GeometricRecaptureCandidate(captured.side, recapturer, target)
              ) ++ legalRecaptures.map(resource =>
                VerticalRelationPremiseRole.LegalRecapture(resource)
              )
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
            )
          exactRoles(expectedRoles) && exactAbsences(expectedAbsences)

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
                VerticalRelationPremiseRole.MovementResource(
                  checkedSide,
                  king,
                  destination.resource
                ) :: destination.controllers.map(controller =>
                  VerticalRelationPremiseRole.KingDestinationControl(
                    checkedSide,
                    destination.resource,
                    controller,
                    destination.opponentTarget
                  )
                )
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
          exactRoles(expectedRoles) && exactAbsences(expectedAbsences)

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

        case RelationWitnessDetail.MovementAffordanceLegalRestriction(
              _,
              restrictedSide,
              piece,
              movementResources,
              legalResources,
              unavailableResources,
              _
          ) =>
          val triggerRoles = obligations.map(_.role).collect {
            case role @ VerticalRelationPremiseRole.LegalRestrictionTrigger(_, _, _, _) => role
          }
          val unavailableSet = unavailableResources.toSet
          val exactRelationTriggers = triggerRoles.nonEmpty &&
            triggerRoles.forall {
              case VerticalRelationPremiseRole.LegalRestrictionTrigger(
                    exactSide,
                    exactPiece,
                    resource,
                    _
                  ) =>
                exactSide == restrictedSide && exactPiece == piece && unavailableSet(resource)
            } && unavailableResources.forall(resource =>
              triggerRoles.exists {
                case VerticalRelationPremiseRole.LegalRestrictionTrigger(
                      exactSide,
                      exactPiece,
                      exactResource,
                      _
                    ) =>
                  exactSide == restrictedSide && exactPiece == piece && exactResource == resource
              }
            )
          val fixedRoles =
            VerticalRelationPremiseRole.RootMove ::
              movementResources.map(resource =>
                VerticalRelationPremiseRole.MovementResource(restrictedSide, piece, resource)
              ) ++ legalResources.map(resource =>
                VerticalRelationPremiseRole.LegalMoveResource(RelationSnapshotOccurrence.After, resource)
              )
          val expectedAbsences = unavailableResources.map(resource =>
            ClosedRelationAbsencePremise(
              RelationSnapshotOccurrence.After,
              PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
                restrictedSide,
                piece.square,
                resource.destination
              )
            )
          )
          val enPassantUnavailable = unavailableResources.collect {
            case resource @ RelationMovementResourceWitness(
                  _,
                  RelationControlTarget.Empty,
                  RelationMovementResourceMode.EnPassantCapture(_)
                ) => resource
          }
          val exactTransitionTrigger =
            if enPassantUnavailable.isEmpty then exactRelationTriggers && exactStates(Nil)
            else if enPassantUnavailable.size == unavailableResources.size && triggerRoles.isEmpty then
              enPassantUnavailable match
                case RelationMovementResourceWitness(
                      destination,
                      _,
                      RelationMovementResourceMode.EnPassantCapture(capturedPawn)
                    ) :: Nil =>
                  states.collect {
                    case ClosedPositionStatePremise(
                          RelationSnapshotOccurrence.Before,
                          PositionRelationExtractor.ClosedPositionStateQuery.PotentialEnPassant(beforeSquare)
                        ) => beforeSquare
                  } match
                    case beforeSquare :: Nil =>
                      beforeSquare != Some(destination) && exactStates(List(
                        ClosedPositionStatePremise(
                          RelationSnapshotOccurrence.Before,
                          PositionRelationExtractor.ClosedPositionStateQuery.PotentialEnPassant(beforeSquare)
                        ),
                        ClosedPositionStatePremise(
                          RelationSnapshotOccurrence.After,
                          PositionRelationExtractor.ClosedPositionStateQuery.PotentialEnPassant(Some(destination))
                        ),
                        ClosedPositionStatePremise(
                          RelationSnapshotOccurrence.After,
                          PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(capturedPawn)
                        )
                      ))
                    case _ => false
                case _ => false
            else false
          exactTransitionTrigger && exactRoles(fixedRoles ++ triggerRoles) && exactAbsences(expectedAbsences)

        case RelationWitnessDetail.AbsolutePinMovementRestriction(
              _,
              restrictedSide,
              pinner,
              pinned,
              kingSquare,
              axis,
              movementResources,
              legalResources,
              newlyPinForbiddenResources,
              _
            ) =>
          val pinEstablishmentRoles = obligations.map(_.role).collect {
            case role @ VerticalRelationPremiseRole.PinEstablishmentTrigger(_, _, _, _, _, _) => role
          }
          val resourceTriggerRoles = obligations.map(_.role).collect {
            case role @ VerticalRelationPremiseRole.LegalRestrictionTrigger(_, _, _, _) => role
          }
          val previousPinRoles = obligations.map(_.role).collect {
            case role @ VerticalRelationPremiseRole.AbsolutePinBarrier(
                  RelationSnapshotOccurrence.Before,
                  _,
                  _,
                  _,
                  _,
                  _
                ) => role
          }
          val exactPreviousPins = previousPinRoles.size <= 1 && previousPinRoles.forall {
            case VerticalRelationPremiseRole.AbsolutePinBarrier(
                  _,
                  exactRestrictedSide,
                  _,
                  exactPinned,
                  exactKingSquare,
                  exactAxis
                ) =>
              exactRestrictedSide == restrictedSide && exactPinned == pinned &&
                exactKingSquare == kingSquare && exactAxis == axis
          }
          val fixedRoles =
            VerticalRelationPremiseRole.RootMove ::
              VerticalRelationPremiseRole.AbsolutePinBarrier(
                RelationSnapshotOccurrence.After,
                restrictedSide,
                pinner,
                pinned,
                kingSquare,
                axis
              ) ::
              previousPinRoles ++
              movementResources.map(resource =>
                VerticalRelationPremiseRole.MovementResource(restrictedSide, pinned, resource)
              ) ++ legalResources.map(resource =>
                VerticalRelationPremiseRole.LegalMoveResource(RelationSnapshotOccurrence.After, resource)
              )
          val expectedAbsences = newlyPinForbiddenResources.map(resource =>
            ClosedRelationAbsencePremise(
              RelationSnapshotOccurrence.After,
              PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
                restrictedSide,
                pinned.square,
                resource.destination
              )
            )
          )
          val movementResourceSet = movementResources.toSet
          val exactPinForbidden = newlyPinForbiddenResources.nonEmpty &&
            newlyPinForbiddenResources.forall(resource =>
            movementResourceSet(resource) && resource.destination != pinner.square &&
              !RelationRayProjection.liesStrictlyBetween(
                pinner.square,
                kingSquare,
                resource.destination
              )
          )
          val enPassantResources = newlyPinForbiddenResources.collect {
            case resource @ RelationMovementResourceWitness(
                  _,
                  _,
                  RelationMovementResourceMode.EnPassantCapture(_)
                ) => resource
          }
          val exactEnPassantStates =
            if enPassantResources.isEmpty then exactStates(Nil)
            else
              val beforePotential = states.collect {
                case state @ ClosedPositionStatePremise(
                      RelationSnapshotOccurrence.Before,
                      PositionRelationExtractor.ClosedPositionStateQuery.PotentialEnPassant(_)
                    ) => state
              }
              (enPassantResources, beforePotential) match
                case (
                      RelationMovementResourceWitness(
                        destination,
                        RelationControlTarget.Empty,
                        RelationMovementResourceMode.EnPassantCapture(capturedPawn)
                      ) :: Nil,
                      ClosedPositionStatePremise(
                        RelationSnapshotOccurrence.Before,
                        PositionRelationExtractor.ClosedPositionStateQuery.PotentialEnPassant(beforeSquare)
                      ) :: Nil
                    ) =>
                  val expectedStates = List(
                    ClosedPositionStatePremise(
                      RelationSnapshotOccurrence.Before,
                      PositionRelationExtractor.ClosedPositionStateQuery.PotentialEnPassant(beforeSquare)
                    ),
                    ClosedPositionStatePremise(
                      RelationSnapshotOccurrence.After,
                      PositionRelationExtractor.ClosedPositionStateQuery.PotentialEnPassant(Some(destination))
                    ),
                    ClosedPositionStatePremise(
                      RelationSnapshotOccurrence.After,
                      PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(capturedPawn)
                    )
                  )
                  beforeSquare != Some(destination) && exactStates(expectedStates)
                case _ => false
          val exactPinEstablishment = pinEstablishmentRoles match
            case VerticalRelationPremiseRole.PinEstablishmentTrigger(
                  exactRestrictedSide,
                  exactPinner,
                  exactPinned,
                  exactKingSquare,
                  exactAxis,
                  _
                ) :: Nil =>
              previousPinRoles.isEmpty && exactRestrictedSide == restrictedSide &&
                exactPinner == pinner && exactPinned == pinned && exactKingSquare == kingSquare &&
                exactAxis == axis
            case Nil => previousPinRoles.nonEmpty
            case _   => false
          val nonEnPassantResources = newlyPinForbiddenResources.filterNot(enPassantResources.contains)
          val exactResourceTriggers =
            if previousPinRoles.isEmpty then resourceTriggerRoles.isEmpty
            else
              pinEstablishmentRoles.isEmpty && resourceTriggerRoles.forall {
                case VerticalRelationPremiseRole.LegalRestrictionTrigger(
                      exactSide,
                      exactPiece,
                      resource,
                      _
                    ) =>
                  exactSide == restrictedSide && exactPiece == pinned &&
                    nonEnPassantResources.contains(resource)
              } && nonEnPassantResources.forall(resource =>
                resourceTriggerRoles.exists {
                  case VerticalRelationPremiseRole.LegalRestrictionTrigger(
                        exactSide,
                        exactPiece,
                        exactResource,
                        _
                      ) =>
                    exactSide == restrictedSide && exactPiece == pinned && exactResource == resource
                }
              )
          exactPinEstablishment && exactResourceTriggers && exactPreviousPins && exactPinForbidden &&
            exactEnPassantStates &&
            exactRoles(fixedRoles ++ pinEstablishmentRoles ++ resourceTriggerRoles) &&
            exactAbsences(expectedAbsences)

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

        case RelationWitnessDetail.GeometricMultiTargetContact(
              _,
              controllingSide,
              controllerBefore,
              controllerAfter,
              newlyEstablishedTargets,
              maintainedTargets,
              _
            ) =>
          val continuedRoles = obligations.map(_.role).collect {
            case role @ VerticalRelationPremiseRole.ContinuedEnemyControlChange(_, _, _, _) => role
          }
          val exactContinued = continuedRoles.forall {
            case VerticalRelationPremiseRole.ContinuedEnemyControlChange(
                  exactSide,
                  target,
                  exactBefore,
                  exactAfter
                ) =>
              exactSide == controllingSide && maintainedTargets.contains(target) &&
                exactBefore == controllerBefore && exactAfter == controllerAfter
          }
          val expectedRoles =
            newlyEstablishedTargets.map(target =>
              VerticalRelationPremiseRole.NewEnemyControl(
                controllingSide,
                target,
                controllerBefore,
                controllerAfter
              )
            ) ++ maintainedTargets.map(target =>
              VerticalRelationPremiseRole.BeforeEnemyControl(controllingSide, target, controllerBefore)
            ) ++ (newlyEstablishedTargets ++ maintainedTargets).map(target =>
              VerticalRelationPremiseRole.AfterEnemyControl(controllingSide, target, controllerAfter)
            ) ++ continuedRoles
          exactContinued && exactRoles(expectedRoles) && exactAbsences(Nil)

        case RelationWitnessDetail.GeometricEnemyContactWithoutFriendlySupport(
              _,
              controllingSide,
              controllerBefore,
              controllerAfter,
              target,
              _
            ) =>
          !target.role.name.equalsIgnoreCase(King.name) && exactRoles(List(
            VerticalRelationPremiseRole.NewEnemyControl(
              controllingSide,
              target,
              controllerBefore,
              controllerAfter
            ),
            VerticalRelationPremiseRole.AfterEnemyControl(controllingSide, target, controllerAfter)
          )) && exactAbsences(List(
            ClosedRelationAbsencePremise(
              RelationSnapshotOccurrence.After,
              PositionRelationExtractor.ClosedRelationAbsenceQuery.GeometricFriendlySupportOf(
                !controllingSide,
                target.square
              )
            )
          ))

        case RelationWitnessDetail.SharedGeometricSupportOfEnemyControlledTargets(
              _,
              controllingSide,
              supportedSide,
              sharedSupporter,
              targets,
              _
            ) =>
          val changeRoles = obligations.map(_.role).collect {
            case role @ VerticalRelationPremiseRole.EnemyGeometricControlChange(_, _) => role
          }
          val targetSet = targets.map(_.target).toSet
          val exactChanges = changeRoles.nonEmpty && changeRoles.forall {
            case VerticalRelationPremiseRole.EnemyGeometricControlChange(exactSide, target) =>
              exactSide == controllingSide && targetSet(target)
          }
          val positionRoles = targets.flatMap(target =>
            target.friendlySupporters.map(supporter =>
              VerticalRelationPremiseRole.FriendlyGeometricSupport(supportedSide, target.target, supporter)
            ) ++ target.enemyControllers.map(controller =>
              VerticalRelationPremiseRole.EnemyGeometricControl(controllingSide, target.target, controller)
            )
          )
          val exactSharedSupporter = targets.size >= 2 && targets.forall(_.friendlySupporters.contains(sharedSupporter))
          exactChanges && exactSharedSupporter && exactRoles(changeRoles ++ positionRoles) && exactAbsences(Nil)

        case RelationWitnessDetail.GeometricSupportCausalTransition(
              _,
              supportedSide,
              supportedBeforeSquare,
              supportedBeforeRole,
              supportedAfterSquare,
              supportedAfterRole,
              beforeSupporters,
              afterSupporters,
              removals,
              establishments,
              _
            ) =>
          val supportedBefore = RelationPieceWitness(supportedBeforeSquare, supportedBeforeRole)
          val supportedAfter = RelationPieceWitness(supportedAfterSquare, supportedAfterRole)
          val expectedRoles =
            VerticalRelationPremiseRole.SupportSetDelta(
              supportedSide,
              supportedBefore,
              supportedAfter,
              beforeSupporters,
              afterSupporters,
              removals.map(_.supporter),
              establishments.map(_.supporter)
            ) ::
              removals.flatMap(removal => removal.causes.map(cause =>
                VerticalRelationPremiseRole.SupportRemovalCause(supportedBefore, removal.supporter, cause)
              )) ++ establishments.flatMap(establishment => establishment.causes.map(cause =>
                VerticalRelationPremiseRole.SupportEstablishmentCause(
                  supportedAfter,
                  establishment.supporter,
                  cause
                )
              ))
          val expectedAbsences = Option.when(afterSupporters.isEmpty)(
            ClosedRelationAbsencePremise(
              RelationSnapshotOccurrence.After,
              PositionRelationExtractor.ClosedRelationAbsenceQuery.GeometricFriendlySupportOf(
                supportedSide,
                supportedAfterSquare
              )
            )
          ).toList
          exactRoles(expectedRoles) && exactAbsences(expectedAbsences)

        case RelationWitnessDetail.PawnTopologyTransition(_, before, after, changed, _) =>
          provesPawnTopologyFacets(before, after, changed)

        case RelationWitnessDetail.PawnOccupiedFilePartitionTransition(_, side, before, after, _) =>
          val files = File.all.map(file =>
            EvidenceFile((('a'.toInt + file.value).toChar).toString)
          )
          def pawnsOn(file: EvidenceFile, runs: List[RelationPawnOccupiedFileRunWitness]): List[EvidenceSquare] =
            runs.flatMap(_.pawns).filter(EvidenceFile.contains(file, _)).sortBy(_.key)
          val expectedRoles = VerticalRelationPremiseRole.RootMove ::
            List(
              RelationSnapshotOccurrence.Before -> before,
              RelationSnapshotOccurrence.After -> after
            ).flatMap { case (occurrence, runs) =>
              files.map(file => VerticalRelationPremiseRole.FilePawnGroup(occurrence, side, file, pawnsOn(file, runs)))
            }
          val expectedStates = List(
            ClosedPositionStatePremise(
              RelationSnapshotOccurrence.Before,
              PositionRelationExtractor.ClosedPositionStateQuery.PawnOccupiedFilePartition(side, before)
            ),
            ClosedPositionStatePremise(
              RelationSnapshotOccurrence.After,
              PositionRelationExtractor.ClosedPositionStateQuery.PawnOccupiedFilePartition(side, after)
            )
          )
          RelationPawnOccupiedFileRunWitness.canonicalPartition(before) &&
            RelationPawnOccupiedFileRunWitness.canonicalPartition(after) &&
            before.map(_.files) != after.map(_.files) &&
            exactRoles(expectedRoles) && exactAbsences(Nil) && exactStates(expectedStates)

        case RelationWitnessDetail.MajorPiecePawnFileCorridorTransition(
              _,
              side,
              file,
              sliderBefore,
              sliderAfter,
              occurrenceChange,
              beforePawnFile,
              afterPawnFile,
              beforeCorridor,
              afterCorridor,
              _
            ) =>
          def pawnRoles(
              occurrence: RelationSnapshotOccurrence,
              state: RelationPawnFileStateWitness
          ): List[VerticalRelationPremiseRole] =
            List(
              VerticalRelationPremiseRole.FilePawnGroup(
                occurrence,
                _root_.chess.Color.White,
                file,
                state.whitePawns
              ),
              VerticalRelationPremiseRole.FilePawnGroup(
                occurrence,
                _root_.chess.Color.Black,
                file,
                state.blackPawns
              )
            )
          def corridorRoles(
              occurrence: RelationSnapshotOccurrence,
              slider: Option[RelationPieceWitness],
              corridor: Option[RelationFileCorridorWitness]
          ): List[VerticalRelationPremiseRole] =
            corridor.toList.flatMap(_.rays.flatMap(ray =>
              VerticalSliderReachProofProjection.roles(
                occurrence,
                side,
                slider,
                ray.direction,
                ray.reach
              )
            ))
          def occurrenceStates(
              occurrence: RelationSnapshotOccurrence,
              slider: Option[RelationPieceWitness],
              corridor: Option[RelationFileCorridorWitness]
          ): List[ClosedPositionStatePremise] =
            slider.toList.flatMap(piece =>
              ClosedPositionStatePremise(
                occurrence,
                PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(
                  RelationColoredPieceWitness(piece.square, piece.role, side)
                )
              ) :: corridor.toList.flatMap(_.rays.map(ray =>
                ClosedPositionStatePremise(
                  occurrence,
                  PositionRelationExtractor.ClosedPositionStateQuery.SliderReach(
                    side,
                    piece,
                    ray.direction,
                    ray.reach
                  )
                )
              ))
            )
          val expectedRoles =
            VerticalRelationPremiseRole.RootMove :: occurrenceChange.map(
              VerticalRelationPremiseRole.SliderOccurrenceChange.apply
            ).toList ++ pawnRoles(RelationSnapshotOccurrence.Before, beforePawnFile) ++
              pawnRoles(RelationSnapshotOccurrence.After, afterPawnFile) ++
              corridorRoles(RelationSnapshotOccurrence.Before, sliderBefore, beforeCorridor) ++
              corridorRoles(RelationSnapshotOccurrence.After, sliderAfter, afterCorridor)
          val expectedStates =
            occurrenceStates(RelationSnapshotOccurrence.Before, sliderBefore, beforeCorridor) ++
              occurrenceStates(RelationSnapshotOccurrence.After, sliderAfter, afterCorridor)
          val beforeUsable = beforeCorridor.nonEmpty && beforePawnFile.opennessFor(side).ownPawnAbsent
          val afterUsable = afterCorridor.nonEmpty && afterPawnFile.opennessFor(side).ownPawnAbsent
          val changed = sliderBefore != sliderAfter || beforeCorridor != afterCorridor ||
            beforePawnFile.opennessFor(side) != afterPawnFile.opennessFor(side)
          beforePawnFile.file == file && afterPawnFile.file == file &&
            (beforeUsable || afterUsable) && changed &&
            exactRoles(expectedRoles) && exactAbsences(Nil) && exactStates(expectedStates)

        case RelationWitnessDetail.CastlingRightRemoved(_, side, flank, _) =>
          exactRoles(List(VerticalRelationPremiseRole.RootMove)) && exactAbsences(Nil) && exactStates(List(
            ClosedPositionStatePremise(
              RelationSnapshotOccurrence.Before,
              PositionRelationExtractor.ClosedPositionStateQuery.CastlingRight(side, flank.castleSide, true)
            ),
            ClosedPositionStatePremise(
              RelationSnapshotOccurrence.After,
              PositionRelationExtractor.ClosedPositionStateQuery.CastlingRight(side, flank.castleSide, false)
            )
          ))

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

/** The closed L1 ledger for one transition. Empty lists certify only that a
  * contract ran; they are not negative chess facts.
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
    VerticalRelationContractKind.values.toList
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

  def resultsFor(contract: VerticalRelationContractKind): List[RelationFactEvidence] = synchronized {
    rawByContract.getOrElseUpdate(
      contract,
      {
        val results =
          if input.affected(contract) then VerticalRelationContracts.derive(contract, input).sortBy(_.semanticId)
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
      destination: PositionRelationExtractor.ClosedControlledKingDestination
  ): RelationControlledKingDestinationWitness =
    RelationControlledKingDestinationWitness(
      movementResource(destination.movement),
      destination.opponentControls.head.target,
      destination.opponentControls.map(_.controller)
    )

  def pawnFileState(
      file: PositionRelationExtractor.ClosedPawnFile
  ): RelationPawnFileStateWitness =
    RelationPawnFileStateWitness(file.file, file.whitePawns, file.blackPawns)

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
        ) ++ exactReach.firstOccupant.map(occupant =>
          VerticalRelationPremiseRole.SliderReachBarrier(
            occurrence,
            side,
            exactSlider,
            direction,
            occupant
          )
        )
      case _ => Nil

private[chessjudgment] final class VerticalRelationInput private (
    delta: RelationSemanticDelta,
    combinations: ClosedRelationCombinationResults
):
  import VerticalRelationWitnessProjection.*

  private var derivedBySemanticId = Map.empty[String, RelationFactEvidence]

  private var absenceCertificatesByKey = Map.empty[
    String,
    Option[PositionRelationExtractor.ClosedRelationAbsenceCertificate]
  ]

  private var stateCertificatesByKey = Map.empty[
    String,
    Option[PositionRelationExtractor.ClosedPositionStateCertificate]
  ]

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

  private def controllerPredecessor(
      relation: RelationFactEvidence,
      controllingSide: _root_.chess.Color,
      controller: RelationPieceWitness
  ): RelationPieceWitness =
    val footprint = relation.rootTransitionFootprint.getOrElse(
      throw IllegalArgumentException("a control-set delta needs its admitted root transition footprint")
    )
    val transitions = footprint.pieceTransitions.filter(transition =>
      transition.side == controllingSide &&
        transition.to.key.equalsIgnoreCase(controller.square.key) &&
        transition.afterRole.name.equalsIgnoreCase(controller.role.name)
    )
    require(
      transitions.size <= 1,
      "one after-controller cannot have multiple predecessors in one legal transition"
    )
    transitions.headOption
      .map(transition =>
        RelationPieceWitness(
          EvidenceSquare(transition.from.key),
          EvidencePieceRole(transition.beforeRole.name)
        )
      )
      .getOrElse(controller)

  /** One cached projection of changed enemy controls. All L1 contact
    * contracts consume this owner; none reparses the control-set delta.
    */
  private lazy val enemyControlChangeInput: List[VerticalEnemyControlChange] =
    val changes = controlSetChanges.flatMap { change =>
      change.afterTarget match
        case RelationControlTarget.Enemy(targetRole) if change.controllingSide == change.mover.side =>
          val controllers = change.beforeTarget match
            case RelationControlTarget.Enemy(_) => change.establishedControllers
            case _                              => change.afterControllers
          val target = RelationPieceWitness(change.target, targetRole)
          controllers.map { controllerAfter =>
            val controllerBefore = controllerPredecessor(
              change.relation,
              change.controllingSide,
              controllerAfter
            )
            val continued = change.beforeTarget match
              case RelationControlTarget.Enemy(role) =>
                role == targetRole && change.beforeControllers.contains(controllerBefore)
              case _ => false
            VerticalEnemyControlChange(
              change.relation,
              change.mover,
              change.controllingSide,
              controllerBefore,
              controllerAfter,
              target,
              newlyEstablished = !continued
            )
          }
        case _ => Nil
    }.sortBy(change =>
      (
        change.controllingSide.toString,
        change.controllerAfter.square.key,
        change.target.square.key,
        change.relation.semanticId
      )
    )
    require(
      changes.map(change =>
        (change.controllingSide, change.controllerAfter, change.target, change.relation.semanticId)
      ).distinct.size == changes.size,
      "one changed controller-target relation cannot repeat in the vertical input"
    )
    changes

  private lazy val enemyContactInventoryInput: List[VerticalEnemyContactInventory] =
    val before = positionInventory(RelationSnapshotOccurrence.Before)
    val after = positionInventory(RelationSnapshotOccurrence.After)
    enemyControlChangeInput
      .groupBy(change => change.controllingSide -> change.controllerAfter)
      .toList
      .map { case ((controllingSide, controllerAfter), changes) =>
        val movers = changes.map(_.mover).distinct
        val predecessors = changes.map(_.controllerBefore).distinct
        require(movers.size == 1, "one controller inventory must retain one root mover")
        require(predecessors.size == 1, "one after-controller must retain one exact predecessor")
        val mover = movers.head
        val controllerBefore = predecessors.head
        val changesByTarget = changes.groupBy(_.target)
        require(
          changesByTarget.values.forall(_.size == 1),
          "one controller-target pair must retain one control-set delta"
        )
        val afterControls = after.controlsFrom(controllerAfter.square).filter(control =>
          control.side == controllingSide && control.controller == controllerAfter &&
            (control.target match
              case RelationControlTarget.Enemy(_) => true
              case _                              => false)
        )
        val afterByTarget = afterControls.groupBy(control =>
          control.target match
            case RelationControlTarget.Enemy(role) => RelationPieceWitness(control.targetSquare, role)
            case _ => throw IllegalArgumentException("enemy-contact inventory accepted a non-enemy target")
        )
        require(
          afterByTarget.values.forall(_.size == 1),
          "one controller-target pair must retain one closed geometric-control edge"
        )
        val targets = afterByTarget.toList.map { case (target, exactAfterControls) =>
          val beforeControls = before.controlsFrom(controllerBefore.square).filter(control =>
            control.side == controllingSide && control.controller == controllerBefore &&
              control.targetSquare == target.square && (control.target match
                case RelationControlTarget.Enemy(role) => role == target.role
                case _                                 => false)
          )
          require(
            beforeControls.size <= 1,
            "one predecessor-target pair cannot repeat a closed geometric-control edge"
          )
          val change = changesByTarget.get(target).flatMap(_.headOption)
          require(
            beforeControls.isEmpty == change.exists(_.newlyEstablished) ||
              beforeControls.nonEmpty && change.isEmpty,
            "changed enemy-control direction must agree with exact before/after contact"
          )
          VerticalEnemyContactTarget(target, beforeControls.headOption, exactAfterControls.head, change)
        }.sortBy(target => target.target.square.key -> target.target.role.name)
        VerticalEnemyContactInventory(mover, controllingSide, controllerBefore, controllerAfter, targets)
      }
      .sortBy(inventory =>
        (inventory.controllingSide.toString, inventory.controllerAfter.square.key, inventory.controllerAfter.role.name)
      )

  private[judgment] def enemyContactInventories: List[VerticalEnemyContactInventory] =
    enemyContactInventoryInput

  private lazy val multiTargetContactRelationInput: List[RelationFactEvidence] =
    VerticalRelationContracts.computeGeometricMultiTargetContacts(this)

  private[judgment] def multiTargetContactRelations: List[RelationFactEvidence] =
    multiTargetContactRelationInput

  private lazy val unsupportedEnemyContactInput: List[VerticalUnsupportedEnemyContact] =
    val after = positionInventory(RelationSnapshotOccurrence.After)
    enemyContactInventoryInput.flatMap { inventory =>
      inventory.newlyEstablished.flatMap { target =>
        Option.when(
          !target.target.role.name.equalsIgnoreCase(King.name) &&
            after.supportersOf(!inventory.controllingSide, target.target.square).isEmpty
        )(VerticalUnsupportedEnemyContact(inventory, target))
      }
    }.sortBy(value =>
      (
        value.inventory.controllingSide.toString,
        value.inventory.controllerAfter.square.key,
        value.target.target.square.key
      )
    )

  private[judgment] def unsupportedEnemyContacts: List[VerticalUnsupportedEnemyContact] =
    unsupportedEnemyContactInput

  private lazy val sharedSupportInventoryInput: List[VerticalSharedSupportInventory] =
    final case class SharedKey(
        mover: RelationMoveTransitionWitness,
        controllingSide: _root_.chess.Color,
        supportedSide: _root_.chess.Color,
        supporter: RelationPieceWitness
    )
    val after = positionInventory(RelationSnapshotOccurrence.After)
    val newlyEstablishedRelationIds = enemyControlChangeInput
      .filter(_.newlyEstablished)
      .map(_.relation.semanticId)
      .toSet
    val changedTargets = controlSetChanges
      .filter(change => newlyEstablishedRelationIds(change.relation.semanticId))
      .map { change =>
        val target = change.afterTarget match
          case RelationControlTarget.Enemy(role) => RelationPieceWitness(change.target, role)
          case _ =>
            throw IllegalArgumentException(
              "a newly established enemy-control relation lost its occupied target"
            )
        require(
          change.controllingSide == change.mover.side,
          "a shared-support target must be controlled by the admitted mover side"
        )
        (change.mover, change.controllingSide, target, change.relation)
      }
    require(
      changedTargets.map(_._4.semanticId).toSet == newlyEstablishedRelationIds,
      "every newly established enemy-control target must retain its single control-set owner"
    )
    val grouped = changedTargets.flatMap { case (mover, controllingSide, target, relation) =>
      val supportedSide = !controllingSide
      after.supportersOf(supportedSide, target.square).map(control =>
        SharedKey(mover, controllingSide, supportedSide, control.controller) -> (target -> relation)
      )
    }.groupMap(_._1)(_._2)
    grouped.toList.flatMap { case (key, changed) =>
      val changedByTarget = changed.groupBy(_._1)
      require(
        changedByTarget.values.forall(values => values.map(_._2.semanticId).distinct.size == 1),
        "one shared-support target must retain one changed control relation"
      )
      val targetInputs = after.controlsFrom(key.supporter.square).flatMap { support =>
        support.friendlySupport
          .filter(_ => support.side == key.supportedSide && support.controller == key.supporter)
          .toList
          .flatMap { target =>
            val enemyControls = after.controlsAt(key.controllingSide, target.square).filter(control =>
              control.target match
                case RelationControlTarget.Enemy(role) => role == target.role
                case _                                 => false
            )
            Option.when(enemyControls.nonEmpty) {
              val friendlySupports = after.supportersOf(key.supportedSide, target.square).filter(control =>
                control.target match
                  case RelationControlTarget.Friendly(role) => role == target.role
                  case _                                    => false
              )
              VerticalSharedSupportTarget(
                RelationSharedGeometricSupportTargetWitness(
                  target,
                  enemyControls.map(_.controller).sortBy(value => value.square.key -> value.role.name),
                  friendlySupports.map(_.controller).sortBy(value => value.square.key -> value.role.name)
                ),
                friendlySupports.sortBy(_.source.semanticId),
                enemyControls.sortBy(_.source.semanticId)
              )
            }.toList
          }
      }
      val uniqueTargets = targetInputs.groupBy(_.witness.target)
      require(
        uniqueTargets.values.forall(_.size == 1),
        "one shared supporter cannot repeat a controlled target"
      )
      val targets = uniqueTargets.values.map(_.head).toList.sortBy(_.witness.stableKey)
      val targetSet = targets.map(_.witness.target).toSet
      val exactChanges = changedByTarget.toList.collect {
        case (target, values) if targetSet(target) => target -> values.head._2
      }.sortBy(_._1.square.key)
      Option.when(targets.size >= 2 && exactChanges.nonEmpty)(
        VerticalSharedSupportInventory(
          key.mover,
          key.controllingSide,
          key.supportedSide,
          key.supporter,
          targets,
          exactChanges
        )
      )
    }.sortBy(value =>
      (value.controllingSide.toString, value.sharedSupporter.square.key, value.sharedSupporter.role.name)
    )

  private[judgment] def sharedSupportInventories: List[VerticalSharedSupportInventory] =
    sharedSupportInventoryInput

  private lazy val sharedSupportRelationInput: List[RelationFactEvidence] =
    VerticalRelationContracts.computeSharedGeometricSupportOfEnemyControlledTargets(this)

  private[judgment] def sharedSupportRelations: List[RelationFactEvidence] =
    sharedSupportRelationInput

  private lazy val supportCausalTransitionInput: List[RelationFactEvidence] =
    VerticalRelationContracts.computeSupportCausalTransitions(this)

  private[judgment] def supportCausalTransitions: List[RelationFactEvidence] =
    supportCausalTransitionInput

  private var absolutePinInventoryByPiece =
    Map.empty[RelationPieceWitness, List[VerticalAbsolutePinInventory]]

  private[judgment] def absolutePinInventoriesFor(
      pinned: RelationPieceWitness
  ): List[VerticalAbsolutePinInventory] =
    absolutePinInventoryByPiece.get(pinned) match
      case Some(cached) => cached
      case None =>
        val before = positionInventory(RelationSnapshotOccurrence.Before)
        val after = positionInventory(RelationSnapshotOccurrence.After)
        def resourceWitness(
            resource: PositionRelationExtractor.ClosedMovementAffordance
        ): RelationMovementResourceWitness =
          RelationMovementResourceWitness(resource.destination, resource.target, resource.mode)
        def forbiddenResources(
            inventory: PositionRelationExtractor.PositionRelationInventoryCertificate,
            ray: RelationWitnessDetail.RayBarrier,
            kingSquare: EvidenceSquare,
            exactPinned: RelationPieceWitness
        ): List[PositionRelationExtractor.ClosedMovementAffordance] =
          inventory.movementAffordancesFrom(exactPinned.square).filter(resource =>
            resource.destination != ray.attackerSquare &&
              !RelationRayProjection.liesStrictlyBetweenAttackerAnd(ray, kingSquare, resource.destination)
          )
        val built = after.rayBarriersWithFirstOccupant(pinned.square).flatMap { barrierSource =>
          barrierSource.detail match
            case ray @ RelationWitnessDetail.RayBarrier(owner, attacker, attackerRole, occupants, axis)
                if RelationRayProjection.pattern(ray) == RelationRayPattern.AbsoluteKingPin &&
                  owner == !after.sideToMove && occupants.headOption.exists(piece =>
                    piece.side == after.sideToMove && piece.square == pinned.square && piece.role == pinned.role
                  ) && occupants.lift(1).exists(piece =>
                    piece.side == after.sideToMove && piece.role.name.equalsIgnoreCase(King.name)
                  ) =>
              val king = occupants(1)
              val movement = after.movementAffordancesFrom(pinned.square)
              val destinations = movement.map(_.destination).toSet
              val legal = after.legalMovesFrom(pinned.square)
                .filter(move => destinations(move.destination))
                .sortBy(_.moveUci)
              val forbidden = forbiddenResources(after, ray, king.square, pinned)
              val forbiddenDestinations = forbidden.map(_.destination).toSet
              require(
                legal.forall(resource => !forbiddenDestinations(resource.destination)),
                "an absolute pin cannot retain a legal off-ray movement resource"
              )
              val pinnerBefore = delta.transitionFootprint.pieceTransitions.find(transition =>
                transition.side == owner && transition.to.key.equalsIgnoreCase(attacker.key) &&
                  transition.afterRole.name.equalsIgnoreCase(attackerRole.name)
              ).map(transition =>
                RelationPieceWitness(
                  EvidenceSquare(transition.from.key),
                  EvidencePieceRole(transition.beforeRole.name)
                )
              ).getOrElse(RelationPieceWitness(attacker, attackerRole))
              val previousBarriers = before.rayBarriersWithFirstOccupant(pinned.square).filter { source =>
                source.detail match
                  case previousRay @ RelationWitnessDetail.RayBarrier(
                        previousOwner,
                        previousAttacker,
                        previousRole,
                        previousOccupants,
                        previousAxis
                      ) =>
                    RelationRayProjection.pattern(previousRay) == RelationRayPattern.AbsoluteKingPin &&
                      previousOwner == owner && previousAttacker == pinnerBefore.square &&
                      previousRole == pinnerBefore.role && previousAxis == axis &&
                      previousOccupants.headOption.exists(piece =>
                        piece.side == after.sideToMove && piece.square == pinned.square && piece.role == pinned.role
                      ) && previousOccupants.lift(1).exists(piece =>
                        piece.side == after.sideToMove && piece.square == king.square &&
                          piece.role.name.equalsIgnoreCase(King.name)
                      )
                  case _ => false
              }
              require(
                previousBarriers.size <= 1,
                "one absolute pin occurrence cannot have multiple exact predecessor rays"
              )
              val beforeForbidden = previousBarriers.headOption.toList.flatMap { source =>
                source.detail match
                  case previousRay: RelationWitnessDetail.RayBarrier =>
                    forbiddenResources(before, previousRay, king.square, pinned)
                  case _ => Nil
              }.map(resourceWitness).toSet
              val newlyForbidden =
                if previousBarriers.isEmpty then forbidden
                else forbidden.filterNot(resource => beforeForbidden(resourceWitness(resource)))
              Option.when(newlyForbidden.nonEmpty)(
                VerticalAbsolutePinInventory(
                  after.sideToMove,
                  RelationPieceWitness(attacker, attackerRole),
                  pinned,
                  king.square,
                  axis,
                  movement,
                  legal,
                  newlyForbidden,
                  barrierSource,
                  previousBarriers.headOption.map(source => pinnerBefore -> source),
                  before.stateView.potentialEnPassantSquare,
                  after.stateView.potentialEnPassantSquare
                )
              ).toList
            case _ => Nil
        }.sortBy(value => (value.pinner.square.key, value.pinner.role.name, value.axis.toString))
        absolutePinInventoryByPiece = absolutePinInventoryByPiece.updated(pinned, built)
        built

  private lazy val absolutePinRestrictionInput: List[RelationFactEvidence] =
    VerticalRelationContracts.computeAbsolutePinMovementRestrictions(this)

  private[judgment] def absolutePinRestrictions: List[RelationFactEvidence] =
    absolutePinRestrictionInput

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

  /** Explicit sparse dispatch. The closed ledger still records every contract,
    * including an empty result, but contracts whose exact prerequisite family
    * did not change are not executed. This is deliberately a closed match, not
    * a scheduler or heuristic priority system.
    */
  private[judgment] def affected(contract: VerticalRelationContractKind): Boolean =
    contract match
      case VerticalRelationContractKind.CaptureRecaptureInventory =>
        rootMove.capture.nonEmpty
      case VerticalRelationContractKind.CreatedCheckResponseInventory =>
        transitionResultsFor(RelationCombinationContractKind.GeometricControlSetDelta).nonEmpty
      case VerticalRelationContractKind.RootCheckResponse =>
        positionInventory(RelationSnapshotOccurrence.Before).stateView.inCheck(rootMove.side)
      case VerticalRelationContractKind.MovementAffordanceLegalRestriction =>
        movementAffordanceLegalRestrictions.nonEmpty
      case VerticalRelationContractKind.AbsolutePinMovementRestriction =>
        transitionResultsFor(RelationCombinationContractKind.NamedRayTransition).nonEmpty ||
          movementAffordanceLegalRestrictions.nonEmpty
      case VerticalRelationContractKind.SliderReachDelta =>
        delta.sliderReachTransition.changes.nonEmpty
      case VerticalRelationContractKind.GeometricMultiTargetContact |
          VerticalRelationContractKind.GeometricEnemyContactWithoutFriendlySupport |
          VerticalRelationContractKind.SharedGeometricSupportOfEnemyControlledTargets =>
        transitionResultsFor(RelationCombinationContractKind.GeometricControlSetDelta).nonEmpty
      case VerticalRelationContractKind.GeometricSupportCausalTransition =>
        transitionResultsFor(RelationCombinationContractKind.GeometricSupportDelta).nonEmpty
      case VerticalRelationContractKind.PawnTopologyTransition =>
        List(
          RelationFactKind.PawnFileGroup,
          RelationFactKind.PawnFrontOccupancy,
          RelationFactKind.PawnPassage
        ).exists(kind => delta.ofKind(kind).nonEmpty) ||
          delta.ofKind(RelationFactKind.GeometricControl).exists(change => change.detail match
            case RelationWitnessDetail.GeometricControl(
                  _,
                  _,
                  controllerRole,
                  _,
                  RelationControlTarget.Enemy(targetRole)
                ) =>
              controllerRole.name.equalsIgnoreCase(Pawn.name) && targetRole.name.equalsIgnoreCase(Pawn.name)
            case _ => false)
      case VerticalRelationContractKind.PawnOccupiedFilePartitionTransition =>
        pawnOccupiedFilePartitionChangedSides.nonEmpty
      case VerticalRelationContractKind.MajorPiecePawnFileCorridorTransition =>
        majorPiecePawnFileCorridorTransitions.nonEmpty
      case VerticalRelationContractKind.CastlingRightRemoved =>
        castlingRightRemovals.nonEmpty
      case VerticalRelationContractKind.StalemateTransition =>
        val after = positionInventory(RelationSnapshotOccurrence.After)
        after.kingTerminalState ==
          PositionRelationExtractor.ClosedKingTerminalState.Stalemate

  private[judgment] def rootMove: CanonicalRootLegalMove =
    delta.rootMove

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
      VerticalCaptureRecaptureInput(delta.rootMove, captured, geometric, legal)
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
            change.beforeControllers.sortBy(value => value.square.key -> value.role.name),
            change.afterControllers.sortBy(value => value.square.key -> value.role.name),
            change.removedControllers.sortBy(value => value.square.key -> value.role.name),
            change.establishedControllers.sortBy(value => value.square.key -> value.role.name)
          ))
        case _ => None
    }.sortBy(_.relation.semanticId)

  private[judgment] def newlyEstablishedChecks: List[VerticalCheckTransitionSubject] =
    newlyEstablishedCheckInput

  /** One newly available en-passant movement resource, projected once from
    * the exact potential-EP state and the existing control/movement facts.
    * General legality restrictions and absolute-pin restrictions consume this
    * same occurrence instead of rediscovering it independently.
    */
  private lazy val newlyAvailableEnPassantResourceInput: List[VerticalNewEnPassantResourceInput] =
    val before = positionInventory(RelationSnapshotOccurrence.Before)
    val after = positionInventory(RelationSnapshotOccurrence.After)
    val beforePotential = before.stateView.potentialEnPassantSquare
    val afterPotential = after.stateView.potentialEnPassantSquare
    Option.when(afterPotential.exists(destination => beforePotential != Some(destination)))(afterPotential)
      .flatten.toList.flatMap { destination =>
        after.controlsAt(after.sideToMove, destination).flatMap { control =>
          Option.when(
            control.controller.role.name.equalsIgnoreCase(Pawn.name) &&
              control.target == RelationControlTarget.Empty
          )(control.controller).toList.flatMap { pawn =>
            after.movementAffordancesFrom(pawn.square).flatMap { resource =>
              resource.mode match
                case RelationMovementResourceMode.EnPassantCapture(capturedPawn)
                    if resource.piece == pawn && resource.destination == destination =>
                  List(
                    VerticalNewEnPassantResourceInput(
                      pawn,
                      resource,
                      List(
                        ClosedPositionStatePremise(
                          RelationSnapshotOccurrence.Before,
                          PositionRelationExtractor.ClosedPositionStateQuery.PotentialEnPassant(beforePotential)
                        ),
                        ClosedPositionStatePremise(
                          RelationSnapshotOccurrence.After,
                          PositionRelationExtractor.ClosedPositionStateQuery.PotentialEnPassant(afterPotential)
                        ),
                        ClosedPositionStatePremise(
                          RelationSnapshotOccurrence.After,
                          PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(capturedPawn)
                        )
                      ).sortBy(_.stableKey)
                    )
                  )
                case _ => Nil
            }
          }
        }
      }.sortBy(value => value.pawn.square.key)

  private[judgment] def newlyAvailableEnPassantResources: List[VerticalNewEnPassantResourceInput] =
    newlyAvailableEnPassantResourceInput

  private lazy val movementAffordanceLegalRestrictionInput: List[VerticalMovementAffordanceLegalRestrictionInput] =
    val after = positionInventory(RelationSnapshotOccurrence.After)
    val restrictedSide = after.sideToMove
    val controlledRestrictions = controlSetChanges.flatMap { change =>
          val candidates =
            if change.controllingSide == restrictedSide then
              val resourceControllers =
                if change.beforeTarget != change.afterTarget then change.afterControllers
                else change.establishedControllers
              resourceControllers
            else if change.establishedControllers.nonEmpty then
              after.stateView.kingSquare(restrictedSide).toList.map(square =>
                RelationPieceWitness(square, EvidencePieceRole(King.name))
              )
            else Nil
          candidates.flatMap { candidate =>
            after.occupantAt(candidate.square).filter(occupant =>
              occupant.side == restrictedSide && occupant.role.name.equalsIgnoreCase(candidate.role.name)
            ).toList.flatMap { occupant =>
              val piece = RelationPieceWitness(occupant.square, occupant.role)
              val geometric = after.movementAffordancesFrom(piece.square).filter { resource =>
                resource.piece == piece && resource.destination == change.target &&
                  resource.mode == RelationMovementResourceMode.ControlledDestination
              }
              require(
                geometric.size <= 1,
                s"one piece cannot own multiple canonical controls of '${change.target.key}'"
              )
              geometric.flatMap { control =>
                val legal = after.legalMovesFrom(piece.square)
                  .filter(_.destination == change.target)
                  .sortBy(_.moveUci)
                Option.when(legal.isEmpty)(
                  VerticalMovementAffordanceLegalRestrictionInput(
                    piece,
                    List(control),
                    legal,
                    List(control),
                    List(VerticalMovementRestrictionTrigger(
                      control,
                      change.relation,
                      VerticalRelationPremiseSource.Derived
                    )),
                    Nil
                  )
                )
              }
            }
          }
    }
    val pawnAdvanceRestrictions =
      establishedPositionRelations(RelationFactKind.PawnAdvanceAffordance).flatMap { relation =>
        relation.detail match
          case RelationWitnessDetail.PawnAdvanceAffordance(side, pawnSquare, destination, _)
              if side == restrictedSide =>
            after.occupantAt(pawnSquare).filter(occupant =>
              occupant.side == restrictedSide && occupant.role.name.equalsIgnoreCase(Pawn.name)
            ).toList.flatMap { occupant =>
              val pawn = RelationPieceWitness(occupant.square, occupant.role)
              val movement = after.movementAffordancesFrom(pawnSquare).filter(resource =>
                resource.piece == pawn && resource.destination == destination &&
                  resource.source.semanticId == relation.semanticId && (resource.mode match
                    case RelationMovementResourceMode.PawnAdvance |
                        RelationMovementResourceMode.PawnDoubleAdvance => true
                    case _ => false)
              )
              require(movement.size <= 1, "one established pawn advance must own one exact movement resource")
              movement.flatMap { resource =>
                val legal = after.legalMovesFrom(pawnSquare)
                  .filter(_.destination == destination)
                  .sortBy(_.moveUci)
                Option.when(legal.isEmpty)(
                  VerticalMovementAffordanceLegalRestrictionInput(
                    pawn,
                    List(resource),
                    legal,
                    List(resource),
                    List(VerticalMovementRestrictionTrigger(
                      resource,
                      relation,
                      exactPositionSource(RelationSnapshotOccurrence.After, relation)
                    )),
                    Nil
                  )
                )
              }
            }
          case _ => Nil
      }
    val enPassantRestrictions = newlyAvailableEnPassantResources.flatMap { candidate =>
      val legal = after.legalMovesFrom(candidate.pawn.square)
        .filter(_.destination == candidate.resource.destination)
        .sortBy(_.moveUci)
      Option.when(legal.isEmpty)(
        VerticalMovementAffordanceLegalRestrictionInput(
          candidate.pawn,
          List(candidate.resource),
          legal,
          List(candidate.resource),
          Nil,
          candidate.states
        )
      ).toList
    }
    (controlledRestrictions ++ pawnAdvanceRestrictions ++ enPassantRestrictions)
      .sortBy(view =>
        view.piece.square.key ->
          (view.movement.head.destination.key + view.triggers.map(_.relation.semanticId).mkString)
      )

  private[judgment] def movementAffordanceLegalRestrictions:
      List[VerticalMovementAffordanceLegalRestrictionInput] =
    movementAffordanceLegalRestrictionInput

  private lazy val movementAffordanceLegalRestrictionRelations: List[RelationFactEvidence] =
    VerticalRelationContracts.computeMovementAffordanceLegalRestrictions(this)

  private[judgment] def movementAffordanceLegalRestrictionResults: List[RelationFactEvidence] =
    movementAffordanceLegalRestrictionRelations

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

  /** The board footprint only supplies exact candidate files. A partition
    * transition exists only when one of those files actually crosses the
    * empty/non-empty boundary for that side. Only then do we open the full
    * eight-file partition.
    */
  private lazy val pawnOccupiedFilePartitionChangedSides: List[_root_.chess.Color] =
    val beforeTopology = positionInventory(RelationSnapshotOccurrence.Before).pawnTopologyView
    val afterTopology = positionInventory(RelationSnapshotOccurrence.After).pawnTopologyView
    delta.transitionFootprint.pawnFileChanges
      .groupMap(_._1)(_._2)
      .collect { case (side, files) if files.exists { file =>
        beforeTopology.file(file).pawnsFor(side).isEmpty !=
          afterTopology.file(file).pawnsFor(side).isEmpty
      } => side }
      .toList
      .sortBy(_.toString)

  private lazy val pawnOccupiedFilePartitionTransitionInput:
      List[VerticalPawnOccupiedFilePartitionTransitionInput] =
    val beforeTopology = positionInventory(RelationSnapshotOccurrence.Before).pawnTopologyView
    val afterTopology = positionInventory(RelationSnapshotOccurrence.After).pawnTopologyView
    pawnOccupiedFilePartitionChangedSides.flatMap { side =>
      val beforePartition = beforeTopology.occupiedFilePartition(side)
      val afterPartition = afterTopology.occupiedFilePartition(side)
      val before = beforePartition.witnesses
      val after = afterPartition.witnesses
      Option.when(before.map(_.files) != after.map(_.files))(
        VerticalPawnOccupiedFilePartitionTransitionInput(
          side,
          before,
          after,
          beforePartition.pawnFileSources,
          afterPartition.pawnFileSources,
          List(
            ClosedPositionStatePremise(
              RelationSnapshotOccurrence.Before,
              PositionRelationExtractor.ClosedPositionStateQuery.PawnOccupiedFilePartition(side, before)
            ),
            ClosedPositionStatePremise(
              RelationSnapshotOccurrence.After,
              PositionRelationExtractor.ClosedPositionStateQuery.PawnOccupiedFilePartition(side, after)
            )
          ).sortBy(_.stableKey)
        )
      ).toList
    }

  private[judgment] def pawnOccupiedFilePartitionTransitions:
      List[VerticalPawnOccupiedFilePartitionTransitionInput] =
    pawnOccupiedFilePartitionTransitionInput

  private lazy val pawnOccupiedFilePartitionTransitionRelations: List[RelationFactEvidence] =
    VerticalRelationContracts.computePawnOccupiedFilePartitionTransitions(this)

  private[judgment] def pawnOccupiedFilePartitionTransitionResults: List[RelationFactEvidence] =
    pawnOccupiedFilePartitionTransitionRelations

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

  private lazy val majorPiecePawnFileCorridorTransitionInput: List[VerticalMajorPiecePawnFileCorridorInput] =
    def fileKey(file: File): String =
      (('a'.toInt + file.value).toChar).toString

    def isMajorSlider(piece: RelationPieceWitness): Boolean =
      piece.role.name.equalsIgnoreCase(Rook.name) || piece.role.name.equalsIgnoreCase(Queen.name)

    def pieceFile(piece: RelationPieceWitness): File =
      Square.fromKey(piece.square.key).map(_.file).getOrElse(
        throw IllegalArgumentException(s"invalid slider square '${piece.square.key}'")
      )

    val fileDirections = List(RelationRayDirection(0, -1), RelationRayDirection(0, 1))
      .sortBy(_.stableKey)

    def corridor(
        inventory: PositionRelationExtractor.PositionRelationInventoryCertificate,
        slider: Option[RelationPieceWitness],
        file: File
    ): Option[VerticalFileCorridorInput] =
      slider.filter(piece => pieceFile(piece) == file).map { exactSlider =>
        val reaches = inventory.sliderReachesFrom(exactSlider.square)
          .filter(_.direction.axis == RelationAxisSignal.File)
        require(
          reaches.map(_.direction).distinct.size == reaches.size &&
            reaches.forall(reach => fileDirections.contains(reach.direction)),
          "a closed file corridor cannot repeat or escape its two file directions"
        )
        val byDirection = reaches.map(reach => reach.direction -> reach).toMap
        VerticalFileCorridorInput(
          exactSlider,
          fileDirections.map(direction => VerticalFileRayReachInput(direction, byDirection.get(direction)))
        )
      }

    def occurrenceChange(
        side: _root_.chess.Color,
        before: Option[RelationPieceWitness],
        after: Option[RelationPieceWitness]
    ): Option[RelationSliderOccurrenceChange] =
      def exactMovement(
          matches: BoardPieceTransition => Boolean
      ): RelationSliderOccurrenceChange =
        val found = delta.transitionFootprint.pieceTransitions.filter(matches)
        require(found.size == 1, "a changed major-slider occurrence needs one exact board transition")
        RelationSliderOccurrenceChange.PieceTransition(RelationMoveTransitionWitness.from(found.head))

      (before, after) match
        case (Some(previous), Some(next)) if previous != next =>
          Some(exactMovement(transition =>
            transition.side == side &&
              transition.from.key.equalsIgnoreCase(previous.square.key) &&
              transition.beforeRole.name.equalsIgnoreCase(previous.role.name) &&
              transition.to.key.equalsIgnoreCase(next.square.key) &&
              transition.afterRole.name.equalsIgnoreCase(next.role.name)
          ))
        case (None, Some(next)) =>
          Some(exactMovement(transition =>
            transition.side == side && transition.beforeRole == Pawn &&
              transition.to.key.equalsIgnoreCase(next.square.key) &&
              transition.afterRole.name.equalsIgnoreCase(next.role.name)
          ))
        case (Some(previous), None) =>
          val captured = delta.rootMove.capture.filter(capture =>
            capture.capturedSide == side && capture.capturedSquare == previous.square &&
              capture.capturedRole == previous.role
          )
          require(captured.nonEmpty, "a vanished major-slider occurrence needs the exact root capture")
          Some(RelationSliderOccurrenceChange.Captured(
            RelationColoredPieceWitness(previous.square, previous.role, side)
          ))
        case (Some(previous), Some(next)) =>
          require(previous == next, "a stationary major slider cannot change identity")
          None
        case (None, None) =>
          throw IllegalArgumentException("a file corridor trigger lost both major-slider occurrences")

    def occurrenceStates(
        occurrence: RelationSnapshotOccurrence,
        side: _root_.chess.Color,
        slider: Option[RelationPieceWitness],
        exactCorridor: Option[VerticalFileCorridorInput]
    ): List[ClosedPositionStatePremise] =
      slider.toList.flatMap(exact => ClosedPositionStatePremise(
        occurrence,
        PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(
          RelationColoredPieceWitness(exact.square, exact.role, side)
        )
      ) :: exactCorridor.toList.flatMap(_.rays.map(ray =>
        ClosedPositionStatePremise(
          occurrence,
          PositionRelationExtractor.ClosedPositionStateQuery.SliderReach(
            side,
            exact,
            ray.direction,
            ray.reach.map(_.witness)
          )
        )
      )))

    val relevantReachDeltas = sliderReachDeltas.filter(delta =>
      delta.direction.axis == RelationAxisSignal.File &&
        (delta.sliderBefore.exists(isMajorSlider) || delta.sliderAfter.exists(isMajorSlider))
    )
    type SliderPair = (
        _root_.chess.Color,
        Option[RelationPieceWitness],
        Option[RelationPieceWitness]
    )
    val reachTriggerGroups: Map[SliderPair, List[VerticalSliderReachDeltaInput]] =
      relevantReachDeltas.groupBy(reachDelta =>
        (reachDelta.side, reachDelta.sliderBefore, reachDelta.sliderAfter)
      )
    val changedPawnFiles = delta.transitionFootprint.pawnFileChanges.map(_._2)
    def majorPiecesOnChangedPawnFiles(
        inventory: PositionRelationExtractor.PositionRelationInventoryCertificate
    ): Set[(_root_.chess.Color, RelationPieceWitness)] =
      if changedPawnFiles.isEmpty then Set.empty
      else
        inventory.stateView.occupiedPieces.flatMap { colored =>
          val piece = RelationPieceWitness(colored.square, colored.role)
          Option.when(isMajorSlider(piece) && changedPawnFiles(pieceFile(piece)))(colored.side -> piece)
        }.toSet
    val pawnFileTriggerPairs: Set[SliderPair] =
      val before = majorPiecesOnChangedPawnFiles(delta.beforeInventory)
      val after = majorPiecesOnChangedPawnFiles(delta.afterInventory)
      before.intersect(after).map { case (side, piece) => (side, Some(piece), Some(piece)) }
    val triggerPairs = reachTriggerGroups.keySet ++ pawnFileTriggerPairs
    val candidateFiles = triggerPairs.flatMap { case (_, sliderBefore, sliderAfter) =>
      (sliderBefore.toList ++ sliderAfter.toList).filter(isMajorSlider).map(pieceFile)
    }
    def filesByBoardFile(
        inventory: PositionRelationExtractor.PositionRelationInventoryCertificate
    ): Map[File, PositionRelationExtractor.ClosedPawnFile] =
      val entries = inventory.pawnFiles(candidateFiles).map { file =>
        val boardFile = file.file.key.toLowerCase.headOption.flatMap(File.fromChar).getOrElse(
          throw IllegalArgumentException(s"invalid closed pawn file '${file.file.key}'")
        )
        boardFile -> file
      }
      require(
        entries.map(_._1).toSet == candidateFiles && entries.map(_._1).distinct.size == entries.size,
        "a corridor transition must open each changed candidate file exactly once"
      )
      entries.toMap
    val beforeFiles = filesByBoardFile(delta.beforeInventory)
    val afterFiles = filesByBoardFile(delta.afterInventory)

    triggerPairs.toList
      .sortBy { case (side, before, after) =>
        def pieceKey(piece: Option[RelationPieceWitness]): String =
          piece.map(value => s"${value.role.name.toLowerCase}@${value.square.key.toLowerCase}").getOrElse("-")
        s"${side.toString.toLowerCase}:${pieceKey(before)}:${pieceKey(after)}"
      }
      .flatMap { case (side, sliderBefore, sliderAfter) =>
        val triggers = reachTriggerGroups.getOrElse((side, sliderBefore, sliderAfter), Nil)
        require(
          triggers.map(_.direction).distinct.size == triggers.size,
          "one paired slider occurrence cannot repeat a directional reach delta"
        )
        val exactOccurrenceChange = occurrenceChange(side, sliderBefore, sliderAfter)
        val files = (sliderBefore.toList ++ sliderAfter.toList)
          .filter(isMajorSlider).map(pieceFile).distinct.sortBy(_.value)
        files.flatMap { file =>
          val beforeFile = beforeFiles(file)
          val afterFile = afterFiles(file)
          val beforeCorridor = corridor(delta.beforeInventory, sliderBefore, file)
          val afterCorridor = corridor(delta.afterInventory, sliderAfter, file)
          val beforePawnState = VerticalRelationWitnessProjection.pawnFileState(beforeFile)
          val afterPawnState = VerticalRelationWitnessProjection.pawnFileState(afterFile)
          val beforeUsable = beforeCorridor.nonEmpty && beforePawnState.opennessFor(side).ownPawnAbsent
          val afterUsable = afterCorridor.nonEmpty && afterPawnState.opennessFor(side).ownPawnAbsent
          val changed = sliderBefore != sliderAfter ||
            beforeCorridor.map(_.witness) != afterCorridor.map(_.witness) ||
            beforePawnState.opennessFor(side) != afterPawnState.opennessFor(side)
          Option.when((beforeUsable || afterUsable) && changed)(
            VerticalMajorPiecePawnFileCorridorInput(
              side,
              EvidenceFile(fileKey(file)),
              sliderBefore,
              sliderAfter,
              exactOccurrenceChange,
              beforeFile,
              afterFile,
              beforeCorridor,
              afterCorridor,
              (
                occurrenceStates(
                  RelationSnapshotOccurrence.Before,
                  side,
                  sliderBefore,
                  beforeCorridor
                ) ++ occurrenceStates(
                  RelationSnapshotOccurrence.After,
                  side,
                  sliderAfter,
                  afterCorridor
                )
              ).sortBy(_.stableKey)
            )
          )
        }
      }.sortBy(_.stableKey)

  private[judgment] def majorPiecePawnFileCorridorTransitions: List[VerticalMajorPiecePawnFileCorridorInput] =
    majorPiecePawnFileCorridorTransitionInput

  private lazy val castlingRightRemovalInput: List[VerticalCastlingRightRemovalInput] =
    val before = positionInventory(RelationSnapshotOccurrence.Before).stateView.castlingRights
    val after = positionInventory(RelationSnapshotOccurrence.After).stateView.castlingRights
    List(_root_.chess.Color.White, _root_.chess.Color.Black).flatMap { side =>
      RelationCastlingFlank.values.toList.flatMap { flank =>
        Option.when(before.allows(side, flank.castleSide) && !after.allows(side, flank.castleSide))(
          VerticalCastlingRightRemovalInput(
            side,
            flank,
            List(
              ClosedPositionStatePremise(
                RelationSnapshotOccurrence.Before,
                PositionRelationExtractor.ClosedPositionStateQuery.CastlingRight(side, flank.castleSide, true)
              ),
              ClosedPositionStatePremise(
                RelationSnapshotOccurrence.After,
                PositionRelationExtractor.ClosedPositionStateQuery.CastlingRight(side, flank.castleSide, false)
              )
            ).sortBy(_.stableKey)
          )
        ).toList
      }
    }.sortBy(_.stableKey)

  private[judgment] def castlingRightRemovals: List[VerticalCastlingRightRemovalInput] =
    castlingRightRemovalInput

  private lazy val castlingRightRemovalRelations: List[RelationFactEvidence] =
    VerticalRelationContracts.computeCastlingRightRemovals(this)

  private[judgment] def castlingRightRemovalResults: List[RelationFactEvidence] =
    castlingRightRemovalRelations

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
            _
          ) =>
        rootOwned(mover) && captureRecapture.exists(view =>
          captured == RelationColoredPieceWitness(
            view.captured.capturedSquare,
            view.captured.capturedRole,
            view.captured.capturedSide
          ) && geometricRecapturers == view.geometric.map(_.controller) &&
            legalRecaptures == view.legal.map(legalResource).sortBy(_.stableKey)
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
      case detail @ RelationWitnessDetail.MovementAffordanceLegalRestriction(mover, _, _, _, _, _, _) =>
        rootOwned(mover) && movementAffordanceLegalRestrictionResults.exists(_.detail == detail)
      case detail @ RelationWitnessDetail.AbsolutePinMovementRestriction(mover, _, _, _, _, _, _, _, _, _) =>
        rootOwned(mover) && absolutePinRestrictions.exists(_.detail == detail)
      case detail @ RelationWitnessDetail.SliderReachDelta(mover, _, _, _, _, _, _, _) =>
        rootOwned(mover) && sliderReachDeltaResults.exists(_.detail == detail)
      case detail @ RelationWitnessDetail.GeometricMultiTargetContact(mover, _, _, _, _, _, _) =>
        rootOwned(mover) && multiTargetContactRelations.exists(_.detail == detail)
      case RelationWitnessDetail.GeometricEnemyContactWithoutFriendlySupport(
            mover,
            controllingSide,
            controllerBefore,
            controllerAfter,
            target,
            _
          ) =>
        rootOwned(mover) && unsupportedEnemyContacts.exists(value =>
          value.inventory.mover == mover && value.inventory.controllingSide == controllingSide &&
            value.inventory.controllerBefore == controllerBefore &&
            value.inventory.controllerAfter == controllerAfter && value.target.target == target
        )
      case detail @ RelationWitnessDetail.SharedGeometricSupportOfEnemyControlledTargets(
            mover,
            _,
            _,
            _,
            _,
            _
          ) =>
        rootOwned(mover) && sharedSupportRelations.exists(_.detail == detail)
      case detail @ RelationWitnessDetail.GeometricSupportCausalTransition(
            mover,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        rootOwned(mover) && supportCausalTransitions.exists(_.detail == detail)
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
      case detail @ RelationWitnessDetail.PawnOccupiedFilePartitionTransition(mover, _, _, _, _) =>
        rootOwned(mover) && pawnOccupiedFilePartitionTransitionResults.exists(_.detail == detail)
      case RelationWitnessDetail.MajorPiecePawnFileCorridorTransition(
            mover,
            side,
            file,
            sliderBefore,
            sliderAfter,
            occurrenceChange,
            beforePawnFile,
            afterPawnFile,
            beforeCorridor,
            afterCorridor,
            _
          ) =>
        rootOwned(mover) && majorPiecePawnFileCorridorTransitions.exists(view =>
          view.side == side && view.file == file && view.sliderBefore == sliderBefore &&
            view.sliderAfter == sliderAfter && view.occurrenceChange == occurrenceChange &&
            pawnFileState(view.beforePawnFile) == beforePawnFile &&
            pawnFileState(view.afterPawnFile) == afterPawnFile &&
            view.beforeCorridor.map(_.witness) == beforeCorridor &&
            view.afterCorridor.map(_.witness) == afterCorridor
        )
      case detail @ RelationWitnessDetail.CastlingRightRemoved(mover, _, _, _) =>
        rootOwned(mover) && castlingRightRemovalResults.exists(_.detail == detail)
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
    absenceCertificatesByKey.get(absence.stableKey) match
      case Some(cached) => cached
      case None =>
        val inventory = absence.occurrence match
          case RelationSnapshotOccurrence.Before => delta.beforeInventory
          case RelationSnapshotOccurrence.After  => delta.afterInventory
        val certified = inventory.certifyAbsence(absence.query)
        absenceCertificatesByKey = absenceCertificatesByKey.updated(absence.stableKey, certified)
        certified

  private[judgment] def stateCertificate(
      state: ClosedPositionStatePremise
  ): Option[PositionRelationExtractor.ClosedPositionStateCertificate] =
    stateCertificatesByKey.get(state.stableKey) match
      case Some(cached) => cached
      case None =>
        val inventory = state.occurrence match
          case RelationSnapshotOccurrence.Before => delta.beforeInventory
          case RelationSnapshotOccurrence.After  => delta.afterInventory
        val certified = inventory.certifyState(state.query)
        stateCertificatesByKey = stateCertificatesByKey.updated(state.stableKey, certified)
        certified

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
    legal: List[PositionRelationExtractor.ClosedLegalMove]
)

private[chessjudgment] final case class VerticalMovementAffordanceLegalRestrictionInput(
    piece: RelationPieceWitness,
    movement: List[PositionRelationExtractor.ClosedMovementAffordance],
    legal: List[PositionRelationExtractor.ClosedLegalMove],
    unavailable: List[PositionRelationExtractor.ClosedMovementAffordance],
    triggers: List[VerticalMovementRestrictionTrigger],
    states: List[ClosedPositionStatePremise]
):
  require(movement.nonEmpty && unavailable.nonEmpty && (triggers.nonEmpty || states.nonEmpty))
  require(unavailable.forall(movement.contains))
  require(movement.forall(_.piece == piece), "one movement restriction must belong to one exact piece")
  require(triggers.forall(trigger => unavailable.contains(trigger.resource)))
  require(
    triggers.map(_.stableKey).distinct.size == triggers.size && triggers == triggers.sortBy(_.stableKey),
    "movement restriction triggers must be exact and canonically ordered"
  )
  require(
    if states.nonEmpty then
      triggers.isEmpty && unavailable.forall(_.mode match
        case RelationMovementResourceMode.EnPassantCapture(_) => true
        case _                                                 => false)
    else unavailable.forall(resource => triggers.exists(_.resource == resource)),
    "every unavailable movement resource needs its exact transition trigger or en-passant state"
  )
  require(states.distinct.size == states.size && states == states.sortBy(_.stableKey))

private[chessjudgment] final case class VerticalMovementRestrictionTrigger(
    resource: PositionRelationExtractor.ClosedMovementAffordance,
    relation: RelationFactEvidence,
    source: VerticalRelationPremiseSource
):
  def stableKey: String =
    s"${resource.piece.square.key.toLowerCase}:${resource.piece.role.name.toLowerCase}:${resource.destination.key.toLowerCase}:${resource.mode.stableKey}:${VerticalRelationPremiseSource.id(source)}:${relation.semanticId}"

private[chessjudgment] final case class VerticalNewEnPassantResourceInput(
    pawn: RelationPieceWitness,
    resource: PositionRelationExtractor.ClosedMovementAffordance,
    states: List[ClosedPositionStatePremise]
):
  require(resource.piece == pawn, "one en-passant resource must belong to its exact pawn")
  require(
    resource.mode match
      case RelationMovementResourceMode.EnPassantCapture(_) => true
      case _                                                 => false,
    "a shared en-passant input must contain an en-passant capture resource"
  )
  require(states.size == 3 && states.distinct.size == states.size && states == states.sortBy(_.stableKey))

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

private[chessjudgment] final case class VerticalPawnOccupiedFilePartitionTransitionInput(
    side: _root_.chess.Color,
    before: List[RelationPawnOccupiedFileRunWitness],
    after: List[RelationPawnOccupiedFileRunWitness],
    beforeFileSources: List[RelationFactEvidence],
    afterFileSources: List[RelationFactEvidence],
    states: List[ClosedPositionStatePremise]
):
  require(
    RelationPawnOccupiedFileRunWitness.canonicalPartition(before) &&
      RelationPawnOccupiedFileRunWitness.canonicalPartition(after),
    "a pawn-occupied-file-run transition needs canonical before and after partitions"
  )
  require(before.map(_.files) != after.map(_.files), "a pawn-occupied-file-run transition must change file connectivity")
  require(
    beforeFileSources.size == File.all.size && afterFileSources.size == File.all.size &&
      beforeFileSources.map(_.semanticId).distinct.size == beforeFileSources.size &&
      afterFileSources.map(_.semanticId).distinct.size == afterFileSources.size,
    "a pawn-occupied-file-run transition needs every side-specific file group exactly once"
  )
  require(states.size == 2 && states.distinct.size == states.size && states == states.sortBy(_.stableKey))

  def stableKey: String =
    val beforeKey = before.map(_.stableKey).mkString("[", ",", "]")
    val afterKey = after.map(_.stableKey).mkString("[", ",", "]")
    s"${side.toString.toLowerCase}:$beforeKey:$afterKey"

private[chessjudgment] final case class VerticalFileRayReachInput(
    direction: RelationRayDirection,
    reach: Option[PositionRelationExtractor.ClosedSliderReach]
):
  require(direction.axis == RelationAxisSignal.File)
  def witness: RelationFileRayReachWitness = RelationFileRayReachWitness(direction, reach.map(_.witness))

private[chessjudgment] final case class VerticalFileCorridorInput(
    slider: RelationPieceWitness,
    rays: List[VerticalFileRayReachInput]
):
  private val expectedDirections = Set(RelationRayDirection(0, -1), RelationRayDirection(0, 1))
  require(
    rays.size == expectedDirections.size && rays.map(_.direction).toSet == expectedDirections &&
      rays == rays.sortBy(_.direction.stableKey) && rays.exists(_.reach.nonEmpty),
    "an input file corridor must close both directions"
  )
  require(rays.flatMap(_.reach).forall(_.slider == slider))

  def witness: RelationFileCorridorWitness =
    RelationFileCorridorWitness(rays.map(_.witness).sortBy(_.stableKey))

private[chessjudgment] final case class VerticalMajorPiecePawnFileCorridorInput(
    side: _root_.chess.Color,
    file: EvidenceFile,
    sliderBefore: Option[RelationPieceWitness],
    sliderAfter: Option[RelationPieceWitness],
    occurrenceChange: Option[RelationSliderOccurrenceChange],
    beforePawnFile: PositionRelationExtractor.ClosedPawnFile,
    afterPawnFile: PositionRelationExtractor.ClosedPawnFile,
    beforeCorridor: Option[VerticalFileCorridorInput],
    afterCorridor: Option[VerticalFileCorridorInput],
    states: List[ClosedPositionStatePremise]
):
  private def major(piece: RelationPieceWitness): Boolean =
    piece.role.name.equalsIgnoreCase(Rook.name) || piece.role.name.equalsIgnoreCase(Queen.name)
  private def onFile(piece: RelationPieceWitness): Boolean =
    EvidenceFile.contains(file, piece.square)
  private def ownPawnAbsent(fileState: PositionRelationExtractor.ClosedPawnFile): Boolean =
    VerticalRelationWitnessProjection.pawnFileState(fileState).opennessFor(side).ownPawnAbsent

  require(
    beforePawnFile.file == file && afterPawnFile.file == file &&
      beforePawnFile.sourceGroups.map(_.semanticId).distinct.size == beforePawnFile.sourceGroups.size &&
      afterPawnFile.sourceGroups.map(_.semanticId).distinct.size == afterPawnFile.sourceGroups.size,
    "a pawn-file corridor needs both sides' unique pawn-file groups at both occurrences"
  )
  require(sliderBefore.nonEmpty || sliderAfter.nonEmpty)
  require(sliderBefore.forall(major) && sliderAfter.forall(major))
  require(
    occurrenceChange.isEmpty == (sliderBefore == sliderAfter),
    "a changed slider identity needs one exact occurrence cause"
  )
  occurrenceChange.foreach {
    case RelationSliderOccurrenceChange.PieceTransition(movement) =>
      require(
        movement.side == side && sliderAfter.exists(after =>
          movement.to == after.square && movement.afterRole == after.role
        ),
        "a slider movement cause must terminate at the exact after occurrence"
      )
      sliderBefore match
        case Some(before) =>
          require(
            movement.from == before.square && movement.beforeRole == before.role,
            "a moved slider cause must begin at the exact before occurrence"
          )
        case None =>
          require(
            movement.beforeRole.name.equalsIgnoreCase(Pawn.name),
            "a newly created major slider must be the exact result of promotion"
          )
    case RelationSliderOccurrenceChange.Captured(piece) =>
      require(
        sliderBefore.contains(RelationPieceWitness(piece.square, piece.role)) &&
          sliderAfter.isEmpty && piece.side == side,
        "a captured slider cause must match the exact vanished occurrence"
      )
  }
  require(beforeCorridor.forall(value => sliderBefore.contains(value.slider) && onFile(value.slider)))
  require(afterCorridor.forall(value => sliderAfter.contains(value.slider) && onFile(value.slider)))
  require(sliderBefore.exists(onFile) == beforeCorridor.nonEmpty)
  require(sliderAfter.exists(onFile) == afterCorridor.nonEmpty)
  require(
    beforeCorridor.nonEmpty && ownPawnAbsent(beforePawnFile) ||
      afterCorridor.nonEmpty && ownPawnAbsent(afterPawnFile),
    "a pawn-file corridor transition needs one usable before or after reach"
  )
  require(
    sliderBefore != sliderAfter || beforeCorridor.map(_.witness) != afterCorridor.map(_.witness) ||
      VerticalRelationWitnessProjection.pawnFileState(beforePawnFile).opennessFor(side) !=
        VerticalRelationWitnessProjection.pawnFileState(afterPawnFile).opennessFor(side),
    "a pawn-file corridor transition must change its occurrence, owner-relative openness, or reach"
  )
  require(
    states.size == sliderBefore.size + sliderAfter.size +
      2 * (beforeCorridor.size + afterCorridor.size) &&
      states.distinct.size == states.size && states == states.sortBy(_.stableKey),
    "a pawn-file corridor needs exact occupancy and both ray states for every existing occurrence"
  )

  def stableKey: String =
    s"${side.toString.toLowerCase}:${file.key.toLowerCase}:${sliderBefore.map(piece => s"${piece.square.key}:${piece.role.name}").getOrElse("-")}:${sliderAfter.map(piece => s"${piece.square.key}:${piece.role.name}").getOrElse("-")}:${occurrenceChange.map(_.stableKey).getOrElse("stationary")}:${beforeCorridor.map(_.witness.stableKey).getOrElse("-")}:${afterCorridor.map(_.witness.stableKey).getOrElse("-")}"

private[chessjudgment] final case class VerticalCastlingRightRemovalInput(
    side: _root_.chess.Color,
    flank: RelationCastlingFlank,
    states: List[ClosedPositionStatePremise]
):
  require(states.size == 2 && states.distinct.size == states.size && states == states.sortBy(_.stableKey))

  def stableKey: String = s"${side.toString.toLowerCase}:${flank.stableKey}"

private[chessjudgment] final case class VerticalCheckTransitionSubject(
    relation: RelationFactEvidence,
    mover: RelationMoveTransitionWitness,
    checkedSide: _root_.chess.Color,
    kingSquare: EvidenceSquare,
    beforeCheckers: List[RelationPieceWitness],
    afterCheckers: List[RelationPieceWitness],
    removedCheckers: List[RelationPieceWitness],
    establishedCheckers: List[RelationPieceWitness]
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

private[chessjudgment] final case class VerticalEnemyControlChange(
    relation: RelationFactEvidence,
    mover: RelationMoveTransitionWitness,
    controllingSide: _root_.chess.Color,
    controllerBefore: RelationPieceWitness,
    controllerAfter: RelationPieceWitness,
    target: RelationPieceWitness,
    newlyEstablished: Boolean
)

private[chessjudgment] final case class VerticalEnemyContactTarget(
    target: RelationPieceWitness,
    beforeControl: Option[PositionRelationExtractor.ClosedGeometricControl],
    afterControl: PositionRelationExtractor.ClosedGeometricControl,
    change: Option[VerticalEnemyControlChange]
)

private[chessjudgment] final case class VerticalEnemyContactInventory(
    mover: RelationMoveTransitionWitness,
    controllingSide: _root_.chess.Color,
    controllerBefore: RelationPieceWitness,
    controllerAfter: RelationPieceWitness,
    targets: List[VerticalEnemyContactTarget]
):
  require(targets.nonEmpty, "an enemy-contact inventory cannot be empty")
  require(
    targets.map(_.target).distinct.size == targets.size,
    "one controller cannot repeat an enemy target in its closed inventory"
  )
  require(
    targets.forall(target =>
      target.afterControl.side == controllingSide && target.afterControl.controller == controllerAfter
    ),
    "an enemy-contact inventory must retain its exact controller and side"
  )

  def newlyEstablished: List[VerticalEnemyContactTarget] =
    targets.filter(_.beforeControl.isEmpty)

  def maintained: List[VerticalEnemyContactTarget] =
    targets.filter(_.beforeControl.nonEmpty)

private[chessjudgment] final case class VerticalUnsupportedEnemyContact(
    inventory: VerticalEnemyContactInventory,
    target: VerticalEnemyContactTarget
):
  require(target.beforeControl.isEmpty, "an unsupported new contact must be newly established")
  require(target.change.exists(_.newlyEstablished), "an unsupported new contact needs its exact positive delta")

private[chessjudgment] final case class VerticalSharedSupportTarget(
    witness: RelationSharedGeometricSupportTargetWitness,
    friendlySupports: List[PositionRelationExtractor.ClosedGeometricControl],
    enemyControls: List[PositionRelationExtractor.ClosedGeometricControl]
)

private[chessjudgment] final case class VerticalSharedSupportInventory(
    mover: RelationMoveTransitionWitness,
    controllingSide: _root_.chess.Color,
    supportedSide: _root_.chess.Color,
    sharedSupporter: RelationPieceWitness,
    targets: List[VerticalSharedSupportTarget],
    changedControls: List[(RelationPieceWitness, RelationFactEvidence)]
):
  require(controllingSide != supportedSide)
  require(targets.size >= 2, "shared support needs at least two exact controlled targets")
  require(changedControls.nonEmpty, "shared support needs one newly established enemy-control relation")

private[chessjudgment] final case class VerticalAbsolutePinInventory(
    restrictedSide: _root_.chess.Color,
    pinner: RelationPieceWitness,
    pinned: RelationPieceWitness,
    kingSquare: EvidenceSquare,
    axis: RelationAxisSignal,
    movementResources: List[PositionRelationExtractor.ClosedMovementAffordance],
    legalResources: List[PositionRelationExtractor.ClosedLegalMove],
    newlyForbiddenResources: List[PositionRelationExtractor.ClosedMovementAffordance],
    barrierSource: RelationFactEvidence,
    previousBarrier: Option[(RelationPieceWitness, RelationFactEvidence)],
    beforePotentialEnPassant: Option[EvidenceSquare],
    afterPotentialEnPassant: Option[EvidenceSquare]
):
  require(newlyForbiddenResources.nonEmpty, "an absolute-pin transition needs one newly forbidden resource")
  require(newlyForbiddenResources.forall(movementResources.contains))

  def pinEstablished: Boolean = previousBarrier.isEmpty

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

  private final case class SupportChangeJoinKey(
      direction: RelationChangeDirection,
      supportedSquare: EvidenceSquare,
      supportedRole: EvidencePieceRole,
      supporterSquare: EvidenceSquare,
      supporterRole: EvidencePieceRole
  )

  private final case class SupportChangeSubject(
      relation: RelationFactEvidence,
      mover: RelationMoveTransitionWitness,
      supportedSide: _root_.chess.Color,
      supportedBefore: EvidenceSquare,
      supportedBeforeRole: EvidencePieceRole,
      supportedAfter: EvidenceSquare,
      supportedAfterRole: EvidencePieceRole,
      beforeSupporters: List[RelationPieceWitness],
      afterSupporters: List[RelationPieceWitness],
      removedSupporters: List[RelationPieceWitness],
      establishedSupporters: List[RelationPieceWitness]
  ):
    def removalJoinKey(supporter: RelationPieceWitness): SupportChangeJoinKey =
      SupportChangeJoinKey(
        RelationChangeDirection.Removed,
        supportedBefore,
        supportedBeforeRole,
        supporter.square,
        supporter.role
      )

    def establishmentJoinKey(supporter: RelationPieceWitness): SupportChangeJoinKey =
      SupportChangeJoinKey(
        RelationChangeDirection.Established,
        supportedAfter,
        supportedAfterRole,
        supporter.square,
        supporter.role
      )

  private final case class SupportChangeCauseEvidence(
      relation: RelationFactEvidence,
      joinKey: SupportChangeJoinKey,
      controllerSide: Option[_root_.chess.Color],
      cause: RelationSupportChangeCause
  )

  private final case class SupportRemovalExplanation(
      supporter: RelationPieceWitness,
      cause: RelationSupportChangeCause,
      source: RelationFactEvidence,
      premiseSource: VerticalRelationPremiseSource
  ):
    def stableKey: String =
      s"${supporter.square.key.toLowerCase}:${supporter.role.name.toLowerCase}:${cause.stableKey}:${VerticalRelationPremiseSource.id(premiseSource)}:${source.semanticId}" 

  private final case class SupportEstablishmentExplanation(
      supporter: RelationPieceWitness,
      cause: RelationSupportChangeCause,
      source: RelationFactEvidence,
      premiseSource: VerticalRelationPremiseSource
  ):
    def stableKey: String =
      s"${supporter.square.key.toLowerCase}:${supporter.role.name.toLowerCase}:${cause.stableKey}:${VerticalRelationPremiseSource.id(premiseSource)}:${source.semanticId}"

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
      case VerticalRelationContractKind.MovementAffordanceLegalRestriction =>
        input.movementAffordanceLegalRestrictionResults
      case VerticalRelationContractKind.AbsolutePinMovementRestriction =>
        input.absolutePinRestrictions
      case VerticalRelationContractKind.SliderReachDelta =>
        input.sliderReachDeltaResults
      case VerticalRelationContractKind.GeometricMultiTargetContact =>
        input.multiTargetContactRelations
      case VerticalRelationContractKind.GeometricEnemyContactWithoutFriendlySupport =>
        deriveGeometricEnemyContactsWithoutFriendlySupport(input)
      case VerticalRelationContractKind.SharedGeometricSupportOfEnemyControlledTargets =>
        input.sharedSupportRelations
      case VerticalRelationContractKind.GeometricSupportCausalTransition =>
        input.supportCausalTransitions
      case VerticalRelationContractKind.PawnTopologyTransition =>
        derivePawnTopologyTransitions(input)
      case VerticalRelationContractKind.PawnOccupiedFilePartitionTransition =>
        input.pawnOccupiedFilePartitionTransitionResults
      case VerticalRelationContractKind.MajorPiecePawnFileCorridorTransition =>
        deriveMajorPiecePawnFileCorridorTransitions(input)
      case VerticalRelationContractKind.CastlingRightRemoved =>
        input.castlingRightRemovalResults
      case VerticalRelationContractKind.StalemateTransition =>
        deriveStalemateTransition(input)

  private[judgment] def computeSupportCausalTransitions(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    val supportDeltas = input.transitionResultsFor(RelationCombinationContractKind.GeometricSupportDelta)
    val causes =
      input.transitionResultsFor(RelationCombinationContractKind.GeometricSupporterCapture) ++
        input.transitionResultsFor(RelationCombinationContractKind.SliderLineInterruption) ++
        input.transitionResultsFor(RelationCombinationContractKind.GeometricLineControlAfterBlockerRemoval)
    val causesByChangedRelation = causes
      .flatMap(causeEvidence)
      .groupMap(_.joinKey)(identity)
    supportDeltas
      .flatMap(supportChangeSubject)
      .flatMap(subject => supportCausalTransitionProof(subject, causesByChangedRelation, input.rootMove))
      .sortBy(_.semanticId)

  def proofOf(detail: RelationWitnessDetail): Option[VerticalRelationDerivationProof] =
    detail match
      case RelationWitnessDetail.CaptureRecaptureInventory(_, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.CreatedCheckResponseInventory(_, _, _, _, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.RootCheckResponse(_, _, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.MovementAffordanceLegalRestriction(_, _, _, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.AbsolutePinMovementRestriction(_, _, _, _, _, _, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.SliderReachDelta(_, _, _, _, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.GeometricMultiTargetContact(_, _, _, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.GeometricEnemyContactWithoutFriendlySupport(_, _, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.SharedGeometricSupportOfEnemyControlledTargets(_, _, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.GeometricSupportCausalTransition(_, _, _, _, _, _, _, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.PawnTopologyTransition(_, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.PawnOccupiedFilePartitionTransition(_, _, _, _, proof) => Some(proof)
      case RelationWitnessDetail.MajorPiecePawnFileCorridorTransition(_, _, _, _, _, _, _, _, _, _, proof) =>
        Some(proof)
      case RelationWitnessDetail.CastlingRightRemoved(_, _, _, proof) => Some(proof)
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
        )
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
        )
      val proof = VerticalRelationDerivationProof.fromRootAndSources(
        VerticalRelationContractKind.CaptureRecaptureInventory,
        VerticalRelationPremiseRole.RootCapture,
        root,
        premises,
        absences
      )
      RelationFactEvidence.from(
        RelationWitnessDetail.CaptureRecaptureInventory(
          root.witness,
          captured,
          recapturers,
          legal,
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
          (
            VerticalRelationPremiseRole.MovementResource(
              subject.checkedSide,
              RelationPieceWitness(subject.kingSquare, EvidencePieceRole(King.name)),
              resource
            ),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            destination.movement.source
          ) :: destination.opponentControls.map(control =>
            (
              VerticalRelationPremiseRole.KingDestinationControl(
                subject.checkedSide,
                resource,
                control.controller,
                control.target
              ),
              VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
              control.source
            )
          )
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
      val proof = VerticalRelationDerivationProof.fromSources(
        VerticalRelationContractKind.CreatedCheckResponseInventory,
        premises,
        absences
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

  private[judgment] def computeMovementAffordanceLegalRestrictions(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    val after = input.positionInventory(RelationSnapshotOccurrence.After)
    input.movementAffordanceLegalRestrictions.map { view =>
      val movement = view.movement.map(control =>
        RelationMovementResourceWitness(control.destination, control.target, control.mode)
      ).sortBy(_.stableKey)
      val legal = view.legal.map(legalResource).sortBy(_.stableKey)
      val unavailable = view.unavailable.map(control =>
        RelationMovementResourceWitness(control.destination, control.target, control.mode)
      ).sortBy(_.stableKey)
      val absences = unavailable.map(resource =>
        ClosedRelationAbsencePremise(
          RelationSnapshotOccurrence.After,
          PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
            after.sideToMove,
            view.piece.square,
            resource.destination
          )
        )
      )
      val premises =
        List(
          (
            VerticalRelationPremiseRole.RootMove,
            VerticalRelationPremiseSource.RootTransition,
            input.rootMove.fact
          )
        ) ++ view.triggers.map { trigger =>
          (
            VerticalRelationPremiseRole.LegalRestrictionTrigger(
              after.sideToMove,
              view.piece,
              RelationMovementResourceWitness(
                trigger.resource.destination,
                trigger.resource.target,
                trigger.resource.mode
              ),
              trigger.relation.semanticId
            ),
            trigger.source,
            trigger.relation
          )
        } ++ view.movement.map(control =>
          (
            VerticalRelationPremiseRole.MovementResource(
              after.sideToMove,
              view.piece,
              RelationMovementResourceWitness(control.destination, control.target, control.mode)
            ),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            control.source
          )
        ) ++ view.legal.map(move =>
          (
            VerticalRelationPremiseRole.LegalMoveResource(RelationSnapshotOccurrence.After, legalResource(move)),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            move.source
          )
        )
      val proof = VerticalRelationDerivationProof.fromSources(
        VerticalRelationContractKind.MovementAffordanceLegalRestriction,
        premises,
        absences,
        view.states
      )
      RelationFactEvidence.from(
        RelationWitnessDetail.MovementAffordanceLegalRestriction(
          input.rootMove.witness,
          after.sideToMove,
          view.piece,
          movement,
          legal,
          unavailable,
          proof
        ),
        List(input.rootMove.moveUci)
      )
    }.sortBy(_.semanticId)

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
      } ++ exact.barrierSource.toList.map { barrier =>
        (
          VerticalRelationPremiseRole.SliderReachBarrier(
            occurrence,
            side,
            exact.slider,
            direction,
            exact.firstOccupant.getOrElse(
              throw IllegalArgumentException("a slider barrier source lost its first occupant")
            )
          ),
          input.exactPositionSource(occurrence, barrier),
          barrier
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

  private[judgment] def computeAbsolutePinMovementRestrictions(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    final case class ResourceCandidate(
        pinned: RelationPieceWitness,
        resource: PositionRelationExtractor.ClosedMovementAffordance,
        trigger: Option[(RelationFactEvidence, VerticalRelationPremiseSource)]
    )
    val namedPinRestrictions = input
      .transitionResultsFor(RelationCombinationContractKind.NamedRayTransition)
      .flatMap(relation => relation.detail match
        case RelationWitnessDetail.NamedRayTransition(
              _,
              owner,
              attacker,
              attackerRole,
              barrier,
              _,
              axis,
              RelationRayPattern.AbsoluteKingPin,
              RelationChangeDirection.Established,
              _
            ) =>
          val pinned = RelationPieceWitness(barrier.square, barrier.role)
          input.absolutePinInventoriesFor(pinned)
            .filter(view =>
              view.pinEstablished && view.restrictedSide == barrier.side && owner == !view.restrictedSide &&
                view.pinner == RelationPieceWitness(attacker, attackerRole) && view.axis == axis
            )
            .map(view =>
              absolutePinMovementRestriction(
                input.rootMove,
                view,
                view.newlyForbiddenResources,
                Some(relation -> VerticalRelationPremiseSource.Derived)
              )
            )
        case _ => Nil
      )
    val resourceCandidates = input.movementAffordanceLegalRestrictions.flatMap { restriction =>
      val triggered = restriction.triggers.map(trigger =>
        ResourceCandidate(
          restriction.piece,
          trigger.resource,
          Some(trigger.relation -> trigger.source)
        )
      )
      val triggeredResources = restriction.triggers.map(_.resource).toSet
      val stateOnly = restriction.unavailable.filterNot(triggeredResources).map(resource =>
        ResourceCandidate(restriction.piece, resource, None)
      )
      triggered ++ stateOnly
    }
      .sortBy(candidate =>
        (
          candidate.pinned.square.key,
          candidate.pinned.role.name,
          candidate.resource.destination.key,
          candidate.resource.mode.stableKey,
          candidate.trigger.map(_._1.semanticId).getOrElse("en-passant-state")
        )
      )
    require(
      resourceCandidates.map(candidate =>
        (candidate.pinned, candidate.resource, candidate.trigger.map(_._1.semanticId))
      ).distinct.size == resourceCandidates.size,
      "one maintained-pin resource cannot repeat an exact lower trigger path"
    )
    val maintainedResourceRestrictions = resourceCandidates.flatMap { candidate =>
      input.absolutePinInventoriesFor(candidate.pinned)
        .filterNot(_.pinEstablished)
        .flatMap { view =>
          val restricted = view.newlyForbiddenResources.filter(_ == candidate.resource)
          Option.when(restricted.nonEmpty)(
            absolutePinMovementRestriction(
              input.rootMove,
              view,
              restricted,
              candidate.trigger
            )
          )
        }
    }
    val results =
      (namedPinRestrictions ++ maintainedResourceRestrictions)
        .sortBy(_.semanticId)
    require(
      results.map(_.semanticId).distinct.size == results.size,
      "one absolute-pin restriction cannot repeat an exact proof path"
    )
    results

  private def absolutePinMovementRestriction(
      root: CanonicalRootLegalMove,
      view: VerticalAbsolutePinInventory,
      restrictedResources: List[PositionRelationExtractor.ClosedMovementAffordance],
      trigger: Option[(RelationFactEvidence, VerticalRelationPremiseSource)]
  ): RelationFactEvidence =
    require(
      restrictedResources.nonEmpty && restrictedResources.forall(view.newlyForbiddenResources.contains),
      "a pin claim may name only newly forbidden resources from its closed occurrence"
    )
    require(
      if view.pinEstablished then trigger.nonEmpty
      else trigger.nonEmpty || restrictedResources.forall(_.mode match
        case RelationMovementResourceMode.EnPassantCapture(_) => true
        case _                                                 => false),
      "an absolute-pin restriction needs the exact pin or movement-resource transition"
    )
    val resources = view.movementResources.map(resource =>
      RelationMovementResourceWitness(resource.destination, resource.target, resource.mode)
    ).sortBy(_.stableKey)
    val legal = view.legalResources.map(legalResource).sortBy(_.stableKey)
    val restricted = restrictedResources.map(resource =>
      RelationMovementResourceWitness(resource.destination, resource.target, resource.mode)
    ).sortBy(_.stableKey)
    val afterPinPremise = (
        VerticalRelationPremiseRole.AbsolutePinBarrier(
          RelationSnapshotOccurrence.After,
          view.restrictedSide,
          view.pinner,
          view.pinned,
          view.kingSquare,
          view.axis
        ),
        VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
        view.barrierSource
      )
    val beforePinPremises = view.previousBarrier.toList.map { case (previousPinner, source) =>
      (
        VerticalRelationPremiseRole.AbsolutePinBarrier(
          RelationSnapshotOccurrence.Before,
          view.restrictedSide,
          previousPinner,
          view.pinned,
          view.kingSquare,
          view.axis
        ),
        VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.Before),
        source
      )
    }
    val movementPremises = view.movementResources.map(resource =>
        (
          VerticalRelationPremiseRole.MovementResource(
            view.restrictedSide,
            view.pinned,
            RelationMovementResourceWitness(resource.destination, resource.target, resource.mode)
          ),
          VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
          resource.source
        )
      )
    val legalPremises = view.legalResources.map(move =>
        (
          VerticalRelationPremiseRole.LegalMoveResource(RelationSnapshotOccurrence.After, legalResource(move)),
          VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
          move.source
        )
      )
    val positionPremises = afterPinPremise :: (beforePinPremises ++ movementPremises ++ legalPremises)
    val absences = restricted.map(resource =>
      ClosedRelationAbsencePremise(
        RelationSnapshotOccurrence.After,
        PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
          view.restrictedSide,
          view.pinned.square,
          resource.destination
        )
      )
    )
    val enPassantStates = restricted.collect {
      case RelationMovementResourceWitness(
            destination,
            RelationControlTarget.Empty,
            RelationMovementResourceMode.EnPassantCapture(capturedPawn)
          ) =>
        require(
          view.afterPotentialEnPassant.contains(destination) &&
            view.beforePotentialEnPassant != view.afterPotentialEnPassant,
          "an en-passant pin restriction needs the exact newly available en-passant state"
        )
        List(
          ClosedPositionStatePremise(
            RelationSnapshotOccurrence.Before,
            PositionRelationExtractor.ClosedPositionStateQuery.PotentialEnPassant(
              view.beforePotentialEnPassant
            )
          ),
          ClosedPositionStatePremise(
            RelationSnapshotOccurrence.After,
            PositionRelationExtractor.ClosedPositionStateQuery.PotentialEnPassant(
              view.afterPotentialEnPassant
            )
          ),
          ClosedPositionStatePremise(
            RelationSnapshotOccurrence.After,
            PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(capturedPawn)
          )
        )
    }
    require(enPassantStates.size <= 1, "one pinned piece can have at most one en-passant resource")
    val triggerPremises = trigger.toList.flatMap { case (relation, source) =>
      if view.pinEstablished then
        List((
          VerticalRelationPremiseRole.PinEstablishmentTrigger(
            view.restrictedSide,
            view.pinner,
            view.pinned,
            view.kingSquare,
            view.axis,
            relation.semanticId
          ),
          source,
          relation
        ))
      else
        restrictedResources.map { resource =>
          (
            VerticalRelationPremiseRole.LegalRestrictionTrigger(
              view.restrictedSide,
              view.pinned,
              RelationMovementResourceWitness(resource.destination, resource.target, resource.mode),
              relation.semanticId
            ),
            source,
            relation
          )
        }
    }
    val proof = VerticalRelationDerivationProof.fromRootAndSources(
      VerticalRelationContractKind.AbsolutePinMovementRestriction,
      VerticalRelationPremiseRole.RootMove,
      root,
      triggerPremises ++ positionPremises,
      absences = absences,
      states = enPassantStates.flatten
    )
    RelationFactEvidence.from(
      RelationWitnessDetail.AbsolutePinMovementRestriction(
        root.witness,
        view.restrictedSide,
        view.pinner,
        view.pinned,
        view.kingSquare,
        view.axis,
        resources,
        legal,
        restricted,
        proof
      ),
      List(root.moveUci)
    )

  private[judgment] def computeSharedGeometricSupportOfEnemyControlledTargets(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    input.sharedSupportInventories.map { inventory =>
      val positionSources = inventory.targets.flatMap { target =>
        target.friendlySupports.map(control =>
          (
            VerticalRelationPremiseRole.FriendlyGeometricSupport(
              inventory.supportedSide,
              target.witness.target,
              control.controller
            ),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            control.source
          )
        ) ++ target.enemyControls.map(control =>
          (
            VerticalRelationPremiseRole.EnemyGeometricControl(
              inventory.controllingSide,
              target.witness.target,
              control.controller
            ),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            control.source
          )
        )
      }
      val transitionSources = inventory.changedControls.map { case (target, relation) =>
        (
          VerticalRelationPremiseRole.EnemyGeometricControlChange(inventory.controllingSide, target),
          VerticalRelationPremiseSource.Derived,
          relation
        )
      }
      val proof = VerticalRelationDerivationProof.fromSources(
        VerticalRelationContractKind.SharedGeometricSupportOfEnemyControlledTargets,
        transitionSources ++ positionSources
      )
      RelationFactEvidence.from(
        RelationWitnessDetail.SharedGeometricSupportOfEnemyControlledTargets(
          inventory.mover,
          inventory.controllingSide,
          inventory.supportedSide,
          inventory.sharedSupporter,
          inventory.targets.map(_.witness),
          proof
        ),
        List(input.rootMove.moveUci)
      )
    }.sortBy(_.semanticId)

  /** Exact after-position contact inventory for one controller. At least one
    * enemy-piece control edge must be new in this transition and the same
    * controller must geometrically contact at least two enemy pieces. This is
    * deliberately not named a fork and makes no claim about legal captures or
    * material gain.
    */
  private[judgment] def computeGeometricMultiTargetContacts(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    input.enemyContactInventories.flatMap { inventory =>
      val newlyEstablished = inventory.newlyEstablished
      val maintained = inventory.maintained
      Option.when(inventory.targets.size >= 2 && newlyEstablished.nonEmpty) {
        val changedSources = newlyEstablished.map { target =>
          val change = target.change.getOrElse(
            throw IllegalArgumentException("a newly established enemy contact needs its exact delta")
          )
          (
            VerticalRelationPremiseRole.NewEnemyControl(
              inventory.controllingSide,
              target.target,
              inventory.controllerBefore,
              inventory.controllerAfter
            ),
            VerticalRelationPremiseSource.Derived,
            change.relation
          )
        } ++ maintained.flatMap(target => target.change.toList.map(change =>
          (
            VerticalRelationPremiseRole.ContinuedEnemyControlChange(
              inventory.controllingSide,
              target.target,
              inventory.controllerBefore,
              inventory.controllerAfter
            ),
            VerticalRelationPremiseSource.Derived,
            change.relation
          )
        ))
        val beforeSources = maintained.map { target =>
          val control = target.beforeControl.getOrElse(
            throw IllegalArgumentException("a maintained enemy contact needs its exact before edge")
          )
          (
            VerticalRelationPremiseRole.BeforeEnemyControl(
              inventory.controllingSide,
              target.target,
              inventory.controllerBefore
            ),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.Before),
            control.source
          )
        }
        val afterSources = inventory.targets.map(target =>
          (
            VerticalRelationPremiseRole.AfterEnemyControl(
              inventory.controllingSide,
              target.target,
              inventory.controllerAfter
            ),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            target.afterControl.source
          )
        )
        val proof = VerticalRelationDerivationProof.fromSources(
          VerticalRelationContractKind.GeometricMultiTargetContact,
          changedSources ++ beforeSources ++ afterSources,
          Nil
        )
        RelationFactEvidence.from(
          RelationWitnessDetail.GeometricMultiTargetContact(
            inventory.mover,
            inventory.controllingSide,
            inventory.controllerBefore,
            inventory.controllerAfter,
            newlyEstablished.map(_.target),
            maintained.map(_.target),
            proof
          ),
          List(input.rootMove.moveUci)
        )
      }.toList
    }.sortBy(_.semanticId)

  private def deriveGeometricEnemyContactsWithoutFriendlySupport(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    input.unsupportedEnemyContacts.map { unsupported =>
      val inventory = unsupported.inventory
      val target = unsupported.target
      val change = target.change.getOrElse(
        throw IllegalArgumentException("an unsupported new contact needs its exact positive delta")
      )
      val absence = ClosedRelationAbsencePremise(
        RelationSnapshotOccurrence.After,
        PositionRelationExtractor.ClosedRelationAbsenceQuery.GeometricFriendlySupportOf(
          !inventory.controllingSide,
          target.target.square
        )
      )
      val proof = VerticalRelationDerivationProof.fromSources(
        VerticalRelationContractKind.GeometricEnemyContactWithoutFriendlySupport,
        List(
          (
            VerticalRelationPremiseRole.NewEnemyControl(
              inventory.controllingSide,
              target.target,
              inventory.controllerBefore,
              inventory.controllerAfter
            ),
            VerticalRelationPremiseSource.Derived,
            change.relation
          ),
          (
            VerticalRelationPremiseRole.AfterEnemyControl(
              inventory.controllingSide,
              target.target,
              inventory.controllerAfter
            ),
            VerticalRelationPremiseSource.Position(RelationPremiseOccurrence.After),
            target.afterControl.source
          )
        ),
        List(absence)
      )
      RelationFactEvidence.from(
        RelationWitnessDetail.GeometricEnemyContactWithoutFriendlySupport(
          inventory.mover,
          inventory.controllingSide,
          inventory.controllerBefore,
          inventory.controllerAfter,
          target.target,
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

  private[judgment] def computePawnOccupiedFilePartitionTransitions(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    def filePremises(
        occurrence: RelationSnapshotOccurrence,
        side: _root_.chess.Color,
        runs: List[RelationPawnOccupiedFileRunWitness],
        sources: List[RelationFactEvidence]
    ): List[(VerticalRelationPremiseRole, VerticalRelationPremiseSource, RelationFactEvidence)] =
      val exact = sources.map { source =>
        val (file, pawns) = source.detail match
          case RelationWitnessDetail.PawnFileGroup(owner, exactFile, exactPawns) if owner == side =>
            exactFile -> exactPawns
          case _ =>
            throw IllegalArgumentException("an occupied-file partition proof lost a side-specific PawnFileGroup source")
        (
          VerticalRelationPremiseRole.FilePawnGroup(occurrence, side, file, pawns),
          input.exactPositionSource(occurrence, source),
          source
        )
      }.sortBy { case (role, _, _) => VerticalRelationPremiseRole.id(role) }
      val expected = File.all.map { file =>
        val exactFile = EvidenceFile((('a'.toInt + file.value).toChar).toString)
        val pawns = runs.flatMap(_.pawns)
          .filter(EvidenceFile.contains(exactFile, _)).sortBy(_.key)
        VerticalRelationPremiseRole.FilePawnGroup(occurrence, side, exactFile, pawns)
      }.toSet
      require(
        exact.map { case (role, _, _) => role }.toSet == expected && exact.size == expected.size,
        "an occupied-file partition proof needs every side-specific board file exactly once"
      )
      exact

    input.pawnOccupiedFilePartitionTransitions.map { view =>
      val proof = VerticalRelationDerivationProof.fromRootAndSources(
        VerticalRelationContractKind.PawnOccupiedFilePartitionTransition,
        VerticalRelationPremiseRole.RootMove,
        input.rootMove,
        filePremises(RelationSnapshotOccurrence.Before, view.side, view.before, view.beforeFileSources) ++
          filePremises(RelationSnapshotOccurrence.After, view.side, view.after, view.afterFileSources),
        states = view.states
      )
      RelationFactEvidence.from(
        RelationWitnessDetail.PawnOccupiedFilePartitionTransition(
          input.rootMove.witness,
          view.side,
          view.before,
          view.after,
          proof
        ),
        List(input.rootMove.moveUci)
      )
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
        RelationPawnTopologyFacet.EnemyPawnContacts -> contacts.map(_._2).sortBy(_.semanticId),
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

  private def deriveMajorPiecePawnFileCorridorTransitions(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    val root = input.rootMove

    def filePremises(
        occurrence: RelationSnapshotOccurrence,
        sources: List[RelationFactEvidence],
        state: RelationPawnFileStateWitness
    ): List[(VerticalRelationPremiseRole, VerticalRelationPremiseSource, RelationFactEvidence)] =
      val exact = sources.map { source =>
        val (side, pawns) = source.detail match
          case RelationWitnessDetail.PawnFileGroup(owner, exactFile, exactPawns)
              if exactFile == state.file => owner -> exactPawns
          case _ =>
            throw IllegalArgumentException("a pawn-file corridor proof lost an exact PawnFileGroup source")
        (
          VerticalRelationPremiseRole.FilePawnGroup(occurrence, side, state.file, pawns),
          input.exactPositionSource(occurrence, source),
          source
        )
      }.sortBy { case (role, _, _) => VerticalRelationPremiseRole.id(role) }
      require(
        exact.map { case (role, _, _) => role }.toSet == Set(
          VerticalRelationPremiseRole.FilePawnGroup(
            occurrence,
            _root_.chess.Color.White,
            state.file,
            state.whitePawns
          ),
          VerticalRelationPremiseRole.FilePawnGroup(
            occurrence,
            _root_.chess.Color.Black,
            state.file,
            state.blackPawns
          )
        ),
        "a pawn-file corridor proof needs both sides' exact pawn-file state"
      )
      exact

    def corridorPremises(
        occurrence: RelationSnapshotOccurrence,
        view: VerticalMajorPiecePawnFileCorridorInput,
        corridor: Option[VerticalFileCorridorInput]
    ): List[(VerticalRelationPremiseRole, VerticalRelationPremiseSource, RelationFactEvidence)] =
      corridor.toList.flatMap(_.rays.flatMap(ray =>
        sliderReachSources(input, occurrence, view.side, ray.direction, ray.reach)
      ))

    input.majorPiecePawnFileCorridorTransitions.map { view =>
      val premises =
        view.occurrenceChange.toList.map(change => (
          VerticalRelationPremiseRole.SliderOccurrenceChange(change),
          VerticalRelationPremiseSource.RootTransition,
          root.fact
        )) ++
          filePremises(
            RelationSnapshotOccurrence.Before,
            view.beforePawnFile.sourceGroups,
            pawnFileState(view.beforePawnFile)
          ) ++
          filePremises(
            RelationSnapshotOccurrence.After,
            view.afterPawnFile.sourceGroups,
            pawnFileState(view.afterPawnFile)
          ) ++
          corridorPremises(RelationSnapshotOccurrence.Before, view, view.beforeCorridor) ++
          corridorPremises(RelationSnapshotOccurrence.After, view, view.afterCorridor)
      val proof = VerticalRelationDerivationProof.fromRootAndSources(
        VerticalRelationContractKind.MajorPiecePawnFileCorridorTransition,
        VerticalRelationPremiseRole.RootMove,
        root,
        premises,
        states = view.states
      )
      RelationFactEvidence.from(
        RelationWitnessDetail.MajorPiecePawnFileCorridorTransition(
          root.witness,
          view.side,
          view.file,
          view.sliderBefore,
          view.sliderAfter,
          view.occurrenceChange,
          pawnFileState(view.beforePawnFile),
          pawnFileState(view.afterPawnFile),
          view.beforeCorridor.map(_.witness),
          view.afterCorridor.map(_.witness),
          proof
        ),
        List(root.moveUci)
      )
    }.sortBy(_.semanticId)

  private[judgment] def computeCastlingRightRemovals(
      input: VerticalRelationInput
  ): List[RelationFactEvidence] =
    input.castlingRightRemovals.map { removal =>
      val proof = VerticalRelationDerivationProof.fromRootAndSources(
        VerticalRelationContractKind.CastlingRightRemoved,
        VerticalRelationPremiseRole.RootMove,
        input.rootMove,
        Nil,
        states = removal.states
      )
      RelationFactEvidence.from(
        RelationWitnessDetail.CastlingRightRemoved(
          input.rootMove.witness,
          removal.side,
          removal.flank,
          proof
        ),
        List(input.rootMove.moveUci)
      )
    }.sortBy(_.semanticId)

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

  private def supportChangeSubject(
      supportDelta: RelationFactEvidence
  ): Option[SupportChangeSubject] =
    supportDelta.detail match
      case RelationWitnessDetail.GeometricSupportDelta(
            mover,
            supportedSide,
            supportedBefore,
            supportedBeforeRole,
            supportedAfter,
            supportedAfterRole,
            beforeSupporters,
            afterSupporters,
            removedSupporters,
            establishedSupporters,
            _
          ) if removedSupporters.nonEmpty || establishedSupporters.nonEmpty =>
        Some(SupportChangeSubject(
          supportDelta,
          mover,
          supportedSide,
          supportedBefore,
          supportedBeforeRole,
          supportedAfter,
          supportedAfterRole,
          beforeSupporters,
          afterSupporters,
          removedSupporters,
          establishedSupporters
        ))
      case _ => None

  private def causeEvidence(
      relation: RelationFactEvidence
  ): List[SupportChangeCauseEvidence] =
    relation.detail match
      case RelationWitnessDetail.GeometricSupporterCapture(
            _,
            supporterSquare,
            supporterRole,
            supportedSquare,
            supportedRole,
            _
          ) =>
        List(SupportChangeCauseEvidence(
          relation,
          SupportChangeJoinKey(
            RelationChangeDirection.Removed,
            supportedSquare,
            supportedRole,
            supporterSquare,
            supporterRole
          ),
          None,
          RelationSupportChangeCause.SupporterCaptured
        ))
      case RelationWitnessDetail.SliderLineInterruption(
            _,
            controllerSide,
            controllerBeforeSquare,
            _,
            controllerRole,
            interruptedTargets,
            _
          ) =>
        interruptedTargets.collect {
          case RelationControlReachWitness(targetSquare, RelationControlTarget.Friendly(targetRole)) =>
            SupportChangeCauseEvidence(
              relation,
              SupportChangeJoinKey(
                RelationChangeDirection.Removed,
                targetSquare,
                targetRole,
                controllerBeforeSquare,
                controllerRole
              ),
              Some(controllerSide),
              RelationSupportChangeCause.SliderLineInterrupted
            )
        }
      case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(
            _,
            controllerSide,
            _,
            controllerAfterSquare,
            controllerRole,
            _,
            _,
            openedTargets,
            _,
            _,
            _
          ) =>
        openedTargets.collect {
          case RelationControlReachWitness(targetSquare, RelationControlTarget.Friendly(targetRole)) =>
            SupportChangeCauseEvidence(
              relation,
              SupportChangeJoinKey(
                RelationChangeDirection.Established,
                targetSquare,
                targetRole,
                controllerAfterSquare,
                controllerRole
              ),
              Some(controllerSide),
              RelationSupportChangeCause.SliderLineOpened
            )
        }
      case _ => Nil

  private def supportCausalTransitionProof(
      subject: SupportChangeSubject,
      causesByChangedRelation: Map[SupportChangeJoinKey, List[SupportChangeCauseEvidence]],
      root: CanonicalRootLegalMove
  ): List[RelationFactEvidence] =
    val targetRelocation = supportedPieceRelocation(subject)
    val removalChoices = subject.removedSupporters.map { supporter =>
      val supporterMoved = supporterRelocation(subject, supporter).map { movement =>
        SupportRemovalExplanation(
          supporter,
          RelationSupportChangeCause.SupporterRelocated(movement),
          root.fact,
          VerticalRelationPremiseSource.RootTransition
        )
      }.toList
      val targetMoved = targetRelocation.map { movement =>
        SupportRemovalExplanation(
          supporter,
          RelationSupportChangeCause.SupportedPieceRelocated(movement),
          root.fact,
          VerticalRelationPremiseSource.RootTransition
        )
      }.toList
      val relational = causesByChangedRelation
        .getOrElse(subject.removalJoinKey(supporter), Nil)
        .flatMap(cause => explanationFromCause(subject, supporter, cause).toList)
      (supporterMoved ++ targetMoved ++ relational).sortBy(_.stableKey)
    }
    val establishmentChoices = subject.establishedSupporters.map { supporter =>
      val supporterMoved = establishedSupporterRelocation(subject, supporter).map { movement =>
        SupportEstablishmentExplanation(
          supporter,
          RelationSupportChangeCause.SupporterRelocated(movement),
          root.fact,
          VerticalRelationPremiseSource.RootTransition
        )
      }.toList
      val targetMoved = targetRelocation.map { movement =>
        SupportEstablishmentExplanation(
          supporter,
          RelationSupportChangeCause.SupportedPieceRelocated(movement),
          root.fact,
          VerticalRelationPremiseSource.RootTransition
        )
      }.toList
      val relational = causesByChangedRelation
        .getOrElse(subject.establishmentJoinKey(supporter), Nil)
        .flatMap(cause => establishmentExplanationFromCause(subject, supporter, cause).toList)
      (supporterMoved ++ targetMoved ++ relational).sortBy(_.stableKey)
    }
    require(
      removalChoices.forall(_.nonEmpty),
      s"every removed support relation must retain an exact transition cause for ${subject.supportedBefore.key}"
    )
    require(
      establishmentChoices.forall(_.nonEmpty),
      s"every established support relation must retain an exact transition cause for ${subject.supportedAfter.key}"
    )

    List(supportCausalTransition(subject, removalChoices, establishmentChoices))

  private def explanationFromCause(
      subject: SupportChangeSubject,
      supporter: RelationPieceWitness,
      evidence: SupportChangeCauseEvidence
  ): Option[SupportRemovalExplanation] =
    val exactSide = evidence.cause match
      case RelationSupportChangeCause.SupporterCaptured =>
        subject.supportedSide != subject.mover.side
      case RelationSupportChangeCause.SliderLineInterrupted =>
        evidence.controllerSide.contains(subject.supportedSide)
      case RelationSupportChangeCause.SliderLineOpened => false
      case _: RelationSupportChangeCause.SupporterRelocated |
          _: RelationSupportChangeCause.SupportedPieceRelocated =>
        false
    Option.when(evidence.joinKey == subject.removalJoinKey(supporter) && exactSide)(
      SupportRemovalExplanation(
        supporter,
        evidence.cause,
        evidence.relation,
        VerticalRelationPremiseSource.Derived
      )
    )

  private def establishmentExplanationFromCause(
      subject: SupportChangeSubject,
      supporter: RelationPieceWitness,
      evidence: SupportChangeCauseEvidence
  ): Option[SupportEstablishmentExplanation] =
    val exactSide = evidence.cause match
      case RelationSupportChangeCause.SliderLineOpened =>
        evidence.controllerSide.contains(subject.supportedSide)
      case _ => false
    Option.when(evidence.joinKey == subject.establishmentJoinKey(supporter) && exactSide)(
      SupportEstablishmentExplanation(
        supporter,
        evidence.cause,
        evidence.relation,
        VerticalRelationPremiseSource.Derived
      )
    )

  private def supporterRelocation(
      subject: SupportChangeSubject,
      supporter: RelationPieceWitness
  ): Option[RelationMoveTransitionWitness] =
    subject.relation.rootTransitionFootprint.flatMap(_.pieceTransitions.find(transition =>
      transition.side == subject.supportedSide &&
        transition.from.key.equalsIgnoreCase(supporter.square.key) &&
        transition.beforeRole.name.equalsIgnoreCase(supporter.role.name)
    ).map(RelationMoveTransitionWitness.from))

  private def establishedSupporterRelocation(
      subject: SupportChangeSubject,
      supporter: RelationPieceWitness
  ): Option[RelationMoveTransitionWitness] =
    subject.relation.rootTransitionFootprint.flatMap(_.pieceTransitions.find(transition =>
      transition.side == subject.supportedSide &&
        transition.to.key.equalsIgnoreCase(supporter.square.key) &&
        transition.afterRole.name.equalsIgnoreCase(supporter.role.name)
    ).map(RelationMoveTransitionWitness.from))

  private def supportedPieceRelocation(
      subject: SupportChangeSubject
  ): Option[RelationMoveTransitionWitness] =
    subject.relation.rootTransitionFootprint.flatMap(_.pieceTransitions.find(transition =>
      transition.side == subject.supportedSide &&
        transition.from.key.equalsIgnoreCase(subject.supportedBefore.key) &&
        transition.to.key.equalsIgnoreCase(subject.supportedAfter.key) &&
        transition.beforeRole.name.equalsIgnoreCase(subject.supportedBeforeRole.name) &&
        transition.afterRole.name.equalsIgnoreCase(subject.supportedAfterRole.name)
    ).map(RelationMoveTransitionWitness.from))

  private def supportCausalTransition(
      subject: SupportChangeSubject,
      removalsBySupporter: List[List[SupportRemovalExplanation]],
      establishmentsBySupporter: List[List[SupportEstablishmentExplanation]]
  ): RelationFactEvidence =
    val explainedSupporters = removalsBySupporter.map(_.head.supporter)
    require(
      removalsBySupporter.forall(alternatives =>
        alternatives.forall(_.supporter == alternatives.head.supporter) &&
          alternatives.map(_.stableKey).distinct.size == alternatives.size
      ) && explainedSupporters.distinct.size == explainedSupporters.size &&
        explainedSupporters.toSet == subject.removedSupporters.toSet,
      "a support transition must explain every removed supporter exactly once"
    )
    val explainedEstablishments = establishmentsBySupporter.map(_.head.supporter)
    require(
      establishmentsBySupporter.forall(alternatives =>
        alternatives.forall(_.supporter == alternatives.head.supporter) &&
          alternatives.map(_.stableKey).distinct.size == alternatives.size
      ) && explainedEstablishments.distinct.size == explainedEstablishments.size &&
        explainedEstablishments.toSet == subject.establishedSupporters.toSet &&
        (removalsBySupporter.nonEmpty || establishmentsBySupporter.nonEmpty),
      "a support transition must explain every established supporter exactly once"
    )
    val absences = Option.when(subject.afterSupporters.isEmpty)(
      ClosedRelationAbsencePremise(
        RelationSnapshotOccurrence.After,
        PositionRelationExtractor.ClosedRelationAbsenceQuery.GeometricFriendlySupportOf(
          subject.supportedSide,
          subject.supportedAfter
        )
      )
    ).toList
    val proof = VerticalRelationDerivationProof.fromSources(
      VerticalRelationContractKind.GeometricSupportCausalTransition,
      (
        VerticalRelationPremiseRole.SupportSetDelta(
          subject.supportedSide,
          RelationPieceWitness(subject.supportedBefore, subject.supportedBeforeRole),
          RelationPieceWitness(subject.supportedAfter, subject.supportedAfterRole),
          subject.beforeSupporters,
          subject.afterSupporters,
          subject.removedSupporters,
          subject.establishedSupporters
        ),
        VerticalRelationPremiseSource.Derived,
        subject.relation
      ) :: (removalsBySupporter.flatten.map { explanation =>
        (
          VerticalRelationPremiseRole.SupportRemovalCause(
            RelationPieceWitness(subject.supportedBefore, subject.supportedBeforeRole),
            explanation.supporter,
            explanation.cause
          ),
          explanation.premiseSource,
          explanation.source
        )
      } ++ establishmentsBySupporter.flatten.map { explanation =>
        (
          VerticalRelationPremiseRole.SupportEstablishmentCause(
            RelationPieceWitness(subject.supportedAfter, subject.supportedAfterRole),
            explanation.supporter,
            explanation.cause
          ),
          explanation.premiseSource,
          explanation.source
        )
      }),
      absences
    )
    val removals = removalsBySupporter
      .map(alternatives =>
        RelationSupportRemovalWitness(
          alternatives.head.supporter,
          alternatives.map(_.cause).distinct.sortBy(_.stableKey)
        )
      )
      .sortBy(removal =>
        (removal.supporter.square.key, removal.supporter.role.name)
      )
    val establishments = establishmentsBySupporter
      .map(alternatives =>
        RelationSupportEstablishmentWitness(
          alternatives.head.supporter,
          alternatives.map(_.cause).distinct.sortBy(_.stableKey)
        )
      )
      .sortBy(establishment =>
        (establishment.supporter.square.key, establishment.supporter.role.name)
      )
    RelationFactEvidence.from(
      RelationWitnessDetail.GeometricSupportCausalTransition(
        subject.mover,
        subject.supportedSide,
        subject.supportedBefore,
        subject.supportedBeforeRole,
        subject.supportedAfter,
        subject.supportedAfterRole,
        subject.beforeSupporters,
        subject.afterSupporters,
        removals,
        establishments,
        proof
      ),
      subject.relation.lineMoves
    )
