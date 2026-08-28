package lila.chessjudgment.analysis.position

import chess.{ Bishop, Board, Castles, Color, File, King, Move, Pawn, Position, Queen, Role, Rook, Side, Square }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.position.*

object PositionRelationExtractor:

  private[position] enum BoardRelationInventoryDomain:
    case StaticBoard
    case PositionOccurrence

  private def inventoryDomain(kind: RelationFactKind): Option[BoardRelationInventoryDomain] =
    import BoardRelationInventoryDomain.*
    kind match
      case RelationFactKind.GeometricControl | RelationFactKind.PawnFileGroup |
          RelationFactKind.PawnFrontOccupancy |
          RelationFactKind.PawnAdvanceAffordance | RelationFactKind.PawnPassage |
          RelationFactKind.RayBarrier =>
        Some(StaticBoard)
      case RelationFactKind.LegalMove =>
        Some(PositionOccurrence)
      case RelationFactKind.GeometricControlSetDelta | RelationFactKind.GeometricSupporterCapture |
          RelationFactKind.GeometricSupportDelta |
          RelationFactKind.SliderLineInterruption |
          RelationFactKind.GeometricLineControlAfterBlockerRemoval |
          RelationFactKind.NamedRayTransition |
          RelationFactKind.GeometricSupportCausalTransition |
          RelationFactKind.CaptureRecaptureInventory |
          RelationFactKind.CreatedCheckResponseInventory |
          RelationFactKind.RootCheckResponse |
          RelationFactKind.MovementAffordanceLegalRestriction |
          RelationFactKind.AbsolutePinMovementRestriction |
          RelationFactKind.SliderReachDelta |
          RelationFactKind.GeometricMultiTargetContact |
          RelationFactKind.GeometricEnemyContactWithoutFriendlySupport |
          RelationFactKind.SharedGeometricSupportOfEnemyControlledTargets |
          RelationFactKind.PawnTopologyTransition |
          RelationFactKind.PawnOccupiedFilePartitionTransition |
          RelationFactKind.MajorPiecePawnFileCorridorTransition |
          RelationFactKind.CastlingRightRemoved |
          RelationFactKind.StalemateTransition =>
        None

  private[position] final case class BoardRelationRefreshFootprint(
      keys: Set[RelationDependencyKey]
  ):
    require(keys.nonEmpty, "a legal transition must refresh at least one relation dependency")

  private[position] final case class BoardRelationDetailTransition(
      removed: List[CanonicalRelationFact],
      established: List[CanonicalRelationFact]
  ):
    require(
      removed.map(_.semanticId).distinct.size == removed.size,
      "removed relation facts must be unique"
    )
    require(
      established.map(_.semanticId).distinct.size == established.size,
      "established relation facts must be unique"
    )
    require(
      removed.map(_.semanticId).toSet.intersect(established.map(_.semanticId).toSet).isEmpty,
      "a relation fact cannot be both removed and established"
    )

  private[position] final case class BoardRelationUpdate(
      snapshot: BoardRelationSnapshot,
      transition: BoardRelationDetailTransition
  )

  private[position] final class BoardRelationSnapshot private (
      private[position] val domain: BoardRelationInventoryDomain,
      private[position] val factsByDetail: Map[RelationWitnessDetail, CanonicalRelationFact],
      factsBySemanticId: Map[String, CanonicalRelationFact],
      detailsByRefreshKey: Map[RelationDependencyKey, Set[RelationWitnessDetail]],
      refreshKeysByDetail: Map[RelationWitnessDetail, Set[RelationDependencyKey]]
  ):
    require(
      factsByDetail.values.forall(fact => inventoryDomain(fact.kind).contains(domain)),
      s"$domain inventory contains a relation owned by another producer domain"
    )
    private[position] def factsUnordered: Iterable[CanonicalRelationFact] = factsByDetail.values

    private lazy val factsByKind: Map[RelationFactKind, List[CanonicalRelationFact]] =
      factsByDetail.values.toList.groupBy(_.kind).view.mapValues(sortFacts).toMap

    private[position] def factsOf(kind: RelationFactKind): List[CanonicalRelationFact] =
      factsByKind.getOrElse(kind, Nil)

    private[position] def semanticIds: Set[String] = factsBySemanticId.keySet

    private[position] def factBySemanticId(semanticId: String): Option[CanonicalRelationFact] =
      factsBySemanticId.get(semanticId)

    private[position] def factCount: Int = factsBySemanticId.size

    private[position] def factsFor(
        keys: Set[RelationDependencyKey]
    ): List[CanonicalRelationFact] =
      sortFacts(detailsFor(keys).toList.map(factsByDetail))

    def transitionTo(
        after: BoardRelationSnapshot,
        refresh: BoardRelationRefreshFootprint
    ): BoardRelationDetailTransition =
      require(domain == after.domain, "relation transition inventories must share one producer domain")
      val beforeAffected = detailsFor(refresh.keys)
      val afterAffected = after.detailsFor(refresh.keys)
      val removedDetails = beforeAffected -- afterAffected
      val establishedDetails = afterAffected -- beforeAffected
      BoardRelationDetailTransition(
        removed = sortFacts(removedDetails.toList.map(factsByDetail)),
        established = sortFacts(establishedDetails.toList.map(after.factsByDetail))
      )

    def updated(
        refreshed: List[RelationWitnessDetail],
        refresh: BoardRelationRefreshFootprint
    ): BoardRelationUpdate =
      require(
        domain == BoardRelationInventoryDomain.StaticBoard,
        "only the static-board inventory supports sparse dependency refresh"
      )
      require(refreshed.distinct.size == refreshed.size, "one incremental producer emitted duplicate relation details")
      val affected = detailsFor(refresh.keys)
      val refreshedSet = refreshed.toSet
      val refreshedFacts = refreshedSet.map(detail =>
        detail -> factsByDetail.getOrElse(detail, CanonicalRelationFact.from(detail, Nil))
      ).toMap
      require(
        refreshedFacts.values.forall(fact =>
          fact.dependencyFootprint.exists(_.keys.exists(refresh.keys))
        ),
        "an incrementally produced relation must belong to the exact refresh footprint"
      )
      val nextFactsByDetail = (factsByDetail -- affected) ++ refreshedFacts
      val affectedSemanticIds = affected.flatMap(detail => factsByDetail.get(detail).map(_.semanticId))
      var nextFactsBySemanticId = factsBySemanticId -- affectedSemanticIds
      refreshedFacts.values.foreach { fact =>
        nextFactsBySemanticId.get(fact.semanticId) match
          case Some(existing) if existing.detail == fact.detail => ()
          case Some(existing) =>
            throw IllegalArgumentException(
              s"canonical relation semantic id collision: ${fact.semanticId} " +
                s"(${RelationWitnessDetail.stableKey(existing.detail)} != ${RelationWitnessDetail.stableKey(fact.detail)})"
            )
          case None =>
            nextFactsBySemanticId = nextFactsBySemanticId.updated(fact.semanticId, fact)
      }
      val nextKeysByDetail = (refreshKeysByDetail -- affected) ++ refreshedFacts.map { case (detail, fact) =>
        detail -> fact.dependencyFootprint.map(_.keys).getOrElse(
          throw IllegalArgumentException(s"non-board relation '${detail.detailName}' entered the board relation snapshot")
        )
      }
      val refreshedByKey = refreshedSet.toList
        .flatMap(detail => nextKeysByDetail(detail).map(_ -> detail))
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.toSet)
        .toMap
      val touchedKeys = affected.flatMap(refreshKeysByDetail) ++ refreshedByKey.keySet
      val nextByRefreshKey = touchedKeys.foldLeft(detailsByRefreshKey) { (indexed, key) =>
        val retained = indexed.getOrElse(key, Set.empty) -- affected
        val added = refreshedByKey.getOrElse(key, Set.empty)
        val values = retained ++ added
        if values.isEmpty then indexed - key else indexed.updated(key, values)
      }
      val snapshot = new BoardRelationSnapshot(
        domain,
        nextFactsByDetail,
        nextFactsBySemanticId,
        nextByRefreshKey,
        nextKeysByDetail
      )
      BoardRelationUpdate(
        snapshot = snapshot,
        transition = BoardRelationDetailTransition(
          removed = sortFacts((affected -- refreshedSet).toList.map(factsByDetail)),
          established = sortFacts((refreshedSet -- affected).toList.map(refreshedFacts))
        )
      )

    private def detailsFor(keys: Set[RelationDependencyKey]): Set[RelationWitnessDetail] =
      keys.flatMap(key => detailsByRefreshKey.getOrElse(key, Set.empty))

  private[position] object BoardRelationSnapshot:
    def from(
        domain: BoardRelationInventoryDomain,
        details: List[RelationWitnessDetail]
    ): BoardRelationSnapshot =
      require(details.distinct.size == details.size, "one cold producer emitted duplicate relation details")
      val factsByDetail = details.map(detail => detail -> CanonicalRelationFact.from(detail, Nil)).toMap
      val factsBySemanticId = factsByDetail.values.map(fact => fact.semanticId -> fact).toMap
      require(
        factsBySemanticId.size == factsByDetail.size,
        "one cold producer emitted colliding canonical relation semantic ids"
      )
      val keysByDetail = factsByDetail.map { case (detail, fact) =>
        detail -> fact.dependencyFootprint.map(_.keys).getOrElse(
          throw IllegalArgumentException(s"non-board relation '${detail.detailName}' entered the board relation snapshot")
        )
      }
      val byKey = keysByDetail.toList
        .flatMap { case (detail, keys) => keys.map(_ -> detail) }
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.toSet)
        .toMap
      new BoardRelationSnapshot(domain, factsByDetail, factsBySemanticId, byKey, keysByDetail)

  private[position] final case class PositionRelationSnapshot(
      staticBoard: BoardRelationSnapshot,
      occurrence: BoardRelationSnapshot
  ):
    require(
      staticBoard.domain == BoardRelationInventoryDomain.StaticBoard &&
        occurrence.domain == BoardRelationInventoryDomain.PositionOccurrence,
      "a closed position inventory requires exactly one static and one occurrence producer"
    )
    require(
      staticBoard.semanticIds.intersect(occurrence.semanticIds).isEmpty,
      "static and occurrence relation owners cannot emit the same semantic fact"
    )

    lazy val facts: List[CanonicalRelationFact] =
      sortFacts((staticBoard.factsUnordered ++ occurrence.factsUnordered).toList)

    private[position] def factsOf(kind: RelationFactKind): List[CanonicalRelationFact] =
      sortFacts(staticBoard.factsOf(kind) ++ occurrence.factsOf(kind))

    private[position] def factBySemanticId(semanticId: String): Option[CanonicalRelationFact] =
      staticBoard.factBySemanticId(semanticId).orElse(occurrence.factBySemanticId(semanticId))

    private[position] def factCount: Int = staticBoard.factCount + occurrence.factCount

    private[position] def factsFor(
        keys: Set[RelationDependencyKey]
    ): List[CanonicalRelationFact] =
      sortFacts(staticBoard.factsFor(keys) ++ occurrence.factsFor(keys))

  private[chessjudgment] final case class ClosedCastlingRights private[position] (
      whiteKingSide: Boolean,
      whiteQueenSide: Boolean,
      blackKingSide: Boolean,
      blackQueenSide: Boolean
  ):
    def allows(side: Color, castleSide: Side): Boolean =
      (side, castleSide) match
        case (Color.White, Side.KingSide)  => whiteKingSide
        case (Color.White, Side.QueenSide) => whiteQueenSide
        case (Color.Black, Side.KingSide)  => blackKingSide
        case (Color.Black, Side.QueenSide) => blackQueenSide

  /** One existing canonical control edge viewed in controller- and
    * target-centric directions. `source` remains the sole graph fact.
    */
  private[chessjudgment] final case class ClosedGeometricControl private[position] (
      source: RelationFactEvidence,
      side: Color,
      controller: RelationPieceWitness,
      targetSquare: EvidenceSquare,
      target: RelationControlTarget
  ):
    def friendlySupport: Option[RelationPieceWitness] =
      target match
        case RelationControlTarget.Friendly(role) => Some(RelationPieceWitness(targetSquare, role))
        case _                                    => None

  private[position] object ClosedGeometricControl:
    def from(source: RelationFactEvidence): ClosedGeometricControl =
      source.detail match
        case RelationWitnessDetail.GeometricControl(side, controller, role, target, targetState) =>
          ClosedGeometricControl(
            source,
            side,
            RelationPieceWitness(controller, role),
            target,
            targetState
          )
        case _ =>
          throw IllegalArgumentException(s"relation '${source.semanticId}' is not a geometric control")

  private[chessjudgment] final case class ClosedPawnAdvanceAffordance private[position] (
      source: RelationFactEvidence,
      side: Color,
      pawn: EvidenceSquare,
      destination: EvidenceSquare,
      traversedSquares: List[EvidenceSquare]
  ):
    require(
      traversedSquares.nonEmpty && traversedSquares.size <= 2 && traversedSquares.last == destination,
      "a closed pawn advance must retain its exact empty corridor"
    )

  private[chessjudgment] final case class ClosedMovementAffordance private[position] (
      source: RelationFactEvidence,
      piece: RelationPieceWitness,
      destination: EvidenceSquare,
      target: RelationControlTarget,
      mode: RelationMovementResourceMode
  )

  /** One complete currently reachable slider segment in one direction. It is
    * projected only from canonical control edges and the optional RayBarrier
    * fact for the first occupant; no board ray is scanned again.
    */
  private[chessjudgment] final case class ClosedSliderReach private[position] (
      side: Color,
      slider: RelationPieceWitness,
      direction: RelationRayDirection,
      controls: List[ClosedGeometricControl],
      firstOccupant: Option[RelationColoredPieceWitness],
      barrierSource: Option[RelationFactEvidence]
  ):
    require(controls.nonEmpty, "a closed slider reach needs one controlled square")
    require(
      controls.forall(control => control.side == side && control.controller == slider),
      "a closed slider reach must retain one exact controller occurrence"
    )
    require(
      firstOccupant.nonEmpty == barrierSource.nonEmpty,
      "a closed slider reach must bind its first occupant to the canonical ray barrier"
    )

    def segment: List[RelationControlReachWitness] =
      controls.map(control => RelationControlReachWitness(control.targetSquare, control.target))

    def witness: RelationSliderReachWitness =
      RelationSliderReachWitness(segment, firstOccupant)

  private[chessjudgment] enum ClosedLegalMovementMode:
    case ControlledDestination
    case PawnAdvance
    case PawnDoubleAdvance
    case Castling

  private[chessjudgment] final case class ClosedBoardCellChange private[position] (
      square: EvidenceSquare,
      before: Option[RelationColoredPieceWitness],
      after: Option[RelationColoredPieceWitness]
  ):
    require(before != after, "a closed legal move cell must change occupant identity")
    require(
      before.forall(_.square == square) && after.forall(_.square == square),
      "a closed legal move cell witness must name its exact square"
    )

  /** Exact projection of one already-generated legal `Move`. No child
    * position is searched and no second legal-move producer exists.
    */
  private[chessjudgment] final case class ClosedLegalMove private[position] (
      source: RelationFactEvidence,
      movement: RelationMoveTransitionWitness,
      destination: EvidenceSquare,
      moveUci: String,
      capture: Option[RelationLegalCaptureWitness],
      mode: ClosedLegalMovementMode,
      geometricControl: Option[ClosedGeometricControl],
      traversedSquares: List[EvidenceSquare],
      boardEffect: BoardMoveEffect
  ):
    require(
      EvidenceRef.sameMove(moveUci, source.detail match
        case RelationWitnessDetail.LegalMove(_, _, _, _, exactUci, _) => exactUci
        case _ => ""),
      "a closed legal move must retain its canonical LegalMove source"
    )
    require(
      boardEffect.pieceTransitions.exists(transition =>
        transition.side == movement.side && evidenceSquare(transition.from) == movement.from &&
          evidenceSquare(transition.to) == movement.to && evidenceRole(transition.beforeRole) == movement.beforeRole &&
          evidenceRole(transition.afterRole) == movement.afterRole
      ),
      "a closed legal move must retain its exact primary move effect"
    )
    require(
      (mode == ClosedLegalMovementMode.ControlledDestination) == geometricControl.nonEmpty,
      "only control-shaped movement may claim a canonical geometric-control edge"
    )

    lazy val changedCells: List[ClosedBoardCellChange] =
      boardEffect.cellChanges.map(change =>
        ClosedBoardCellChange(
          evidenceSquare(change.square),
          change.before.map(piece =>
            RelationColoredPieceWitness(evidenceSquare(change.square), evidenceRole(piece.role), piece.color)
          ),
          change.after.map(piece =>
            RelationColoredPieceWitness(evidenceSquare(change.square), evidenceRole(piece.role), piece.color)
          )
        )
      )

  private[chessjudgment] enum ClosedKingResponseMode:
    case KingMove
    case CaptureChecker
    case Interpose

  private[chessjudgment] final case class ClosedKingResponse private[position] (
      move: ClosedLegalMove,
      modes: Set[ClosedKingResponseMode]
  ):
    require(modes.nonEmpty, "every legal check response needs its exact response mechanism")

  /** A geometrically reachable king destination that is controlled in the
    * current position and absent from the closed legal-move inventory. This
    * intentionally says nothing about controls that would appear only after
    * the king vacates or captures on the destination.
    */
  private[chessjudgment] final case class ClosedControlledKingDestination private[position] (
      movement: ClosedMovementAffordance,
      opponentControls: List[ClosedGeometricControl]
  ):
    require(
      movement.piece.role.name.equalsIgnoreCase(King.name) &&
        movement.mode == RelationMovementResourceMode.ControlledDestination,
      "a controlled king destination needs one exact geometric king movement"
    )
    require(opponentControls.nonEmpty, "a controlled king destination needs a current opponent control")
    require(
      opponentControls.forall(_.targetSquare == movement.destination),
      "every opponent control must terminate on the exact king destination"
    )
    require(
      opponentControls.map(_.target).distinct.size == 1,
      "one occupied destination must have one opponent-side target classification"
    )
    require(
      opponentControls.distinct.size == opponentControls.size &&
        opponentControls == opponentControls.sortBy(control =>
          control.controller.square.key -> control.controller.role.name
        ),
      "opponent controls of a king destination must be unique and canonical"
    )

    private[position] def stableKey: String =
      s"${movement.destination.key.toLowerCase}:${opponentControls.map(control => s"${control.controller.role.name.toLowerCase}@${control.controller.square.key.toLowerCase}").mkString(",")}"

  private[chessjudgment] enum ClosedKingTerminalState:
    case Ongoing
    case Checkmate
    case Stalemate

  /** Closed legal-response inventory for the actual side to move. Multiple
    * mechanisms are retained when one response both moves the king and
    * captures the checker, or both captures and interposes.
    */
  private[chessjudgment] final case class ClosedKingResponseInventory private[position] (
      side: Color,
      kingSquare: Option[EvidenceSquare],
      inCheck: Boolean,
      checkers: List[ClosedGeometricControl],
      legalMoves: List[ClosedLegalMove],
      responses: List[ClosedKingResponse],
      controlledKingDestinations: List[ClosedControlledKingDestination],
      interpositionRay: Option[RelationFactEvidence],
      terminal: ClosedKingTerminalState
  ):
    require(inCheck == checkers.nonEmpty, "check state and canonical checker controls must agree")
    require(!inCheck || responses.map(_.move) == legalMoves, "a check response inventory must classify every legal move")
    require(inCheck || responses.isEmpty, "non-check positions do not manufacture check responses")
    require(
      inCheck || controlledKingDestinations.isEmpty,
      "non-check positions do not open controlled king-destination details"
    )
    require(
      controlledKingDestinations.distinct.size == controlledKingDestinations.size &&
        controlledKingDestinations.map(_.movement.destination).distinct.size == controlledKingDestinations.size &&
        controlledKingDestinations == controlledKingDestinations.sortBy(_.stableKey),
      "controlled king destinations must be unique and canonical"
    )
    require(
      controlledKingDestinations.forall(destination =>
        kingSquare.contains(destination.movement.piece.square) &&
          destination.opponentControls.forall(_.side == !side) &&
          !legalMoves.exists(move =>
            move.movement.beforeRole.name.equalsIgnoreCase(King.name) &&
              move.movement.from == destination.movement.piece.square &&
              move.destination == destination.movement.destination
          )
      ),
      "controlled king destinations must belong to the checked king and be absent from legal resources"
    )
    require(
      responses.exists(_.modes(ClosedKingResponseMode.Interpose)) == interpositionRay.nonEmpty,
      "an interposition response must retain its exact canonical checking ray"
    )
    require(
      terminal match
        case ClosedKingTerminalState.Ongoing   => legalMoves.nonEmpty
        case ClosedKingTerminalState.Checkmate => inCheck && legalMoves.isEmpty
        case ClosedKingTerminalState.Stalemate => !inCheck && legalMoves.isEmpty,
      "king terminal state must follow the closed legal-move inventory"
    )

  private[chessjudgment] enum ClosedPawnFileOccupancy:
    case Open
    case WhitePawnsOnly
    case BlackPawnsOnly
    case BothSides

  private[chessjudgment] final case class ClosedPawnFile private[position] (
      file: EvidenceFile,
      whitePawns: List[EvidenceSquare],
      blackPawns: List[EvidenceSquare],
      occupancy: ClosedPawnFileOccupancy,
      sourceGroups: List[RelationFactEvidence]
  ):
    require(sourceGroups.size == 2, "a closed pawn file needs both sides' canonical file groups")
    require(
      occupancy == ((whitePawns.nonEmpty, blackPawns.nonEmpty) match
        case (false, false) => ClosedPawnFileOccupancy.Open
        case (true, false)  => ClosedPawnFileOccupancy.WhitePawnsOnly
        case (false, true)  => ClosedPawnFileOccupancy.BlackPawnsOnly
        case (true, true)   => ClosedPawnFileOccupancy.BothSides),
      "pawn-file occupancy must follow its two closed file groups"
    )

    def semiOpenFor(side: Color): Boolean =
      side match
        case Color.White => occupancy == ClosedPawnFileOccupancy.BlackPawnsOnly
        case Color.Black => occupancy == ClosedPawnFileOccupancy.WhitePawnsOnly

    def isOpen: Boolean = occupancy == ClosedPawnFileOccupancy.Open

    def pawnsFor(side: Color): List[EvidenceSquare] =
      if side.white then whitePawns else blackPawns

    def sourceFor(side: Color): RelationFactEvidence =
      val exact = sourceGroups.filter(_.detail match
        case RelationWitnessDetail.PawnFileGroup(owner, exactFile, exactPawns) =>
          owner == side && exactFile == file && exactPawns == pawnsFor(side)
        case _ => false
      )
      require(exact.size == 1, "one closed pawn file needs one exact source per side")
      exact.head

  private[chessjudgment] enum ClosedPawnConnectionKind:
    case GeometricSupport
    case Phalanx

  private[chessjudgment] final case class ClosedPawnConnection private[position] (
      side: Color,
      controller: EvidenceSquare,
      target: EvidenceSquare,
      kind: ClosedPawnConnectionKind,
      sources: List[RelationFactEvidence]
  ):
    require(controller != target, "a pawn connection needs two distinct pawns")
    require(
      kind != ClosedPawnConnectionKind.Phalanx || controller.key < target.key,
      "a phalanx connection must retain canonical endpoint order"
    )
    require(sources.nonEmpty, "a pawn connection needs canonical source relations")

    def contains(square: EvidenceSquare): Boolean =
      controller == square || target == square

    def peerOf(square: EvidenceSquare): EvidenceSquare =
      require(contains(square), "a pawn connection peer query needs one exact endpoint")
      if controller == square then target else controller

  private[chessjudgment] final case class ClosedPawnComponent private[position] (
      side: Color,
      pawns: List[EvidenceSquare],
      connections: List[ClosedPawnConnection],
      pawnGroupSources: List[RelationFactEvidence]
  ):
    require(pawns.nonEmpty, "a pawn component cannot be empty")
    require(pawnGroupSources.nonEmpty, "a pawn component needs its canonical PawnFileGroup sources")

  /** One exact maximal run of consecutive occupied files for a single side.
    * This deliberately does not use the human term "pawn island": books use
    * that term for incompatible pawn-connectivity notions. The closed
    * side-level partition, not an individual run, owns the empty boundary
    * files needed to prove maximality.
    */
  private[chessjudgment] final case class ClosedPawnOccupiedFileRun private[position] (
      side: Color,
      files: List[EvidenceFile],
      pawns: List[EvidenceSquare]
  ):
    val witness: RelationPawnOccupiedFileRunWitness = RelationPawnOccupiedFileRunWitness(files, pawns)

  /** Complete side-wide occupied-file partition. Its eight same-side
    * PawnFileGroup sources are the closure authority; a run by itself cannot
    * prove that an intervening or boundary file is empty.
    */
  private[chessjudgment] final class ClosedPawnOccupiedFilePartition private[position] (
      val side: Color,
      closedFiles: List[ClosedPawnFile]
  ):
    private val expectedFiles = File.all.map(file =>
      EvidenceFile((('a'.toInt + file.value).toChar).toString)
    )
    require(
      closedFiles.map(_.file) == expectedFiles,
      "an occupied-file partition must own all eight files in board order"
    )

    val pawnFileSources: List[RelationFactEvidence] =
      closedFiles.map(_.sourceFor(side))

    val runs: List[ClosedPawnOccupiedFileRun] =
      val result = List.newBuilder[ClosedPawnOccupiedFileRun]
      var current = List.empty[ClosedPawnFile]
      closedFiles.foreach { file =>
        if file.pawnsFor(side).nonEmpty then current = current :+ file
        else if current.nonEmpty then
          result += ClosedPawnOccupiedFileRun(
            side,
            current.map(_.file),
            current.flatMap(_.pawnsFor(side)).sortBy(_.key)
          )
          current = Nil
      }
      if current.nonEmpty then
        result += ClosedPawnOccupiedFileRun(
          side,
          current.map(_.file),
          current.flatMap(_.pawnsFor(side)).sortBy(_.key)
        )
      result.result()

    val witnesses: List[RelationPawnOccupiedFileRunWitness] = runs.map(_.witness)

  private[chessjudgment] final case class ClosedPawnState private[position] (
      side: Color,
      square: EvidenceSquare,
      file: EvidenceFile,
      doubled: Boolean,
      isolated: Boolean,
      passed: Boolean,
      geometricallyProtectedPasser: Boolean,
      frontSquare: Option[EvidenceSquare],
      frontOccupant: Option[RelationColoredPieceWitness],
      componentPawns: List[EvidenceSquare],
      componentConnections: List[ClosedPawnConnection],
      componentPawnGroupSources: List[RelationFactEvidence],
      ownFileSource: RelationFactEvidence,
      adjacentFileSources: List[RelationFactEvidence],
      frontSource: RelationFactEvidence,
      passageSource: RelationFactEvidence
  ):
    require(!geometricallyProtectedPasser || passed, "a geometrically protected passer must first be passed")
    def frontBlocked: Boolean = frontOccupant.nonEmpty

  /** Canonical opposing-pawn contact projected from one white-pawn
    * GeometricControl edge. The reciprocal black-pawn edge is checked when
    * the closed topology is built, but is not copied into a second owner.
    */
  private[chessjudgment] final case class ClosedEnemyPawnContact private[position] (
      whitePawn: EvidenceSquare,
      blackPawn: EvidenceSquare,
      source: RelationFactEvidence
  ):
    require(source.detail match
      case RelationWitnessDetail.GeometricControl(
            Color.White,
            controller,
            controllerRole,
            target,
            RelationControlTarget.Enemy(targetRole)
          ) =>
        controller == whitePawn && target == blackPawn &&
          controllerRole.name.equalsIgnoreCase(Pawn.name) && targetRole.name.equalsIgnoreCase(Pawn.name)
      case _ => false,
      "an enemy-pawn contact must retain its exact white-pawn control source"
    )

  private[position] final case class ClosedPawnTopologyDetails(
      pawns: List[ClosedPawnState],
      connections: List[ClosedPawnConnection],
      components: List[ClosedPawnComponent],
      enemyPawnContacts: List[ClosedEnemyPawnContact]
  )

  /** Deterministic pawn semantics over PawnFileGroup, PawnFrontOccupancy,
    * PawnPassage, and the canonical control graph. The closed file
    * facet stays independent so file-only consumers do not build connectivity,
    * passage, or front-occupancy projections. Connectivity is closed over the
    * position's complete pawn graph once per inventory: one changed edge can
    * merge or split a component at arbitrary graph distance, while chess
    * intrinsically bounds this graph to sixteen pawns. A radius-limited local
    * refresh would therefore be both less exact and more complex than this
    * cached closure.
    */
  private[chessjudgment] final class ClosedPawnTopology private[position] (
      fileProjection: File => ClosedPawnFile,
      occupiedFilePartitionProjection: Color => ClosedPawnOccupiedFilePartition,
      detailProjection: () => ClosedPawnTopologyDetails
  ):
    lazy val files: List[ClosedPawnFile] =
      val exact = File.all.map(fileProjection)
      require(exact.size == File.all.size, "closed pawn topology must certify every board file")
      exact

    private[chessjudgment] def file(file: File): ClosedPawnFile = fileProjection(file)

    private lazy val whiteOccupiedFilePartition = occupiedFilePartitionProjection(Color.White)
    private lazy val blackOccupiedFilePartition = occupiedFilePartitionProjection(Color.Black)

    /** This file-only projection is cached independently for each side and
      * does not force support, passage, or contact closure.
      */
    private[chessjudgment] def occupiedFilePartition(side: Color): ClosedPawnOccupiedFilePartition =
      if side.white then whiteOccupiedFilePartition else blackOccupiedFilePartition

    private lazy val details = detailProjection()
    lazy val pawns: List[ClosedPawnState] = details.pawns
    lazy val connections: List[ClosedPawnConnection] = details.connections
    lazy val components: List[ClosedPawnComponent] = details.components
    lazy val enemyPawnContacts: List[ClosedEnemyPawnContact] = details.enemyPawnContacts

    /** Complete, canonical per-pawn state owned by this closed topology. L1
      * contracts consume this cached projection instead of rebuilding the
      * topology witness from its individual source relations.
      */
    lazy val stateWitnesses: List[RelationPawnTopologyStateWitness] =
      val connectionsByPawn = connections.flatMap { connection =>
        connection.kind match
          case ClosedPawnConnectionKind.GeometricSupport =>
            List(
              (connection.side -> connection.controller) -> RelationPawnConnectionWitness(
                connection.target,
                RelationPawnConnectionKind.GeometricSupport,
                RelationPawnConnectionDirection.Supports
              ),
              (connection.side -> connection.target) -> RelationPawnConnectionWitness(
                connection.controller,
                RelationPawnConnectionKind.GeometricSupport,
                RelationPawnConnectionDirection.SupportedBy
              )
            )
          case ClosedPawnConnectionKind.Phalanx =>
            List(
              (connection.side -> connection.controller) -> RelationPawnConnectionWitness(
                connection.target,
                RelationPawnConnectionKind.Phalanx,
                RelationPawnConnectionDirection.Peer
              ),
              (connection.side -> connection.target) -> RelationPawnConnectionWitness(
                connection.controller,
                RelationPawnConnectionKind.Phalanx,
                RelationPawnConnectionDirection.Peer
              )
            )
      }.groupMap(_._1)(_._2)
      val contactsByPawn = enemyPawnContacts.flatMap(contact =>
        List(
          (Color.White -> contact.whitePawn) -> contact.blackPawn,
          (Color.Black -> contact.blackPawn) -> contact.whitePawn
        )
      ).groupMap(_._1)(_._2)
      val exact = pawns.map { pawn =>
        RelationPawnTopologyStateWitness(
          pawn.side,
          pawn.square,
          pawn.doubled,
          pawn.isolated,
          pawn.passed,
          pawn.geometricallyProtectedPasser,
          pawn.frontSquare,
          pawn.frontOccupant,
          pawn.componentPawns.sortBy(_.key),
          connectionsByPawn.getOrElse(pawn.side -> pawn.square, Nil).sortBy(_.stableKey),
          contactsByPawn.getOrElse(pawn.side -> pawn.square, Nil).sortBy(_.key)
        )
      }.sortBy(state => state.side.toString -> state.square.key)
      require(
        exact.map(state => state.side -> state.square).distinct.size == exact.size,
        "one closed pawn topology must own one state per pawn identity"
      )
      exact

    private[chessjudgment] def stateWitness(
        side: Color,
        square: EvidenceSquare
    ): Option[RelationPawnTopologyStateWitness] =
      stateWitnesses.find(state => state.side == side && state.square == square)

    private[chessjudgment] def occupiedFileRunWitnesses(side: Color): List[RelationPawnOccupiedFileRunWitness] =
      occupiedFilePartition(side).witnesses

  /** Typed access to the exact board state owned by one closed position
    * inventory. Consumers never reopen a Board or parse FEN to decide whether
    * an absence query is admissible.
    */
  private[chessjudgment] final class ClosedPositionStateFacet private[position] (
      position: Position,
      canonicalInCheck: Color => Boolean
  ):
    val sideToMove: Color = position.color

    lazy val occupiedPieces: List[RelationColoredPieceWitness] =
      position.board.occupied.squares.toList.sortBy(_.key).flatMap { square =>
        position.board.pieceAt(square).map(piece =>
          RelationColoredPieceWitness(
            evidenceSquare(square),
            evidenceRole(piece.role),
            piece.color
          )
        )
      }

    private lazy val piecesBySideAndRole: Map[(Color, String), List[RelationColoredPieceWitness]] =
      occupiedPieces
        .groupMap(piece => piece.side -> piece.role.name.toLowerCase)(identity)
        .view
        .mapValues(_.sortBy(_.square.key))
        .toMap

    lazy val castlingRights: ClosedCastlingRights =
      val rights = position.history.castles
      ClosedCastlingRights(
        whiteKingSide = Castles.whiteKingSide(rights),
        whiteQueenSide = Castles.whiteQueenSide(rights),
        blackKingSide = Castles.blackKingSide(rights),
        blackQueenSide = Castles.blackQueenSide(rights)
      )

    /** Potential FEN en-passant state, independent of whether a legal
      * en-passant capture currently exists. Legal captures remain owned by the
      * LegalMove inventory.
      */
    lazy val potentialEnPassantSquare: Option[EvidenceSquare] =
      position.potentialEpSquare.map(evidenceSquare)

    def occupantAt(square: EvidenceSquare): Option[RelationColoredPieceWitness] =
      Square.fromKey(square.key).flatMap(position.board.pieceAt).map(piece =>
        RelationColoredPieceWitness(square, evidenceRole(piece.role), piece.color)
      )

    def pieces(
        side: Color,
        role: EvidencePieceRole
    ): List[RelationColoredPieceWitness] =
      piecesBySideAndRole.getOrElse(side -> role.name.toLowerCase, Nil)

    def kingSquares(side: Color): List[EvidenceSquare] =
      pieces(side, EvidencePieceRole(King.name)).map(_.square)

    def kingSquare(side: Color): Option[EvidenceSquare] =
      kingSquares(side) match
        case exact :: Nil => Some(exact)
        case _            => None

    def inCheck(side: Color): Boolean =
      canonicalInCheck(side)

  /** Closed queries whose negative result has a precise chess meaning.
    * There is deliberately no generic `NoDefender` or `NoResponse` label:
    * geometric support and legal reachability are different proof domains.
    */
  private[chessjudgment] enum ClosedRelationAbsenceQuery:
    case AnyLegalMove(side: Color)
    case LegalMoveFromTo(
        side: Color,
        from: EvidenceSquare,
        destination: EvidenceSquare
    )
    case LegalCaptureOf(side: Color, capturedSquare: EvidenceSquare)
    case GeometricControlOf(side: Color, targetSquare: EvidenceSquare)
    case GeometricFriendlySupportOf(side: Color, supportedSquare: EvidenceSquare)
    case GeometricPawnSupportOf(side: Color, supportedSquare: EvidenceSquare)
    case KingCheck(side: Color, kingSquare: EvidenceSquare)

    private[position] def admissible(state: ClosedPositionStateFacet): Boolean =
      this match
        case AnyLegalMove(side) =>
          side == state.sideToMove
        case LegalMoveFromTo(side, from, destination) =>
          side == state.sideToMove &&
            state.occupantAt(from).exists(_.side == side) &&
            Square.fromKey(destination.key).nonEmpty
        case LegalCaptureOf(side, capturedSquare) =>
          side == state.sideToMove && state.occupantAt(capturedSquare).exists(_.side != side)
        case GeometricControlOf(_, targetSquare) =>
          Square.fromKey(targetSquare.key).nonEmpty
        case GeometricFriendlySupportOf(side, supportedSquare) =>
          state.occupantAt(supportedSquare).exists(_.side == side)
        case GeometricPawnSupportOf(side, supportedSquare) =>
          state.occupantAt(supportedSquare).exists(piece =>
            piece.side == side && piece.role.name.equalsIgnoreCase(Pawn.name)
          )
        case KingCheck(side, kingSquare) =>
          state.kingSquare(side).contains(kingSquare)

    def stableKey: String =
      this match
        case AnyLegalMove(side) => s"legal-move:${side.toString.toLowerCase}"
        case LegalMoveFromTo(side, from, destination) =>
          s"legal-move-from-to:${side.toString.toLowerCase}:${from.key.toLowerCase}:${destination.key.toLowerCase}"
        case LegalCaptureOf(side, capturedSquare) =>
          s"legal-capture:${side.toString.toLowerCase}:${capturedSquare.key.toLowerCase}"
        case GeometricControlOf(side, targetSquare) =>
          s"geometric-control:${side.toString.toLowerCase}:${targetSquare.key.toLowerCase}"
        case GeometricFriendlySupportOf(side, supportedSquare) =>
          s"geometric-friendly-support:${side.toString.toLowerCase}:${supportedSquare.key.toLowerCase}"
        case GeometricPawnSupportOf(side, supportedSquare) =>
          s"geometric-pawn-support:${side.toString.toLowerCase}:${supportedSquare.key.toLowerCase}"
        case KingCheck(side, kingSquare) =>
          s"king-check:${side.toString.toLowerCase}:${kingSquare.key.toLowerCase}"

  /** Demand-created positive state queries. These are not board relations:
    * they let a vertical proof name an exact closed state cell without
    * materializing all 64 squares as persistent graph records.
    */
  private[chessjudgment] enum ClosedPositionStateQuery:
    case OccupiedBy(piece: RelationColoredPieceWitness)
    case PawnTopology(state: RelationPawnTopologyStateWitness)
    case PawnOccupiedFilePartition(side: Color, runs: List[RelationPawnOccupiedFileRunWitness])
    case PotentialEnPassant(square: Option[EvidenceSquare])
    case CastlingRight(side: Color, castleSide: Side, retained: Boolean)
    case SliderReach(
        side: Color,
        slider: RelationPieceWitness,
        direction: RelationRayDirection,
        reach: Option[RelationSliderReachWitness]
    )

    private[position] def admissible(state: ClosedPositionStateFacet): Boolean =
      this match
        case OccupiedBy(piece) =>
          Square.fromKey(piece.square.key).nonEmpty
        case PawnTopology(exact) =>
          Square.fromKey(exact.square.key).nonEmpty
        case PawnOccupiedFilePartition(_, exact) =>
          RelationPawnOccupiedFileRunWitness.canonicalPartition(exact)
        case PotentialEnPassant(square) =>
          square.forall(value => Square.fromKey(value.key).nonEmpty)
        case CastlingRight(_, _, _) => true
        case SliderReach(_, slider, _, reach) =>
          Square.fromKey(slider.square.key).nonEmpty &&
            reach.toList.flatMap(_.segment).forall(value => Square.fromKey(value.square.key).nonEmpty)

    def stableKey: String =
      this match
        case OccupiedBy(piece) =>
          s"occupied-by:${piece.side.toString.toLowerCase}:${piece.square.key.toLowerCase}:${piece.role.name.toLowerCase}"
        case PawnTopology(state) =>
          s"pawn-topology:${state.stableKey}"
        case PawnOccupiedFilePartition(side, runs) =>
          s"pawn-occupied-file-partition:${side.toString.toLowerCase}:${runs.map(_.stableKey).mkString("[", ",", "]")}"
        case PotentialEnPassant(square) =>
          s"potential-en-passant:${square.map(_.key.toLowerCase).getOrElse("none")}"
        case CastlingRight(side, castleSide, retained) =>
          s"castling-right:${side.toString.toLowerCase}:${castleSide.toString.toLowerCase}:$retained"
        case SliderReach(side, slider, direction, reach) =>
          s"slider-reach:${side.toString.toLowerCase}:${slider.role.name.toLowerCase}@${slider.square.key.toLowerCase}:${direction.stableKey}:${reach.map(_.stableKey).getOrElse("none")}"

  private[chessjudgment] final class ClosedPositionStateCertificate private[position] (
      val query: ClosedPositionStateQuery,
      private val inventory: PositionRelationInventoryCertificate
  ):
    private[position] def sameOwner(
        certificate: PositionRelationInventoryCertificate
    ): Boolean =
      inventory.sameOwner(certificate)

  private[chessjudgment] final class ClosedPositionStateProof private[position] (
      val query: ClosedPositionStateQuery,
      val position: PositionNodeRef,
      val scope: EvidenceScope,
      private val inventory: PositionRelationInventoryCertificate
  ):
    private[chessjudgment] def sameOwner(
        exactPosition: PositionNodeRef,
        exactScope: EvidenceScope,
        certificate: PositionRelationInventoryCertificate
    ): Boolean =
      position == exactPosition && scope == exactScope && inventory.sameOwner(certificate)

  /** Demand-created negative proof tied to one exact closed inventory. It is
    * never stored as another graph fact, so repeated queries cannot accumulate
    * negative records.
    */
  private[chessjudgment] final class ClosedRelationAbsenceCertificate private[position] (
      val query: ClosedRelationAbsenceQuery,
      private val inventory: PositionRelationInventoryCertificate
  ):
    private[position] def sameOwner(
        certificate: PositionRelationInventoryCertificate
    ): Boolean =
      inventory.sameOwner(certificate)

  private[chessjudgment] final class ClosedRelationAbsenceProof private[position] (
      val query: ClosedRelationAbsenceQuery,
      val position: PositionNodeRef,
      val scope: EvidenceScope,
      private val inventory: PositionRelationInventoryCertificate
  ):
    private[chessjudgment] def sameOwner(
        exactPosition: PositionNodeRef,
        exactScope: EvidenceScope,
        certificate: PositionRelationInventoryCertificate
    ): Boolean =
      position == exactPosition && scope == exactScope && inventory.sameOwner(certificate)

  /** Opaque proof that both canonical position producers ran to completion.
    * It retains the cached snapshots instead of copying relation ids or facts.
    */
  private[chessjudgment] final class PositionRelationInventoryCertificate private[position] (
      private val position: Position,
      private val snapshot: PositionRelationSnapshot,
      private val actualLegalMoves: List[Move]
  ):
    private val state = new ClosedPositionStateFacet(position, canonicalInCheck)
    private lazy val semanticFen = chess.format.Fen.write(position).value
    require(
      actualLegalMoves.forall(move => move.before == position && move.piece.color == position.color),
      "a closed position inventory accepts only its already-generated legal moves"
    )

    private def uniqueIndex[K, V](entries: List[(K, V)], label: String): Map[K, V] =
      entries.groupMap(_._1)(_._2).map {
        case (key, value :: Nil) => key -> value
        case (key, values) =>
          throw IllegalArgumentException(s"$label repeated key '$key' ${values.size} times")
      }

    private val certifiedRelationCache =
      scala.collection.mutable.HashMap.empty[String, RelationFactEvidence]

    private def certified(fact: CanonicalRelationFact): RelationFactEvidence =
      certifiedRelationCache.synchronized {
        certifiedRelationCache.get(fact.semanticId) match
          case Some(existing) =>
            require(
              existing.canonicalFact.asInstanceOf[AnyRef].eq(fact.asInstanceOf[AnyRef]),
              s"position relation '${fact.semanticId}' changed canonical owner"
            )
            existing
          case None =>
            val relation = RelationFactEvidence.certifiedFromCanonicalPositionFact(fact, semanticFen)
            certifiedRelationCache.update(fact.semanticId, relation)
            relation
      }

    private lazy val certifiedRelations: List[RelationFactEvidence] =
      snapshot.facts.map(certified)

    private def relationsOf(kind: RelationFactKind): List[RelationFactEvidence] =
      snapshot.factsOf(kind).map(certified)

    private val closedGeometricControlCache =
      scala.collection.mutable.HashMap.empty[String, ClosedGeometricControl]

    private def closedGeometricControl(relation: RelationFactEvidence): ClosedGeometricControl =
      require(relation.kind == RelationFactKind.GeometricControl)
      closedGeometricControlCache.synchronized {
        closedGeometricControlCache.getOrElseUpdate(
          relation.semanticId,
          ClosedGeometricControl.from(relation)
        )
      }

    private val controlsFromCache =
      scala.collection.mutable.HashMap.empty[EvidenceSquare, List[ClosedGeometricControl]]

    private def indexedControlsFrom(origin: EvidenceSquare): List[ClosedGeometricControl] =
      controlsFromCache.synchronized {
        controlsFromCache.getOrElseUpdate(
          origin,
          relationsFor(RelationDependencyKey.AttackOrigin(boardSquare(origin)))
            .filter(_.kind == RelationFactKind.GeometricControl)
            .map(closedGeometricControl)
            .filter(control => control.controller.square == origin)
            .sortBy(control => control.targetSquare.key -> control.source.semanticId)
        )
      }

    private val controlsAtCache =
      scala.collection.mutable.HashMap.empty[(Color, EvidenceSquare), List[ClosedGeometricControl]]

    private def indexedControlsAt(side: Color, target: EvidenceSquare): List[ClosedGeometricControl] =
      controlsAtCache.synchronized {
        controlsAtCache.getOrElseUpdate(
          side -> target,
          relationsFor(RelationDependencyKey.AttackTarget(boardSquare(target)))
            .filter(_.kind == RelationFactKind.GeometricControl)
            .map(closedGeometricControl)
            .filter(control => control.side == side && control.targetSquare == target)
            .sortBy(control => control.controller.square.key -> control.source.semanticId)
        )
      }

    private val pawnAdvancesFromCache =
      scala.collection.mutable.HashMap.empty[EvidenceSquare, List[ClosedPawnAdvanceAffordance]]

    private def indexedPawnAdvancesFrom(origin: EvidenceSquare): List[ClosedPawnAdvanceAffordance] =
      pawnAdvancesFromCache.synchronized {
        pawnAdvancesFromCache.getOrElseUpdate(
          origin,
          state.occupantAt(origin).filter(_.role.name.equalsIgnoreCase(Pawn.name)).toList.flatMap { pawn =>
            relationsFor(RelationDependencyKey.PawnIdentity(pawn.side, boardSquare(origin)))
              .filter(_.kind == RelationFactKind.PawnAdvanceAffordance)
              .map { relation =>
                relation.detail match
                  case RelationWitnessDetail.PawnAdvanceAffordance(side, exactPawn, destination, traversed)
                      if side == pawn.side && exactPawn == origin =>
                    ClosedPawnAdvanceAffordance(relation, side, exactPawn, destination, traversed)
                  case _ => throw IllegalArgumentException("pawn advance source changed identity")
              }
              .sortBy(_.destination.key)
          }
        )
      }

    private def canonicalCheckers(side: Color): List[ClosedGeometricControl] =
      state.kingSquare(side).toList.flatMap(square =>
        indexedControlsAt(!side, square).filter(_.target match
          case RelationControlTarget.Enemy(role) => role.name.equalsIgnoreCase(King.name)
          case _                                 => false
        )
      )

    private def canonicalInCheck(side: Color): Boolean = canonicalCheckers(side).nonEmpty

    private final class IndexedLegalMove(
        val move: Move,
        val source: RelationFactEvidence
    ):
      lazy val closed: ClosedLegalMove = closedLegalMove(move, source)

    private lazy val indexedLegalMovesByUci: Map[String, IndexedLegalMove] =
      val sources = uniqueIndex(
        relationsOf(RelationFactKind.LegalMove).map { relation =>
          relation.detail match
            case RelationWitnessDetail.LegalMove(_, _, _, _, moveUci, _) =>
              PrincipalVariationEvidence.normalizeUci(moveUci) -> relation
            case _ => throw IllegalArgumentException("legal source changed kind")
        },
        "canonical legal inventory"
      )
      val moves = uniqueIndex(
        actualLegalMoves.map(move => PrincipalVariationEvidence.normalizeUci(move.toUci.uci) -> move),
        "already-generated legal moves"
      )
      require(
        moves.keySet == sources.keySet,
        "the closed LegalMove facts and already-generated legal moves must be exhaustive"
      )
      moves.map { case (moveUci, move) =>
        moveUci -> new IndexedLegalMove(move, sources(moveUci))
      }

    private lazy val indexedLegalMovesOrdered: List[IndexedLegalMove] =
      actualLegalMoves.map(move =>
        indexedLegalMovesByUci(PrincipalVariationEvidence.normalizeUci(move.toUci.uci))
      )

    private lazy val closedLegalMoveInventory: List[ClosedLegalMove] =
      indexedLegalMovesOrdered.map(_.closed)

    private lazy val indexedLegalMovesByOrigin: Map[EvidenceSquare, List[IndexedLegalMove]] =
      indexedLegalMovesOrdered.groupBy(entry => entry.source.detail match
        case RelationWitnessDetail.LegalMove(_, from, _, _, _, _) => from
        case _ => throw IllegalArgumentException("legal source changed kind")
      )

    private lazy val indexedLegalCapturesByTarget: Map[EvidenceSquare, List[IndexedLegalMove]] =
      indexedLegalMovesOrdered.flatMap { entry =>
        entry.source.detail match
          case RelationWitnessDetail.LegalMove(_, _, _, _, _, capture) =>
            capture.map(_.capturedSquare -> entry).toList
          case _ => throw IllegalArgumentException("legal source changed kind")
      }.groupMap(_._1)(_._2)

    private lazy val closedKingTerminalState: ClosedKingTerminalState =
      if actualLegalMoves.nonEmpty then ClosedKingTerminalState.Ongoing
      else if canonicalInCheck(state.sideToMove) then ClosedKingTerminalState.Checkmate
      else ClosedKingTerminalState.Stalemate

    private lazy val closedKingInventory: ClosedKingResponseInventory =
      val side = state.sideToMove
      val king = state.kingSquare(side)
      val checkers = canonicalCheckers(side)
      val inCheck = checkers.nonEmpty
      val checkingSliderRay = (king.toList, checkers) match
        case (kingSquare :: Nil, checker :: Nil) =>
          val exact = rayBarriersWithFirstOccupant(kingSquare).filter(_.detail match
            case RelationWitnessDetail.RayBarrier(owner, attacker, attackerRole, occupants, _) =>
              owner == checker.side && attacker == checker.controller.square &&
                attackerRole.name.equalsIgnoreCase(checker.controller.role.name) &&
                occupants.headOption.exists(piece =>
                  piece.side == side && piece.square == kingSquare &&
                    piece.role.name.equalsIgnoreCase(King.name)
                )
            case _ => false
          )
          require(exact.size <= 1, "one direct slider check must retain one canonical ray")
          exact.headOption
        case _ => None
      val interpositionSquares = checkingSliderRay.toList.flatMap(_.detail match
        case RelationWitnessDetail.RayBarrier(_, attacker, _, _, _) =>
          king.toList.flatMap(kingSquare =>
            BoardGeometry
              .lineSpan(boardSquare(attacker), boardSquare(kingSquare))
              .drop(1)
              .dropRight(1)
              .map(evidenceSquare)
          )
        case _ => Nil
      ).toSet
      val responses =
        if !inCheck then Nil
        else
          closedLegalMoveInventory.map { move =>
            val modes = Set.newBuilder[ClosedKingResponseMode]
            if move.movement.beforeRole.name.equalsIgnoreCase(King.name) then
              modes += ClosedKingResponseMode.KingMove
            if move.capture.exists(capture => checkers.exists(_.controller.square == capture.capturedSquare)) then
              modes += ClosedKingResponseMode.CaptureChecker
            if !move.movement.beforeRole.name.equalsIgnoreCase(King.name) &&
                interpositionSquares(move.destination)
            then modes += ClosedKingResponseMode.Interpose
            ClosedKingResponse(move, modes.result())
          }
      val controlledKingDestinations =
        if !inCheck then Nil
        else
          val legalKingDestinations = closedLegalMoveInventory.collect {
            case move if king.contains(move.movement.from) &&
                move.movement.beforeRole.name.equalsIgnoreCase(King.name) => move.destination
          }.toSet
          king.toList.flatMap { kingSquare =>
            movementAffordancesFrom(kingSquare).flatMap { movement =>
              val opponentControls = controlsAt(!side, movement.destination)
                .sortBy(control => control.controller.square.key -> control.controller.role.name)
              Option.when(
                movement.mode == RelationMovementResourceMode.ControlledDestination &&
                  !legalKingDestinations(movement.destination) && opponentControls.nonEmpty
              )(
                ClosedControlledKingDestination(movement, opponentControls)
              ).toList
            }
          }.sortBy(_.stableKey)
      val interpositionRay = Option.when(
        responses.exists(_.modes(ClosedKingResponseMode.Interpose))
      )(checkingSliderRay).flatten
      ClosedKingResponseInventory(
        side,
        king,
        inCheck,
        checkers,
        closedLegalMoveInventory,
        responses,
        controlledKingDestinations,
        interpositionRay,
        closedKingTerminalState
      )

    private val rayBarriersFromCache =
      scala.collection.mutable.HashMap.empty[EvidenceSquare, List[RelationFactEvidence]]

    private def rayBarriersFrom(origin: EvidenceSquare): List[RelationFactEvidence] =
      rayBarriersFromCache.synchronized {
        rayBarriersFromCache.getOrElseUpdate(
          origin,
          relationsFor(RelationDependencyKey.SliderOrigin(boardSquare(origin)))
            .filter(_.kind == RelationFactKind.RayBarrier)
            .filter(_.detail match
              case RelationWitnessDetail.RayBarrier(_, attacker, _, _, _) => attacker == origin
              case _ => false
            )
            .sortBy(_.semanticId)
        )
      }

    private def directionFrom(
        origin: EvidenceSquare,
        target: EvidenceSquare
    ): RelationRayDirection =
      val from = boardSquare(origin)
      val to = boardSquare(target)
      RelationRayDirection(
        Integer.signum(to.file.value - from.file.value),
        Integer.signum(to.rank.value - from.rank.value)
      )

    private def rayDistance(origin: EvidenceSquare, target: EvidenceSquare): Int =
      val from = boardSquare(origin)
      val to = boardSquare(target)
      math.max((to.file.value - from.file.value).abs, (to.rank.value - from.rank.value).abs)

    private val sliderReachCache =
      scala.collection.mutable.Map.empty[EvidenceSquare, List[ClosedSliderReach]]

    private def buildSliderReaches(square: EvidenceSquare): List[ClosedSliderReach] =
      state.occupantAt(square).toList.filter(coloredSlider =>
        List(Bishop.name, Rook.name, Queen.name).exists(_.equalsIgnoreCase(coloredSlider.role.name))
      ).flatMap { coloredSlider =>
        val slider = RelationPieceWitness(coloredSlider.square, coloredSlider.role)
        indexedControlsFrom(slider.square)
          .groupBy(control => directionFrom(slider.square, control.targetSquare))
          .toList
          .map { case (direction, exactControls) =>
            val ordered = exactControls.sortBy(control => rayDistance(slider.square, control.targetSquare))
            require(
              ordered.dropRight(1).forall(_.target == RelationControlTarget.Empty),
              "a slider reach cannot continue beyond its first occupied square"
            )
            val firstOccupant = ordered.lastOption.flatMap { last =>
              def exactOccupant(role: EvidencePieceRole, friendly: Boolean): RelationColoredPieceWitness =
                val occupant = state.occupantAt(last.targetSquare).getOrElse(
                  throw IllegalArgumentException("a non-empty slider target lost its exact occupant")
                )
                require(
                  occupant.role == role && (occupant.side == coloredSlider.side) == friendly,
                  "a slider target state must match its exact first occupant"
                )
                occupant
              last.target match
                case RelationControlTarget.Empty => None
                case RelationControlTarget.Friendly(role) => Some(exactOccupant(role, friendly = true))
                case RelationControlTarget.Enemy(role)    => Some(exactOccupant(role, friendly = false))
            }
            val barrier = firstOccupant.flatMap { occupant =>
              val exact = rayBarriersFrom(slider.square).filter(_.detail match
                case RelationWitnessDetail.RayBarrier(side, attacker, attackerRole, occupants, axis) =>
                  side == coloredSlider.side && attacker == slider.square && attackerRole == slider.role &&
                    axis == direction.axis && occupants.headOption.contains(occupant)
                case _ => false
              )
              require(exact.size == 1, "one occupied slider reach needs one canonical ray barrier")
              exact.headOption
            }
            ClosedSliderReach(
              coloredSlider.side,
              slider,
              direction,
              ordered,
              firstOccupant,
              barrier
            )
          }
          .sortBy(_.direction.stableKey)
      }

    private lazy val pawnFileGroupSources: Map[(Color, String), (List[EvidenceSquare], RelationFactEvidence)] =
      val entries = relationsOf(RelationFactKind.PawnFileGroup).map { relation =>
        relation.detail match
          case RelationWitnessDetail.PawnFileGroup(side, file, pawns) =>
            (side -> file.key.toLowerCase) -> (pawns -> relation)
          case _ => throw IllegalArgumentException("pawn file-group source changed kind")
      }
      val indexed = uniqueIndex(entries, "closed pawn file groups")
      val expectedKeys = List(Color.White, Color.Black).flatMap(side =>
        File.all.map(file => side -> fileKey(file))
      ).toSet
      require(indexed.keySet == expectedKeys, "closed pawn files need both sides of every board file")
      indexed

    private def closedPawnFile(file: File): ClosedPawnFile =
      val key = fileKey(file)
      val (whitePawns, whiteSource) = pawnFileGroupSources(Color.White -> key)
      val (blackPawns, blackSource) = pawnFileGroupSources(Color.Black -> key)
      val occupancy = (whitePawns.nonEmpty, blackPawns.nonEmpty) match
        case (false, false) => ClosedPawnFileOccupancy.Open
        case (true, false)  => ClosedPawnFileOccupancy.WhitePawnsOnly
        case (false, true)  => ClosedPawnFileOccupancy.BlackPawnsOnly
        case (true, true)   => ClosedPawnFileOccupancy.BothSides
      ClosedPawnFile(
        EvidenceFile(key),
        whitePawns,
        blackPawns,
        occupancy,
        List(whiteSource, blackSource)
      )

    private final class IndexedPawnFile(val file: File):
      lazy val closed: ClosedPawnFile = closedPawnFile(file)

    private lazy val indexedPawnFilesByKey: Map[String, IndexedPawnFile] =
      uniqueIndex(
        File.all.map(file => fileKey(file) -> new IndexedPawnFile(file)),
        "board pawn files"
      )

    private def indexedClosedPawnFile(file: File): ClosedPawnFile =
      indexedPawnFilesByKey(fileKey(file)).closed

    private lazy val closedPawnFileInventory: List[ClosedPawnFile] =
      File.all.map(indexedClosedPawnFile)

    private lazy val whitePawnOccupiedFilePartition =
      new ClosedPawnOccupiedFilePartition(Color.White, closedPawnFileInventory)
    private lazy val blackPawnOccupiedFilePartition =
      new ClosedPawnOccupiedFilePartition(Color.Black, closedPawnFileInventory)

    private def closedPawnOccupiedFilePartition(side: Color): ClosedPawnOccupiedFilePartition =
      if side.white then whitePawnOccupiedFilePartition else blackPawnOccupiedFilePartition

    private lazy val closedPawnTopologyDetails: ClosedPawnTopologyDetails =
      val files = closedPawnFileInventory
      val pawnWitnesses = List(Color.White, Color.Black).flatMap { side =>
        files.flatMap(file => (if side.white then file.whitePawns else file.blackPawns).map(side -> _))
      }
      val pawnSet = pawnWitnesses.toSet
      val frontSources = uniqueIndex(relationsOf(RelationFactKind.PawnFrontOccupancy).map { relation =>
        relation.detail match
          case RelationWitnessDetail.PawnFrontOccupancy(side, pawn, _, _) => (side -> pawn) -> relation
          case _ => throw IllegalArgumentException("pawn front source changed kind")
      }, "closed pawn front states")
      val passageSources = uniqueIndex(relationsOf(RelationFactKind.PawnPassage).map { relation =>
        relation.detail match
          case RelationWitnessDetail.PawnPassage(side, pawn, _) => (side -> pawn) -> relation
          case _ => throw IllegalArgumentException("pawn passage source changed kind")
      }, "closed pawn passage states")
      require(
        frontSources.keySet == pawnSet && passageSources.keySet == pawnSet,
        "every canonical pawn needs one front and one passage state"
      )
      val pawnControls = pawnWitnesses.flatMap { case (_, square) => indexedControlsFrom(square) }
      val supportConnections = pawnControls.flatMap { control =>
        control.target match
          case RelationControlTarget.Friendly(role)
              if control.controller.role.name.equalsIgnoreCase(Pawn.name) && role.name.equalsIgnoreCase(Pawn.name) =>
            List(
              ClosedPawnConnection(
                control.side,
                control.controller.square,
                control.targetSquare,
                ClosedPawnConnectionKind.GeometricSupport,
                List(control.source)
              )
            )
          case _ => Nil
      }
      val phalanxConnections = List(Color.White, Color.Black).flatMap { side =>
        val sidePawns = pawnWitnesses.collect { case (`side`, square) => square }.sortBy(_.key)
        sidePawns.combinations(2).flatMap {
          case List(first, second)
              if (boardSquare(first).file.value - boardSquare(second).file.value).abs == 1 &&
                boardSquare(first).rank == boardSquare(second).rank =>
            val firstSource = pawnFileGroupSources(side -> first.key.head.toString.toLowerCase)._2
            val secondSource = pawnFileGroupSources(side -> second.key.head.toString.toLowerCase)._2
            List(
              ClosedPawnConnection(
                side,
                first,
                second,
                ClosedPawnConnectionKind.Phalanx,
                List(firstSource, secondSource)
              )
            )
          case _ => Nil
        }.toList
      }
      val connections = (supportConnections ++ phalanxConnections).sortBy(connection =>
        (connection.side.toString, connection.controller.key, connection.target.key, connection.kind.toString)
      )
      val components = List(Color.White, Color.Black).flatMap { side =>
        var remaining = pawnWitnesses.collect { case (`side`, square) => square }.toSet
        val built = List.newBuilder[ClosedPawnComponent]
        while remaining.nonEmpty do
          var component = Set(remaining.minBy(_.key))
          var expanded = true
          while expanded do
            val adjacent = connections.collect {
              case connection if connection.side == side && component(connection.controller) => connection.target
              case connection if connection.side == side && component(connection.target) => connection.controller
            }.toSet
            val next = component ++ adjacent
            expanded = next.size != component.size
            component = next
          val componentConnections = connections.filter(connection =>
            connection.side == side && component(connection.controller) && component(connection.target)
          )
          val componentGroupSources = component.toList
            .groupBy(square => square.key.head.toString.toLowerCase)
            .keys
            .toList
            .sorted
            .map(file => pawnFileGroupSources(side -> file)._2)
          built += ClosedPawnComponent(
            side,
            component.toList.sortBy(_.key),
            componentConnections,
            componentGroupSources
          )
          remaining --= component
        built.result()
      }
      val componentByPawn = uniqueIndex(
        components.flatMap(component => component.pawns.map(square => (component.side -> square) -> component)),
        "closed pawn components"
      )
      val whitePawnContacts = pawnControls.collect {
        case control
            if control.side == Color.White && control.controller.role.name.equalsIgnoreCase(Pawn.name) &&
              (control.target match
                case RelationControlTarget.Enemy(role) => role.name.equalsIgnoreCase(Pawn.name)
                case _                                 => false) =>
          ClosedEnemyPawnContact(control.controller.square, control.targetSquare, control.source)
      }.sortBy(contact => contact.whitePawn.key -> contact.blackPawn.key)
      val blackPawnContactPairs = pawnControls.collect {
        case control
            if control.side == Color.Black && control.controller.role.name.equalsIgnoreCase(Pawn.name) &&
              (control.target match
                case RelationControlTarget.Enemy(role) => role.name.equalsIgnoreCase(Pawn.name)
                case _                                 => false) =>
          control.targetSquare -> control.controller.square
      }.toSet
      require(
        whitePawnContacts.map(contact => contact.whitePawn -> contact.blackPawn).toSet == blackPawnContactPairs,
        "opposing pawn contact must be reciprocal in the closed geometric-control graph"
      )
      val pawnStates = pawnWitnesses.map { case (side, square) =>
        val boardPawn = boardSquare(square)
        val file = EvidenceFile(fileKey(boardPawn.file))
        val ownGroup = pawnFileGroupSources(side -> file.key)._2
        val adjacentFiles = File.all.filter(candidate => (candidate.value - boardPawn.file.value).abs == 1)
        val adjacentGroups = adjacentFiles.map(candidate => pawnFileGroupSources(side -> fileKey(candidate))._2)
        val isolated = adjacentFiles.forall(candidate =>
          pawnFileGroupSources(side -> fileKey(candidate))._1.isEmpty
        )
        val frontSource = frontSources(side -> square)
        val (frontSquare, frontOccupant) = frontSource.detail match
          case RelationWitnessDetail.PawnFrontOccupancy(_, _, exactFront, occupant) => exactFront -> occupant
          case _ => throw IllegalArgumentException("pawn front source changed kind")
        val passageSource = passageSources(side -> square)
        val opposingPawns = passageSource.detail match
          case RelationWitnessDetail.PawnPassage(_, _, exactOpposingPawns) => exactOpposingPawns
          case _ => throw IllegalArgumentException("pawn passage source changed kind")
        val pawnSupporters = controlsAt(side, square).filter(control =>
          control.controller.role.name.equalsIgnoreCase(Pawn.name) &&
            control.friendlySupport.exists(_.role.name.equalsIgnoreCase(Pawn.name))
        )
        val passed = opposingPawns.isEmpty
        ClosedPawnState(
          side,
          square,
          file,
          doubled = pawnFileGroupSources(side -> file.key)._1.size > 1,
          isolated = isolated,
          passed = passed,
          geometricallyProtectedPasser = passed && pawnSupporters.nonEmpty,
          frontSquare = frontSquare,
          frontOccupant = frontOccupant,
          componentPawns = componentByPawn(side -> square).pawns,
          componentConnections = componentByPawn(side -> square).connections,
          componentPawnGroupSources = componentByPawn(side -> square).pawnGroupSources,
          ownFileSource = ownGroup,
          adjacentFileSources = adjacentGroups.toList,
          frontSource = frontSource,
          passageSource = passageSource
        )
      }.sortBy(pawn => pawn.side.toString -> pawn.square.key)
      ClosedPawnTopologyDetails(pawnStates, connections, components, whitePawnContacts)

    private lazy val closedPawnTopologyInventory: ClosedPawnTopology =
      new ClosedPawnTopology(
        file => indexedClosedPawnFile(file),
        side => closedPawnOccupiedFilePartition(side),
        () => closedPawnTopologyDetails
      )

    private def closedLegalMove(move: Move, source: RelationFactEvidence): ClosedLegalMove =
      val moveUci = move.toUci.uci
      val actualDestination = legalDestination(move)
      val (sourceSide, sourceFrom, sourceRole, sourceDestination, capture) = source.detail match
        case RelationWitnessDetail.LegalMove(side, from, role, destination, _, exactCapture) =>
          (side, from, role, destination, exactCapture)
        case _ => throw IllegalArgumentException("legal source changed kind")
      val afterRole = move.after.board.roleAt(actualDestination).getOrElse(
        throw IllegalArgumentException(s"legal move '$moveUci' leaves no mover on its destination")
      )
      val movement = RelationMoveTransitionWitness(
        move.piece.color,
        evidenceSquare(move.orig),
        evidenceSquare(actualDestination),
        evidenceRole(move.piece.role),
        evidenceRole(afterRole)
      )
      require(
        sourceSide == movement.side && sourceFrom == movement.from && sourceRole == movement.beforeRole &&
          sourceDestination == movement.to,
        "a LegalMove source must describe its exact already-generated move"
      )
      val rankDistance = (actualDestination.rank.value - move.orig.rank.value).abs
      val mode =
        if move.castle.nonEmpty then ClosedLegalMovementMode.Castling
        else if move.piece.role == Pawn && move.orig.file == actualDestination.file && rankDistance == 2 then
          ClosedLegalMovementMode.PawnDoubleAdvance
        else if move.piece.role == Pawn && move.orig.file == actualDestination.file then
          ClosedLegalMovementMode.PawnAdvance
        else ClosedLegalMovementMode.ControlledDestination
      val geometricControl =
        if mode != ClosedLegalMovementMode.ControlledDestination then None
        else
          controlsFrom(movement.from).filter(control =>
            control.side == movement.side && control.controller.role == movement.beforeRole &&
              control.targetSquare == movement.to
          ) match
            case exact :: Nil => Some(exact)
            case controls =>
              throw IllegalArgumentException(
                s"legal control-shaped move '$moveUci' needs exactly one geometric source, found ${controls.size}"
              )
      val boardEffect = BoardGeometry.moveEffect(move)
      val traversedSquares =
        if move.castle.nonEmpty then
          BoardGeometry.lineSpan(move.orig, actualDestination).drop(1).dropRight(1).map(evidenceSquare)
        else BoardGeometry.movementPath(move.piece, move.orig, actualDestination).map(evidenceSquare)
      ClosedLegalMove(
        source,
        movement,
        movement.to,
        moveUci,
        capture,
        mode,
        geometricControl,
        traversedSquares,
        boardEffect
      )

    private def boardSquare(square: EvidenceSquare): Square =
      Square.fromKey(square.key).getOrElse(
        throw IllegalArgumentException(s"invalid canonical relation square '${square.key}'")
      )

    private def enPassantCapturedPawn(
        side: Color,
        pawn: EvidenceSquare,
        destination: EvidenceSquare
    ): Option[RelationColoredPieceWitness] =
      val origin = boardSquare(pawn)
      val target = boardSquare(destination)
      val capturedSquare = evidenceSquare(Square(target.file, origin.rank))
      Option.when(
        state.sideToMove == side && state.potentialEnPassantSquare.contains(destination) &&
          state.occupantAt(destination).isEmpty
      )(state.occupantAt(capturedSquare)).flatten.filter(piece =>
        piece.side == !side && piece.role.name.equalsIgnoreCase(Pawn.name)
      )

    private[chessjudgment] def relations: List[RelationFactEvidence] = certifiedRelations

    private[chessjudgment] def relationBySemanticId(
        semanticId: String
    ): Option[RelationFactEvidence] =
      snapshot.factBySemanticId(semanticId).map(certified)

    private[chessjudgment] def stateView: ClosedPositionStateFacet = state

    private[chessjudgment] def occupantAt(
        square: EvidenceSquare
    ): Option[RelationColoredPieceWitness] =
      state.occupantAt(square)

    private[chessjudgment] def sideToMove: Color = state.sideToMove

    private[chessjudgment] def controlsFrom(
        controller: EvidenceSquare
    ): List[ClosedGeometricControl] =
      indexedControlsFrom(controller)

    private[chessjudgment] def controlsAt(
        side: Color,
        target: EvidenceSquare
    ): List[ClosedGeometricControl] =
      indexedControlsAt(side, target)

    private def pawnAdvancesFrom(
        pawn: EvidenceSquare
    ): List[ClosedPawnAdvanceAffordance] =
      indexedPawnAdvancesFrom(pawn)

    private[chessjudgment] def movementAffordancesFrom(
        origin: EvidenceSquare
    ): List[ClosedMovementAffordance] =
      occupantAt(origin).toList.flatMap { occupant =>
        val piece = RelationPieceWitness(occupant.square, occupant.role)
        val controlled = controlsFrom(origin).flatMap { control =>
          Option.when(control.side == occupant.side && control.controller == piece)(control).toList.flatMap {
            exact =>
              val mode = exact.target match
                case RelationControlTarget.Enemy(_) =>
                  Some(RelationMovementResourceMode.ControlledDestination)
                case RelationControlTarget.Empty if !occupant.role.name.equalsIgnoreCase(Pawn.name) =>
                  Some(RelationMovementResourceMode.ControlledDestination)
                case RelationControlTarget.Empty =>
                  enPassantCapturedPawn(occupant.side, origin, exact.targetSquare).map(
                    RelationMovementResourceMode.EnPassantCapture.apply
                  )
                case RelationControlTarget.Friendly(_) => None
              mode.map(exactMode =>
                ClosedMovementAffordance(
                  exact.source,
                  piece,
                  exact.targetSquare,
                  exact.target,
                  exactMode
                )
              ).toList
          }
        }
        val advances = pawnAdvancesFrom(origin).filter(advance =>
          advance.side == occupant.side && advance.pawn == origin &&
            occupant.role.name.equalsIgnoreCase(Pawn.name)
        ).map(advance =>
          ClosedMovementAffordance(
            advance.source,
            piece,
            advance.destination,
            RelationControlTarget.Empty,
            if advance.traversedSquares.size == 2 then RelationMovementResourceMode.PawnDoubleAdvance
            else RelationMovementResourceMode.PawnAdvance
          )
        )
        (controlled ++ advances).sortBy(resource => resource.destination.key -> resource.mode.stableKey)
      }

    private[chessjudgment] def supportersOf(
        side: Color,
        supported: EvidenceSquare
    ): List[ClosedGeometricControl] =
      controlsAt(side, supported).filter(_.friendlySupport.nonEmpty)

    private[chessjudgment] def legalMove(moveUci: String): Option[ClosedLegalMove] =
      indexedLegalMovesByUci.get(PrincipalVariationEvidence.normalizeUci(moveUci)).map(_.closed)

    private[chessjudgment] def legalMovesFrom(origin: EvidenceSquare): List[ClosedLegalMove] =
      indexedLegalMovesByOrigin.getOrElse(origin, Nil).map(_.closed)

    private[chessjudgment] def legalCapturesOf(target: EvidenceSquare): List[ClosedLegalMove] =
      indexedLegalCapturesByTarget.getOrElse(target, Nil).map(_.closed)

    /** Terminal classification closes the LegalMove key set but does not
      * materialize every legal move's board effect. Full response details are
      * opened only after a check/terminal contract is actually selected.
      */
    private[chessjudgment] def kingTerminalState: ClosedKingTerminalState = closedKingTerminalState

    private[chessjudgment] def kingResponses: ClosedKingResponseInventory = closedKingInventory

    private[chessjudgment] def rayBarriersWithFirstOccupant(
        square: EvidenceSquare
    ): List[RelationFactEvidence] =
      relationsFor(RelationDependencyKey.RayFirstOccupant(boardSquare(square)))
        .filter(_.kind == RelationFactKind.RayBarrier)
        .filter(_.detail match
          case RelationWitnessDetail.RayBarrier(_, _, _, occupants, _) =>
            occupants.headOption.exists(_.square == square)
          case _ => false
        )
        .sortBy(_.semanticId)

    private[chessjudgment] def sliderReachesFrom(
        square: EvidenceSquare
    ): List[ClosedSliderReach] = sliderReachCache.synchronized {
      sliderReachCache.getOrElseUpdate(square, buildSliderReaches(square))
    }

    private[chessjudgment] def pawnTopologyView: ClosedPawnTopology = closedPawnTopologyInventory

    /** File-only projection with an empty-footprint fast path. Consumers that
      * already know the transition footprint need not open any pawn facet.
      */
    private[chessjudgment] def pawnFiles(affectedFiles: Set[File]): List[ClosedPawnFile] =
      if affectedFiles.isEmpty then Nil
      else affectedFiles.toList.sortBy(_.value).map(file => indexedPawnFilesByKey(fileKey(file)).closed)

    private[chessjudgment] def relationsFor(
        key: RelationDependencyKey
    ): List[RelationFactEvidence] =
      snapshot.factsFor(Set(key)).map(certified)

    private[chessjudgment] def sameOwner(
        other: PositionRelationInventoryCertificate
    ): Boolean =
      this.asInstanceOf[AnyRef].eq(other.asInstanceOf[AnyRef])

    private[chessjudgment] def certifies(
        positionRef: PositionNodeRef,
        relations: IterableOnce[(String, RelationFactEvidence)]
    ): Boolean =
      val exactBoard = PrincipalVariationEvidence.sameBoardState(
        semanticFen,
        positionRef.fen
      )
      val iterator = relations.iterator
      var count = 0
      var exact = exactBoard && positionRef.sideToMove.contains(position.color)
      while exact && iterator.hasNext do
        val (semanticId, relation) = iterator.next()
        count += 1
        exact =
          relation.semanticId == semanticId &&
            relation.isPositionRelation &&
            snapshot
              .factBySemanticId(semanticId)
              .exists(expected => expected.asInstanceOf[AnyRef].eq(relation.canonicalFact.asInstanceOf[AnyRef])) &&
            (relation.origin match
              case RelationEvidenceOrigin.PositionSnapshot(semanticFen) =>
                PrincipalVariationEvidence.sameBoardState(semanticFen, positionRef.fen)
              case _ => false)
      exact && count == snapshot.factCount

    /** Targeted closed lookup shared by semantic L1 certification and
      * occurrence-owned absence capability minting. No relation-kind scan is
      * repeated here.
      */
    private[chessjudgment] def absenceHolds(
        query: ClosedRelationAbsenceQuery
    ): Boolean =
      query.admissible(state) && (query match
        case ClosedRelationAbsenceQuery.AnyLegalMove(_) =>
          indexedLegalMovesByUci.isEmpty
        case ClosedRelationAbsenceQuery.LegalMoveFromTo(_, from, destination) =>
          !indexedLegalMovesByOrigin.getOrElse(from, Nil).exists(entry => entry.source.detail match
            case RelationWitnessDetail.LegalMove(_, _, _, exactDestination, _, _) =>
              exactDestination == destination
            case _ => false
          )
        case ClosedRelationAbsenceQuery.LegalCaptureOf(_, capturedSquare) =>
          indexedLegalCapturesByTarget.getOrElse(capturedSquare, Nil).isEmpty
        case ClosedRelationAbsenceQuery.GeometricControlOf(side, targetSquare) =>
          controlsAt(side, targetSquare).isEmpty
        case ClosedRelationAbsenceQuery.GeometricFriendlySupportOf(side, supportedSquare) =>
          supportersOf(side, supportedSquare).isEmpty
        case ClosedRelationAbsenceQuery.GeometricPawnSupportOf(side, supportedSquare) =>
          controlsAt(side, supportedSquare).forall(control =>
            !control.controller.role.name.equalsIgnoreCase(Pawn.name) || (control.target match
              case RelationControlTarget.Friendly(role) => !role.name.equalsIgnoreCase(Pawn.name)
              case _                                     => true)
          )
        case ClosedRelationAbsenceQuery.KingCheck(side, kingSquare) =>
          controlsAt(!side, kingSquare).forall(control => control.target match
            case RelationControlTarget.Enemy(role) => !role.name.equalsIgnoreCase(King.name)
            case _                                 => true
          ))

    private[chessjudgment] def certifyAbsence(
        query: ClosedRelationAbsenceQuery
    ): Option[ClosedRelationAbsenceCertificate] =
      Option.when(absenceHolds(query))(
        new ClosedRelationAbsenceCertificate(query, this)
      )

    private[chessjudgment] def certifyState(
        query: ClosedPositionStateQuery
    ): Option[ClosedPositionStateCertificate] =
      val holds = query.admissible(state) && (query match
        case ClosedPositionStateQuery.OccupiedBy(piece) =>
          state.occupantAt(piece.square).contains(piece)
        case ClosedPositionStateQuery.PawnTopology(exact) =>
          closedPawnTopologyInventory.stateWitness(exact.side, exact.square).contains(exact)
        case ClosedPositionStateQuery.PawnOccupiedFilePartition(side, exact) =>
          closedPawnTopologyInventory.occupiedFileRunWitnesses(side) == exact
        case ClosedPositionStateQuery.PotentialEnPassant(exact) =>
          state.potentialEnPassantSquare == exact
        case ClosedPositionStateQuery.CastlingRight(side, castleSide, retained) =>
          state.castlingRights.allows(side, castleSide) == retained
        case ClosedPositionStateQuery.SliderReach(side, slider, direction, exact) =>
          sliderReachesFrom(slider.square)
            .find(reach => reach.side == side && reach.slider == slider && reach.direction == direction)
            .map(_.witness) == exact
      )
      Option.when(holds)(new ClosedPositionStateCertificate(query, this))

    private[chessjudgment] def bindAbsence(
        certificate: ClosedRelationAbsenceCertificate,
        positionRef: PositionNodeRef,
        scope: EvidenceScope
    ): Option[ClosedRelationAbsenceProof] =
      val exactOccurrenceState =
        PrincipalVariationEvidence.sameBoardState(semanticFen, positionRef.fen) &&
          positionRef.sideToMove.contains(state.sideToMove)
      Option.when(exactOccurrenceState && certificate.sameOwner(this))(
        new ClosedRelationAbsenceProof(certificate.query, positionRef, scope, this)
      )

    private[chessjudgment] def bindState(
        certificate: ClosedPositionStateCertificate,
        positionRef: PositionNodeRef,
        scope: EvidenceScope
    ): Option[ClosedPositionStateProof] =
      val exactOccurrenceState =
        PrincipalVariationEvidence.sameBoardState(semanticFen, positionRef.fen) &&
          positionRef.sideToMove.contains(state.sideToMove)
      Option.when(exactOccurrenceState && certificate.sameOwner(this))(
        new ClosedPositionStateProof(certificate.query, positionRef, scope, this)
      )

  private[position] def closedPositionInventory(
      snapshot: PositionRelationSnapshot,
      position: Position,
      actualLegalMoves: List[Move]
  ): PositionRelationInventoryCertificate =
    new PositionRelationInventoryCertificate(position, snapshot, actualLegalMoves)

  def records(
      relations: List[RelationFactEvidence],
      positionRef: PositionNodeRef,
      scope: EvidenceScope,
      idFor: RelationFactEvidence => String
  ): List[EvidenceRecord] =
    relations.map { relation =>
      RelationFactEvidence.record(
        id = idFor(relation),
        payload = relation,
        position = positionRef,
        line = None,
        scope = scope,
        confidence = EvidenceConfidence.BoardDerived
      )
    }

  private[position] def extractStaticBoardRelationSnapshot(
      board: Board,
      occupiedSliderRays: List[OccupiedSliderRay],
      pawnTopology: PawnTopologySnapshot,
      geometricControlInventory: GeometricControlInventory
  ): BoardRelationSnapshot =
    val relations =
      (
      rayRelations(occupiedSliderRays) ++
        geometricControls(board, geometricControlInventory) ++
        pawnRelations(board, pawnTopology)
      )
    BoardRelationSnapshot.from(BoardRelationInventoryDomain.StaticBoard, sortDetails(relations))

  private[position] def extractStaticBoardRelationSnapshotAfter(
      previous: BoardRelationSnapshot,
      board: Board,
      refreshedOccupiedSliderRays: List[OccupiedSliderRay],
      pawnTopologyProduction: PawnTopologyProduction,
      geometricControlInventory: GeometricControlInventory,
      footprint: BoardTransitionFootprint
  ): BoardRelationUpdate =
    val refresh = staticRelationRefreshFootprint(footprint)
    val changedSquares = footprint.changedSquareSet
    val affectedAttackOrigins = footprint.geometricControlOriginsToRefresh
    val occupiedAfter = board.occupied.squares.toSet
    val refreshedAttackMapOrigins = affectedAttackOrigins.intersect(occupiedAfter)
    val changedTargetAttackOrigins = changedSquares.flatMap(target =>
      List(Color.White, Color.Black).flatMap(side =>
        geometricControlInventory.controllersByTarget.getOrElse(side -> target, Set.empty)
      )
    ).diff(refreshedAttackMapOrigins)
    val refreshed =
      rayRelations(refreshedOccupiedSliderRays) ++
        geometricControls(board, geometricControlInventory, refreshedAttackMapOrigins) ++
        geometricControls(board, geometricControlInventory, changedTargetAttackOrigins, changedSquares) ++
        pawnRelationsAfter(board, pawnTopologyProduction, footprint)

    previous.updated(sortDetails(refreshed), refresh)

  private[position] def extractOccurrenceRelationSnapshot(
      position: Position,
      legalMoves: List[Move]
  ): BoardRelationSnapshot =
    BoardRelationSnapshot.from(
      BoardRelationInventoryDomain.PositionOccurrence,
      sortDetails(legalMoveRelations(position, legalMoves))
    )

  private[position] def certifyPositionRelationTransition(
      transition: BoardRelationDetailTransition,
      before: PositionRelationInventoryCertificate,
      after: PositionRelationInventoryCertificate
  ): (List[RelationFactEvidence], List[RelationFactEvidence]) =
    def existing(
        facts: List[CanonicalRelationFact],
        inventory: PositionRelationInventoryCertificate,
        label: String
    ): List[RelationFactEvidence] =
      facts.map { fact =>
        inventory.relationBySemanticId(fact.semanticId).filter(relation =>
          relation.canonicalFact.asInstanceOf[AnyRef].eq(fact.asInstanceOf[AnyRef])
        ).getOrElse(
          throw IllegalArgumentException(
            s"$label relation '${fact.semanticId}' is absent from its closed position inventory"
          )
        )
      }
    existing(transition.removed, before, "removed") ->
      existing(transition.established, after, "established")

  /** One exact ray inventory owns every occupied square in distance order.
    * Pin, x-ray, battery, and skewer are deterministic projections of this
    * complete topology, never parallel board producers.
    */
  private def rayRelations(rays: List[OccupiedSliderRay]): List[RelationWitnessDetail] =
    rays.map(rayRelation)

  private def rayRelation(ray: OccupiedSliderRay): RelationWitnessDetail =
    RelationWitnessDetail.RayBarrier(
      side = ray.attackerColor,
      attackerSquare = evidenceSquare(ray.attackerSquare),
      attackerRole = evidenceRole(ray.attackerRole),
      occupants = ray.occupants.map(piece =>
        RelationColoredPieceWitness(
          evidenceSquare(piece.square),
          evidenceRole(piece.role),
          piece.color
        )
      ),
      axis = relationAxis(ray.axis)
    )

  private def relationAxis(axis: BoardLineAxis): RelationAxisSignal =
    axis match
      case BoardLineAxis.File     => RelationAxisSignal.File
      case BoardLineAxis.Rank     => RelationAxisSignal.Rank
      case BoardLineAxis.Diagonal => RelationAxisSignal.Diagonal

  /** Geometric control follows the piece rules and current occupancy.  It is
    * deliberately independent of turn, check, and king-safety legality.
    */
  private def controlTarget(
      board: Board,
      side: Color,
      target: Square
  ): RelationControlTarget =
    board.pieceAt(target) match
      case None => RelationControlTarget.Empty
      case Some(piece) if piece.color == side =>
        RelationControlTarget.Friendly(evidenceRole(piece.role))
      case Some(piece) =>
        RelationControlTarget.Enemy(evidenceRole(piece.role))

  /** Legal reachability is emitted only for the actual side to move. It is a
    * different fact from geometric control and owns captures and recaptures.
    */
  private def legalMoveRelations(
      position: Position,
      legalMoves: List[Move]
  ): List[RelationWitnessDetail] =
    legalMoves.map { move =>
      val actualDestination = legalDestination(move)
      val captured = Option.when(move.captures)(move.capture.getOrElse(move.dest)).flatMap(square =>
        position.board.pieceAt(square).map(piece =>
          RelationLegalCaptureWitness(
            capturedSquare = evidenceSquare(square),
            capturedRole = evidenceRole(piece.role),
            capturedSide = piece.color
          )
        )
      )
      RelationWitnessDetail.LegalMove(
        side = move.piece.color,
        moverSquare = evidenceSquare(move.orig),
        moverRole = evidenceRole(move.piece.role),
        destinationSquare = evidenceSquare(actualDestination),
        moveUci = move.toUci.uci,
        capture = captured
      )
    }

  private def legalDestination(move: Move): Square =
    move.castle.map(_.kingTo).getOrElse(move.dest)

  private def geometricControls(
      board: Board,
      inventory: GeometricControlInventory
  ): List[RelationWitnessDetail] =
    geometricControls(board, inventory, board.occupied.squares.toSet)

  private def geometricControls(
      board: Board,
      inventory: GeometricControlInventory,
      origins: Set[Square],
      targets: Set[Square] = Square.all.toSet
  ): List[RelationWitnessDetail] =
    origins.toList.sortBy(_.key).flatMap { attackerSquare =>
      for
        attacker <- board.pieceAt(attackerSquare).toList
        targetSquare <- inventory.byOrigin(attackerSquare).squares.toList.filter(targets).sortBy(_.key)
      yield RelationWitnessDetail.GeometricControl(
        side = attacker.color,
        attackerSquare = evidenceSquare(attackerSquare),
        attackerRole = evidenceRole(attacker.role),
        targetSquare = evidenceSquare(targetSquare),
        target = controlTarget(board, attacker.color, targetSquare)
      )
    }

  private def pawnRelations(
      board: Board,
      topology: PawnTopologySnapshot
  ): List[RelationWitnessDetail] =
    val fileGroups = pawnFileGroups(
      topology,
      List(Color.White, Color.Black).flatMap(side => File.all.map(side -> _)).toSet
    )
    val front = topology.frontOccupancy.values.toList
      .sortBy(state => (state.side.toString, state.pawn.key))
      .map(pawnFrontDetail)
    val passage = topology.passageOpponents.values.toList
      .sortBy(state => (state.side.toString, state.pawn.key))
      .map(pawnPassageDetail)
    fileGroups ++ front ++ passage ++
      pawnAdvanceAffordances(board, board.pawns.squares.toSet)

  private def pawnRelationsAfter(
      board: Board,
      production: PawnTopologyProduction,
      footprint: BoardTransitionFootprint
  ): List[RelationWitnessDetail] =
    val front = production.frontStatesProduced.values.toList
      .sortBy(state => (state.side.toString, state.pawn.key))
      .map(pawnFrontDetail)
    val passage = production.passageStatesProduced.values.toList
      .sortBy(state => (state.side.toString, state.pawn.key))
      .map(pawnPassageDetail)
    pawnFileGroups(production.snapshot, production.affectedPawnFileKeys) ++
      front ++ passage ++ pawnAdvanceAffordancesAfter(board, footprint)

  private def pawnAdvanceAffordancesAfter(
      board: Board,
      footprint: BoardTransitionFootprint
  ): List[RelationWitnessDetail] =
    val changedSquares = footprint.changedSquareSet
    val changedSquareKeys = changedSquares.map(_.key.toLowerCase)
    val candidateOrigins = footprint.pawnIdentityChanges.map(_._2) ++ changedSquares.flatMap { changed =>
      List(Color.White, Color.Black).flatMap { side =>
        val step = if side.white then 1 else -1
        List(1, 2).flatMap(distance =>
          Square.at(changed.file.value, changed.rank.value - step * distance)
        )
      }
    }
    pawnAdvanceAffordances(board, candidateOrigins).filter {
      case RelationWitnessDetail.PawnAdvanceAffordance(side, pawn, _, traversed) =>
        footprint.pawnIdentityChanges.exists { case (exactSide, square) =>
          exactSide == side && square.key.equalsIgnoreCase(pawn.key)
        } || traversed.exists(square => changedSquareKeys(square.key.toLowerCase))
      case _ => false
    }

  private def pawnAdvanceAffordances(
      board: Board,
      origins: Set[Square]
  ): List[RelationWitnessDetail] =
    origins.toList.sortBy(_.key).flatMap { origin =>
      board.pieceAt(origin).toList.filter(_.role == Pawn).flatMap { pawn =>
        val step = if pawn.color.white then 1 else -1
        val front = Square.at(origin.file.value, origin.rank.value + step)
        val oneStep = front.filter(board.pieceAt(_).isEmpty)
        val startingRank = if pawn.color.white then 1 else 6
        val twoStep = Option.when(origin.rank.value == startingRank)(()).flatMap(_ => oneStep).flatMap { first =>
          Square.at(origin.file.value, origin.rank.value + 2 * step)
            .filter(board.pieceAt(_).isEmpty)
            .map(first -> _)
        }
        oneStep.map(destination =>
          RelationWitnessDetail.PawnAdvanceAffordance(
            pawn.color,
            evidenceSquare(origin),
            evidenceSquare(destination),
            List(evidenceSquare(destination))
          )
        ).toList ++ twoStep.map { case (first, destination) =>
          RelationWitnessDetail.PawnAdvanceAffordance(
            pawn.color,
            evidenceSquare(origin),
            evidenceSquare(destination),
            List(evidenceSquare(first), evidenceSquare(destination))
          )
        }
      }
    }

  private def pawnFrontDetail(state: PawnFrontOccupancy): RelationWitnessDetail.PawnFrontOccupancy =
    RelationWitnessDetail.PawnFrontOccupancy(
      side = state.side,
      pawnSquare = evidenceSquare(state.pawn),
      frontSquare = state.front.map(evidenceSquare),
      occupant = for
        square <- state.front
        occupant <- state.occupant
      yield RelationColoredPieceWitness(evidenceSquare(square), evidenceRole(occupant.role), occupant.color)
    )

  private def pawnPassageDetail(state: PawnPassage): RelationWitnessDetail.PawnPassage =
    RelationWitnessDetail.PawnPassage(
      side = state.side,
      pawnSquare = evidenceSquare(state.pawn),
      opposingPawnSquares = state.opposingPawns.map(evidenceSquare)
    )

  private def pawnFileGroups(
      topology: PawnTopologySnapshot,
      keys: Set[(Color, File)]
  ): List[RelationWitnessDetail] =
    keys.toList.sortBy { case (side, file) => (side.toString, file.value) }.map { case (side, file) =>
      RelationWitnessDetail.PawnFileGroup(
        side = side,
        file = EvidenceFile(fileKey(file)),
        pawns = topology.pawns(side, file).map(evidenceSquare)
      )
    }

  private def sortDetails(details: List[RelationWitnessDetail]): List[RelationWitnessDetail] =
    details.sortBy(RelationWitnessDetail.stableKey)

  private def sortFacts(facts: List[CanonicalRelationFact]): List[CanonicalRelationFact] =
    facts.sortBy(fact => RelationWitnessDetail.stableKey(fact.detail))

  private[position] def staticRelationRefreshFootprint(
      footprint: BoardTransitionFootprint
  ): BoardRelationRefreshFootprint =
    import RelationDependencyKey.*
    val changedSquares = footprint.changedSquareSet
    BoardRelationRefreshFootprint(
      footprint.geometricControlOriginsToRefresh.map(AttackOrigin.apply) ++
        changedSquares.map(AttackTarget.apply) ++
        footprint.affectedOccupiedRayOrigins.map(SliderOrigin.apply) ++
        changedSquares.map(RayFirstOccupant.apply) ++
        footprint.pawnIdentityChanges.map { case (side, square) => PawnIdentity(side, square) } ++
        changedSquares.flatMap(square => List(Color.White, Color.Black).map(PawnFrontCell(_, square))) ++
        footprint.pawnFileChanges.map { case (side, file) => PawnFile(side, file) }
    )

  private def evidenceSquare(square: Square): EvidenceSquare =
    EvidenceSquare(square.key)

  private def evidenceRole(role: Role): EvidencePieceRole =
    EvidencePieceRole(role.name)

  private def fileKey(file: File): String =
    file match
      case File.A => "a"
      case File.B => "b"
      case File.C => "c"
      case File.D => "d"
      case File.E => "e"
      case File.F => "f"
      case File.G => "g"
      case File.H => "h"
