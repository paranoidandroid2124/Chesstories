package lila.chessjudgment.analysis.position

import chess.{ Bitboard, Board, Color, File, King, Move, Pawn, Position, Queen, Role, Rook, Square }
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
          RelationFactKind.PawnTension | RelationFactKind.PawnFrontOccupancy |
          RelationFactKind.PawnPassage | RelationFactKind.MajorPieceFileOccupancy |
          RelationFactKind.RayBarrier =>
        Some(StaticBoard)
      case RelationFactKind.LegalMove =>
        Some(PositionOccurrence)
      case RelationFactKind.GeometricControlSetDelta | RelationFactKind.GeometricSupporterCapture |
          RelationFactKind.GeometricSupportDelta |
          RelationFactKind.SliderControlInterference |
          RelationFactKind.GeometricLineControlAfterBlockerRemoval |
          RelationFactKind.CheckingEnemyControlBundle |
          RelationFactKind.DoubleCheck =>
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

    def ++(other: BoardRelationDetailTransition): BoardRelationDetailTransition =
      BoardRelationDetailTransition(
        removed = sortFacts(removed ++ other.removed),
        established = sortFacts(established ++ other.established)
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
    lazy val facts: List[CanonicalRelationFact] = sortFacts(factsByDetail.values.toList)

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

    def transitionAllTo(after: BoardRelationSnapshot): BoardRelationDetailTransition =
      require(domain == after.domain, "relation transition inventories must share one producer domain")
      val beforeDetails = factsByDetail.keySet
      val afterDetails = after.factsByDetail.keySet
      BoardRelationDetailTransition(
        removed = sortFacts((beforeDetails -- afterDetails).toList.map(factsByDetail)),
        established = sortFacts((afterDetails -- beforeDetails).toList.map(after.factsByDetail))
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
    private lazy val allFacts = staticBoard.facts ++ occurrence.facts
    require(
      allFacts.map(_.semanticId).distinct.size == allFacts.size,
      "static and occurrence relation owners cannot emit the same semantic fact"
    )

    lazy val facts: List[CanonicalRelationFact] = sortFacts(allFacts)

    private[position] def factBySemanticId(semanticId: String): Option[CanonicalRelationFact] =
      staticBoard.factBySemanticId(semanticId).orElse(occurrence.factBySemanticId(semanticId))

    private[position] def factCount: Int = staticBoard.factCount + occurrence.factCount

    private[position] def factsFor(
        keys: Set[RelationDependencyKey]
    ): List[CanonicalRelationFact] =
      sortFacts(staticBoard.factsFor(keys) ++ occurrence.factsFor(keys))

    def transitionTo(
        after: PositionRelationSnapshot,
        staticRefresh: BoardRelationRefreshFootprint
    ): BoardRelationDetailTransition =
      staticBoard.transitionTo(after.staticBoard, staticRefresh) ++
        occurrence.transitionAllTo(after.occurrence)

  /** Typed access to the exact board state owned by one closed position
    * inventory. Consumers never reopen a Board or parse FEN to decide whether
    * an absence query is admissible.
    */
  private[position] final class ClosedPositionStateFacet private[position] (
      position: Position
  ):
    val sideToMove: Color = position.color

    def occupantAt(square: EvidenceSquare): Option[RelationColoredPieceWitness] =
      Square.fromKey(square.key).flatMap(position.board.pieceAt).map(piece =>
        RelationColoredPieceWitness(
          square,
          EvidencePieceRole(piece.role.name),
          piece.color
        )
      )

  /** Closed queries whose negative result has a precise chess meaning.
    * There is deliberately no generic `NoDefender` or `NoResponse` label:
    * geometric support and legal reachability are different proof domains.
    */
  private[chessjudgment] enum ClosedRelationAbsenceQuery:
    case AnyLegalMove(side: Color)
    case ExactLegalMove(
        side: Color,
        from: EvidenceSquare,
        destination: EvidenceSquare,
        moveUci: String
    )
    case LegalCaptureOf(side: Color, capturedSquare: EvidenceSquare)
    case GeometricControlOf(side: Color, targetSquare: EvidenceSquare)
    case GeometricFriendlySupportOf(side: Color, supportedSquare: EvidenceSquare)

    private[position] def admissible(state: ClosedPositionStateFacet): Boolean =
      this match
        case AnyLegalMove(side) =>
          side == state.sideToMove
        case ExactLegalMove(side, from, destination, moveUci) =>
          val normalized = PrincipalVariationEvidence.normalizeUci(moveUci)
          side == state.sideToMove &&
            Square.fromKey(from.key).nonEmpty &&
            Square.fromKey(destination.key).nonEmpty &&
            (normalized.length == 4 || normalized.length == 5) &&
            normalized.take(2).equalsIgnoreCase(from.key) &&
            normalized.slice(2, 4).equalsIgnoreCase(destination.key)
        case LegalCaptureOf(side, capturedSquare) =>
          side == state.sideToMove && state.occupantAt(capturedSquare).exists(_.side != side)
        case GeometricControlOf(_, targetSquare) =>
          Square.fromKey(targetSquare.key).nonEmpty
        case GeometricFriendlySupportOf(side, supportedSquare) =>
          state.occupantAt(supportedSquare).exists(_.side == side)

    private[position] def matches(detail: RelationWitnessDetail): Boolean =
      this match
        case AnyLegalMove(side) =>
          detail match
            case RelationWitnessDetail.LegalMove(owner, _, _, _, _, _) => owner == side
            case _                                                       => false
        case ExactLegalMove(side, from, destination, moveUci) =>
          detail match
            case RelationWitnessDetail.LegalMove(owner, exactFrom, _, exactDestination, exactUci, _) =>
              owner == side && exactFrom == from && exactDestination == destination &&
                EvidenceRef.sameMove(exactUci, moveUci)
            case _ => false
        case LegalCaptureOf(side, capturedSquare) =>
          detail match
            case RelationWitnessDetail.LegalMove(owner, _, _, _, _, Some(capture)) =>
              owner == side && capture.capturedSquare == capturedSquare
            case _ => false
        case GeometricControlOf(side, targetSquare) =>
          detail match
            case RelationWitnessDetail.GeometricControl(owner, _, _, target, _) =>
              owner == side && target == targetSquare
            case _ => false
        case GeometricFriendlySupportOf(side, supportedSquare) =>
          detail match
            case RelationWitnessDetail.GeometricControl(
                  owner,
                  _,
                  _,
                  target,
                  RelationControlTarget.Friendly(_)
                ) =>
              owner == side && target == supportedSquare
            case _ => false

    def stableKey: String =
      this match
        case AnyLegalMove(side) => s"legal-move:${side.toString.toLowerCase}"
        case ExactLegalMove(side, from, destination, moveUci) =>
          s"exact-legal-move:${side.toString.toLowerCase}:${from.key.toLowerCase}:${destination.key.toLowerCase}:${PrincipalVariationEvidence.normalizeUci(moveUci)}"
        case LegalCaptureOf(side, capturedSquare) =>
          s"legal-capture:${side.toString.toLowerCase}:${capturedSquare.key.toLowerCase}"
        case GeometricControlOf(side, targetSquare) =>
          s"geometric-control:${side.toString.toLowerCase}:${targetSquare.key.toLowerCase}"
        case GeometricFriendlySupportOf(side, supportedSquare) =>
          s"geometric-friendly-support:${side.toString.toLowerCase}:${supportedSquare.key.toLowerCase}"

  /** Demand-created negative proof tied to one exact closed inventory. It is
    * never stored as another graph fact, so repeated queries cannot accumulate
    * negative records.
    */
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
      private val snapshot: PositionRelationSnapshot
  ):
    private val state = new ClosedPositionStateFacet(position)

    private[chessjudgment] def occupantAt(
        square: EvidenceSquare
    ): Option[RelationColoredPieceWitness] =
      state.occupantAt(square)

    private[chessjudgment] def sideToMove: Color = state.sideToMove

    private[chessjudgment] def relationsFor(
        key: RelationDependencyKey
    ): List[RelationFactEvidence] =
      certify(snapshot.factsFor(Set(key)), position)

    private[chessjudgment] def sameOwner(
        other: PositionRelationInventoryCertificate
    ): Boolean =
      this.asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef]

    private[chessjudgment] def certifies(
        positionRef: PositionNodeRef,
        relations: IterableOnce[(String, RelationFactEvidence)]
    ): Boolean =
      val exactBoard = PrincipalVariationEvidence.sameBoardState(
        chess.format.Fen.write(position).value,
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
              .exists(expected => expected.asInstanceOf[AnyRef] eq relation.canonicalFact.asInstanceOf[AnyRef]) &&
            (relation.origin match
              case RelationEvidenceOrigin.PositionSnapshot(semanticFen) =>
                PrincipalVariationEvidence.sameBoardState(semanticFen, positionRef.fen)
              case _ => false)
      exact && count == snapshot.factCount

    private[chessjudgment] def proveAbsence(
        query: ClosedRelationAbsenceQuery,
        positionRef: PositionNodeRef,
        scope: EvidenceScope
    ): Option[ClosedRelationAbsenceProof] =
      val exactOccurrenceState =
        PrincipalVariationEvidence.sameBoardState(chess.format.Fen.write(position).value, positionRef.fen) &&
          positionRef.sideToMove.contains(state.sideToMove)
      Option.when(
        exactOccurrenceState && query.admissible(state) &&
          !snapshot.facts.exists(fact => query.matches(fact.detail))
      )(new ClosedRelationAbsenceProof(query, positionRef, scope, this))

  private[position] def closedPositionInventory(
      snapshot: PositionRelationSnapshot,
      position: Position
  ): PositionRelationInventoryCertificate =
    new PositionRelationInventoryCertificate(position, snapshot)

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
        pawnRelations(pawnTopology) ++
        fileAccess(board, pawnTopology)
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
    val pawnTopology = pawnTopologyProduction.snapshot
    val refresh = staticRelationRefreshFootprint(footprint)
    val changedSquares = footprint.changedSquareSet
    val affectedAttackOrigins = footprint.geometricControlOriginsToRefresh
    val fileAccessAffected =
      pawnTopologyProduction.affectedPawnFileKeys.flatMap { case (_, file) =>
        List(Color.White -> file, Color.Black -> file)
      } ++
        footprint.majorPieceFileChanges
    val occupiedAfter = board.occupied.squares.toSet
    val refreshedAttackMapOrigins = affectedAttackOrigins.intersect(occupiedAfter)
    val changedTargetAttackOrigins = changedSquares.flatMap(target =>
      List(Color.White, Color.Black).flatMap(side =>
        geometricControlInventory.attackersByTarget.getOrElse(side -> target, Set.empty)
      )
    ).diff(refreshedAttackMapOrigins)
    val refreshed =
      rayRelations(refreshedOccupiedSliderRays) ++
        geometricControls(board, geometricControlInventory, refreshedAttackMapOrigins) ++
        geometricControls(board, geometricControlInventory, changedTargetAttackOrigins, changedSquares) ++
        pawnRelationsAfter(pawnTopologyProduction) ++
        fileAccess(board, pawnTopology, fileAccessAffected)

    previous.updated(sortDetails(refreshed), refresh)

  private[position] def extractOccurrenceRelationSnapshot(
      position: Position,
      legalMoves: List[Move]
  ): BoardRelationSnapshot =
    BoardRelationSnapshot.from(
      BoardRelationInventoryDomain.PositionOccurrence,
      sortDetails(legalMoveRelations(position, legalMoves))
    )

  private[position] def certifyPositionRelations(
      facts: List[CanonicalRelationFact],
      position: Position
  ): List[RelationFactEvidence] =
    certify(facts, position)

  private[position] def certifyPositionRelationTransition(
      transition: BoardRelationDetailTransition,
      before: Position,
      after: Position
  ): (List[RelationFactEvidence], List[RelationFactEvidence]) =
    certify(transition.removed, before) -> certify(transition.established, after)

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
    legalMoves.sortBy(_.toUci.uci).map { move =>
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
        destinationSquare = evidenceSquare(move.dest),
        moveUci = move.toUci.uci,
        capture = captured
      )
    }

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
      topology: PawnTopologySnapshot
  ): List[RelationWitnessDetail] =
    val fileGroups = pawnFileGroups(
      topology,
      List(Color.White, Color.Black).flatMap(side => File.all.map(side -> _)).toSet
    )
    val tension = topology.tension.toList
      .sortBy(edge => (edge.whitePawn.key, edge.blackPawn.key))
      .map(pawnTensionDetail)
    val front = topology.frontOccupancy.values.toList
      .sortBy(state => (state.side.toString, state.pawn.key))
      .map(pawnFrontDetail)
    val passage = topology.passageBlockers.values.toList
      .sortBy(state => (state.side.toString, state.pawn.key))
      .map(pawnPassageDetail)
    fileGroups ++ tension ++ front ++ passage

  private def pawnRelationsAfter(
      production: PawnTopologyProduction
  ): List[RelationWitnessDetail] =
    val tension = production.tensionEdgesProduced.toList
      .sortBy(edge => (edge.whitePawn.key, edge.blackPawn.key))
      .map(pawnTensionDetail)
    val front = production.frontStatesProduced.values.toList
      .sortBy(state => (state.side.toString, state.pawn.key))
      .map(pawnFrontDetail)
    val passage = production.passageStatesProduced.values.toList
      .sortBy(state => (state.side.toString, state.pawn.key))
      .map(pawnPassageDetail)
    pawnFileGroups(production.snapshot, production.affectedPawnFileKeys) ++
      tension ++ front ++ passage

  private def pawnTensionDetail(edge: PawnTensionEdge): RelationWitnessDetail.PawnTension =
    RelationWitnessDetail.PawnTension(
      whitePawnSquare = evidenceSquare(edge.whitePawn),
      blackPawnSquare = evidenceSquare(edge.blackPawn)
    )

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
      blockerSquares = state.blockers.map(evidenceSquare)
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

  private def fileAccess(
      board: Board,
      topology: PawnTopologySnapshot,
      keys: Set[(Color, File)] = List(Color.White, Color.Black)
        .flatMap(side => File.all.map(side -> _))
        .toSet
  ): List[RelationWitnessDetail] =
    val statesByFile = keys.map(_._2).map(file => file -> topology.fileState(file)).toMap
    keys.toList.sortBy { case (side, file) => (side.toString, file.value) }.flatMap { case (side, file) =>
      val state = statesByFile(file)
      val fileName = fileKey(file)
      val open = state.isOpen
      val semiOpen = state.isSemiOpenFor(side)
      val majorPieces = ((board.byPiece(side, Rook) | board.byPiece(side, Queen)) & Bitboard.file(file))
        .squares
        .toList
        .flatMap(square =>
          board.roleAt(square).map(role => RelationPieceWitness(evidenceSquare(square), evidenceRole(role)))
        )
        .sortBy(_.square.key)
      Option.when((open || semiOpen) && majorPieces.nonEmpty)(
        RelationWitnessDetail.MajorPieceFileOccupancy(
          side = side,
          file = EvidenceFile(fileName),
          occupants = majorPieces,
          open = open
        )
      ).toList
    }

  private def certify(
      facts: List[CanonicalRelationFact],
      position: Position
  ): List[RelationFactEvidence] =
    RelationFactEvidence.certifiedFromCanonicalPositionFacts(facts, position)

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
        footprint.pawnIdentityChanges.map { case (side, square) => PawnIdentity(side, square) } ++
        changedSquares.flatMap(square => List(Color.White, Color.Black).map(PawnFrontCell(_, square))) ++
        footprint.pawnFileChanges.map { case (side, file) => PawnFile(side, file) } ++
        footprint.pawnFileChanges.flatMap { case (_, file) =>
          List(FileAccess(Color.White, file), FileAccess(Color.Black, file))
        } ++
        footprint.majorPieceFileChanges.map { case (side, file) => FileAccess(side, file) }
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
