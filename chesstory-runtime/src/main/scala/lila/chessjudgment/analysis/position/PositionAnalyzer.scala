package lila.chessjudgment.analysis.position

import chess._
import chess.format.Fen
import lila.chessjudgment.model.line.{
  LegalReplayStep,
  PositionOccurrenceKey,
  PrincipalVariationEvidence,
  StripedLruCache
}
import lila.chessjudgment.model.position.*
import lila.chessjudgment.model.judgment.{
  RelationFactEvidence,
  RelationInventoryTransition
}

private[position] final case class GeometricControlInventory(
    byOrigin: Map[Square, Bitboard],
    controllersByTarget: Map[(Color, Square), Set[Square]]
)

private[position] object GeometricControlInventory:
  def from(board: Board, sliderRays: List[SliderRay]): GeometricControlInventory =
    val byOrigin = attacksFor(board, board.occupied.squares.toSet, sliderRays)
    GeometricControlInventory(byOrigin, inverse(board, byOrigin))

  def after(
      previous: GeometricControlInventory,
      before: Board,
      after: Board,
      invalidatedOrigins: Set[Square],
      refreshedSliderRays: List[SliderRay]
  ): GeometricControlInventory =
    val retainedByOrigin = previous.byOrigin -- invalidatedOrigins
    val refreshedOrigins = invalidatedOrigins.filter(after.pieceAt(_).nonEmpty)
    val refreshedByOrigin = attacksFor(after, refreshedOrigins, refreshedSliderRays)
    require(
      refreshedByOrigin.keySet == refreshedOrigins,
      "every occupied invalidated attack origin must be produced exactly once"
    )
    val withoutInvalidated = invalidatedOrigins.foldLeft(previous.controllersByTarget) { (indexed, origin) =>
      val invalidatedTargets = before.pieceAt(origin).toList.flatMap { piece =>
        previous.byOrigin.get(origin).toList.flatMap(attacks =>
          attacks.squares.map(target => piece.color -> target)
        )
      }
      invalidatedTargets.foldLeft(indexed) { (targets, key) =>
        val remaining = targets.getOrElse(key, Set.empty) - origin
        if remaining.isEmpty then targets - key else targets.updated(key, remaining)
      }
    }
    val refreshedInverse = refreshedByOrigin.toList.foldLeft(withoutInvalidated) { case (indexed, (origin, attacks)) =>
      val side = after.pieceAt(origin).map(_.color).getOrElse(
        throw IllegalArgumentException(s"refreshed attack origin '${origin.key}' must remain occupied")
      )
      attacks.squares.foldLeft(indexed) { (targets, target) =>
        val key = side -> target
        targets.updated(key, targets.getOrElse(key, Set.empty) + origin)
      }
    }
    GeometricControlInventory(retainedByOrigin ++ refreshedByOrigin, refreshedInverse)

  def controllersAtAfterMove(
      previous: GeometricControlInventory,
      after: Board,
      footprint: BoardTransitionFootprint,
      side: Color,
      target: Square
  ): Set[Square] =
    val invalidated = footprint.geometricControlOriginsToRefresh
    val retained = previous.controllersByTarget.getOrElse(side -> target, Set.empty) -- invalidated
    val refreshedOrigins = invalidated.filter(after.pieceAt(_).nonEmpty)
    val refreshedByOrigin = attacksFor(
      after,
      refreshedOrigins,
      BoardGeometry.sliderRays(after, refreshedOrigins)
    )
    val refreshed = refreshedByOrigin.collect {
      case (origin, controls)
          if after.pieceAt(origin).exists(_.color == side) && controls.contains(target) => origin
    }
    retained ++ refreshed

  private def attacksFor(
      board: Board,
      origins: Set[Square],
      sliderRays: List[SliderRay]
  ): Map[Square, Bitboard] =
    val sliderOrigins = origins.filter(square =>
      board.roleAt(square).exists(role => List(Bishop, Rook, Queen).contains(role))
    )
    val sliderControls = BoardGeometry.controlsFromSliderRays(
      sliderRays.filter(ray => sliderOrigins(ray.attackerSquare))
    )
    require(
      sliderControls.keySet == sliderOrigins,
      "every requested slider origin must own one complete canonical ray projection"
    )
    origins.toList.flatMap { square =>
      board.pieceAt(square).map(piece =>
        square -> sliderControls.getOrElse(
          square,
          BoardGeometry.fixedPatternControls(piece.role, square, piece.color)
        )
      )
    }.toMap

  private def inverse(
      board: Board,
      byOrigin: Map[Square, Bitboard]
  ): Map[(Color, Square), Set[Square]] =
    byOrigin.toList
      .flatMap { case (origin, attacks) =>
        board.pieceAt(origin).toList.flatMap(piece =>
          attacks.squares.map(target => (piece.color -> target) -> origin)
        )
      }
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap

private[position] final class StaticBoardGeometryComputation private (
    val board: Board,
    private[position] val geometricControlInventory: GeometricControlInventory,
    val pawnTopology: PawnTopologySnapshot,
    private[position] val relationSnapshot: PositionRelationExtractor.BoardRelationSnapshot
)

private[position] final case class StaticBoardGeometryBuild(
    geometry: StaticBoardGeometryComputation,
    relationTransition: PositionRelationExtractor.BoardRelationDetailTransition
)

private[position] object StaticBoardGeometryComputation:
  def cold(board: Board): StaticBoardGeometryComputation =
    val rays = BoardGeometry.sliderRays(board)
    val geometricControls = GeometricControlInventory.from(board, rays)
    val pawnTopology = PawnTopologySnapshot.from(board)
    val relationSnapshot = PositionRelationExtractor.extractStaticBoardRelationSnapshot(
      board,
      rays,
      pawnTopology,
      geometricControls
    )
    new StaticBoardGeometryComputation(
      board = board,
      geometricControlInventory = geometricControls,
      pawnTopology = pawnTopology,
      relationSnapshot = relationSnapshot
    )

  def buildAfter(
      previous: StaticBoardGeometryComputation,
      board: Board,
      footprint: BoardTransitionFootprint
  ): StaticBoardGeometryBuild =
    val rayOrigins = footprint.affectedOccupiedRayOrigins
    val refreshedRays = BoardGeometry.sliderRays(
      board,
      rayOrigins
    )
    val invalidatedAttackOrigins = footprint.geometricControlOriginsToRefresh
    val geometricControls = GeometricControlInventory.after(
      previous.geometricControlInventory,
      previous.board,
      board,
      invalidatedAttackOrigins,
      refreshedRays
    )
    val pawnTopologyProduction = previous.pawnTopology.afterTransition(
      board,
      footprint
    )
    val pawnTopology = pawnTopologyProduction.snapshot
    val relationUpdate = PositionRelationExtractor.extractStaticBoardRelationSnapshotAfter(
      previous.relationSnapshot,
      board,
      refreshedRays,
      pawnTopologyProduction,
      geometricControls,
      footprint
    )
    val geometry = new StaticBoardGeometryComputation(
      board = board,
      geometricControlInventory = geometricControls,
      pawnTopology = pawnTopology,
      relationSnapshot = relationUpdate.snapshot
    )
    StaticBoardGeometryBuild(geometry, relationUpdate.transition)

private[position] final class PositionComputation private (
    val staticBoard: StaticBoardGeometryComputation,
    val position: Position,
    private[position] val actualLegalMoveEffects: List[(Move, BoardMoveEffect)],
    private[position] val occurrenceRelationSnapshot: PositionRelationExtractor.BoardRelationSnapshot
):
  val actualLegalMoves: List[Move] = actualLegalMoveEffects.map(_._1)
  val pawnTopology: PawnTopologySnapshot = staticBoard.pawnTopology
  private[position] val relationSnapshot = PositionRelationExtractor.PositionRelationSnapshot(
    staticBoard = staticBoard.relationSnapshot,
    occurrence = occurrenceRelationSnapshot
  )

  private[chessjudgment] lazy val relationInventory:
      PositionRelationExtractor.PositionRelationInventoryCertificate =
    PositionRelationExtractor.closedPositionInventory(
      relationSnapshot,
      position,
      actualLegalMoveEffects,
      staticBoard.geometricControlInventory
    )

  lazy val boardRelations: List[RelationFactEvidence] = relationInventory.relations

private[position] object PositionComputation:
  def from(
      staticBoard: StaticBoardGeometryComputation,
      position: Position
  ): PositionComputation =
    require(
      staticBoard.board == position.board,
      "static board facts and occurrence facts must describe the same board"
    )
    val legalMoves = PrincipalVariationEvidence.actualLegalMoves(position)
    val legalMoveEffects = legalMoves.map(move => move -> BoardGeometry.moveEffect(move))
    new PositionComputation(
      staticBoard = staticBoard,
      position = position,
      actualLegalMoveEffects = legalMoveEffects,
      occurrenceRelationSnapshot = PositionRelationExtractor.extractOccurrenceRelationSnapshot(position, legalMoveEffects)
    )

private[position] final class PositionRelationTransitionCalculation(
    detailTransition: () => PositionRelationExtractor.BoardRelationDetailTransition,
    beforeInventory: () => PositionRelationExtractor.PositionRelationInventoryCertificate,
    afterInventory: () => PositionRelationExtractor.PositionRelationInventoryCertificate
):
  lazy val value: RelationInventoryTransition =
    val before = beforeInventory()
    val after = afterInventory()
    val (boardRemoved, boardEstablished) =
      PositionRelationExtractor.certifyPositionRelationTransition(detailTransition(), before, after)
    RelationInventoryTransition.fromStaticTransition(
      boardRemoved = boardRemoved,
      boardEstablished = boardEstablished,
      beforeInventory = before,
      afterInventory = after
    )

private[position] final class StaticRelationTransitionKey(
    val before: PositionRelationExtractor.BoardRelationSnapshot,
    val after: PositionRelationExtractor.BoardRelationSnapshot,
    val refresh: PositionRelationExtractor.BoardRelationRefreshFootprint
):
  private val cachedHash =
    var hash = 17
    hash = 31 * hash + System.identityHashCode(before)
    hash = 31 * hash + System.identityHashCode(after)
    31 * hash + refresh.##

  override def hashCode(): Int = cachedHash

  override def equals(other: Any): Boolean =
    other match
      case that: StaticRelationTransitionKey =>
        (before.asInstanceOf[AnyRef] eq that.before.asInstanceOf[AnyRef]) &&
          (after.asInstanceOf[AnyRef] eq that.after.asInstanceOf[AnyRef]) &&
          refresh == that.refresh
      case _ => false

private[chessjudgment] final class PositionAnalysis private[position] (
    val occurrence: PositionOccurrenceState,
    val position: Position,
    private[position] val computation: PositionComputation,
    private[chessjudgment] val transitionFootprint: Option[BoardTransitionFootprint],
    private val transitionCalculation: Option[PositionRelationTransitionCalculation]
):
  def actualLegalMoves: List[Move] = computation.actualLegalMoves
  private[chessjudgment] def boardRelations: List[RelationFactEvidence] = computation.boardRelations
  private[chessjudgment] def relationInventory:
      PositionRelationExtractor.PositionRelationInventoryCertificate =
    computation.relationInventory
  private[position] def pawnTopology: PawnTopologySnapshot = computation.pawnTopology
  private[chessjudgment] lazy val relationTransition: Option[RelationInventoryTransition] =
    transitionCalculation.map(_.value)

  /** Re-label one already calculated occurrence without acquiring a second
    * legal-move or relation calculation owner. A transition destination can
    * never be rebased to ply zero; an initial line occurrence may be.
    */
  def atPly(plyCount: Int): PositionAnalysis =
    require(plyCount >= 0, "an occurrence ply cannot be negative")
    require(
      transitionFootprint.isEmpty || plyCount > 0,
      "a transition destination occurrence must have a positive ply"
    )
    if occurrence.plyCount == plyCount then this
    else
      new PositionAnalysis(
        occurrence = occurrence.copy(plyCount = plyCount),
        position = position,
        computation = computation,
        transitionFootprint = transitionFootprint,
        transitionCalculation = transitionCalculation
      )

private[position] object PositionAnalysis:
  def fresh(
      occurrence: PositionOccurrenceState,
      position: Position,
      computation: PositionComputation,
      transitionFootprint: Option[BoardTransitionFootprint],
      transitionCalculation: Option[PositionRelationTransitionCalculation]
  ): PositionAnalysis =
    new PositionAnalysis(
      occurrence,
      position,
      computation,
      transitionFootprint,
      transitionCalculation
    )

object PositionAnalyzer:

  def analyze(position: Position, fen: String, plyCount: Int): PositionAnalysis =
    require(plyCount >= 0, "a position occurrence ply cannot be negative")
    require(
      PrincipalVariationEvidence.sameBoardState(Fen.write(position).value, fen),
      "the supplied position must match the supplied FEN"
    )
    val occurrence = PrincipalVariationEvidence.withFenHalfMoveClock(position, fen)
    val computation = positionComputationCache.getOrCompute(PositionOccurrenceKey.from(occurrence)) {
      val staticBoard = staticBoardGeometryCache.getOrCompute(occurrence.board) {
        StaticBoardGeometryComputation.cold(occurrence.board)
      }
      PositionComputation.from(staticBoard, occurrence)
    }
    PositionAnalysis.fresh(
      occurrence = computeOccurrence(occurrence, fen).copy(plyCount = plyCount),
      position = occurrence,
      computation = computation,
      transitionFootprint = None,
      transitionCalculation = None
    )

  def analyzeAfter(
      previousAnalysis: PositionAnalysis,
      legalStep: LegalReplayStep,
      fen: String
  ): PositionAnalysis =
    require(
      previousAnalysis.occurrence.plyCount == legalStep.ply - 1 &&
        legalStep.before == previousAnalysis.position &&
        PrincipalVariationEvidence.sameBoardState(
          previousAnalysis.occurrence.fen,
          Fen.write(legalStep.before).value
        ),
      "the previous analysis must own the transition source occurrence"
    )
    analyzeAfterComputation(
      previousAnalysis.computation,
      legalStep,
      fen
    )

  private def analyzeAfterComputation(
      previousComputation: PositionComputation,
      legalStep: LegalReplayStep,
      fen: String
  ): PositionAnalysis =
    require(legalStep.ply > 0, "a transition destination must have a positive ply")
    require(
      PrincipalVariationEvidence.sameBoardState(Fen.write(legalStep.after).value, fen),
      "the transition destination FEN must match the admitted legal move"
    )
    require(
      previousComputation.position == legalStep.before,
      "the previous computation must own the admitted transition source occurrence"
    )
    val occurrence = PrincipalVariationEvidence.withFenHalfMoveClock(legalStep.after, fen)
    val moveEffect = previousComputation.relationInventory.legalMove(legalStep.uci).map(_.boardEffect).getOrElse(
      throw IllegalArgumentException(s"'${legalStep.uci}' is absent from its source occurrence's closed legal inventory")
    )
    val footprint = BoardGeometry.transitionFootprint(legalStep.move, moveEffect)
    var builtStaticTransition = Option.empty[PositionRelationExtractor.BoardRelationDetailTransition]
    val computation = positionComputationCache.getOrCompute(PositionOccurrenceKey.from(occurrence)) {
      val staticBoard = staticBoardGeometryCache.getOrCompute(occurrence.board) {
        val build = StaticBoardGeometryComputation.buildAfter(
          previousComputation.staticBoard,
          occurrence.board,
          footprint
        )
        builtStaticTransition = Some(build.relationTransition)
        build.geometry
      }
      PositionComputation.from(staticBoard, occurrence)
    }
    val destinationStaticBoard = computation.staticBoard
    val beforeStaticRelations = previousComputation.staticBoard.relationSnapshot
    val afterStaticRelations = destinationStaticBoard.relationSnapshot
    val staticRefresh = PositionRelationExtractor.staticRelationRefreshFootprint(footprint)
    val staticTransitionKey = new StaticRelationTransitionKey(
      beforeStaticRelations,
      afterStaticRelations,
      staticRefresh
    )
    val transitionCalculation = new PositionRelationTransitionCalculation(
      detailTransition = () => staticRelationTransitionCache.getOrCompute(staticTransitionKey) {
        builtStaticTransition.getOrElse(
          beforeStaticRelations.transitionTo(afterStaticRelations, staticRefresh)
        )
      },
      beforeInventory = () => previousComputation.relationInventory,
      afterInventory = () => computation.relationInventory
    )
    PositionAnalysis.fresh(
      occurrence = computeOccurrence(occurrence, fen).copy(plyCount = legalStep.ply),
      position = occurrence,
      computation = computation,
      transitionFootprint = Some(footprint),
      transitionCalculation = Some(transitionCalculation)
    )

  private val CacheMaxEntries = 4096
  private val staticBoardGeometryCache =
    new StripedLruCache[Board, StaticBoardGeometryComputation](CacheMaxEntries)
  private val positionComputationCache =
    new StripedLruCache[PositionOccurrenceKey, PositionComputation](CacheMaxEntries)
  private val staticRelationTransitionCache =
    new StripedLruCache[StaticRelationTransitionKey, PositionRelationExtractor.BoardRelationDetailTransition](
      CacheMaxEntries
    )

  private[position] def computeOccurrence(
      position: Position,
      fen: String
  ): PositionOccurrenceState =
    PositionOccurrenceState(
      fen = fen,
      sideToMove = position.color,
      plyCount = 0
    )
