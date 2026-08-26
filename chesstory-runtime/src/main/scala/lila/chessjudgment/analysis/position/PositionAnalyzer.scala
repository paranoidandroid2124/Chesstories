package lila.chessjudgment.analysis.position

import chess._
import chess.format.Fen
import lila.chessjudgment.model.line.{ LegalReplayStep, PrincipalVariationEvidence, StripedLruCache }
import lila.chessjudgment.model.position.*
import lila.chessjudgment.model.judgment.{
  RelationFactEvidence,
  RelationInventoryTransition
}

private[position] final case class GeometricControlInventory(
    byOrigin: Map[Square, Bitboard],
    attackersByTarget: Map[(Color, Square), Set[Square]]
)

private[position] object GeometricControlInventory:
  def from(board: Board): GeometricControlInventory =
    val byOrigin = attacksFor(board, board.occupied.squares.toSet)
    GeometricControlInventory(byOrigin, inverse(board, byOrigin))

  def after(
      previous: GeometricControlInventory,
      before: Board,
      after: Board,
      invalidatedOrigins: Set[Square]
  ): GeometricControlInventory =
    val retainedByOrigin = previous.byOrigin -- invalidatedOrigins
    val refreshedOrigins = invalidatedOrigins.filter(after.pieceAt(_).nonEmpty)
    val refreshedByOrigin = attacksFor(after, refreshedOrigins)
    require(
      refreshedByOrigin.keySet == refreshedOrigins,
      "every occupied invalidated attack origin must be produced exactly once"
    )
    val withoutInvalidated = invalidatedOrigins.foldLeft(previous.attackersByTarget) { (indexed, origin) =>
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

  private def attacksFor(board: Board, origins: Set[Square]): Map[Square, Bitboard] =
    origins.toList.flatMap { square =>
      board.pieceAt(square).map(piece =>
        square -> BoardGeometry.geometricControls(piece.role, square, piece.color, board.occupied)
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
    private[position] val occupiedSliderRaysByOrigin: Map[Square, List[OccupiedSliderRay]],
    private[position] val relationSnapshot: PositionRelationExtractor.BoardRelationSnapshot
)

private[position] final case class StaticBoardGeometryBuild(
    geometry: StaticBoardGeometryComputation,
    relationTransition: PositionRelationExtractor.BoardRelationDetailTransition
)

private[position] object StaticBoardGeometryComputation:
  def cold(board: Board): StaticBoardGeometryComputation =
    val geometricControls = GeometricControlInventory.from(board)
    val pawnTopology = PawnTopologySnapshot.from(board, geometricControls.byOrigin)
    val raysByOrigin = BoardGeometry.occupiedSliderRays(board).groupBy(_.attackerSquare)
    val rays = raysByOrigin.toList.sortBy(_._1.key).flatMap(_._2)
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
      occupiedSliderRaysByOrigin = raysByOrigin,
      relationSnapshot = relationSnapshot
    )

  def buildAfter(
      previous: StaticBoardGeometryComputation,
      board: Board,
      footprint: BoardTransitionFootprint
  ): StaticBoardGeometryBuild =
    val rayOrigins = footprint.affectedOccupiedRayOrigins
    val refreshedRays = BoardGeometry.occupiedSliderRays(
      board,
      rayOrigins
    )
    val occupiedSliderRaysByOrigin =
      (previous.occupiedSliderRaysByOrigin -- rayOrigins) ++ refreshedRays.groupBy(_.attackerSquare)
    val invalidatedAttackOrigins = footprint.geometricControlOriginsToRefresh
    val geometricControls = GeometricControlInventory.after(
      previous.geometricControlInventory,
      previous.board,
      board,
      invalidatedAttackOrigins
    )
    val pawnTopologyProduction = previous.pawnTopology.afterTransition(
      board,
      footprint,
      geometricControls.byOrigin
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
      occupiedSliderRaysByOrigin = occupiedSliderRaysByOrigin,
      relationSnapshot = relationUpdate.snapshot
    )
    StaticBoardGeometryBuild(geometry, relationUpdate.transition)

private[position] final class PositionComputation private (
    val staticBoard: StaticBoardGeometryComputation,
    val position: Position,
    val actualLegalMoves: List[Move],
    private[position] val occurrenceRelationSnapshot: PositionRelationExtractor.BoardRelationSnapshot
):
  val pawnTopology: PawnTopologySnapshot = staticBoard.pawnTopology
  private[position] val relationSnapshot = PositionRelationExtractor.PositionRelationSnapshot(
    staticBoard = staticBoard.relationSnapshot,
    occurrence = occurrenceRelationSnapshot
  )

  lazy val boardRelations: List[RelationFactEvidence] =
    PositionRelationExtractor.certifyPositionRelations(relationSnapshot.facts, position)

  private[chessjudgment] lazy val relationInventory:
      PositionRelationExtractor.PositionRelationInventoryCertificate =
    PositionRelationExtractor.closedPositionInventory(relationSnapshot, position)

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
    new PositionComputation(
      staticBoard = staticBoard,
      position = position,
      actualLegalMoves = legalMoves,
      occurrenceRelationSnapshot = PositionRelationExtractor.extractOccurrenceRelationSnapshot(position, legalMoves)
    )

private[position] final class PositionRelationTransitionCalculation(
    detailTransition: PositionRelationExtractor.BoardRelationDetailTransition,
    before: Position,
    after: Position,
    beforeInventory: PositionRelationExtractor.PositionRelationInventoryCertificate,
    afterInventory: PositionRelationExtractor.PositionRelationInventoryCertificate
):
  lazy val value: RelationInventoryTransition =
    val (boardRemoved, boardEstablished) =
      PositionRelationExtractor.certifyPositionRelationTransition(detailTransition, before, after)
    RelationInventoryTransition.fromBoardTransition(
      boardRemoved = boardRemoved,
      boardEstablished = boardEstablished,
      beforeInventory = beforeInventory,
      afterInventory = afterInventory
    )

private[chessjudgment] final class PositionAnalysis private[position] (
    val features: PositionFeatures,
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
  def pawnTopology: PawnTopologySnapshot = computation.pawnTopology
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
    if features.plyCount == plyCount then this
    else
      new PositionAnalysis(
        features = features.copy(plyCount = plyCount),
        position = position,
        computation = computation,
        transitionFootprint = transitionFootprint,
        transitionCalculation = transitionCalculation
      )

private[position] object PositionAnalysis:
  def fresh(
      features: PositionFeatures,
      position: Position,
      computation: PositionComputation,
      transitionFootprint: Option[BoardTransitionFootprint],
      transitionCalculation: Option[PositionRelationTransitionCalculation]
  ): PositionAnalysis =
    new PositionAnalysis(
      features,
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
    val staticBoard = staticBoardGeometryCache.getOrCompute(occurrence.board) {
      StaticBoardGeometryComputation.cold(occurrence.board)
    }
    val computation = positionComputationCache.getOrCompute(occurrence) {
      PositionComputation.from(staticBoard, occurrence)
    }
    PositionAnalysis.fresh(
      features = computeFeatures(occurrence, fen).copy(plyCount = plyCount),
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
      previousAnalysis.features.plyCount == legalStep.ply - 1 &&
        PrincipalVariationEvidence.sameBoardState(
          previousAnalysis.features.fen,
          Fen.write(legalStep.before).value
        ),
      "the previous analysis must own the transition source occurrence"
    )
    analyzeAfterComputation(
      previousAnalysis.computation,
      previousAnalysis.position,
      legalStep,
      fen
    )

  private def analyzeAfterComputation(
      previousComputation: PositionComputation,
      beforeOccurrence: Position,
      legalStep: LegalReplayStep,
      fen: String
  ): PositionAnalysis =
    require(legalStep.ply > 0, "a transition destination must have a positive ply")
    require(
      PrincipalVariationEvidence.sameBoardState(Fen.write(legalStep.after).value, fen),
      "the transition destination FEN must match the admitted legal move"
    )
    require(
      PrincipalVariationEvidence.sameBoardState(
        Fen.write(previousComputation.position).value,
        Fen.write(legalStep.before).value
      ),
      "the previous geometry must match the admitted transition source"
    )
    val occurrence = PrincipalVariationEvidence.withFenHalfMoveClock(legalStep.after, fen)
    val footprint = BoardGeometry.transitionFootprint(legalStep.move)
    var builtStaticTransition = Option.empty[PositionRelationExtractor.BoardRelationDetailTransition]
    val staticBoard = staticBoardGeometryCache.getOrCompute(occurrence.board) {
      val build = StaticBoardGeometryComputation.buildAfter(
        previousComputation.staticBoard,
        occurrence.board,
        footprint
      )
      builtStaticTransition = Some(build.relationTransition)
      build.geometry
    }
    val computation = positionComputationCache.getOrCompute(occurrence) {
      PositionComputation.from(staticBoard, occurrence)
    }
    val staticTransition =
      builtStaticTransition.getOrElse(
        previousComputation.staticBoard.relationSnapshot.transitionTo(
          staticBoard.relationSnapshot,
          PositionRelationExtractor.staticRelationRefreshFootprint(footprint)
        )
      )
    val occurrenceTransition =
      previousComputation.occurrenceRelationSnapshot.transitionAllTo(computation.occurrenceRelationSnapshot)
    val detailTransition = staticTransition ++ occurrenceTransition
    val transitionCalculation = new PositionRelationTransitionCalculation(
      detailTransition = detailTransition,
      before = beforeOccurrence,
      after = occurrence,
      beforeInventory = previousComputation.relationInventory,
      afterInventory = computation.relationInventory
    )
    PositionAnalysis.fresh(
      features = computeFeatures(occurrence, fen).copy(plyCount = legalStep.ply),
      position = occurrence,
      computation = computation,
      transitionFootprint = Some(footprint),
      transitionCalculation = Some(transitionCalculation)
    )

  private val CacheMaxEntries = 4096
  private val staticBoardGeometryCache =
    new StripedLruCache[Board, StaticBoardGeometryComputation](CacheMaxEntries)
  private val positionComputationCache =
    new StripedLruCache[Position, PositionComputation](CacheMaxEntries)

  private[position] def computeFeatures(
      position: Position,
      fen: String
  ): PositionFeatures =
    PositionFeatures(
      fen = fen,
      sideToMove = position.color,
      plyCount = 0
    )
