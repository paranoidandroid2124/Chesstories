package lila.chessjudgment.model.position

import chess.*

final case class PositionFeatures(
    fen: String,
    sideToMove: Color,
    plyCount: Int
)

object PositionFeatures:
  def empty: PositionFeatures = PositionFeatures(
    fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
    sideToMove = Color.White,
    plyCount = 0
  )

private[chessjudgment] final case class PawnTensionEdge(
    whitePawn: Square,
    blackPawn: Square
)

private[chessjudgment] final case class PawnFrontOccupancy(
    side: Color,
    pawn: Square,
    front: Option[Square],
    occupant: Option[Piece]
)

private[chessjudgment] final case class PawnPassage(
    side: Color,
    pawn: Square,
    blockers: List[Square]
)

private[chessjudgment] final case class PawnFileTopologyState(
    whitePawns: List[Square],
    blackPawns: List[Square]
):
  def pawns(side: Color): List[Square] =
    if side.white then whitePawns else blackPawns

  def isOpen: Boolean =
    whitePawns.isEmpty && blackPawns.isEmpty

  def isSemiOpenFor(side: Color): Boolean =
    pawns(side).isEmpty && pawns(!side).nonEmpty

private[chessjudgment] final case class PawnTopologyProduction(
    snapshot: PawnTopologySnapshot,
    affectedPawnFileKeys: Set[(Color, File)],
    tensionEdgesProduced: Set[PawnTensionEdge],
    frontStatesProduced: Map[(Color, Square), PawnFrontOccupancy],
    passageStatesProduced: Map[(Color, Square), PawnPassage]
)

/** Complete, immutable pawn topology for one board.
  *
  * The four stored indexes are factual board relations only: file membership,
  * opposing tension, the immediately-forward square (including emptiness),
  * and all opposing pawns in a pawn's passage. Friendly pawn support belongs
  * to the canonical geometric-control inventory shared by every piece. Terms such as
  * "weak", "backward", "space" and "outpost" are deliberately absent: they
  * require a separate causal contract rather than a static label.
  */
private[chessjudgment] final case class PawnTopologySnapshot private (
    byFile: Map[(Color, File), List[Square]],
    tension: Set[PawnTensionEdge],
    frontOccupancy: Map[(Color, Square), PawnFrontOccupancy],
    passageBlockers: Map[(Color, Square), PawnPassage]
):
  def pawns(side: Color, file: File): List[Square] =
    byFile(side -> file)

  def fileState(file: File): PawnFileTopologyState =
    PawnFileTopologyState(
      whitePawns = pawns(Color.White, file),
      blackPawns = pawns(Color.Black, file)
    )

  def afterTransition(
      after: Board,
      footprint: BoardTransitionFootprint,
      geometricControlsByOrigin: Map[Square, Bitboard]
  ): PawnTopologyProduction =
    val pawnChanges = footprint.pawnCellChanges
    if pawnChanges.nonEmpty then
      require(
        geometricControlsByOrigin.keySet == after.occupied.squares.toSet,
        "pawn topology must consume the complete canonical attack inventory"
      )
      val changedPawnSquares = pawnChanges.map(_.square).toSet
      val affectedPawnFileKeys = pawnChanges.flatMap(change =>
        (change.before.toList ++ change.after.toList).collect {
          case piece if piece.role == Pawn => piece.color -> change.square.file
        }
      ).toSet
      val removedPawnKeys = pawnChanges.flatMap(change =>
        change.before.collect { case piece if piece.role == Pawn => piece.color -> change.square }
      ).toSet
      val addedPawnKeys = pawnChanges.flatMap(change =>
        change.after.collect { case piece if piece.role == Pawn => piece.color -> change.square }
      ).toSet
      val afterPawnKeys = (frontOccupancy.keySet -- removedPawnKeys) ++ addedPawnKeys

      val updatedByFile = pawnChanges.foldLeft(byFile) { (files, change) =>
        val removed = change.before.filter(_.role == Pawn).fold(files) { piece =>
          val key = piece.color -> change.square.file
          files.updated(key, files(key).filterNot(_ == change.square))
        }
        change.after.filter(_.role == Pawn).fold(removed) { piece =>
          val key = piece.color -> change.square.file
          removed.updated(key, (change.square :: removed(key).filterNot(_ == change.square)).sortBy(_.key))
        }
      }

      val pawnAttackOrigins = changedPawnSquares.flatMap { square =>
        List(Color.White, Color.Black).flatMap { side =>
          square :: PawnTopologySnapshot.pawnAttackOrigins(side, square)
        }
      }.flatMap(square =>
        after.pieceAt(square).collect {
          case piece if piece.role == Pawn => piece.color -> square
        }
      )
      val retainedTension = tension.filterNot(edge =>
        changedPawnSquares(edge.whitePawn) || changedPawnSquares(edge.blackPawn)
      )
      val refreshedTension = pawnAttackOrigins.flatMap { case (side, pawn) =>
        geometricControlsByOrigin(pawn).squares.flatMap { target =>
          after.pieceAt(target).collect {
            case piece if piece == Piece(!side, Pawn) =>
              if side.white then PawnTensionEdge(pawn, target)
              else PawnTensionEdge(target, pawn)
          }
        }
      }.filter(edge =>
        changedPawnSquares(edge.whitePawn) || changedPawnSquares(edge.blackPawn)
      ).toSet

      val affectedFrontKeys = (
        addedPawnKeys ++ PawnTopologySnapshot.frontDependentPawnKeys(changedPawnSquares)
      ).intersect(afterPawnKeys)
      val updatedFront = frontOccupancy
        .filter { case (key, _) => afterPawnKeys(key) && !affectedFrontKeys(key) } ++
        affectedFrontKeys.map { case key @ (side, pawn) =>
          key -> PawnTopologySnapshot.frontState(after, side, pawn)
        }

      val passageDependents = pawnChanges.flatMap { change =>
        (change.before.toList ++ change.after.toList)
          .collect { case piece if piece.role == Pawn => piece.color }
          .distinct
          .flatMap(changedSide =>
            PawnTopologySnapshot.passageDependentPawns(
              side = !changedSide,
              changedPawnSquare = change.square,
              pawnsByFile = updatedByFile
            )
          )
      }.toSet
      val affectedPassageKeys = (addedPawnKeys ++ passageDependents).intersect(afterPawnKeys)
      val updatedPassage = passageBlockers
        .filter { case (key, _) => afterPawnKeys(key) && !affectedPassageKeys(key) } ++
        affectedPassageKeys.map { case key @ (side, pawn) =>
          key -> PawnTopologySnapshot.passageState(
            side,
            pawn,
            PawnTopologySnapshot.passageOpponentCandidates(side, pawn, updatedByFile)
          )
        }

      PawnTopologyProduction(
        snapshot = PawnTopologySnapshot(
          byFile = updatedByFile,
          tension = retainedTension ++ refreshedTension,
          frontOccupancy = updatedFront,
          passageBlockers = updatedPassage
        ),
        affectedPawnFileKeys = affectedPawnFileKeys,
        tensionEdgesProduced = refreshedTension,
        frontStatesProduced = affectedFrontKeys.flatMap(key => updatedFront.get(key).map(key -> _)).toMap,
        passageStatesProduced = affectedPassageKeys.flatMap(key => updatedPassage.get(key).map(key -> _)).toMap
      )
    else
      val affectedFrontKeys = PawnTopologySnapshot
        .frontDependentPawnKeys(footprint.changedSquareSet)
        .intersect(frontOccupancy.keySet)
      val updated =
        if affectedFrontKeys.isEmpty then this
        else
          copy(
            frontOccupancy = affectedFrontKeys.foldLeft(frontOccupancy) { case (states, key @ (side, pawn)) =>
              states.updated(key, PawnTopologySnapshot.frontState(after, side, pawn))
            }
          )
      PawnTopologyProduction(
        snapshot = updated,
        affectedPawnFileKeys = Set.empty,
        tensionEdgesProduced = Set.empty,
        frontStatesProduced = affectedFrontKeys.flatMap(key => updated.frontOccupancy.get(key).map(key -> _)).toMap,
        passageStatesProduced = Map.empty
      )

private[chessjudgment] object PawnTopologySnapshot:
  private val Sides = List(Color.White, Color.Black)

  private def frontSquare(side: Color, pawn: Square): Option[Square] =
    Square.at(pawn.file.value, pawn.rank.value + (if side.white then 1 else -1))

  private def pawnBehindFront(side: Color, front: Square): Option[Square] =
    Square.at(front.file.value, front.rank.value - (if side.white then 1 else -1))

  private def frontDependentPawnKeys(changedSquares: Set[Square]): Set[(Color, Square)] =
    changedSquares.flatMap(front =>
      Sides.flatMap(side => pawnBehindFront(side, front).map(side -> _))
    )

  private def pawnAttackOrigins(side: Color, target: Square): List[Square] =
    val originRank = target.rank.value - (if side.white then 1 else -1)
    List(target.file.value - 1, target.file.value + 1).flatMap(file =>
      Square.at(file, originRank)
    )

  private def frontState(board: Board, side: Color, pawn: Square): PawnFrontOccupancy =
    val front = frontSquare(side, pawn)
    PawnFrontOccupancy(side, pawn, front, front.flatMap(board.pieceAt))

  private def inPassage(side: Color, pawn: Square, square: Square): Boolean =
    (square.file.value - pawn.file.value).abs <= 1 &&
      (if side.white then square.rank.value > pawn.rank.value else square.rank.value < pawn.rank.value)

  private def adjacentFiles(file: File): List[File] =
    File.all.filter(candidate => (candidate.value - file.value).abs <= 1)

  private def passageDependentPawns(
      side: Color,
      changedPawnSquare: Square,
      pawnsByFile: Map[(Color, File), List[Square]]
  ): Set[(Color, Square)] =
    adjacentFiles(changedPawnSquare.file)
      .flatMap(file => pawnsByFile(side -> file))
      .filter(inPassage(side, _, changedPawnSquare))
      .map(side -> _)
      .toSet

  private def passageOpponentCandidates(
      side: Color,
      pawn: Square,
      pawnsByFile: Map[(Color, File), List[Square]]
  ): List[Square] =
    adjacentFiles(pawn.file)
      .flatMap(file => pawnsByFile(!side -> file))
      .filter(inPassage(side, pawn, _))
      .sortBy(_.key)

  private def passageState(
      side: Color,
      pawn: Square,
      opponents: List[Square]
  ): PawnPassage =
    PawnPassage(side, pawn, opponents.filter(inPassage(side, pawn, _)).sortBy(_.key))

  def from(board: Board, geometricControlsByOrigin: Map[Square, Bitboard]): PawnTopologySnapshot =
    require(
      geometricControlsByOrigin.keySet == board.occupied.squares.toSet,
      "pawn topology must consume the complete canonical attack inventory"
    )
    val sides = List(Color.White, Color.Black)
    val pawnsBySide = sides.map { side =>
      side -> board.byPiece(side, Pawn).squares.toList.sortBy(_.key)
    }.toMap
    val byFile = sides.flatMap { side =>
      val grouped = pawnsBySide(side).groupBy(_.file)
      File.all.map(file => (side -> file) -> grouped.getOrElse(file, Nil))
    }.toMap

    def pawns(side: Color): List[Square] =
      pawnsBySide(side)

    val blackPawns = pawns(Color.Black).toSet
    val tension = pawns(Color.White).flatMap { whitePawn =>
      geometricControlsByOrigin(whitePawn).squares.filter(blackPawns).map(blackPawn =>
        PawnTensionEdge(whitePawn, blackPawn)
      )
    }.toSet

    val frontOccupancy = sides.flatMap { side =>
      pawns(side).map { pawn =>
        (side -> pawn) -> frontState(board, side, pawn)
      }
    }.toMap

    val passageBlockers = sides.flatMap { side =>
      val opponents = pawns(!side)
      pawns(side).map { pawn =>
        (side -> pawn) -> passageState(side, pawn, opponents)
      }
    }.toMap

    PawnTopologySnapshot(byFile, tension, frontOccupancy, passageBlockers)
