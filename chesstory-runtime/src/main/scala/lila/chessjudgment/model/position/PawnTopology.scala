package lila.chessjudgment.model.position

import chess.*

private[chessjudgment] final case class PawnFrontOccupancy(
    side: Color,
    pawn: Square,
    front: Option[Square],
    occupant: Option[Piece]
)

private[chessjudgment] final case class PawnPassage(
    side: Color,
    pawn: Square,
    opposingPawns: List[Square]
)

private[chessjudgment] final case class PawnTopologyProduction(
    snapshot: PawnTopologySnapshot,
    affectedPawnFileKeys: Set[(Color, File)],
    frontStatesProduced: Map[(Color, Square), PawnFrontOccupancy],
    passageStatesProduced: Map[(Color, Square), PawnPassage]
)

/** Complete, immutable pawn topology for one board.
  *
  * The three stored indexes are factual pawn-only board relations: file
  * membership, the immediately-forward square (including emptiness), and all
  * opposing pawns in a pawn's passage. Enemy contact and friendly support are
  * projections of the canonical geometric-control inventory shared by every
  * piece; pawn topology does not produce those edges again. Terms such as
  * "weak", "backward", "space" and "outpost" are deliberately absent: they
  * require a separate causal contract rather than a static label.
  */
private[chessjudgment] final case class PawnTopologySnapshot private (
    byFile: Map[(Color, File), List[Square]],
    frontOccupancy: Map[(Color, Square), PawnFrontOccupancy],
    passageOpponents: Map[(Color, Square), PawnPassage]
):
  def pawns(side: Color, file: File): List[Square] =
    byFile(side -> file)

  def afterTransition(
      after: Board,
      footprint: BoardTransitionFootprint
  ): PawnTopologyProduction =
    val pawnChanges = footprint.pawnCellChanges
    if pawnChanges.nonEmpty then
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
      val updatedPassage = passageOpponents
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
          frontOccupancy = updatedFront,
          passageOpponents = updatedPassage
        ),
        affectedPawnFileKeys = affectedPawnFileKeys,
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

  def from(board: Board): PawnTopologySnapshot =
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

    val frontOccupancy = sides.flatMap { side =>
      pawns(side).map { pawn =>
        (side -> pawn) -> frontState(board, side, pawn)
      }
    }.toMap

    val passageOpponents = sides.flatMap { side =>
      val opponents = pawns(!side)
      pawns(side).map { pawn =>
        (side -> pawn) -> passageState(side, pawn, opponents)
      }
    }.toMap

    PawnTopologySnapshot(byFile, frontOccupancy, passageOpponents)
