package lila.chessjudgment.analysis.structure

import _root_.chess.{ Board, Color, Pawn, Square }

private[chessjudgment] final case class CarlsbadTarget(
    targetSquare: String,
    enemyChainSquare: String,
    friendlyMinorityPawns: List[String],
    friendlyChainPawn: String
)

private[chessjudgment] object PawnStructureTargets:

  def carlsbadTargetForBoard(
      board: Board,
      pressureSide: Color
  ): Option[CarlsbadTarget] =
    val mirror =
      if pressureSide.white then
        CarlsbadTarget(
          targetSquare = "c6",
          enemyChainSquare = "d5",
          friendlyMinorityPawns = List("b2", "b4", "b5"),
          friendlyChainPawn = "d4"
        )
      else
        CarlsbadTarget(
          targetSquare = "c3",
          enemyChainSquare = "d4",
          friendlyMinorityPawns = List("b7", "b5", "b4"),
          friendlyChainPawn = "d5"
        )
    Option.when(
      boardHasEnemyPawn(board, pressureSide, mirror.targetSquare) &&
        boardHasEnemyPawn(board, pressureSide, mirror.enemyChainSquare) &&
        mirror.friendlyMinorityPawns.exists(square => boardHasFriendlyPawn(board, pressureSide, square)) &&
        boardHasFriendlyPawn(board, pressureSide, mirror.friendlyChainPawn)
    )(mirror)

  def weakPawnTargetsForPressure(
      board: Board,
      pressureSide: Color
  ): Set[String] =
    (
      WeaknessTargetProfile.targetsForPressure(board, pressureSide).map(_.targetSquare) ++
        fixedPawnTargets(board, pressureSide) ++
        carlsbadTargetForBoard(board, pressureSide).map(_.targetSquare).toList
    ).toSet

  def fixedPawnTarget(
      board: Board,
      pressureSide: Color,
      targetSquare: String
  ): Boolean =
    square(targetSquare).exists { target =>
      val weakSide = !pressureSide
      board.pieceAt(target).exists(piece => piece.color == weakSide && piece.role == Pawn) &&
      forwardSquare(target, weakSide).exists(blocker =>
        board.pieceAt(blocker).exists(piece => piece.color == pressureSide && piece.role == Pawn)
      )
    }

  def boardHasEnemyPawn(board: Board, pressureSide: Color, squareKey: String): Boolean =
    square(squareKey).flatMap(board.pieceAt).exists(piece => piece.color != pressureSide && piece.role == Pawn)

  def boardHasFriendlyPawn(board: Board, pressureSide: Color, squareKey: String): Boolean =
    square(squareKey).flatMap(board.pieceAt).exists(piece => piece.color == pressureSide && piece.role == Pawn)

  private def fixedPawnTargets(
      board: Board,
      pressureSide: Color
  ): List[String] =
    Square.all
      .filter(square => fixedPawnTarget(board, pressureSide, square.key))
      .map(_.key)
      .toList

  private def square(squareKey: String): Option[Square] =
    Square.all.find(_.key == normalize(squareKey))

  private def forwardSquare(square: Square, side: Color): Option[Square] =
    val rank = square.rank.value + (if side.white then 1 else -1)
    Square.at(square.file.value, rank)

  private def normalize(value: String): String =
    Option(value).getOrElse("").trim.toLowerCase
