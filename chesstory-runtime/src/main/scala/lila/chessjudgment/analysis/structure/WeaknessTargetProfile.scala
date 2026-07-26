package lila.chessjudgment.analysis.structure

import _root_.chess.{ Board, Color, File, Pawn, Square }

import lila.chessjudgment.analysis.position.PositionAnalyzer

private[chessjudgment] final case class WeaknessTargetProfile(
    targetSquare: String,
    weakSide: Color,
    kind: String,
    structureCode: Option[String],
    pressureFiles: List[String],
    adjacentWeakSquares: List[String],
    evidenceCodes: List[String]
)

private[chessjudgment] object WeaknessTargetProfile:
  val BackwardPawn = "backward_pawn"
  val IsolatedPawn = "isolated_pawn"
  val IQP = "iqp"
  val DoubledPawn = "doubled_pawn"
  val FixedPawn = "fixed_pawn"

  private val priority =
    Map(
      IQP -> 0,
      BackwardPawn -> 1,
      IsolatedPawn -> 2,
      DoubledPawn -> 3,
      FixedPawn -> 4
    )

  def targetsForPressure(
      board: Board,
      pressureSide: Color,
      structureCode: Option[String] = None
  ): List[WeaknessTargetProfile] =
    forWeakSide(board, !pressureSide, structureCode)

  def forWeakSide(
      board: Board,
      weakSide: Color,
      structureCode: Option[String] = None
  ): List[WeaknessTargetProfile] =
    val pawns = board.byPiece(weakSide, Pawn)
    val candidates =
      PositionAnalyzer.backwardPawns(weakSide, pawns, board).map(profile(weakSide, BackwardPawn, structureCode)) ++
        PositionAnalyzer.isolatedPawns(pawns).map(profile(weakSide, IsolatedPawn, structureCode)) ++
        iqpTargets(board, weakSide).map(profile(weakSide, IQP, structureCode)) ++
        PositionAnalyzer.doubledPawns(pawns).map(profile(weakSide, DoubledPawn, structureCode)) ++
        fixedPawns(board, weakSide).map(profile(weakSide, FixedPawn, structureCode))
    candidates
      .groupBy(_.targetSquare)
      .values
      .map(_.minBy(target => priority.getOrElse(target.kind, 99)))
      .toList
      .sortBy(target => (priority.getOrElse(target.kind, 99), target.targetSquare))

  private def profile(
      weakSide: Color,
      kind: String,
      structureCode: Option[String]
  )(square: Square): WeaknessTargetProfile =
    val target = square.key
    WeaknessTargetProfile(
      targetSquare = target,
      weakSide = weakSide,
      kind = kind,
      structureCode = structureCode,
      pressureFiles = pressureFiles(target),
      adjacentWeakSquares = adjacentSquares(target),
      evidenceCodes =
        (
          List(
            s"weakness_target:$target",
            s"weakness_kind:$kind",
            s"weak_side:${sideLabel(weakSide)}",
            s"target_file:${target.take(1)}"
          ) ++ structureCode.map(code => s"structure:$code")
        ).distinct
    )

  private def iqpTargets(board: Board, weakSide: Color): List[Square] =
    val pawns = board.byPiece(weakSide, Pawn)
    val dPawns = pawns.squares.filter(_.file == File.D).toList
    dPawns.filter { dPawn =>
      dPawns.size == 1 &&
        PositionAnalyzer.isolatedPawns(pawns).contains(dPawn)
    }

  private def fixedPawns(board: Board, weakSide: Color): List[Square] =
    board.byPiece(weakSide, Pawn).squares.filter { pawn =>
      forwardSquare(pawn, weakSide).exists(square =>
        board.pieceAt(square).exists(piece => piece.color != weakSide && piece.role == Pawn)
      )
    }.toList

  private def forwardSquare(square: Square, side: Color): Option[Square] =
    val rank = square.rank.value + (if side.white then 1 else -1)
    Square.at(square.file.value, rank)

  private def pressureFiles(square: String): List[String] =
    square.headOption.toList.map(_.toString)

  private def adjacentSquares(square: String): List[String] =
    for
      file <- square.headOption.toList
      rank <- square.lift(1).toList
      fileIndex = file - 'a'
      rankIndex = rank.asDigit
      df <- List(-1, 0, 1)
      dr <- List(-1, 0, 1)
      if df != 0 || dr != 0
      nextFile = (fileIndex + df + 'a').toChar
      nextRank = rankIndex + dr
      if nextFile >= 'a' && nextFile <= 'h' && nextRank >= 1 && nextRank <= 8
    yield s"$nextFile$nextRank"

  private def sideLabel(side: Color): String =
    if side.white then "white" else "black"
