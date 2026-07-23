package lila.chessjudgment.analysis.structure

import _root_.chess.{ Board, Color, File, Pawn, Square }
import _root_.chess.format.{ Fen, Uci }
import _root_.chess.variant.Standard

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
  val Persistent = "persistent"
  val ResolvedByPressure = "resolved_by_pressure"
  val LiquidatedByDefense = "liquidated_by_defense"

  private val TargetEvidencePrefixes =
    List("weakness_target:", "fixed_target:", "coordinated_target:", "target_fixing:", "enemy_weak_square:", "weak_complex:", "target:")

  final case class LineOutcome(
      targetSquare: String,
      targetKind: String,
      status: String
  )

  private val priority =
    Map(
      IQP -> 0,
      BackwardPawn -> 1,
      IsolatedPawn -> 2,
      DoubledPawn -> 3,
      FixedPawn -> 4
    )

  def fromFenForMover(fen: String): List[WeaknessTargetProfile] =
    Fen.read(Standard, Fen.Full(fen)).map { position =>
      targetsForPressure(
        board = position.board,
        pressureSide = position.color,
        structureCode = structureCode(fen, position.board)
      )
    }.getOrElse(Nil)

  def targetsAfterLineFromFen(
      fen: String,
      moves: List[String],
      resultingFen: Option[String] = None,
      maxPlies: Int = 6
  ): List[WeaknessTargetProfile] =
    Fen.read(Standard, Fen.Full(fen)).map { start =>
      replayedLineBoard(start, moves, targetSquare = None, resultingFen, maxPlies).toList
        .flatMap { case (board, _) => targetsForPressure(board, start.color) }
    }.getOrElse(Nil)

  def lineOutcomeFromFen(
      fen: String,
      moves: List[String],
      targetSquare: String,
      resultingFen: Option[String] = None,
      maxPlies: Int = 6
  ): Option[LineOutcome] =
    Fen.read(Standard, Fen.Full(fen)).flatMap { start =>
      val target = normalizeSquare(targetSquare)
      val initialTarget =
        targetsForPressure(start.board, start.color).find(_.targetSquare == target)
      initialTarget.flatMap { initial =>
        replayedLineBoard(start, moves, targetSquare = Some(target), resultingFen, maxPlies).map { case (board, capturedByPressure) =>
          val stillTarget =
            targetsForPressure(board, start.color).exists(_.targetSquare == target)
          val status =
            if stillTarget then Persistent
            else if capturedByPressure then ResolvedByPressure
            else LiquidatedByDefense
          LineOutcome(target, initial.kind, status)
        }
      }
    }

  def targetEvidenceSquares(evidenceCodes: List[String]): List[String] =
    evidenceCodes.flatMap(targetEvidenceSquare).distinct

  def targetEvidenceSquare(evidenceCode: String): Option[String] =
    val lower = Option(evidenceCode).getOrElse("").trim.toLowerCase
    TargetEvidencePrefixes
      .collectFirst { case prefix if lower.startsWith(prefix) =>
        lower.stripPrefix(prefix).trim
      }
      .filter(_.matches("""[a-h][1-8]"""))

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

  private def structureCode(fen: String, board: Board): Option[String] =
    PositionAnalyzer.extractFeatures(fen, 1).map { features =>
      PawnStructureAssessor.assess(features, board).primary.toString
    }.filterNot(_ == "Unknown")

  private def sideLabel(side: Color): String =
    if side.white then "white" else "black"

  private def replayLine(
      start: _root_.chess.Position,
      moves: List[String],
      targetSquare: Option[String]
  ): Option[(_root_.chess.Position, Boolean)] =
    val pressureSide = start.color
    def loop(position: _root_.chess.Position, remaining: List[String], capturedByPressure: Boolean)
        : Option[(_root_.chess.Position, Boolean)] =
      remaining match
        case Nil => Some((position, capturedByPressure))
        case raw :: rest =>
          legalUciMove(position, raw).flatMap { move =>
            val targetCapture =
              targetSquare.forall(_ == move.dest.key) &&
                position.board
                  .pieceAt(move.dest)
                  .exists(piece => piece.color != pressureSide && piece.role == Pawn) &&
                move.piece.color == pressureSide
            loop(move.after, rest, capturedByPressure || targetCapture)
          }
    loop(start, moves, capturedByPressure = false)

  private def replayedLineBoard(
      start: _root_.chess.Position,
      moves: List[String],
      targetSquare: Option[String],
      resultingFen: Option[String],
      maxPlies: Int
  ): Option[(Board, Boolean)] =
    val boundedMoves = moves.take(maxPlies)
    if resultingFen.forall(_.trim.isEmpty) && targetSquare.isEmpty then
      replayLine(start, boundedMoves, targetSquare).map { case (position, capturedByPressure) =>
        position.board -> capturedByPressure
      }
    else
      val replayedFull =
        if moves.nonEmpty then replayLine(start, moves, targetSquare) else None
      trustedResultingBoard(moves, resultingFen, replayedFull)
        .map(board => board -> replayedFull.exists(_._2))
        .orElse {
          val replayedBounded =
            if moves.nonEmpty && boundedMoves == moves then replayedFull
            else replayLine(start, boundedMoves, targetSquare)
          replayedBounded
            .map(_._1.board)
            .map(board => board -> replayedFull.orElse(replayedBounded).exists(_._2))
        }

  private def trustedResultingBoard(
      moves: List[String],
      resultingFen: Option[String],
      replayedFull: Option[(_root_.chess.Position, Boolean)]
  ): Option[Board] =
    resultingFen.flatMap(next => Fen.read(Standard, Fen.Full(next)).map(_.board)).filter { board =>
      moves.isEmpty || replayedFull.exists { case (position, _) => position.board == board }
    }

  private def legalUciMove(position: _root_.chess.Position, raw: String): Option[_root_.chess.Move] =
    Uci(raw).collect { case move: Uci.Move => move }.flatMap(position.move(_).toOption)

  private def normalizeSquare(square: String): String =
    square.trim.toLowerCase
