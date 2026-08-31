package lila.chessjudgment.model.position

import chess.*

private[chessjudgment] enum BoardLineAxis:
  case File
  case Rank
  case Diagonal

private[chessjudgment] final case class OccupiedRayPiece(
    square: Square,
    role: Role,
    color: Color
)

private[chessjudgment] final case class BoardRayDirection(
    fileStep: Int,
    rankStep: Int,
    axis: BoardLineAxis
):
  require(
    (-1 to 1).contains(fileStep) && (-1 to 1).contains(rankStep) &&
      (fileStep != 0 || rankStep != 0),
    "a board ray direction needs one non-zero unit step"
  )

/** Exact topology in one direction from a slider. `squares` closes the empty
  * result as well as occupied barriers; `occupants` is the ordered occupied
  * projection of that same sequence.
  */
private[chessjudgment] final case class SliderRay(
    attackerSquare: Square,
    attackerRole: Role,
    attackerColor: Color,
    direction: BoardRayDirection,
    squares: List[Square],
    occupants: List[OccupiedRayPiece],
):
  require(
    occupants.map(_.square) == squares.filter(square => occupants.exists(_.square == square)),
    "slider-ray occupants must retain exact board-distance order"
  )

  lazy val controlledSquares: List[Square] =
    occupants.headOption match
      case Some(first) => squares.takeWhile(_ != first.square) :+ first.square
      case None        => squares

  lazy val controlDependencyByTarget: Map[Square, List[Square]] =
    controlledSquares
      .scanLeft(List(attackerSquare))((path, square) => path :+ square)
      .tail
      .map(path => path.last -> path)
      .toMap

private[chessjudgment] final case class BoardCellChange(
    square: Square,
    before: Option[Piece],
    after: Option[Piece]
):
  require(before != after, "a transition cell must change its occupant identity")

private[chessjudgment] final case class BoardPieceTransition(
    side: Color,
    beforeRole: Role,
    afterRole: Role,
    from: Square,
    to: Square
):
  require(from != to || beforeRole != afterRole, "a piece transition must change square or role")

/** Exact board cells and piece identities changed by one admitted legal move.
  * Castling, en passant, and promotion are interpreted once here.
  */
private[chessjudgment] final case class BoardMoveEffect(
    cellChanges: List[BoardCellChange],
    pieceTransitions: List[BoardPieceTransition]
):
  require(cellChanges.nonEmpty, "a legal move effect must change at least one board cell")
  require(cellChanges.map(_.square).distinct.size == cellChanges.size, "move-effect cells must be unique")
  require(pieceTransitions.nonEmpty, "a legal move effect must move or transform at least one piece")
  require(pieceTransitions.map(_.from).distinct.size == pieceTransitions.size, "move-effect origins must be unique")
  require(pieceTransitions.map(_.to).distinct.size == pieceTransitions.size, "move-effect destinations must be unique")

/** Dependency closure of one exact move effect over the two canonical slider
  * inventories.
  *
  * Geometric controls stop at the first occupied square of a ray. An occupied
  * slider relation owns the complete ordered occupant chain to the board edge,
  * so every aligned slider behind a changed cell must refresh that chain. The
  * closures remain separate because a distant occupant changes ray topology but
  * cannot change the attack map through an unchanged nearer barrier.
  */
private[chessjudgment] final case class BoardTransitionFootprint(
    moveEffect: BoardMoveEffect,
    affectedGeometricControlOrigins: Set[Square],
    affectedOccupiedRayOrigins: Set[Square]
):
  def cellChanges: List[BoardCellChange] = moveEffect.cellChanges
  def pieceTransitions: List[BoardPieceTransition] = moveEffect.pieceTransitions

  lazy val changedSquares: List[Square] = cellChanges.map(_.square)
  lazy val changedSquareSet: Set[Square] = changedSquares.toSet
  lazy val geometricControlOriginsToRefresh: Set[Square] =
    changedSquareSet ++ affectedGeometricControlOrigins
  lazy val pawnCellChanges: List[BoardCellChange] =
    cellChanges.filter(change =>
      change.before.exists(_.role == Pawn) || change.after.exists(_.role == Pawn)
    )
  lazy val pawnIdentityChanges: Set[(Color, Square)] =
    pawnCellChanges.flatMap(change =>
      (change.before.toList ++ change.after.toList).collect {
        case piece if piece.role == Pawn => piece.color -> change.square
      }
    ).toSet
  lazy val pawnFileChanges: Set[(Color, File)] =
    pawnIdentityChanges.map { case (side, square) => side -> square.file }

/** One board-law implementation for attack geometry and occupied slider rays.
  * It knows nothing about material value, plans, target hints, or whose turn
  * should hypothetically be changed.
  */
private[chessjudgment] object BoardGeometry:

  private final case class SliderDependencyClosure(
      geometricControlOrigins: Set[Square],
      occupiedRayOrigins: Set[Square]
  )

  def moveEffect(move: Move): BoardMoveEffect =
    val before = move.before.board
    val after = move.after.board
    val castlingSquares = move.castle.toList.flatMap(castle =>
      List(castle.king, castle.kingTo, castle.rook, castle.rookTo)
    )
    val candidateSquares =
      (List(move.orig, move.dest) ++ move.capture.toList ++ castlingSquares).distinct
    val cellChanges = candidateSquares
      .flatMap { square =>
        val beforePiece = before.pieceAt(square)
        val afterPiece = after.pieceAt(square)
        Option.when(beforePiece != afterPiece)(BoardCellChange(square, beforePiece, afterPiece))
      }
      .sortBy(_.square.key)
    val movementEndpoints = move.castle
      .map(castle => List(castle.king -> castle.kingTo, castle.rook -> castle.rookTo))
      .getOrElse(List(move.orig -> move.dest))
    val pieceTransitions = movementEndpoints.flatMap { case (from, to) =>
      for
        beforePiece <- before.pieceAt(from).toList
        afterPiece <- after.pieceAt(to).toList
        if beforePiece.color == afterPiece.color
        if from != to || beforePiece.role != afterPiece.role
      yield BoardPieceTransition(
        side = beforePiece.color,
        beforeRole = beforePiece.role,
        afterRole = afterPiece.role,
        from = from,
        to = to
      )
    }
    BoardMoveEffect(cellChanges, pieceTransitions)

  def transitionFootprint(move: Move, exactMoveEffect: BoardMoveEffect): BoardTransitionFootprint =
    val before = move.before.board
    val after = move.after.board
    val cellChanges = exactMoveEffect.cellChanges
    val changedSquares = cellChanges.map(_.square)
    val changedSliderOrigins = cellChanges.flatMap(change =>
      (change.before.toList ++ change.after.toList)
        .filter(piece => directions(piece.role).nonEmpty)
        .map(_ => change.square)
    ).toSet
    val boards = List(before, after)
    val dependencyClosures = boards.flatMap(board =>
      changedSquares.map(sliderDependencyClosure(board, _))
    )
    val affectedGeometricControlOrigins =
      changedSliderOrigins ++ dependencyClosures.flatMap(_.geometricControlOrigins)
    val affectedOccupiedRayOrigins =
      changedSliderOrigins ++ dependencyClosures.flatMap(_.occupiedRayOrigins)
    BoardTransitionFootprint(
      exactMoveEffect,
      affectedGeometricControlOrigins.toSet,
      affectedOccupiedRayOrigins.toSet
    )

  def fixedPatternControls(
      role: Role,
      square: Square,
      color: Color
  ): Bitboard =
    role match
      case Pawn   => square.pawnAttacks(color)
      case Knight => square.knightAttacks
      case King   => square.kingAttacks
      case _ =>
        throw IllegalArgumentException("slider controls must be projected from canonical rays")

  def sliderRays(board: Board): List[SliderRay] =
    sliderRays(board, (board.bishops | board.rooks | board.queens).squares.toSet)

  def sliderRays(board: Board, origins: Set[Square]): List[SliderRay] =
    origins.toList.sortBy(_.key).flatMap { attackerSquare =>
      board.pieceAt(attackerSquare).toList.flatMap { attacker =>
        directions(attacker.role).flatMap { direction =>
          val squares = squaresAlong(attackerSquare, direction.fileStep, direction.rankStep)
          val occupants = squares.flatMap { square =>
            board.pieceAt(square).map(piece => OccupiedRayPiece(square, piece.role, piece.color))
          }
          Option.when(squares.nonEmpty)(SliderRay(
            attackerSquare = attackerSquare,
            attackerRole = attacker.role,
            attackerColor = attacker.color,
            direction = direction,
            squares = squares,
            occupants = occupants
          )).toList
        }
      }
    }

  def controlsFromSliderRays(rays: List[SliderRay]): Map[Square, Bitboard] =
    rays.groupBy(_.attackerSquare).map { case (origin, exactRays) =>
      val first = exactRays.head
      require(
        exactRays.forall(ray =>
          ray.attackerSquare == origin &&
            ray.attackerRole == first.attackerRole &&
            ray.attackerColor == first.attackerColor
        ),
        "one slider origin must retain one exact piece identity"
      )
      val expectedDirections = directions(first.attackerRole)
        .filter(direction => squaresAlong(origin, direction.fileStep, direction.rankStep).nonEmpty)
        .toSet
      require(
        exactRays.map(_.direction).toSet == expectedDirections &&
          exactRays.map(_.direction).distinct.size == exactRays.size,
        "slider controls require the complete canonical ray inventory for their origin"
      )
      val controlledSquares = exactRays.flatMap(_.controlledSquares)
      require(
        controlledSquares.distinct.size == controlledSquares.size,
        "one slider target must belong to one canonical direction"
      )
      origin -> controlledSquares.foldLeft(Bitboard.empty)((controls, square) => controls.add(square))
    }

  private def sliderDependencyClosure(
      board: Board,
      changed: Square
  ): SliderDependencyClosure =
    val directionalOccupied = directions(Queen).map { direction =>
      direction.axis -> occupiedAlong(board, changed, direction.fileStep, direction.rankStep)
    }
    SliderDependencyClosure(
      geometricControlOrigins = directionalOccupied.flatMap { case (axis, occupied) =>
        occupied.headOption.filter(square => board.roleAt(square).exists(roleSupportsAxis(_, axis)))
      }.toSet,
      occupiedRayOrigins = directionalOccupied.flatMap { case (axis, occupied) =>
        occupied.filter(square => board.roleAt(square).exists(roleSupportsAxis(_, axis)))
      }.toSet
    )

  private def roleSupportsAxis(role: Role, axis: BoardLineAxis): Boolean =
    role == Queen ||
      role == Rook && axis != BoardLineAxis.Diagonal ||
      role == Bishop && axis == BoardLineAxis.Diagonal

  private def occupiedAlong(
      board: Board,
      from: Square,
      fileStep: Int,
      rankStep: Int
  ): List[Square] =
    squaresAlong(from, fileStep, rankStep).filter(board.pieceAt(_).nonEmpty)

  private def squaresAlong(from: Square, fileStep: Int, rankStep: Int): List[Square] =
    (1 to 7).flatMap { offset =>
      Square.at(
        from.file.value + fileStep * offset,
        from.rank.value + rankStep * offset
      )
    }.toList

  private def directions(role: Role): List[BoardRayDirection] =
    val orthogonal = List(
      BoardRayDirection(1, 0, BoardLineAxis.Rank),
      BoardRayDirection(-1, 0, BoardLineAxis.Rank),
      BoardRayDirection(0, 1, BoardLineAxis.File),
      BoardRayDirection(0, -1, BoardLineAxis.File)
    )
    val diagonal = List(
      BoardRayDirection(1, 1, BoardLineAxis.Diagonal),
      BoardRayDirection(1, -1, BoardLineAxis.Diagonal),
      BoardRayDirection(-1, 1, BoardLineAxis.Diagonal),
      BoardRayDirection(-1, -1, BoardLineAxis.Diagonal)
    )
    role match
      case Bishop => diagonal
      case Rook   => orthogonal
      case Queen  => orthogonal ++ diagonal
      case _      => Nil
