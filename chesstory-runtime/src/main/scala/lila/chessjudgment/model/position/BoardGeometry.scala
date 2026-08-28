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

/** Exact occupied topology in one direction from a slider.
  *
  * `occupants` contains every occupied square in distance order. The first is
  * the immediate barrier. Named patterns such as pin or battery are
  * projections of this topology; they are not separate geometry producers.
  */
private[chessjudgment] final case class OccupiedSliderRay(
    attackerSquare: Square,
    attackerRole: Role,
    attackerColor: Color,
    occupants: List[OccupiedRayPiece],
    axis: BoardLineAxis
):
  require(occupants.nonEmpty, "an occupied slider ray needs at least its immediate barrier")

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

  def geometricControls(
      role: Role,
      square: Square,
      color: Color,
      occupied: Bitboard
  ): Bitboard =
    role match
      case Pawn   => square.pawnAttacks(color)
      case Knight => square.knightAttacks
      case Bishop => square.bishopAttacks(occupied)
      case Rook   => square.rookAttacks(occupied)
      case Queen  => square.queenAttacks(occupied)
      case King   => square.kingAttacks

  def occupiedSliderRays(board: Board): List[OccupiedSliderRay] =
    occupiedSliderRays(board, (board.bishops | board.rooks | board.queens).squares.toSet)

  def occupiedSliderRays(board: Board, origins: Set[Square]): List[OccupiedSliderRay] =
    origins.toList.sortBy(_.key).flatMap { attackerSquare =>
      board.pieceAt(attackerSquare).toList.flatMap { attacker =>
        directions(attacker.role).flatMap { case (fileStep, rankStep, axis) =>
          val occupants = occupiedAlong(board, attackerSquare, fileStep, rankStep).flatMap { square =>
            board.pieceAt(square).map(piece => OccupiedRayPiece(square, piece.role, piece.color))
          }
          Option.when(occupants.nonEmpty) {
              OccupiedSliderRay(
                attackerSquare = attackerSquare,
                attackerRole = attacker.role,
                attackerColor = attacker.color,
                occupants = occupants,
                axis = axis
              )
          }
        }
      }
    }

  private def sliderDependencyClosure(
      board: Board,
      changed: Square
  ): SliderDependencyClosure =
    val directionalOccupied = directions(Queen).map { case (fileStep, rankStep, axis) =>
      axis -> occupiedAlong(board, changed, fileStep, rankStep)
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

  def lineSpan(from: Square, to: Square): List[Square] =
    if from == to then List(from)
    else from :: movementPath(Piece(White, Queen), from, to) ::: List(to)

  /** Complete board ray beginning at `from` and passing through `through`.
    * A closed `RayBarrier` depends on this span because its ordered occupant
    * list also certifies that no omitted occupant exists beyond the last one.
    */
  def raySpanToEdge(from: Square, through: Square): List[Square] =
    lineDirection(Queen, from, through)
      .map { case (fileStep, rankStep) => from :: squaresAlong(from, fileStep, rankStep) }
      .getOrElse(Nil)

  def liesStrictlyBetween(from: Square, to: Square, square: Square): Boolean =
    lineSpan(from, to).drop(1).dropRight(1).contains(square)

  /** Squares crossed by the named piece on one geometrically valid move.
    * Endpoints are excluded; occupancy and move legality remain the caller's
    * responsibility.
    */
  def movementPath(piece: Piece, from: Square, to: Square): List[Square] =
    val fileDelta = to.file.value - from.file.value
    val rankDelta = to.rank.value - from.rank.value
    val pawnDoubleStep =
      piece.role == Pawn &&
        fileDelta == 0 &&
        rankDelta == (if piece.color.white then 2 else -2)
    if pawnDoubleStep then
      Square.at(from.file.value, from.rank.value + (if piece.color.white then 1 else -1)).toList
    else
      lineDirection(piece.role, from, to).toList.flatMap { case (fileStep, rankStep) =>
        squaresAlong(from, fileStep, rankStep).take(math.max(fileDelta.abs, rankDelta.abs) - 1)
      }

  private def occupiedAlong(
      board: Board,
      from: Square,
      fileStep: Int,
      rankStep: Int
  ): List[Square] =
    squaresAlong(from, fileStep, rankStep).filter(board.pieceAt(_).nonEmpty)

  private def lineDirection(role: Role, from: Square, to: Square): Option[(Int, Int)] =
    val fileDelta = to.file.value - from.file.value
    val rankDelta = to.rank.value - from.rank.value
    val diagonal = fileDelta.abs == rankDelta.abs && fileDelta != 0
    val straight = (fileDelta == 0) != (rankDelta == 0)
    Option.when(
      (diagonal || straight) &&
        (role == Queen || role == Bishop && diagonal || role == Rook && straight)
    )(Integer.signum(fileDelta) -> Integer.signum(rankDelta))

  private def squaresAlong(from: Square, fileStep: Int, rankStep: Int): List[Square] =
    (1 to 7).flatMap { offset =>
      Square.at(
        from.file.value + fileStep * offset,
        from.rank.value + rankStep * offset
      )
    }.toList

  private def directions(role: Role): List[(Int, Int, BoardLineAxis)] =
    val orthogonal = List(
      (1, 0, BoardLineAxis.Rank),
      (-1, 0, BoardLineAxis.Rank),
      (0, 1, BoardLineAxis.File),
      (0, -1, BoardLineAxis.File)
    )
    val diagonal = List(
      (1, 1, BoardLineAxis.Diagonal),
      (1, -1, BoardLineAxis.Diagonal),
      (-1, 1, BoardLineAxis.Diagonal),
      (-1, -1, BoardLineAxis.Diagonal)
    )
    role match
      case Bishop => diagonal
      case Rook   => orthogonal
      case Queen  => orthogonal ++ diagonal
      case _      => Nil
