package lila.chessjudgment.model.line

import chess.{ Move, Position, Role }
import chess.format.{ Fen, Uci }
import chess.variant.Standard

/**
 * The single authoritative result of replaying a legal engine line.
 * Board-aware consumers share this object instead of parsing the same UCI
 * sequence into tactical, structural, and assessment-specific line types.
 */
private[chessjudgment] final case class LegalReplayStep(
    ply: Int,
    uci: String,
    before: Position,
    move: Move,
    after: Position,
    capturedRole: Option[Role]
)

private[chessjudgment] object PrincipalVariationEvidence:

  final case class LineMoveRef(
      ply: Int,
      uci: String,
      fenAfter: String
  )

  final case class LineVariationRef(
      moves: List[LineMoveRef]
  )

  final case class LineFacts(
      line: LineVariationRef,
      first: LineMoveRef,
      reply: Option[LineMoveRef],
      continuation: Option[LineMoveRef],
      continuationTail: List[LineMoveRef] = Nil
  )

  final case class ValidatedLine(
      line: LineVariationRef,
      moves: List[LineMoveRef]
  ):
    def first: Option[LineMoveRef] = moves.headOption
    def reply: Option[LineMoveRef] = moves.lift(1)
    def continuation: Option[LineMoveRef] = moves.lift(2)

  def validatedLineFromStart(
      startFen: String,
      line: LineVariationRef
  ): Option[ValidatedLine] =
    val moves = line.moves
    Option.when(
      moves.nonEmpty &&
        moves.forall(_.fenAfter.trim.nonEmpty) &&
        strictlyOrdered(moves)
    )(moves)
      .flatMap(replay(startFen, _))
      .map(validatedMoves => ValidatedLine(line, validatedMoves))

  def legalFenAfter(fen: String, uciMove: String): Option[String] =
    legalMoveReplay(fen, List(uciMove), startPly = 0)
      .flatMap(_.headOption)
      .map(step => Fen.write(step.after).value)

  def legalReplay(startFen: String, moves: List[String], startPly: Int): Option[List[(String, LineMoveRef)]] =
    legalMoveReplay(startFen, moves, startPly).map(_.map { step =>
      Fen.write(step.before).value ->
        LineMoveRef(step.ply, step.uci, Fen.write(step.after).value)
    })

  def legalMoveReplay(
      startFen: String,
      moves: List[String],
      startPly: Int
  ): Option[List[LegalReplayStep]] =
    val accepted = scala.collection.mutable.ListBuffer.empty[LegalReplayStep]
    var ply = startPly
    var legal = true
    var current = Fen.read(Standard, Fen.Full(normalizeFen(startFen)))
    moves.iterator.map(normalizeUci).foreach { uci =>
      if legal then
        current.flatMap(position => legalMove(position, uci)) match
          case Some(move) =>
            val before = current.get
            val capturedRole =
              move.capture
                .flatMap(before.board.roleAt)
                .orElse(before.board.roleAt(move.dest))
            ply += 1
            accepted += LegalReplayStep(
              ply = ply,
              uci = uci,
              before = before,
              move = move,
              after = move.after,
              capturedRole = capturedRole
            )
            current = Some(move.after)
          case None =>
            legal = false
    }
    Option.when(legal)(accepted.toList)

  def normalizeUci(uci: String): String =
    Option(uci).getOrElse("").trim.toLowerCase

  def sameBoardState(left: String, right: String): Boolean =
    boardStateFen(left) == boardStateFen(right)

  private def legalMove(position: Position, uci: String): Option[Move] =
    Option
      .when(uci.matches("(?i)^[a-h][1-8][a-h][1-8][qrbn]?$"))(uci)
      .flatMap(Uci(_).collect { case move: Uci.Move => move })
      .flatMap(position.move(_).toOption)

  private def replay(startFen: String, moves: List[LineMoveRef]): Option[List[LineMoveRef]] =
    legalReplay(startFen, moves.map(_.uci), moves.headOption.map(_.ply - 1).getOrElse(0))
      .map(_.map(_._2))
      .filter(replayed =>
        replayed.zip(moves).forall { case (actual, expected) =>
          actual.ply == expected.ply && sameBoardState(actual.fenAfter, expected.fenAfter)
        }
      )

  private def strictlyOrdered(moves: List[LineMoveRef]): Boolean =
    moves.sliding(2).forall {
      case List(left, right) => left.ply < right.ply
      case _                 => true
    }

  private def normalizeFen(fen: String): String =
    Option(fen).getOrElse("").trim.split("\\s+").filter(_.nonEmpty).mkString(" ")

  private def boardStateFen(fen: String): String =
    normalizeFen(fen).split("\\s+").take(4).mkString(" ")
