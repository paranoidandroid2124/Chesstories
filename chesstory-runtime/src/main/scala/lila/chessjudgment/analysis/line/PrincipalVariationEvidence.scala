package lila.chessjudgment.analysis.line

import chess.format.{ Fen, Uci }
import chess.variant.Standard

private[chessjudgment] object PrincipalVariationEvidence:

  final case class LineMoveRef(
      ply: Int,
      uci: String,
      fenAfter: String
  )

  final case class LineVariationRef(
      moves: List[LineMoveRef]
  )

  final case class LineEvidenceRefs(
      startFen: String,
      variations: List[LineVariationRef]
  )

  final case class LineFacts(
      line: LineVariationRef,
      first: LineMoveRef,
      reply: Option[LineMoveRef],
      continuation: Option[LineMoveRef],
      continuationTail: List[LineMoveRef] = Nil
  ):
    def checkedContinuations: List[LineMoveRef] =
      (continuation.toList ++ continuationTail).distinct

  final case class ValidatedLine(
      line: LineVariationRef,
      moves: List[LineMoveRef]
  ):
    def first: Option[LineMoveRef] = moves.headOption
    def reply: Option[LineMoveRef] = moves.lift(1)
    def continuation: Option[LineMoveRef] = moves.lift(2)

  def firstCoupled(startFen: String, playedUci: String, refs: Option[LineEvidenceRefs]): Option[LineFacts] =
    val normalizedPlayed = normalizeUci(playedUci)
    refs.filter(ref => normalizeFen(ref.startFen) == normalizeFen(startFen)).toList
      .flatMap(_.variations)
      .flatMap(line => validatedLine(startFen, line, normalizedPlayed))
      .find(_.reply.nonEmpty)
      .flatMap { validated =>
        validated.first.map { first =>
          LineFacts(
            validated.line,
            first,
            validated.reply,
            validated.continuation,
            validated.moves.drop(3).take(3)
          )
        }
      }

  def playedCoupled(startFen: String, playedUci: String, refs: Option[LineEvidenceRefs]): Option[LineFacts] =
    val normalizedPlayed = normalizeUci(playedUci)
    refs.filter(ref => normalizeFen(ref.startFen) == normalizeFen(startFen)).toList
      .flatMap(_.variations)
      .flatMap(line => validatedLine(startFen, line, normalizedPlayed))
      .flatMap { validated =>
        validated.first.map { first =>
          LineFacts(
            validated.line,
            first,
            validated.reply,
            validated.continuation,
            validated.moves.drop(3).take(3)
          )
        }
      }
      .headOption

  def validatedLine(
      startFen: String,
      line: LineVariationRef,
      playedUci: String
  ): Option[ValidatedLine] =
    val normalizedPlayed = normalizeUci(playedUci)
    val moves = line.moves
    Option.when(
      moves.headOption.exists(move => normalizeUci(move.uci) == normalizedPlayed) &&
        moves.forall(_.fenAfter.trim.nonEmpty) &&
        strictlyOrdered(moves)
    )(moves)
      .flatMap(replay(startFen, _))
      .map(validatedMoves => ValidatedLine(line, validatedMoves))

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
    val normalized = normalizeUci(uciMove)
    Option.when(normalized.matches("(?i)^[a-h][1-8][a-h][1-8][qrbn]?$"))(normalized).flatMap { moveStr =>
      Fen.read(Standard, Fen.Full(fen)).flatMap { position =>
        Uci(moveStr)
          .collect { case move: Uci.Move => move }
          .flatMap(position.move(_).toOption)
          .map(result => Fen.write(result.after).value)
          .filter(after => boardStateFen(after) != boardStateFen(fen))
      }
    }

  def legalReplay(startFen: String, moves: List[String], startPly: Int): Option[List[(String, LineMoveRef)]] =
    val accepted = scala.collection.mutable.ListBuffer.empty[(String, LineMoveRef)]
    var fen = normalizeFen(startFen)
    var ply = startPly
    var legal = true
    moves.iterator.map(normalizeUci).foreach { move =>
      if legal then
        legalFenAfter(fen, move) match
          case Some(after) =>
            ply += 1
            accepted += fen -> LineMoveRef(ply, move, after)
            fen = after
          case None =>
            legal = false
    }
    Option.when(legal)(accepted.toList)

  def normalizeUci(uci: String): String =
    Option(uci).getOrElse("").trim.toLowerCase

  def sameBoardState(left: String, right: String): Boolean =
    boardStateFen(left) == boardStateFen(right)

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
