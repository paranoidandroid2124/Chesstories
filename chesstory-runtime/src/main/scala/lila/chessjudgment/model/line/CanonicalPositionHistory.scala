package lila.chessjudgment.model.line

import chess.{ FullMoveNumber, HalfMoveClock, Move, Position, Role }
import chess.format.Fen
import chess.variant.Standard

enum CanonicalPositionHistoryFailure:
  case InvalidInitialFen
  case InvalidCurrentFen
  case InitialPlyOutOfRange
  case MovePrefixPlyOutOfRange
  case IllegalMovePrefix
  case HalfMoveClockOverflow
  case FullMoveNumberOverflow
  case CurrentFenMismatch

enum PreInitialHistoryKnowledge:
  case KnownEmpty, Unknown

final case class CanonicalPositionHistoryStep(
    index: Int,
    ply: Int,
    uci: String,
    before: Position,
    beforeFen: String,
    move: Move,
    capturedRole: Option[Role],
    castle: Option[Move.Castle],
    after: Position,
    afterFen: String
)

/**
 * The canonical legal history for one position-commentary job.
 *
 * scalachess owns board, turn, castling, en-passant, legal replay, and
 * repetition hashes. This owner retains the unclamped FEN counters that the
 * dependency's FEN reader intentionally bounds.
 */
final class CanonicalPositionHistory private (
    val initialFen: String,
    val movePrefixUci: List[String],
    val segmentReplaySteps: List[CanonicalPositionHistoryStep],
    val preInitialHistoryKnowledge: PreInitialHistoryKnowledge,
    val currentFen: String,
    val currentPosition: Position,
    val repetitionHistoryComplete: Boolean,
    private val currentFullMoveNumber: Int,
    val currentPly: Int
):
  /** The canonical root move set for this history occurrence. scalachess
    * generates it once; scheduling and admission reuse the same ordered value.
    */
  lazy val currentLegalMoveUcis: List[String] =
    PrincipalVariationEvidence.actualLegalMoves(currentPosition).map(_.toUci.uci).sorted

  def extend(
      relativeMovesUci: List[String]
  ): Either[CanonicalPositionHistoryFailure, CanonicalPositionHistory] =
    val normalizedMoves = relativeMovesUci.map(PrincipalVariationEvidence.normalizeUci)
    if currentPly.toLong + normalizedMoves.size.toLong > Int.MaxValue then
      Left(CanonicalPositionHistoryFailure.MovePrefixPlyOutOfRange)
    else
      PrincipalVariationEvidence
        .legalMoveReplayFromPosition(currentPosition, normalizedMoves, currentPly)
        .toRight(CanonicalPositionHistoryFailure.IllegalMovePrefix)
        .flatMap(extendAdmitted)

  /** Extends this history with steps already admitted by the canonical replay
    * owner. This validates occurrence continuity but never generates moves.
    */
  private[chessjudgment] def extendAdmitted(
      additionalSteps: List[LegalReplayStep]
  ): Either[CanonicalPositionHistoryFailure, CanonicalPositionHistory] =
    val contiguous = additionalSteps.zipWithIndex.forall { case (step, index) =>
      val expectedBefore = additionalSteps.lift(index - 1).map(_.after).getOrElse(currentPosition)
      step.ply == currentPly + index + 1 &&
        step.before == expectedBefore &&
        step.move.after == step.after
    }
    if currentPly.toLong + additionalSteps.size.toLong > Int.MaxValue then
      Left(CanonicalPositionHistoryFailure.MovePrefixPlyOutOfRange)
    else if !contiguous then
      Left(CanonicalPositionHistoryFailure.IllegalMovePrefix)
    else
      CanonicalPositionHistory.acceptTerminalBoundaries(additionalSteps)
        .flatMap(_ => CanonicalPositionHistory.acceptHalfMoveClocks(additionalSteps))
        .flatMap(_ => CanonicalPositionHistory.fullMoveNumberAfter(currentFullMoveNumber, additionalSteps))
        .map { nextFullMoveNumber =>
          val normalizedMoves = additionalSteps.map(step => PrincipalVariationEvidence.normalizeUci(step.uci))
          val nextPosition = additionalSteps.lastOption.fold(currentPosition)(_.after)
          val nextSegmentReplaySteps = CanonicalPositionHistory.materializeSteps(
            existingSteps = segmentReplaySteps,
            beforeFullMoveNumber = currentFullMoveNumber,
            replay = additionalSteps
          )
          new CanonicalPositionHistory(
            initialFen = initialFen,
            movePrefixUci = movePrefixUci ++ normalizedMoves,
            segmentReplaySteps = nextSegmentReplaySteps,
            preInitialHistoryKnowledge = preInitialHistoryKnowledge,
            currentFen = CanonicalPositionHistory.render(nextPosition, nextFullMoveNumber),
            currentPosition = nextPosition,
            repetitionHistoryComplete = repetitionHistoryComplete ||
              additionalSteps.exists(CanonicalPositionHistory.establishesRepetitionCompleteness),
            currentFullMoveNumber = nextFullMoveNumber,
            currentPly = currentPly + additionalSteps.size
          )
        }

  /**
   * Replays an engine suffix in full, while exposing only its first automatic
   * terminal boundary. Unlike [[extend]], this inspection deliberately does
   * not admit canonical history after that boundary.
   */
  def inspectAutomaticTerminalBoundary(
      relativeMovesUci: List[String]
  ): Either[CanonicalPositionHistoryFailure, Option[(List[String], AutomaticTerminal)]] =
    val normalizedMoves = relativeMovesUci.map(PrincipalVariationEvidence.normalizeUci)
    if currentPly.toLong + normalizedMoves.size.toLong > Int.MaxValue then
      Left(CanonicalPositionHistoryFailure.MovePrefixPlyOutOfRange)
    else
      PrincipalVariationEvidence
        .legalMoveReplayFromPosition(currentPosition, normalizedMoves, currentPly)
        .toRight(CanonicalPositionHistoryFailure.IllegalMovePrefix)
        .flatMap { replay =>
          CanonicalPositionHistory.acceptHalfMoveClocks(replay)
            .flatMap(_ => CanonicalPositionHistory.fullMoveNumberAfter(currentFullMoveNumber, replay))
            .map { _ =>
              replay.zipWithIndex.collectFirst {
                case (step, index) if PositionRuleAssessment.automaticTerminal(step.after).nonEmpty =>
                  replay.take(index + 1).map(_.uci) ->
                    PositionRuleAssessment.automaticTerminal(step.after).get
              }
            }
        }

object CanonicalPositionHistory:
  def normalizeFen(fen: String): String =
    PrincipalVariationEvidence.normalizeFen(fen)

  def sameBoardState(leftFen: String, rightFen: String): Boolean =
    PrincipalVariationEvidence.sameBoardState(leftFen, rightFen)

  def from(
      initialFen: String,
      movePrefixUci: List[String],
      currentFen: String
  ): Either[CanonicalPositionHistoryFailure, CanonicalPositionHistory] =
    for
      initial <- parseCanonicalFen(initialFen, CanonicalPositionHistoryFailure.InvalidInitialFen)
      expectedCurrent <- parseCanonicalFen(currentFen, CanonicalPositionHistoryFailure.InvalidCurrentFen)
      initialPly <- plyAt(initial.fullMoveNumber, initial.position)
        .toRight(CanonicalPositionHistoryFailure.InitialPlyOutOfRange)
      normalizedMoves = movePrefixUci.map(PrincipalVariationEvidence.normalizeUci)
      _ <- Either.cond(
        initialPly.toLong + normalizedMoves.size.toLong <= Int.MaxValue,
        (),
        CanonicalPositionHistoryFailure.MovePrefixPlyOutOfRange
      )
      replay <- PrincipalVariationEvidence
        .legalMoveReplayFromPosition(initial.position, normalizedMoves, initialPly)
        .toRight(CanonicalPositionHistoryFailure.IllegalMovePrefix)
      _ <- acceptTerminalBoundaries(replay)
      _ <- acceptHalfMoveClocks(replay)
      currentFullMoveNumber <- fullMoveNumberAfter(initial.fullMoveNumber, replay)
      replayedPosition = replay.lastOption.fold(initial.position)(_.after)
      segmentReplaySteps = materializeSteps(Nil, initial.fullMoveNumber, replay)
      replayedCurrentFen = segmentReplaySteps.lastOption.fold(render(replayedPosition, currentFullMoveNumber))(_.afterFen)
      _ <- Either.cond(
        replayedCurrentFen == expectedCurrent.canonicalFen,
        (),
        CanonicalPositionHistoryFailure.CurrentFenMismatch
      )
    yield new CanonicalPositionHistory(
      initialFen = initial.canonicalFen,
      movePrefixUci = normalizedMoves,
      segmentReplaySteps = segmentReplaySteps,
      preInitialHistoryKnowledge =
        if initial.canonicalFen == Standard.initialFen.value then PreInitialHistoryKnowledge.KnownEmpty
        else PreInitialHistoryKnowledge.Unknown,
      currentFen = replayedCurrentFen,
      currentPosition = replayedPosition,
      repetitionHistoryComplete = initial.halfMoveClock == HalfMoveClock.initial ||
        replay.exists(establishesRepetitionCompleteness),
      currentFullMoveNumber = currentFullMoveNumber,
      currentPly = initialPly + normalizedMoves.size
    )

  private def materializeSteps(
      existingSteps: List[CanonicalPositionHistoryStep],
      beforeFullMoveNumber: Int,
      replay: List[LegalReplayStep]
  ): List[CanonicalPositionHistoryStep] =
    val (_, appendedSteps) = replay.foldLeft(beforeFullMoveNumber -> List.empty[CanonicalPositionHistoryStep]) {
      case ((fullMoveNumber, steps), step) =>
        val afterFullMoveNumber =
          if step.before.color.black then fullMoveNumber + 1
          else fullMoveNumber
        val materialized = CanonicalPositionHistoryStep(
          index = existingSteps.size + steps.size,
          ply = step.ply,
          uci = step.uci,
          before = step.before,
          beforeFen = render(step.before, fullMoveNumber),
          move = step.move,
          capturedRole = step.capturedRole,
          castle = step.move.castle,
          after = step.after,
          afterFen = render(step.after, afterFullMoveNumber)
        )
        afterFullMoveNumber -> (steps :+ materialized)
    }
    existingSteps ++ appendedSteps

  private final case class ParsedCanonicalFen(
      canonicalFen: String,
      position: Position,
      halfMoveClock: HalfMoveClock,
      fullMoveNumber: Int
  )

  private def parseCanonicalFen(
      rawFen: String,
      failure: CanonicalPositionHistoryFailure
  ): Either[CanonicalPositionHistoryFailure, ParsedCanonicalFen] =
    val normalizedFen = rawFen.trim.split("\\s+").filter(_.nonEmpty).mkString(" ")
    normalizedFen.split(" ", -1).toList match
      case List(_, _, _, _, rawHalfMoveClock, rawFullMoveNumber) =>
        for
          halfMoveClock <- rawHalfMoveClock.toIntOption
            .filter(_ >= 0)
            .toRight(failure)
          fullMoveNumber <- rawFullMoveNumber.toIntOption
            .filter(_ >= 1)
            .toRight(failure)
          parsed <- PrincipalVariationEvidence.readPosition(normalizedFen).toRight(failure)
          position = parsed
          canonicalFen = render(position, fullMoveNumber)
          _ <- Either.cond(canonicalFen == normalizedFen, (), failure)
        yield ParsedCanonicalFen(canonicalFen, position, HalfMoveClock(halfMoveClock), fullMoveNumber)
      case _ => Left(failure)

  private def plyAt(fullMoveNumber: Int, position: Position): Option[Int] =
    val ply = (fullMoveNumber.toLong - 1L) * 2L + Option.when(position.color.black)(1L).getOrElse(0L)
    Option.when(ply <= Int.MaxValue)(ply.toInt)

  private def fullMoveNumberAfter(
      initialFullMoveNumber: Int,
      replay: List[LegalReplayStep]
  ): Either[CanonicalPositionHistoryFailure, Int] =
    val blackMoves = replay.count(_.before.color.black).toLong
    val next = initialFullMoveNumber.toLong + blackMoves
    Either.cond(
      next <= Int.MaxValue,
      next.toInt,
      CanonicalPositionHistoryFailure.FullMoveNumberOverflow
    )

  private def acceptHalfMoveClocks(
      replay: List[LegalReplayStep]
  ): Either[CanonicalPositionHistoryFailure, Unit] =
    Either.cond(
      replay.forall(_.after.history.halfMoveClock >= HalfMoveClock.initial),
      (),
      CanonicalPositionHistoryFailure.HalfMoveClockOverflow
    )

  private def acceptTerminalBoundaries(
      replay: List[LegalReplayStep]
  ): Either[CanonicalPositionHistoryFailure, Unit] =
    Either.cond(
      replay.forall(step => PositionRuleAssessment.automaticTerminal(step.before).isEmpty),
      (),
      CanonicalPositionHistoryFailure.IllegalMovePrefix
    )

  private def establishesRepetitionCompleteness(step: LegalReplayStep): Boolean =
    step.after.history.halfMoveClock == HalfMoveClock.initial ||
      step.before.history.castles != step.after.history.castles

  private def render(position: Position, fullMoveNumber: Int): String =
    Fen.write(position, FullMoveNumber(fullMoveNumber)).value
