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
    ply: Int,
    uci: String,
    before: Position,
    beforeFen: String,
    move: Move,
    capturedRole: Option[Role],
    after: Position,
    afterFen: String
)

/** One suffix admitted by the canonical history owner. The complete history
  * retains the exact game occurrence; consumers may bind a logical line to it
  * without generating the same legal moves again.
  */
final class CanonicalHistoryContinuation private[line] (
    val startingFen: String,
    val resultingHistory: CanonicalPositionHistory,
    val moves: List[String],
    val automaticTerminal: Option[AutomaticTerminal]
):
  lazy val resultingRuleAssessment: PositionRuleAssessment =
    automaticTerminal
      .map(PositionRuleAssessment.Terminal.apply)
      .getOrElse(
        PositionRuleAssessment.assessCertifiedAutomaticNonterminal(resultingHistory)
      )

  def bind(
      evaluation: CandidateLineEvaluation
  ): Option[AdmittedCandidateLine] =
    AdmittedCandidateLine.certified(this, evaluation)

  /** Binds a logical line only when its origin and prefix are the exact
    * occurrence immediately preceding this already admitted suffix. The
    * physical history remains the owner; no move is generated or rebased.
    */
  def bindFromOccurrence(
      logicalOriginFen: String,
      logicalPrefixMovesUci: List[String],
      evaluation: CandidateLineEvaluation
  ): Option[AdmittedCandidateLine] =
    val normalizedOriginFen = CanonicalPositionHistory.normalizeFen(logicalOriginFen)
    val normalizedPrefixMoves =
      logicalPrefixMovesUci.map(PrincipalVariationEvidence.normalizeUci)
    val historyBeforeSuffix =
      resultingHistory.segmentReplaySteps.dropRight(moves.size)
    val physicalPrefixSteps = historyBeforeSuffix.takeRight(normalizedPrefixMoves.size)
    val prefixMovesMatch =
      physicalPrefixSteps.size == normalizedPrefixMoves.size &&
        physicalPrefixSteps.map(step => PrincipalVariationEvidence.normalizeUci(step.uci)) ==
          normalizedPrefixMoves
    val prefixBoundaryMatches = physicalPrefixSteps match
      case Nil => normalizedOriginFen == startingFen
      case first :: _ =>
        first.beforeFen == normalizedOriginFen &&
          physicalPrefixSteps.last.afterFen == startingFen
    val expectedEvaluationMoves = normalizedPrefixMoves ++ moves
    val evaluationMovesMatch =
      evaluation.moves.map(PrincipalVariationEvidence.normalizeUci) == expectedEvaluationMoves
    Option
      .when(prefixMovesMatch && prefixBoundaryMatches && evaluationMovesMatch)(())
      .flatMap(_ => bind(evaluation))

/** A scored candidate line bound to one already admitted history occurrence.
  * Runtime scheduling may inspect the evaluation; board-aware normalization
  * obtains the retained steps through [[continuationAfter]] and never replays
  * the UCI sequence.
  */
final class AdmittedCandidateLine private[line] (
    val evaluation: CandidateLineEvaluation,
    private val continuation: CanonicalHistoryContinuation
):
  val automaticTerminal: Option[AutomaticTerminal] = continuation.automaticTerminal

  def continuationAfter(
      prefix: CanonicalPositionHistory
  ): Option[List[CanonicalPositionHistoryStep]] =
    continuation.resultingHistory
      .admittedStepsAfter(prefix)
      .filter(steps =>
        steps.map(step => PrincipalVariationEvidence.normalizeUci(step.uci)) ==
          evaluation.moves.map(PrincipalVariationEvidence.normalizeUci)
      )

private[line] object AdmittedCandidateLine:
  def certified(
      continuation: CanonicalHistoryContinuation,
      evaluation: CandidateLineEvaluation
  ): Option[AdmittedCandidateLine] =
    val normalizedEvaluationMoves = evaluation.moves.map(PrincipalVariationEvidence.normalizeUci)
    val normalizedContinuationMoves = continuation.moves.map(PrincipalVariationEvidence.normalizeUci)
    val occurrenceBound =
      normalizedEvaluationMoves.nonEmpty &&
        normalizedContinuationMoves.nonEmpty &&
        normalizedEvaluationMoves.endsWith(normalizedContinuationMoves)
    val terminalBound = (evaluation, continuation.automaticTerminal) match
      case (CandidateLineEvaluation.EngineSearch(_), None) => true
      case (CandidateLineEvaluation.ExactAutomaticTerminal(_, declared), Some(certified)) =>
        declared == certified
      case _ => false
    Option.when(occurrenceBound && terminalBound)(
      new AdmittedCandidateLine(evaluation, continuation)
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

  private[line] lazy val currentAutomaticTerminal: Option[AutomaticTerminal] =
    PositionRuleAssessment.automaticTerminal(currentPosition)

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

  /** Admits a complete legal suffix once and retains only the portion through
    * its first automatic terminal. Moves beyond that terminal are inspected
    * for report validity but never enter the canonical commentary occurrence.
    */
  def admitToFirstAutomaticTerminal(
      relativeMovesUci: List[String]
  ): Either[CanonicalPositionHistoryFailure, CanonicalHistoryContinuation] =
    val normalizedMoves = relativeMovesUci.map(PrincipalVariationEvidence.normalizeUci)
    if normalizedMoves.isEmpty then
      Left(CanonicalPositionHistoryFailure.IllegalMovePrefix)
    else if currentPly.toLong + normalizedMoves.size.toLong > Int.MaxValue then
      Left(CanonicalPositionHistoryFailure.MovePrefixPlyOutOfRange)
    else if currentAutomaticTerminal.nonEmpty then
      Left(CanonicalPositionHistoryFailure.IllegalMovePrefix)
    else
      PrincipalVariationEvidence
        .legalMoveReplayFromPosition(currentPosition, normalizedMoves, currentPly)
        .toRight(CanonicalPositionHistoryFailure.IllegalMovePrefix)
        .flatMap { replay =>
          for
            _ <- CanonicalPositionHistory.acceptHalfMoveClocks(replay)
            reportFullMoveNumber <- CanonicalPositionHistory.fullMoveNumberAfter(currentFullMoveNumber, replay)
            terminalBoundary = replay.iterator.zipWithIndex.collectFirst(Function.unlift {
              case (step, index) =>
                PositionRuleAssessment.automaticTerminal(step.after).map(index -> _)
            })
            admittedReplay = terminalBoundary match
              case Some((index, _)) => replay.take(index + 1)
              case None             => replay
            admittedFullMoveNumber <-
              if admittedReplay.size == replay.size then Right(reportFullMoveNumber)
              else CanonicalPositionHistory.fullMoveNumberAfter(currentFullMoveNumber, admittedReplay)
            complete = materializeValidatedSuffix(admittedReplay, admittedFullMoveNumber)
          yield new CanonicalHistoryContinuation(
            startingFen = currentFen,
            resultingHistory = complete,
            moves = admittedReplay.map(step => PrincipalVariationEvidence.normalizeUci(step.uci)),
            automaticTerminal = terminalBoundary.map(_._2)
          )
        }

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
        .map(materializeValidatedSuffix(additionalSteps, _))

  private def materializeValidatedSuffix(
      additionalSteps: List[LegalReplayStep],
      nextFullMoveNumber: Int
  ): CanonicalPositionHistory =
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

  private[line] def admittedStepsAfter(
      prefix: CanonicalPositionHistory
  ): Option[List[CanonicalPositionHistoryStep]] =
    val prefixSize = prefix.segmentReplaySteps.size
    val serializedPrefixMatches =
      prefix.initialFen == initialFen &&
        movePrefixUci.startsWith(prefix.movePrefixUci) &&
        segmentReplaySteps.take(prefixSize).zip(prefix.segmentReplaySteps).forall {
          case (retained, expected) =>
            retained.ply == expected.ply &&
              PrincipalVariationEvidence.normalizeUci(retained.uci) ==
                PrincipalVariationEvidence.normalizeUci(expected.uci) &&
              retained.beforeFen == expected.beforeFen &&
              retained.afterFen == expected.afterFen
        }
    val boundaryMatches =
      segmentReplaySteps.lift(prefixSize) match
        case Some(firstContinuation) =>
          firstContinuation.ply == prefix.currentPly + 1 &&
            firstContinuation.beforeFen == prefix.currentFen
        case None => false
    Option.when(
      prefixSize == prefix.movePrefixUci.size &&
        prefixSize < segmentReplaySteps.size &&
        serializedPrefixMatches &&
        boundaryMatches
    )(
      segmentReplaySteps.drop(prefixSize)
    )

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
          ply = step.ply,
          uci = step.uci,
          before = step.before,
          beforeFen = render(step.before, fullMoveNumber),
          move = step.move,
          capturedRole = step.capturedRole,
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
