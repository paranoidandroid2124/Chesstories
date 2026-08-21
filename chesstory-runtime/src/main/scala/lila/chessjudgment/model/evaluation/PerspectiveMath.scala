package lila.chessjudgment.model.evaluation

import chess.Color

/**
 * White POV score helpers converted to mover-relative deltas.
 *
 * Contract:
 * - Input scores are always White POV centipawns.
 * - Returned values are mover-relative positive deltas when the mover improves.
 */
object PerspectiveMath:

  final case class EvalPoint(
      whitePovCp: Int,
      mate: Option[Int]
  )

  final case class EvalDeltaForMover(
      rawCpLossForMover: Int,
      candidateWinPercentDeltaForMover: Double,
      winPercentLossForMover: Double,
      mateDistanceLossForMover: Option[Int]
  )

  private val WinPercentSlope = 0.00368208

  def winPercentFromWhiteCp(whiteCp: Int): Double =
    50.0 + 50.0 * (2.0 / (1.0 + math.exp(-WinPercentSlope * whiteCp.toDouble)) - 1.0)

  def winPercentFromWhiteEval(whiteCp: Int, mate: Option[Int]): Double =
    mate match
      case Some(m) if m > 0 => 100.0
      case Some(m) if m < 0 => 0.0
      case _                => winPercentFromWhiteCp(whiteCp)

  def winPercentForMover(mover: Color, whiteCp: Int): Double =
    val whiteWinPercent = winPercentFromWhiteCp(whiteCp)
    if mover.white then whiteWinPercent else 100.0 - whiteWinPercent

  def winPercentForMover(mover: Color, whiteCp: Int, mate: Option[Int]): Double =
    val whiteWinPercent = winPercentFromWhiteEval(whiteCp, mate)
    if mover.white then whiteWinPercent else 100.0 - whiteWinPercent

  def winPercentAdvantageFor(mover: Color, whiteCp: Int, mate: Option[Int] = None): Double =
    (winPercentForMover(mover, whiteCp, mate) - 50.0).max(0.0)

  def winPercentLossForMover(
      mover: Color,
      bestWhiteCp: Int,
      bestMate: Option[Int],
      playedWhiteCp: Int,
      playedMate: Option[Int]
  ): Double =
    (winPercentForMover(mover, bestWhiteCp, bestMate) - winPercentForMover(mover, playedWhiteCp, playedMate)).max(0.0)

  def compareForMover(
      mover: Color,
      reference: EvalPoint,
      candidate: EvalPoint
  ): EvalDeltaForMover =
    val winPercentDelta =
      winPercentForMover(mover, candidate.whitePovCp, candidate.mate) -
        winPercentForMover(mover, reference.whitePovCp, reference.mate)
    val rawLoss =
      if mover.white then (reference.whitePovCp - candidate.whitePovCp).max(0)
      else (candidate.whitePovCp - reference.whitePovCp).max(0)
    val winPercentLoss =
      winPercentLossForMover(
        mover = mover,
        bestWhiteCp = reference.whitePovCp,
        bestMate = reference.mate,
        playedWhiteCp = candidate.whitePovCp,
        playedMate = candidate.mate
      )
    val referenceMateForMover = mateForMover(mover, reference.mate)
    val candidateMateForMover = mateForMover(mover, candidate.mate)
    val mateDistanceLossForMover =
      mateDistanceLoss(referenceMateForMover, candidateMateForMover)
    EvalDeltaForMover(
      rawCpLossForMover = rawLoss,
      candidateWinPercentDeltaForMover = winPercentDelta,
      winPercentLossForMover = winPercentLoss,
      mateDistanceLossForMover = mateDistanceLossForMover
    )

  def mateForMover(mover: Color, whitePovMate: Option[Int]): Option[Int] =
    if mover.white then whitePovMate else whitePovMate.map(-_)

  def mateDistanceLoss(
      referenceMateForMover: Option[Int],
      candidateMateForMover: Option[Int]
  ): Option[Int] =
    referenceMateForMover.zip(candidateMateForMover).flatMap { case (best, played) =>
      if best > 0 && played > 0 then Option.when(played.abs > best.abs)(played.abs - best.abs)
      else if best < 0 && played < 0 then Option.when(played.abs < best.abs)(best.abs - played.abs)
      else None
    }
