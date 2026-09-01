package lila.chessjudgment.model.line

import chess.{ Color, HalfMoveClock, Position }
import lila.chessjudgment.model.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.model.line.EngineLine

enum AutomaticTerminal:
  case Checkmate(winner: Color)
  case Stalemate
  case InsufficientMaterial
  case FivefoldRepetition
  case SeventyFiveMoveRule

enum DrawClaimRule:
  case ThreefoldRepetition
  case FiftyMoveRule

enum DrawClaimAvailability:
  case Unavailable
  case AvailableNow

enum FivefoldRepetitionKnowledge:
  case CertifiedNonterminal
  case HistoryUnavailable

enum ThreefoldClaimKnowledge:
  case Known(availability: DrawClaimAvailability)
  case HistoryUnavailable

enum PositionRuleAssessment:
  case Terminal(value: AutomaticTerminal)
  case Nonterminal(
      fivefoldRepetition: FivefoldRepetitionKnowledge,
      threefoldClaim: ThreefoldClaimKnowledge,
      fiftyMoveClaim: DrawClaimAvailability
  )

object PositionRuleAssessment:
  def automaticTerminal(position: Position): Option[AutomaticTerminal] =
    if position.checkMate then
      Some(AutomaticTerminal.Checkmate(!position.color))
    else if position.staleMate then
      Some(AutomaticTerminal.Stalemate)
    else if position.variant.isInsufficientMaterial(position) then
      Some(AutomaticTerminal.InsufficientMaterial)
    else if position.history.fivefoldRepetition then
      Some(AutomaticTerminal.FivefoldRepetition)
    else if position.history.halfMoveClock >= HalfMoveClock(150) then
      Some(AutomaticTerminal.SeventyFiveMoveRule)
    else None

  def assess(history: CanonicalPositionHistory): PositionRuleAssessment =
    history.currentAutomaticTerminal
      .map(PositionRuleAssessment.Terminal.apply)
      .getOrElse(assessCertifiedAutomaticNonterminal(history))

  /** Completes the rule assessment after the canonical replay owner has already
    * certified that no automatic terminal applies to the resulting position.
    */
  private[line] def assessCertifiedAutomaticNonterminal(
      history: CanonicalPositionHistory
  ): PositionRuleAssessment =
    val position = history.currentPosition
    PositionRuleAssessment.Nonterminal(
      fivefoldRepetition =
        if history.repetitionHistoryComplete || position.history.halfMoveClock < HalfMoveClock(16) then
          FivefoldRepetitionKnowledge.CertifiedNonterminal
        else FivefoldRepetitionKnowledge.HistoryUnavailable,
      threefoldClaim =
        if position.threefoldRepetition then
          ThreefoldClaimKnowledge.Known(DrawClaimAvailability.AvailableNow)
        else if history.repetitionHistoryComplete || position.history.halfMoveClock < HalfMoveClock(8) then
          ThreefoldClaimKnowledge.Known(DrawClaimAvailability.Unavailable)
        else ThreefoldClaimKnowledge.HistoryUnavailable,
      fiftyMoveClaim =
        if position.history.halfMoveClock >= HalfMoveClock(100) then
          DrawClaimAvailability.AvailableNow
        else DrawClaimAvailability.Unavailable
    )

  def declaredDrawClaims(
      moveUci: String,
      current: PositionRuleAssessment,
      successor: PositionRuleAssessment
  ): Set[DeclaredDrawClaim] =
    (current, successor) match
      case (
            PositionRuleAssessment.Nonterminal(_, currentThreefold, currentFiftyMove),
            PositionRuleAssessment.Nonterminal(_, successorThreefold, successorFiftyMove)
          ) =>
        Set(
          Option.when(
            currentThreefold == ThreefoldClaimKnowledge.Known(DrawClaimAvailability.Unavailable) &&
              successorThreefold == ThreefoldClaimKnowledge.Known(DrawClaimAvailability.AvailableNow)
          )(DeclaredDrawClaim(moveUci, DrawClaimRule.ThreefoldRepetition)),
          Option.when(
            currentFiftyMove == DrawClaimAvailability.Unavailable &&
              successorFiftyMove == DrawClaimAvailability.AvailableNow
          )(DeclaredDrawClaim(moveUci, DrawClaimRule.FiftyMoveRule))
        ).flatten
      case (PositionRuleAssessment.Terminal(_), PositionRuleAssessment.Terminal(_)) |
          (PositionRuleAssessment.Terminal(_), PositionRuleAssessment.Nonterminal(_, _, _)) |
          (PositionRuleAssessment.Nonterminal(_, _, _), PositionRuleAssessment.Terminal(_)) =>
        Set.empty

final case class DeclaredDrawClaim(moveUci: String, rule: DrawClaimRule)

enum DrawClaimAction:
  case AvailableNow(rule: DrawClaimRule)
  case AvailableByDeclaredMoves(rule: DrawClaimRule, moveUcis: Set[String])

enum CandidateLineEvaluation:
  case EngineSearch(line: EngineLine)
  case ExactAutomaticTerminal(lineMoves: List[String], terminal: AutomaticTerminal)

  def moves: List[String] = this match
    case EngineSearch(line)                  => line.moves
    case ExactAutomaticTerminal(moves, _)   => moves

  def engineLine: Option[EngineLine] = this match
    case EngineSearch(line)                => Some(line)
    case ExactAutomaticTerminal(_, _)      => None

  def winPercentForMover(mover: Color): Double = this match
    case EngineSearch(line) =>
      PerspectiveMath.winPercentForMover(mover, line.scoreCp, line.mate)
    case ExactAutomaticTerminal(_, AutomaticTerminal.Checkmate(winner)) =>
      if winner == mover then 100.0 else 0.0
    case ExactAutomaticTerminal(_, _) =>
      50.0

  def rankingScoreForMover(mover: Color): Double = this match
    case EngineSearch(line) =>
      val mateTieBreak =
        PerspectiveMath
          .mateForMover(mover, line.mate)
          .map(mate => math.signum(mate).toDouble / mate.abs.max(1).toDouble)
          .getOrElse(0.0)
      winPercentForMover(mover) + mateTieBreak
    case ExactAutomaticTerminal(_, AutomaticTerminal.Checkmate(winner)) =>
      winPercentForMover(mover) + (if winner == mover then 1.0 else -1.0)
    case ExactAutomaticTerminal(_, _) =>
      winPercentForMover(mover)

  def comparisonReady: Boolean = this match
    case EngineSearch(line) =>
      JudgmentThresholds.engineBackedByDepth(line.depth, line.mate)
    case ExactAutomaticTerminal(_, _) =>
      true
