package lila.chessjudgment.analysis.line

import chess.*
import chess.format.Fen
import lila.chessjudgment.analysis.tactical.TacticalPatternDetectors
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.strategic.EngineLine

/**
 * Truth Boundary (High-Precision Validation Layer)
 *
 * Deterministic detection of mating geometry, tactical structure, and
 * legally replayed immediate checks.
 */
object ForcedLineTruth:

  val ImmediateReplyCheckId = "immediate_reply_check"

  case class VerifiedTheme(
    id: String,
    lineMoves: List[String] = Nil
  )

  def detect(
      fen: String,
      playedUci: String,
      variations: List[EngineLine] = Nil
  ): Option[VerifiedTheme] =
    PrincipalVariationEvidence.legalFenAfter(fen, playedUci).flatMap { afterFen =>
      val posOpt = Fen.read(chess.variant.Standard, Fen.Full(afterFen))
      val beforePosOpt = Fen.read(chess.variant.Standard, Fen.Full(fen))
      posOpt
        .flatMap(pos => detectPatterns(beforePosOpt, pos, playedUci, variations))
        .orElse(detectImmediateReplyCheck(fen, playedUci, variations))
    }

  private def detectPatterns(
      beforePos: Option[Position],
      pos: Position,
      playedUci: String,
      variations: List[EngineLine]
  ): Option[VerifiedTheme] =
    val continuationLines = variations.map(_.moves.map(normalizeUci))
    TacticalPatternDetectors.ordered
      .find(detector =>
        (!detector.requiresMate || pos.checkMate) &&
          detector.matchesWithContinuations(beforePos, pos, playedUci, continuationLines)
      )
      .map(detector => VerifiedTheme(detector.id))

  private def detectImmediateReplyCheck(
      fen: String,
      playedUci: String,
      variations: List[EngineLine]
  ): Option[VerifiedTheme] =
    val played = normalizeUci(playedUci)
    variations.view
      .flatMap { variation =>
        val moves = variation.moves.map(normalizeUci).filter(_.nonEmpty)
        Option.when(moves.headOption.contains(played) && moves.size >= 2)(moves.take(2))
      }
      .find(replyChecksMover(fen, _))
      .map(line => VerifiedTheme(ImmediateReplyCheckId, line))

  private def replyChecksMover(fen: String, line: List[String]): Boolean =
    PrincipalVariationEvidence
      .legalMoveReplay(fen, line, startPly = 0)
      .flatMap(_.lastOption)
      .exists(_.after.check.yes)

  private def normalizeUci(raw: String): String =
    PrincipalVariationEvidence.normalizeUci(raw)
