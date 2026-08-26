package lila.chessjudgment.analysis.line

import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.line.LegalReplayStep
import lila.chessjudgment.model.strategic.EngineLine
import lila.chessjudgment.model.judgment.CanonicalLineReplay

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

  private[chessjudgment] def detect(
      replay: CanonicalLineReplay,
      variations: List[EngineLine]
  ): Option[VerifiedTheme] =
    replay.legalSteps.headOption.flatMap(first => detect(replay.legalSteps, first.uci, variations))

  private def detect(
      replay: List[LegalReplayStep],
      playedUci: String,
      variations: List[EngineLine]
  ): Option[VerifiedTheme] =
    replay.headOption
      .filter(_.uci == PrincipalVariationEvidence.normalizeUci(playedUci))
      .flatMap(first =>
        detectImmediateReplyCheck(first.uci, replay, variations)
      )

  private def detectImmediateReplyCheck(
      playedUci: String,
      replay: List[LegalReplayStep],
      variations: List[EngineLine]
  ): Option[VerifiedTheme] =
    val played = PrincipalVariationEvidence.normalizeUci(playedUci)
    val declared = variations.view
      .flatMap { variation =>
        val moves = variation.moves.map(PrincipalVariationEvidence.normalizeUci).filter(_.nonEmpty)
        Option.when(moves.headOption.contains(played) && moves.size >= 2)(moves.take(2))
      }
      .find(line => replay.take(2).map(_.uci) == line)
    declared
      .filter(_ => replay.lift(1).exists(_.after.check.yes))
      .map(line => VerifiedTheme(ImmediateReplyCheckId, line))
