package lila.chessjudgment.analysis.line

import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.line.EngineLine
import lila.chessjudgment.model.judgment.{
  CanonicalLineReplay,
  RelationWitnessDetail,
  VerticalRelationContractKind
}

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
    replay.legalSteps.headOption.flatMap(first =>
      detectImmediateReplyCheck(first.uci, replay, variations)
    )

  private def detectImmediateReplyCheck(
      playedUci: String,
      replay: CanonicalLineReplay,
      variations: List[EngineLine]
  ): Option[VerifiedTheme] =
    val played = PrincipalVariationEvidence.normalizeUci(playedUci)
    val declared = variations.view
      .flatMap { variation =>
        val moves = variation.moves.map(PrincipalVariationEvidence.normalizeUci).filter(_.nonEmpty)
        Option.when(moves.headOption.contains(played) && moves.size >= 2)(moves.take(2))
      }
      .find(line => replay.legalSteps.take(2).map(_.uci) == line)
    declared
      .filter(_ =>
        replay.replaySteps.lift(1).exists(step =>
          replay.verticalRelationOccurrences(
            step,
            List(VerticalRelationContractKind.CreatedCheckResponseInventory)
          ).exists(_.relation.detail match
            case _: RelationWitnessDetail.CreatedCheckResponseInventory => true
            case _                                                       => false
          )
        )
      )
      .map(line => VerifiedTheme(ImmediateReplyCheckId, line))
