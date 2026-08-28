package lila.chessjudgment.analysis.relation

import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.judgment.*

private[chessjudgment] object ClosedRelationEvidence:

  private[chessjudgment] final class RelationProduction private[chessjudgment] (
      val closed: ClosedRelationTransitionInventory
  ):
    val relations: List[RelationFactEvidence] = closed.allRelations

    def bindClosedOutput(
        before: CanonicalPositionRelationSnapshot,
        after: CanonicalPositionRelationSnapshot,
        canonicalDelta: CanonicalRelationDelta
    ): CanonicalRelationTransitionInventory =
      closed.bind(before, after, canonicalDelta)

  def relationProduction(
      replay: CanonicalLineReplay,
      playedMove: String
  ): RelationProduction =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    val firstReplayStep = replay.replaySteps.headOption.getOrElse(
      throw IllegalArgumentException("a closed relation production needs one replay transition")
    )
    val replayTransition = replay.transition(firstReplayStep).getOrElse(
      throw IllegalArgumentException("the relation replay root has no canonical transition")
    )
    val first = replayTransition.legal
    if first.uci != normalizedPlayed then
      throw IllegalArgumentException(s"'$playedMove' is not the canonical replay root move")
    val delta = replayTransition.relationDelta
    if !EvidenceRef.sameMove(delta.rootMove.moveUci, first.uci) then
      throw IllegalArgumentException(s"'$playedMove' is absent from its closed legal-move inventory")
    new RelationProduction(replayTransition.closedRelationInventory)
