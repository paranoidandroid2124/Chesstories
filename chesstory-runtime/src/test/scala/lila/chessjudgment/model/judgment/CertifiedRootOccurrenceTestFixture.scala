package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] object CertifiedRootOccurrenceTestFixture:

  def from(
      line: LineNodeRef,
      lineOwner: EvidenceRecord,
      replay: CanonicalLineReplay,
      rootProvenance: CausalRootProvenance,
      transitionRole: TransitionEdgeRole = TransitionEdgeRole.Alternative
  ): CertifiedRootOccurrence =
    val rootStep = replay.replaySteps.headOption.getOrElse(
      throw IllegalArgumentException("a test root occurrence needs one replay step")
    )
    val rootReplay = replay.subset(List(rootStep)).getOrElse(
      throw IllegalArgumentException("the root transition must belong to its canonical replay")
    )
    val from = lineOwner.ref.position
    require(
      rootStep.ply == from.ply + 1,
      s"test line owner ply ${from.ply} does not precede replay root ply ${rootStep.ply} for ${line.id}"
    )
    require(
      PrincipalVariationEvidence.normalizeFen(rootStep.fenBefore) ==
        PrincipalVariationEvidence.normalizeFen(from.fen),
      s"test line owner ${line.id} must retain the exact replay-root FEN"
    )
    val to = PositionNodeRef(
      rootStep.fenAfter,
      rootStep.ply,
      None,
      Some(s"${lineOwner.ref.id}:destination")
    )
    val transitionRef = EvidenceRef(
      id = s"${lineOwner.ref.id}:transition",
      producer = EvidenceProducer.MoveTransitionProducer,
      layer = EvidenceLayer.MoveTransition,
      position = from,
      line = None,
      scope = transitionRole.scope,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
    val transition = MoveTransitionEdge(
      transitionRole,
      from,
      rootStep.moveUci,
      to,
      transitionRef
    )
    val transitionOwner = TransitionFactNormalizer.fromMoveTransition(transition, rootReplay)
    CertifiedRootOccurrence(
      line,
      lineOwner,
      replay,
      LineRootOccurrence(from, to, transitionRef.id),
      transition,
      transitionOwner,
      rootProvenance
    )
