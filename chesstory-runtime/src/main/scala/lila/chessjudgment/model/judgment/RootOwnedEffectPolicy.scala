package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] final case class DirectCauseFamilyMetadata(
    kind: RelativeCauseKind,
    sourceSide: RelativeCauseSourceSide
)

private[chessjudgment] object RootOwnedEffectPolicy:
  /** A causal root is one board occurrence, not merely a board shape that may
    * recur later in a line. The semantic FEN comparison includes side to move;
    * ply disambiguates repetitions of that same semantic board.
    */
  private[chessjudgment] def sameCausalRootOccurrence(
      left: PositionNodeRef,
      right: PositionNodeRef
  ): Boolean =
    left.ply == right.ply &&
      PrincipalVariationEvidence.sameBoardState(left.fen, right.fen)

  /** The sole mapping from one exact typed proof family to its Cause meaning.
    * Draft production and final channel certification consume this same value.
    */
  private[chessjudgment] def familyMetadata(
      proof: RootOwnedEffectProof
  ): DirectCauseFamilyMetadata =
    proof match
      case _: RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture |
          _: RootOwnedEffectProof.SoleRecapturerRemovalBeforeTargetCapture =>
        DirectCauseFamilyMetadata(RelativeCauseKind.WrongMoveOrder, RelativeCauseSourceSide.Reference)
      case _: RootOwnedEffectProof.VacatedGateEnablesUnrecapturableSliderCapture =>
        DirectCauseFamilyMetadata(RelativeCauseKind.MissedTacticalResource, RelativeCauseSourceSide.Reference)
      case _: RootOwnedEffectProof.SquareReleaseRoute =>
        DirectCauseFamilyMetadata(RelativeCauseKind.MissedSquareRelease, RelativeCauseSourceSide.Reference)
      case _: RootOwnedEffectProof.PassedPawnProgressRealizedAfterOnlyLegalReply =>
        DirectCauseFamilyMetadata(RelativeCauseKind.PassedPawnProgress, RelativeCauseSourceSide.Candidate)

  /** Sole Cause-kind to exact-family authority. A channel exists only after
    * the typed producer, comparison occurrence, event root, and source side
    * all agree; upper consumers never reclassify the proof family.
    */
  def certify(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      causeBinding: RelativeCauseBinding,
      effect: DirectCauseChannel
  ): List[RootOwnedEffect] =
    Option.when(admitsAt(cause, graph, effect, causeBinding))(effect).toList

  private def admitsAt(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      effect: RootOwnedEffect,
      causeBinding: RelativeCauseBinding
  ): Boolean =
    val proof = effect.rootOwnedProof
    val eventLine = causeBinding.eventLine
    exactFamilyOwnsCause(cause, graph, effect) &&
      sameCausalRootBoard(cause, proof) &&
      proof.eventLine == eventLine &&
      proofOwnsEventRoot(proof, eventLine, graph)

  private def exactFamilyOwnsCause(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      effect: DirectCauseChannel
  ): Boolean =
    val proof = effect.rootOwnedProof
    val family = effect.familyMetadata
    cause.kind == family.kind && cause.sourceSide == family.sourceSide && (proof match
      case RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture(_, result) =>
        result.hasCompleteProofPaths &&
          graph.comparisonFor(cause).exists(comparison =>
            result.occurrence.referenceLine == comparison.referenceLine &&
              result.occurrence.playedLine == comparison.candidateLine
          )
      case RootOwnedEffectProof.SoleRecapturerRemovalBeforeTargetCapture(_, result) =>
        result.hasCompleteProofPaths &&
          graph.comparisonFor(cause).exists(comparison =>
            result.occurrence.referenceLine == comparison.referenceLine &&
              result.occurrence.playedLine == comparison.candidateLine
          )
      case RootOwnedEffectProof.VacatedGateEnablesUnrecapturableSliderCapture(_, result) =>
        result.hasCompleteProofPaths &&
          graph.comparisonFor(cause).exists(comparison =>
            result.occurrence.referenceLine == comparison.referenceLine &&
              result.occurrence.playedLine == comparison.candidateLine
          )
      case RootOwnedEffectProof.SquareReleaseRoute(_, result) =>
        result.hasCompleteProofPaths &&
          graph.comparisonFor(cause).exists(comparison =>
            result.occurrence.referenceLine == comparison.referenceLine &&
              result.occurrence.playedLine == comparison.candidateLine
          )
      case RootOwnedEffectProof.PassedPawnProgressRealizedAfterOnlyLegalReply(_, result) =>
        result.consequenceKind == TransitionConsequenceKind.PassedPawnProgress &&
          result.hasCompleteProofPaths &&
          graph.comparisonFor(cause).exists(comparison =>
            ActionablePlayedVsBestCausalProofDemand.accepts(comparison) &&
              comparison.candidateLine == result.rootLine &&
              comparison.comparison.mover == result.resultActor.side
          )
    )

  private def sameCausalRootBoard(
      cause: RelativeCauseFact,
      proof: RootOwnedEffectProof
  ): Boolean =
    val rootPosition = cause.comparisonEvidence.position
    (proof.primitiveSource :: proof.provenance).forall(ref =>
      sameCausalRootOccurrence(ref.position, rootPosition)
    )

  private def proofOwnsEventRoot(
      proof: RootOwnedEffectProof,
      eventLine: LineNodeRef,
      graph: TypedEvidenceGraph
  ): Boolean =
    proof match
      case RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture(source, result) =>
        source.line.contains(eventLine) &&
          result.occurrence.referenceLine == eventLine &&
          EvidenceRef.sameMove(result.occurrence.triggerStep.moveUci, eventLine.rootMove) &&
          graph.record(source).exists(record => record.payload == result && graph.proofEligible(record))
      case RootOwnedEffectProof.SoleRecapturerRemovalBeforeTargetCapture(source, result) =>
        source.line.contains(eventLine) &&
          result.occurrence.referenceLine == eventLine &&
          EvidenceRef.sameMove(result.occurrence.removalStep.moveUci, eventLine.rootMove) &&
          graph.record(source).exists(record => record.payload == result && graph.proofEligible(record))
      case RootOwnedEffectProof.VacatedGateEnablesUnrecapturableSliderCapture(source, result) =>
        source.line.contains(eventLine) &&
          result.occurrence.referenceLine == eventLine &&
          EvidenceRef.sameMove(result.occurrence.enablingStep.moveUci, eventLine.rootMove) &&
          graph.record(source).exists(record => record.payload == result && graph.proofEligible(record))
      case RootOwnedEffectProof.SquareReleaseRoute(source, result) =>
        source.line.contains(eventLine) &&
          result.occurrence.referenceLine == eventLine &&
          EvidenceRef.sameMove(result.occurrence.releaseStep.moveUci, eventLine.rootMove) &&
          graph.record(source).exists(record => record.payload == result && graph.proofEligible(record))
      case RootOwnedEffectProof.PassedPawnProgressRealizedAfterOnlyLegalReply(source, result) =>
        source.line.contains(eventLine) &&
          result.rootLine == eventLine &&
          EvidenceRef.sameMove(result.rootMove, eventLine.rootMove) &&
          sameCausalRootOccurrence(source.position, result.event.rootTransition.from) &&
          graph.record(source).exists(record => record.payload == result && graph.proofEligible(record))
