package lila.chessjudgment.model.judgment

enum RelativeCauseDominanceStatus:
  case Retained
  case DominatedFallback

final case class RelativeCauseDominanceDecision(
    causeEvidenceId: String,
    status: RelativeCauseDominanceStatus,
    dominatingCauseEvidenceIds: List[String]
):
  def retained: Boolean =
    status == RelativeCauseDominanceStatus.Retained

/** The sole authority for deciding whether a truthful, player-facing generic
  * cause is redundant beside a more specific cause.
  *
  * Readiness is intentionally resolved before this policy is called. A cause
  * that cannot independently reach the player-facing boundary therefore
  * cannot suppress another cause. Likewise, this policy never deletes a C
  * record: it only records the R-stage exposure decision.
  */
object RelativeCauseDominancePolicy:

  private final case class LineChannel(
      role: LineNodeRole,
      rank: Int,
      rootMove: String
  )

  private final case class ComparisonChannel(
      kind: CandidateComparisonKind,
      referenceLine: LineChannel,
      candidateLine: LineChannel,
      eventLine: LineChannel,
      sourceSide: RelativeCauseSourceSide,
      attributionKind: CauseAttributionKind
  )

  private final case class EvidenceSourceChannel(
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      positionFen: String,
      positionPly: Int,
      line: Option[LineChannel],
      scope: EvidenceScope,
      payloadKind: String
  )

  private final case class DirectProofChannel(
      source: EvidenceSourceChannel,
      line: Option[LineChannel],
      actors: Set[String],
      targets: Set[String],
      witnesses: Set[String],
      horizon: Option[String]
  )

  private final case class Candidate(
      causeEvidenceId: String,
      kind: RelativeCauseKind,
      comparison: ComparisonChannel,
      directProofChannels: List[DirectProofChannel]
  )

  def resolve(
      causes: List[(RelativeCauseFact, EvidenceRef)],
      graph: TypedEvidenceGraph
  ): List[RelativeCauseDominanceDecision] =
    val candidates = causes
      .distinctBy(_._2.id)
      .flatMap { case (cause, ref) =>
        graph.record(ref).collect {
          case EvidenceRecord(_, RelativeCauseFactEvidence(registered), _) if registered == cause =>
            candidate(cause, ref, graph)
        }
      }
    candidates.map { fallback =>
      val specificKinds = specificKindsForFallback(fallback.kind)
      val dominators =
        if specificKinds.isEmpty then Nil
        else
          candidates
            .filter(candidate =>
              candidate.causeEvidenceId != fallback.causeEvidenceId &&
                specificKinds(candidate.kind) &&
                sameCausalChannel(fallback, candidate)
            )
            .map(_.causeEvidenceId)
            .distinct
            .sorted
      RelativeCauseDominanceDecision(
        causeEvidenceId = fallback.causeEvidenceId,
        status =
          if dominators.nonEmpty then RelativeCauseDominanceStatus.DominatedFallback
          else RelativeCauseDominanceStatus.Retained,
        dominatingCauseEvidenceIds = dominators
      )
    }

  private def candidate(
      cause: RelativeCauseFact,
      ref: EvidenceRef,
      graph: TypedEvidenceGraph
  ): Candidate =
    val comparison = graph.comparisonFor(cause).getOrElse(
      throw IllegalArgumentException(
        s"relative cause '${ref.id}' is not bound to a candidate comparison"
      )
    )
    val binding = graph.requiredRelativeCauseBinding(cause)
    Candidate(
      causeEvidenceId = ref.id,
      kind = cause.kind,
      comparison = ComparisonChannel(
        kind = comparison.kind,
        referenceLine = lineChannel(comparison.referenceLine),
        candidateLine = lineChannel(comparison.candidateLine),
        eventLine = lineChannel(binding.eventLine),
        sourceSide = cause.sourceSide,
        attributionKind = cause.attribution.kind
      ),
      directProofChannels = directProofChannels(cause, graph)
    )

  private def directProofChannels(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectProofChannel] =
    val directProofIds = cause.proof.toList.flatMap(_.directProof.sourceRefs).map(_.id).toSet
    EvidenceObjectBinding
      .fromRelativeCauseForProjection(cause, graph)
      .filter(binding =>
        directProofIds(binding.source.id) &&
          binding.proofRole.forall(_ == RelativeCauseProofRole.DirectProof) &&
          binding.specificTargetMechanismReady
      )
      .flatMap(binding =>
        graph.record(binding.source).map(record =>
          DirectProofChannel(
            source = EvidenceSourceChannel(
              producer = record.ref.producer,
              layer = record.ref.layer,
              positionFen = record.ref.position.fen,
              positionPly = record.ref.position.ply,
              line = record.ref.line.map(lineChannel),
              scope = record.ref.scope,
              payloadKind = record.payload.getClass.getName
            ),
            line = binding.line.map(lineChannel),
            actors = binding.actor.map(_.signaturePart).toSet,
            targets = binding.target
              .filter(EvidenceObjectBinding.specificSurfaceTargetObject)
              .map(_.signaturePart)
              .toSet,
            witnesses = binding.witness.map(_.signaturePart).toSet,
            horizon = binding.horizon.map(_.trim.toLowerCase)
          )
        )
      )
      .filter(_.targets.nonEmpty)
      .distinct

  private def sameCausalChannel(left: Candidate, right: Candidate): Boolean =
    left.comparison == right.comparison &&
      left.directProofChannels.exists(leftProof =>
        right.directProofChannels.exists(rightProof => directProofChannelCompatible(leftProof, rightProof))
      )

  private def directProofChannelCompatible(
      left: DirectProofChannel,
      right: DirectProofChannel
  ): Boolean =
    val actorCompatible =
      left.actors.isEmpty || right.actors.isEmpty || left.actors.intersect(right.actors).nonEmpty
    val witnessCompatible =
      left.witnesses.isEmpty || right.witnesses.isEmpty || left.witnesses.intersect(right.witnesses).nonEmpty
    val horizonCompatible =
      left.horizon.isEmpty || right.horizon.isEmpty || left.horizon == right.horizon
    left.source == right.source &&
      left.line == right.line &&
      left.targets.intersect(right.targets).nonEmpty &&
      actorCompatible &&
      witnessCompatible &&
      horizonCompatible

  private def lineChannel(line: LineNodeRef): LineChannel =
    LineChannel(
      role = line.role,
      rank = line.rank,
      rootMove = EvidenceRef.normalizeMove(line.rootMove)
    )

  private def specificKindsForFallback(kind: RelativeCauseKind): Set[RelativeCauseKind] =
    kind match
      case RelativeCauseKind.MissedTacticalResource =>
        Set(
          RelativeCauseKind.WrongRecapturer,
          RelativeCauseKind.RecaptureRecoveryWindow,
          RelativeCauseKind.WrongMoveOrder,
          RelativeCauseKind.TempoLoss,
          RelativeCauseKind.KingForcing,
          RelativeCauseKind.ConversionMiss,
          RelativeCauseKind.ConversionSecured
        )
      case RelativeCauseKind.MaterialSwing =>
        Set(
          RelativeCauseKind.TacticalRefutationOfPlayed,
          RelativeCauseKind.CandidateTacticalLiability,
          RelativeCauseKind.WrongRecapturer,
          RelativeCauseKind.RecaptureRecoveryWindow,
          RelativeCauseKind.WrongMoveOrder,
          RelativeCauseKind.TempoLoss,
          RelativeCauseKind.ConversionMiss,
          RelativeCauseKind.KingForcing
        )
      case _ =>
        Set.empty
