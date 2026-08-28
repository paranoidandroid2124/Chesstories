package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.PrincipalVariationEvidence

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

final case class RelativeCauseDominanceResolution(
    decisions: List[RelativeCauseDominanceDecision]
):
  /** Exact fallback obligations already proved by the central dominance
    * policy, indexed by each actual maximal direct dominator.  Consumers may
    * preserve these obligations, but may not infer or union new dominance
    * from channel similarity.
    */
  def fallbackObligationsByDominatorId: Map[String, Set[String]] =
    decisions
      .filter(_.status == RelativeCauseDominanceStatus.DominatedFallback)
      .flatMap(decision =>
        decision.dominatingCauseEvidenceIds.map(dominatorId => dominatorId -> decision.causeEvidenceId)
      )
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap

  /** Every removed fallback must still have at least one of its centrally
    * proved direct dominators in the final selected public Cause set.
    */
  def selectedDominatorClosure(finalSelectedCauseIds: Set[String]): Boolean =
    decisions.forall { decision =>
      decision.status != RelativeCauseDominanceStatus.DominatedFallback ||
        decision.dominatingCauseEvidenceIds.exists(finalSelectedCauseIds)
    }

/** The sole authority for deciding whether a truthful, player-facing generic
  * cause is redundant beside a more specific cause.
  *
  * Readiness and per-Cause exposure eligibility are resolved before any
  * cross-comparison representative is elected. Only an independently
  * selectable Cause may
  * suppress a fallback, and one such Cause must own every compatible direct
  * channel of that fallback without lowering Primary exposure to
  * Complementary. Sibling coverage is never unioned. This policy never
  * deletes a C record: it only records the R-stage exposure decision.
  */
object RelativeCauseDominancePolicy:

  /** Immediate edges in the generic -> strict semantic refinement partial
    * order. These are not "often related" or "usually more informative"
    * edges. With an identical owned causal channel, the right-hand kind must
    * state a narrower cause than the left-hand fallback. Transitive closure is
    * computed centrally; the graph must remain acyclic.
    */
  private val ImmediateMoreSpecific: Map[RelativeCauseKind, Set[RelativeCauseKind]] = Map(
    RelativeCauseKind.MissedTacticalResource -> Set(
      RelativeCauseKind.WrongMoveOrder,
      RelativeCauseKind.RecaptureRecoveryWindow,
      RelativeCauseKind.KingForcing,
      RelativeCauseKind.MaterialSwing
    ),
    RelativeCauseKind.MaterialSwing -> Set(RelativeCauseKind.SacrificeCompensation),
    RelativeCauseKind.CandidateTacticalLiability -> Set(
      RelativeCauseKind.MaterialSwing
    ),
    RelativeCauseKind.TacticalRefutationOfPlayed -> Set(
      RelativeCauseKind.KingForcing
    )
  )

  require(specificityAcyclic, "relative Cause fallback specificity must be acyclic")

  private final case class DominanceFrame(
      rootFen: String,
      sourceSide: RelativeCauseSourceSide,
      attribution: CauseAttributionKind,
      eventLine: SemanticLineKey,
      effectMode: PlayerFacingCauseEffectMode
  )

  private final case class Candidate(
      causeEvidenceId: String,
      kind: RelativeCauseKind,
      frame: DominanceFrame,
      exposureStatus: CrossComparisonExposureStatus,
      directChannels: List[DirectCauseChannel]
  )

  def resolve(
      causes: List[(RelativeCauseFact, EvidenceRef)],
      eligibilityDecisions: List[CrossComparisonExposureDecision],
      directChannelsByCauseId: Map[String, List[DirectCauseChannel]],
      graph: TypedEvidenceGraph
  ): RelativeCauseDominanceResolution =
    val distinctCauses = causes.distinctBy(_._2.id)
    val eligibilityById = eligibilityDecisions.map(decision => decision.causeEvidenceId -> decision).toMap
    val candidates = distinctCauses.flatMap { case (cause, ref) =>
      eligibilityById.get(ref.id).filter(_.selected).flatMap { decision =>
        for
          effectMode <- decision.effectMode
          binding <- graph.relativeCauseBinding(cause)
          candidate <- {
          val compatibleChannels = EvidenceObjectBinding.canonicalCauseChannels(
            PlayerFacingCauseSelectionPolicy
              .compatibleChannels(effectMode, directChannelsByCauseId.getOrElse(ref.id, Nil))
              .map(_._1)
          )
          Option.when(compatibleChannels.nonEmpty)(
            Candidate(
              ref.id,
              cause.kind,
              DominanceFrame(
                rootFen = cause.comparisonEvidence.position.fen,
                sourceSide = cause.sourceSide,
                attribution = cause.attribution.kind,
                eventLine = SemanticLineKey.from(binding.eventLine),
                effectMode = effectMode
              ),
              decision.status,
              compatibleChannels
            )
          )
          }
        yield candidate
      }
    }
    val dominatorsByCauseId = candidates.flatMap { fallback =>
      val eligible = candidates.filter(candidate => directlyDominates(candidate, fallback))
      val maximal = eligible.filterNot(candidate =>
        eligible.exists(other => directlyDominates(other, candidate))
      )
      Option.when(maximal.nonEmpty)(
        fallback.causeEvidenceId -> maximal
          .sortBy(candidate => dominanceOrder(candidate, eligibilityById))
          .map(_.causeEvidenceId)
      )
    }.toMap
    val decisions = distinctCauses.map { case (_, ref) =>
      val dominators = dominatorsByCauseId.getOrElse(ref.id, Nil)
      RelativeCauseDominanceDecision(
        causeEvidenceId = ref.id,
        status =
          if dominators.nonEmpty then RelativeCauseDominanceStatus.DominatedFallback
          else RelativeCauseDominanceStatus.Retained,
        dominatingCauseEvidenceIds = dominators
      )
    }
    RelativeCauseDominanceResolution(
      decisions = decisions
    )

  private def directlyDominates(
      candidate: Candidate,
      fallback: Candidate
  ): Boolean =
    candidate.causeEvidenceId != fallback.causeEvidenceId &&
      sameDominanceFrame(candidate.frame, fallback.frame) &&
      exposureAtLeast(candidate.exposureStatus, fallback.exposureStatus) &&
      specificKindsForFallback(fallback.kind)(candidate.kind) &&
      ownsEveryFallbackEffect(candidate, fallback)

  private def sameDominanceFrame(
      candidate: DominanceFrame,
      fallback: DominanceFrame
  ): Boolean =
    PrincipalVariationEvidence.sameBoardState(candidate.rootFen, fallback.rootFen) &&
      candidate.sourceSide == fallback.sourceSide &&
      candidate.attribution == fallback.attribution &&
      candidate.eventLine == fallback.eventLine &&
      candidate.effectMode == fallback.effectMode

  /** Fallback suppression requires one candidate to own each exact descriptor
    * by itself. A matching public surface, sibling channel, plan label, or
    * evidence id is never sufficient.
    */
  private def ownsEveryFallbackEffect(
      candidate: Candidate,
      fallback: Candidate
  ): Boolean =
    fallback.directChannels.forall(channel => matchingOwnedChannels(candidate, channel).nonEmpty)

  private def matchingOwnedChannels(
      candidate: Candidate,
      fallbackChannel: DirectCauseChannel
  ): List[DirectCauseChannel] =
    if fallbackChannel.importanceDescriptorAmbiguous then Nil
    else
      candidate.directChannels.filter(channel =>
        candidateOwnsFallbackEffect(channel, fallbackChannel)
      )

  /** A strict Cause may add typed mechanism or consequence detail, but it may
    * not change the fallback's actor, target, event line, horizon, causal
    * change, or owned effect.
    */
  private def candidateOwnsFallbackEffect(
      candidate: DirectCauseChannel,
      fallback: DirectCauseChannel
  ): Boolean =
    !candidate.importanceDescriptorAmbiguous &&
      !fallback.importanceDescriptorAmbiguous &&
      candidate.rootOwnedEffectDescriptor.nonEmpty &&
      candidate.rootOwnedEffectDescriptor == fallback.rootOwnedEffectDescriptor &&
      PrincipalVariationEvidence.sameBoardState(
        candidate.binding.source.position.fen,
        fallback.binding.source.position.fen
      ) &&
      exactObjects(candidate.binding.actor, fallback.binding.actor) &&
      exactObjects(candidate.binding.target, fallback.binding.target) &&
      semanticLine(candidate.binding.line) == semanticLine(fallback.binding.line) &&
      normalized(candidate.binding.horizon) == normalized(fallback.binding.horizon) &&
      candidate.directChange == fallback.directChange &&
      containsObjects(candidate.binding.mechanism, fallback.binding.mechanism) &&
      containsObjects(candidate.binding.consequence, fallback.binding.consequence)

  private def exactObjects(
      left: List[ConcreteChessObject],
      right: List[ConcreteChessObject]
  ): Boolean =
    objectSignatures(left) == objectSignatures(right)

  private def containsObjects(
      candidate: List[ConcreteChessObject],
      fallback: List[ConcreteChessObject]
  ): Boolean =
    objectSignatures(fallback).subsetOf(objectSignatures(candidate))

  private def objectSignatures(objects: List[ConcreteChessObject]): Set[String] =
    objects.map(_.signaturePart).toSet

  private def semanticLine(line: Option[LineNodeRef]): Option[(LineNodeRole, String, Int)] =
    line.map(ref => (ref.role, EvidenceRef.normalizeMove(ref.rootMove), ref.rank))

  private def normalized(value: Option[String]): Option[String] =
    value.map(_.trim.toLowerCase)

  private def exposureAtLeast(
      candidate: CrossComparisonExposureStatus,
      fallback: CrossComparisonExposureStatus
  ): Boolean =
    (candidate, fallback) match
      case (CrossComparisonExposureStatus.SelectedPrimary, _) => true
      case (
            CrossComparisonExposureStatus.SelectedComplementary,
            CrossComparisonExposureStatus.SelectedComplementary
          ) => true
      case _ => false

  private def dominanceOrder(
      candidate: Candidate,
      crossById: Map[String, CrossComparisonExposureDecision]
  ): (Int, String) =
    (
      crossById
        .get(candidate.causeEvidenceId)
        .flatMap(_.comparisonExposureRank)
        .getOrElse(Int.MaxValue),
      candidate.causeEvidenceId
    )

  private def specificKindsForFallback(kind: RelativeCauseKind): Set[RelativeCauseKind] =
    def expand(frontier: Set[RelativeCauseKind], seen: Set[RelativeCauseKind]): Set[RelativeCauseKind] =
      val next = frontier.flatMap(item => ImmediateMoreSpecific.getOrElse(item, Set.empty)) -- seen
      if next.isEmpty then seen else expand(next, seen ++ next)
    expand(Set(kind), Set.empty) - kind

  private[judgment] def moreSpecificKinds(kind: RelativeCauseKind): Set[RelativeCauseKind] =
    specificKindsForFallback(kind)

  private[judgment] def specificityAcyclic: Boolean =
    def visit(
        node: RelativeCauseKind,
        active: Set[RelativeCauseKind],
        complete: Set[RelativeCauseKind]
    ): Option[Set[RelativeCauseKind]] =
      if active(node) then None
      else if complete(node) then Some(complete)
      else
        ImmediateMoreSpecific.getOrElse(node, Set.empty).foldLeft(Option(complete)) {
          case (None, _) => None
          case (Some(done), child) => visit(child, active + node, done)
        }.map(_ + node)
    RelativeCauseKind.values.foldLeft(Option(Set.empty[RelativeCauseKind])) {
      case (None, _) => None
      case (Some(done), node) => visit(node, Set.empty, done)
    }.nonEmpty
