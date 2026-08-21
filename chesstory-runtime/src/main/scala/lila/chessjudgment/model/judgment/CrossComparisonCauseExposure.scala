package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.evaluation.JudgmentThresholds

enum PlayerFacingCauseEffectMode:
  case PlayedLiability
  case AlternativeResource
  case PlayedValue

enum PlayerFacingCausalChange:
  case Occurred
  case Prevented
  case Maintained
  case Lost
  case Refuted
  case Missed

object PlayerFacingCausalChange:
  def fromDirect(change: DirectCausalChange): PlayerFacingCausalChange =
    change match
      case DirectCausalChange.Occurred   => PlayerFacingCausalChange.Occurred
      case DirectCausalChange.Prevented  => PlayerFacingCausalChange.Prevented
      case DirectCausalChange.Maintained => PlayerFacingCausalChange.Maintained
      case DirectCausalChange.Lost       => PlayerFacingCausalChange.Lost
      case DirectCausalChange.Refuted    => PlayerFacingCausalChange.Refuted
      case DirectCausalChange.Missed     => PlayerFacingCausalChange.Missed

enum PlayerFacingCauseExposureTier:
  case Primary
  case Complementary

final case class PlayerFacingCauseChannelSelection(
    carrierEvidence: EvidenceRef,
    causalSignature: String,
    directChange: DirectCausalChange,
    playedChange: PlayerFacingCausalChange
)

final case class PlayerFacingCauseSelection(
    causeEvidence: EvidenceRef,
    exposure: PlayerFacingCauseExposureTier,
    effectMode: PlayerFacingCauseEffectMode,
    channels: List[PlayerFacingCauseChannelSelection],
    onlyMoveQualifier: Option[OnlyMoveCauseQualifier] = None,
    comparisonExposureRank: Int = Int.MaxValue,
    selectionOrder: Int = Int.MaxValue
):
  require(channels.nonEmpty, "a player-facing Cause selection requires a direct causal channel")

object PlayerFacingCauseSelectionPolicy:

  def compatibleChannels(
      effectMode: PlayerFacingCauseEffectMode,
      channels: List[DirectCauseChannel]
  ): List[(DirectCauseChannel, PlayerFacingCausalChange)] =
    channels.flatMap(channel =>
      playedChange(effectMode, channel.directChange).map(channel -> _)
    )

  def build(
      causeEvidence: EvidenceRef,
      status: CrossComparisonExposureStatus,
      effectMode: PlayerFacingCauseEffectMode,
      channels: List[DirectCauseChannel],
      onlyMoveQualifier: Option[OnlyMoveCauseQualifier]
  ): Option[PlayerFacingCauseSelection] =
    val exposure = status match
      case CrossComparisonExposureStatus.SelectedPrimary =>
        Some(PlayerFacingCauseExposureTier.Primary)
      case CrossComparisonExposureStatus.SelectedComplementary =>
        Some(PlayerFacingCauseExposureTier.Complementary)
      case _ => None
    val selectedChannels = compatibleChannels(effectMode, channels).map { case (channel, played) =>
      PlayerFacingCauseChannelSelection(
        carrierEvidence = channel.binding.source,
        causalSignature = channel.causalSignature,
        directChange = channel.directChange,
        playedChange = played
      )
    }.groupBy(_.causalSignature).values
      .map(_.minBy(_.carrierEvidence.id))
      .toList
      .sortBy(_.causalSignature)
    for
      tier <- exposure
      if selectedChannels.nonEmpty
    yield PlayerFacingCauseSelection(
      causeEvidence = causeEvidence,
      exposure = tier,
      effectMode = effectMode,
      channels = selectedChannels,
      onlyMoveQualifier = onlyMoveQualifier
    )

  private[judgment] def playedChange(
      effectMode: PlayerFacingCauseEffectMode,
      directChange: DirectCausalChange
  ): Option[PlayerFacingCausalChange] =
    effectMode match
      case PlayerFacingCauseEffectMode.AlternativeResource =>
        Option.when(
          Set(
            DirectCausalChange.Occurred,
            DirectCausalChange.Prevented,
            DirectCausalChange.Maintained
          )(directChange)
        )(PlayerFacingCausalChange.Missed)
      case PlayerFacingCauseEffectMode.PlayedLiability =>
        Option.when(
          Set(
            DirectCausalChange.Occurred,
            DirectCausalChange.Lost,
            DirectCausalChange.Refuted,
            DirectCausalChange.Missed
          )(directChange)
        )(PlayerFacingCausalChange.fromDirect(directChange))
      case PlayerFacingCauseEffectMode.PlayedValue =>
        Option.when(
          Set(
            DirectCausalChange.Occurred,
            DirectCausalChange.Prevented,
            DirectCausalChange.Maintained
          )(directChange)
        )(PlayerFacingCausalChange.fromDirect(directChange))

enum CrossComparisonExposureStatus:
  case SelectedPrimary
  case SelectedComplementary
  case RedundantAcrossComparison
  case InferiorAlternative
  case DiagnosticComparison

  def selected: Boolean =
    this == CrossComparisonExposureStatus.SelectedPrimary ||
      this == CrossComparisonExposureStatus.SelectedComplementary

enum CrossComparisonExposureReason:
  case PlayedVsBestOwnsPlayedResponsibility
  case PlayedVsBestAlternativeResource
  case PlayedMatchesBestWithExactValue
  case PlayedRetainsExactValueWithPlayableLoss
  case PlayedVsBestDirectionInvalid
  case PlayedVsBestOutcomeIsDiagnostic
  case BetterAlternativeCreatesExactResource
  case BetterAlternativeExposesExactPlayedLiability
  case PlayedMoveCreatesValueAgainstAlternative
  case PlayedBestCreatesExactResource
  case HigherAuthorityEquivalentCause
  case AlternativeDoesNotImproveOnPlayed
  case CandidateConstraintIsDiagnostic
  case IndirectComparisonIsDiagnostic
  case ComparisonOrientationMismatch
  case UnsupportedAlternativeAttribution
  case DirectChangeIncompatible
  case ConflictingRootOwnedEffectTruth

final case class CrossComparisonExposureDecision(
    causeEvidenceId: String,
    status: CrossComparisonExposureStatus,
    reason: CrossComparisonExposureReason,
    representativeCauseEvidenceId: String,
    effectMode: Option[PlayerFacingCauseEffectMode],
    comparisonExposureRank: Option[Int] = None
):
  def selected: Boolean = status.selected

enum PlayerFacingIdeaUnitKind:
  case SingleCause
  case ExactPvbResponsibility

enum PlayerFacingIdeaPriorityStatus:
  case UniqueTop
  case Frontier
  case Dominated
  case Unmeasured

/** One sentence-ready idea may contain two exact comparison facets without
  * merging their Cause records.  In particular, a PlayedVsBest liability and
  * the exact best-move resource that prevents or preserves the same effect
  * remain separate Causes (and therefore retain their own polarity and
  * provenance) while belonging to one public idea unit.
  */
final case class PlayerFacingIdeaUnit(
    id: String,
    leadCauseEvidenceId: String,
    memberCauseEvidenceIds: List[String],
    kind: PlayerFacingIdeaUnitKind,
    importanceLayer: Int = 0,
    priorityStatus: PlayerFacingIdeaPriorityStatus = PlayerFacingIdeaPriorityStatus.Unmeasured,
    serializationOrder: Int = Int.MaxValue
):
  require(id.nonEmpty, "a player-facing idea unit requires an id")
  require(memberCauseEvidenceIds.nonEmpty, "a player-facing idea unit requires a Cause")
  require(
    memberCauseEvidenceIds.distinct.size == memberCauseEvidenceIds.size,
    "a player-facing idea unit cannot repeat a Cause"
  )
  require(
    memberCauseEvidenceIds.head == leadCauseEvidenceId,
    "the responsibility-owning Cause must lead its idea unit"
  )
  require(
    kind != PlayerFacingIdeaUnitKind.ExactPvbResponsibility || memberCauseEvidenceIds.size >= 2,
    "an exact PVB responsibility unit requires both endpoint facets"
  )

enum PlayerFacingIdeaFacetRole:
  case Lead
  case Supporting

/** Final R-owned public idea. The sentence unit and its ordered Cause facets
  * travel together so P never reconstructs unit membership or lead ownership
  * from independent id lists.
  */
final case class PlayerFacingNarrativeIdeaFacet(
    role: PlayerFacingIdeaFacetRole,
    ownerClaimId: String,
    selection: PlayerFacingCauseSelection,
    directChannels: List[DirectCauseChannel]
)

final case class PlayerFacingNarrativeIdea(
    unit: PlayerFacingIdeaUnit,
    facets: List[PlayerFacingNarrativeIdeaFacet]
)

final case class ExactPvbResponsibilityPair(
    liabilityCauseEvidenceId: String,
    resourceCauseEvidenceId: String
)

final case class PvbResponsibilityResolution(
    decisions: List[CrossComparisonExposureDecision],
    exactPairs: List[ExactPvbResponsibilityPair]
)

/** Comparison-agnostic readiness for a Cause that may be considered by R.
  *
  * This policy deliberately knows nothing about played moves, comparison
  * kinds, verdicts, importance, claim family, or salience. Those belong to the
  * provisional cross-comparison policy before the single fallback-dominance
  * pass.
  */
object PlayerFacingCauseReadinessPolicy:

  def collect(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph
  ): List[(RelativeCauseFact, EvidenceRef)] =
    claim.evidence
      .distinctBy(_.id)
      .flatMap(ref =>
        graph.record(ref).toList.collect {
          case EvidenceRecord(registeredRef, RelativeCauseFactEvidence(cause), _)
              if registeredRef == ref && ready(cause, registeredRef, graph) =>
            cause -> registeredRef
        }
      )
      .distinctBy(_._2.id)
      .sortBy { case (cause, ref) => (cause.kind.toString, ref.id) }

  private[chessjudgment] def ready(
      cause: RelativeCauseFact,
      ref: EvidenceRef,
      graph: TypedEvidenceGraph
  ): Boolean =
    val registeredCause = graph.record(ref).exists {
      case EvidenceRecord(registeredRef, RelativeCauseFactEvidence(registered), _) =>
        registeredRef == ref && registered == cause
      case _ => false
    }
    registeredCause && RelativeCauseConstructionAdmission.initiallyReady(cause, graph)

final case class PlayerFacingCauseExposureResolution(
    readyByClaim: Map[String, List[(RelativeCauseFact, EvidenceRef)]],
    dominanceDecisions: List[RelativeCauseDominanceDecision],
    crossDecisions: List[CrossComparisonExposureDecision],
    ownerClaimIdByCauseId: Map[String, String],
    directChannelsByCauseId: Map[String, List[DirectCauseChannel]],
    importanceResolution: DirectCauseImportanceResolution,
    narrativeIdeas: List[PlayerFacingNarrativeIdea],
    ideaImportanceResolution: DirectCauseImportanceResolution,
    onlyMoveConstraintResolutions: List[OnlyMoveConstraintResolution]
):
  def retainedCauseEvidenceIds: Set[String] =
    dominanceDecisions.filter(_.retained).map(_.causeEvidenceId).toSet

  def selectedCauseEvidenceIds: Set[String] =
    narrativeIdeas.flatMap(_.facets).map(_.selection.causeEvidence.id).toSet

  def selectionsForClaim(claimId: String): List[PlayerFacingCauseSelection] =
    narrativeIdeas
      .flatMap(_.facets)
      .collect { case facet if facet.ownerClaimId == claimId => facet.selection }

object PlayerFacingCauseExposurePipeline:

  def resolve(
      claims: List[JudgmentClaim],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): PlayerFacingCauseExposureResolution =
    val normalizedPlayedMoves = playedMoves.map(EvidenceRef.normalizeMove).filter(_.nonEmpty)
    val readyByClaim = claims.map(claim =>
      claim.id -> PlayerFacingCauseReadinessPolicy.collect(claim, graph)
    ).toMap
    val allReady = readyByClaim.values.flatten.toList.distinctBy(_._2.id)
    val directChannelsByCauseId = allReady.map { case (cause, ref) =>
      ref.id -> RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph)
    }.toMap
    val dominanceEligibilityDecisions =
      CrossComparisonCauseExposurePolicy.resolveEligibilityForDominance(
        allReady,
        graph,
        normalizedPlayedMoves
      )
    val dominance = RelativeCauseDominancePolicy.resolve(
      allReady,
      dominanceEligibilityDecisions,
      directChannelsByCauseId,
      graph
    )
    val dominanceDecisions = dominance.decisions
    val retainedCauseIds = dominanceDecisions.filter(_.retained).map(_.causeEvidenceId).toSet
    val retainedReady = allReady.filter { case (_, ref) => retainedCauseIds(ref.id) }
    val retainedRepresentatives = CrossComparisonCauseExposurePolicy.resolve(
      retainedReady,
      graph,
      normalizedPlayedMoves,
      dominance.fallbackObligationsByDominatorId
    )
    val retainedSelectedIds = retainedRepresentatives.filter(_.selected).map(_.causeEvidenceId).toSet
    require(
      dominance.selectedDominatorClosure(retainedSelectedIds),
      "every suppressed fallback requires a final selected direct dominator"
    )
    val responsibility = CrossComparisonCauseExposurePolicy
      .resolveRetainedPvbResponsibility(
        retainedRepresentatives,
        retainedReady,
        graph,
        normalizedPlayedMoves
      )
    val crossDecisions = responsibility.decisions
    val selectedIds = crossDecisions.filter(_.selected).map(_.causeEvidenceId).toSet
    val causesById = allReady.map { case (cause, ref) => ref.id -> cause }.toMap
    val ownerClaimIdByCauseId = selectedIds.toList.sorted.flatMap { causeId =>
      val owningClaims = claims.filter(claim =>
        readyByClaim.getOrElse(claim.id, Nil).exists(_._2.id == causeId)
      )
      causesById.get(causeId).flatMap(cause =>
        PlayerFacingCauseClaimOwnershipPolicy
          .ownerClaimId(cause, owningClaims)
          .map(ownerId => causeId -> ownerId)
      )
    }.toMap
    val selectedCauses = allReady.filter { case (_, ref) => selectedIds(ref.id) }
    val onlyMoveConstraintResolutions =
      OnlyMoveConstraintPolicy.resolveAll(graph, selectedCauses, normalizedPlayedMoves)
    val qualifierByCauseId = onlyMoveConstraintResolutions
      .flatMap(_.qualifiers)
      .groupBy(_.causeEvidence.id)
      .flatMap { case (causeId, qualifiers) =>
        qualifiers.distinctBy(item => (item.comparisonEvidence.id, item.causeEvidence.id)) match
          case qualifier :: Nil => Some(causeId -> qualifier)
          case _                => None
      }
    val comparisonOrderedSelections = crossDecisions.flatMap { decision =>
      for
        effectMode <- decision.effectMode
        comparisonExposureRank <- decision.comparisonExposureRank
        causeRef = selectedCauses.find(_._2.id == decision.causeEvidenceId).map(_._2)
        ref <- causeRef
        selection <- PlayerFacingCauseSelectionPolicy.build(
          causeEvidence = ref,
          status = decision.status,
          effectMode = effectMode,
          channels = directChannelsByCauseId.getOrElse(ref.id, Nil),
          onlyMoveQualifier = qualifierByCauseId.get(ref.id)
        )
      yield selection.copy(comparisonExposureRank = comparisonExposureRank)
    }.sortBy(selection => (selection.comparisonExposureRank, selection.causeEvidence.id))
    val importanceResolution = DirectCauseImportancePolicy.resolve(
      selectedCauses = selectedCauses,
      selections = comparisonOrderedSelections,
      directChannelsByCauseId = directChannelsByCauseId,
      graph = graph
    )
    val preliminaryIdeaUnits = PlayerFacingIdeaUnitPolicy.resolve(
      comparisonOrderedSelections,
      responsibility.exactPairs
    )
    val leadCauseIds = preliminaryIdeaUnits.map(_.leadCauseEvidenceId).toSet
    val ideaLeadSelections = comparisonOrderedSelections.filter(selection =>
      leadCauseIds(selection.causeEvidence.id)
    )
    val ideaImportanceResolution = DirectCauseImportancePolicy.resolve(
      selectedCauses = selectedCauses.filter { case (_, ref) => leadCauseIds(ref.id) },
      selections = ideaLeadSelections,
      directChannelsByCauseId = directChannelsByCauseId,
      graph = graph
    )
    val playerFacingOrdering = PlayerFacingIdeaOrderingPolicy.order(
      selections = comparisonOrderedSelections,
      ideaUnits = preliminaryIdeaUnits,
      causeImportanceResolution = importanceResolution,
      ideaImportanceResolution = ideaImportanceResolution
    )
    val orderedSelectionsByCauseId = playerFacingOrdering.selections.map(selection =>
      selection.causeEvidence.id -> selection
    ).toMap
    val narrativeIdeas = playerFacingOrdering.ideaUnits.map { unit =>
      val facets = unit.memberCauseEvidenceIds.zipWithIndex.map { case (causeId, index) =>
        val selection = orderedSelectionsByCauseId(causeId)
        val admittedChannelsBySignature = directChannelsByCauseId(causeId).map(channel =>
          channel.causalSignature -> channel
        ).toMap
        val directChannels = selection.channels.map { selected =>
          admittedChannelsBySignature(selected.causalSignature)
        }
        PlayerFacingNarrativeIdeaFacet(
          role = if index == 0 then PlayerFacingIdeaFacetRole.Lead else PlayerFacingIdeaFacetRole.Supporting,
          ownerClaimId = ownerClaimIdByCauseId(causeId),
          selection = selection,
          directChannels = directChannels
        )
      }
      PlayerFacingNarrativeIdea(unit, facets)
    }
    PlayerFacingCauseExposureResolution(
      readyByClaim = readyByClaim,
      dominanceDecisions = dominanceDecisions,
      crossDecisions = crossDecisions,
      ownerClaimIdByCauseId = ownerClaimIdByCauseId,
      directChannelsByCauseId = directChannelsByCauseId,
      importanceResolution = playerFacingOrdering.causeImportanceResolution,
      narrativeIdeas = narrativeIdeas,
      ideaImportanceResolution = playerFacingOrdering.ideaImportanceResolution,
      onlyMoveConstraintResolutions = onlyMoveConstraintResolutions
    )

/** The sole R-stage authority for per-Cause exposure eligibility and final
  * representative election across comparison relations.
  *
  * C/Jp/Ja records remain intact. `RelativeCauseDominancePolicy` first
  * consumes self-representing eligibility decisions for every independently
  * selectable Cause and is the only policy allowed to suppress a generic
  * fallback. Representatives are elected only among retained Causes and may
  * not discard that policy's proved fallback obligations.
  */
object CrossComparisonCauseExposurePolicy:

  private final case class ExactCausalIdentity(
      effectMode: PlayerFacingCauseEffectMode,
      kind: RelativeCauseKind,
      causalSignatures: List[String]
  )

  /** Cause kind is a label layered over channel truth. It may prevent semantic
    * deduplication, but it must not hide a contradiction on the same exact
    * root-owned channels.
    */
  private final case class ExactCausalTruthBase(
      effectMode: PlayerFacingCauseEffectMode,
      causalSignatures: List[String]
  )

  private final case class ExplanatoryNovelty(
      effectMode: PlayerFacingCauseEffectMode,
      kind: RelativeCauseKind,
      causalSignatures: List[String]
  )

  private final case class CertifiedExplanatoryNovelty(
      novelty: ExplanatoryNovelty,
      effectAgreement: Set[RootOwnedEffectExplanatoryAgreement]
  )

  /** One polarity-neutral responsibility frame for the two endpoints of the
    * same PlayedVsBest relation.  The reviewed and reference moves (and even
    * their concrete root actors) may differ, but the exact effect objects,
    * mechanism, consequence, horizon, primitive identity, and magnitude may
    * not.  Stake and direct change are checked separately as typed opposite
    * endpoint polarities.
    */
  private final case class PvbResponsibilityChannelFrame(
      rootBoardState: String,
      mover: ComparisonEndpointMoverIdentity,
      endpointTargets: List[String],
      endpointMechanisms: List[String],
      endpointConsequences: List[String],
      endpointHorizon: Option[String],
      rawTargets: List[String],
      rawMechanisms: List[String],
      rawConsequences: List[String],
      rawHorizon: Option[String],
      primitiveKind: RootOwnedEffectPrimitiveKind,
      primitiveTargets: List[String],
      planIds: List[String],
      planResult: Option[PlanResultSemanticIdentity],
      strategicAxes: List[PvbResponsibilityStrategicAxis],
      magnitude: DirectEffectMagnitudeKnowledge,
      materialEvent: Option[PvbResponsibilityMaterialEvent]
  )

  private final case class PvbResponsibilityStrategicAxis(
      kind: StrategicAxisKind,
      label: String
  )

  /** Material endpoint scopes intentionally compress every transfer to one
    * generic descriptor.  Keep the exact captured object and durable outcome
    * here so two unrelated material episodes cannot become counterparts.
    * Move UCI is omitted because it is expected to differ across endpoints.
    */
  private final case class PvbResponsibilityMaterialEvent(
      plyOffset: Int,
      capturedRole: String,
      square: String,
      targetValueCp: Int,
      durableNetCp: Int
  )

  private final case class Candidate(
      cause: RelativeCauseFact,
      ref: EvidenceRef,
      comparison: CandidateComparisonFact,
      exactIdentity: Option[ExactCausalIdentity],
      novelty: Option[ExplanatoryNovelty],
      provisionalStatus: CrossComparisonExposureStatus,
      provisionalReason: CrossComparisonExposureReason,
      effectMode: Option[PlayerFacingCauseEffectMode],
      improvement: Double,
      channels: List[DirectCauseChannel],
      truthView: RootOwnedEffectTruthView
  ):
    def certifiedNovelty: Option[CertifiedExplanatoryNovelty] =
      for
        base <- novelty
        agreement <- truthView.certifiedExplanatoryAgreement
      yield CertifiedExplanatoryNovelty(base, agreement)

    def exactTruthBase: Option[ExactCausalTruthBase] =
      exactIdentity.map(identity =>
        ExactCausalTruthBase(identity.effectMode, identity.causalSignatures)
      )

  private def buildCandidates(
      causes: List[(RelativeCauseFact, EvidenceRef)],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): List[Candidate] =
    val normalizedPlayedMoves = playedMoves.map(EvidenceRef.normalizeMove).filter(_.nonEmpty)
    causes
      .distinctBy(_._2.id)
      .flatMap { case (cause, ref) =>
        graph.record(ref).collect {
          case EvidenceRecord(_, RelativeCauseFactEvidence(registered), _) if registered == cause =>
            candidate(cause, ref, graph, normalizedPlayedMoves)
        }
      }
      .sortBy(_.ref.id)

  private def conflictingExactCauseIds(candidates: List[Candidate]): Set[String] =
    candidates
      .filter(_.exactTruthBase.nonEmpty)
      .groupBy(_.exactTruthBase.get)
      .values
      .filterNot(group => causallyAgree(group.toList))
      .flatten
      .map(_.ref.id)
      .toSet

  /** Choose as few representatives as the already-proved fallback
    * obligations allow. One Cause may absorb another only when it personally
    * owns a superset of the other's dominance obligations without reversing
    * comparison authority. At equal semantic priority, a strict obligation
    * superset is preferred before the stable Cause id breaks a true tie.
    * Incomparable obligations remain independently selected; sibling
    * obligation sets are never unioned.
    */
  private def obligationPreservingRepresentatives(
      group: List[Candidate],
      fallbackObligationsByCauseId: Map[String, Set[String]]
  ): Map[String, Candidate] =
    val candidates = group.distinctBy(_.ref.id)
    def obligations(item: Candidate): Set[String] =
      fallbackObligationsByCauseId.getOrElse(item.ref.id, Set.empty)
    def semanticPriority(item: Candidate): (Int, Int, Double, Int) =
      comparisonExposureOrder(item, item.provisionalStatus)
    val priorityOrdering = summon[Ordering[(Int, Int, Double, Int)]]
    def mayAbsorb(representative: Candidate, represented: Candidate): Boolean =
      val representativeObligations = obligations(representative)
      val representedObligations = obligations(represented)
      val representativePriority = semanticPriority(representative)
      val representedPriority = semanticPriority(represented)
      val priorityBefore = priorityOrdering.lt(representativePriority, representedPriority)
      val priorityEqual = priorityOrdering.equiv(representativePriority, representedPriority)
      representedObligations.subsetOf(representativeObligations) &&
        (
          priorityBefore ||
            (
              priorityEqual &&
                (
                  representedObligations != representativeObligations ||
                    representative.ref.id.compareTo(represented.ref.id) < 0
                )
            )
        )
    val representatives = candidates.filterNot { represented =>
      candidates.exists { representative =>
        representative.ref.id != represented.ref.id &&
          mayAbsorb(representative, represented)
      }
    }
    require(representatives.nonEmpty, "an equivalent Cause group requires a representative")
    candidates.map { represented =>
      val covering = representatives.filter(representative =>
        representative.ref.id == represented.ref.id || mayAbsorb(representative, represented)
      )
      require(covering.nonEmpty, "a Cause fallback obligation requires a covering representative")
      represented.ref.id -> covering.sortBy(selectionOrder).head
    }.toMap

  def resolve(
      causes: List[(RelativeCauseFact, EvidenceRef)],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String],
      fallbackObligationsByCauseId: Map[String, Set[String]] = Map.empty
  ): List[CrossComparisonExposureDecision] =
    val candidates = buildCandidates(causes, graph, playedMoves)
    val selectable = candidates.filter(candidate =>
      candidate.provisionalStatus.selected && candidate.exactIdentity.nonEmpty && candidate.novelty.nonEmpty
    )
    val exactConflictIds = conflictingExactCauseIds(candidates)
    val exactEligible = selectable.filterNot(item => exactConflictIds(item.ref.id))
    val exactGroups = exactEligible.groupBy(_.exactIdentity.get)
    val exactRepresentativeByCauseId = exactGroups.values.flatMap(group =>
      obligationPreservingRepresentatives(group.toList, fallbackObligationsByCauseId)
    ).toMap
    val exactRepresentatives = exactRepresentativeByCauseId.values.toList.distinctBy(_.ref.id)
    val noveltyRepresentativeByCauseId = exactRepresentatives
      .flatMap(item => item.certifiedNovelty.map(_ -> item))
      .groupBy(_._1)
      .values
      .flatMap(group =>
        obligationPreservingRepresentatives(
          group.map(_._2).toList,
          fallbackObligationsByCauseId
        )
      )
      .toMap
    val representativeByCauseId = exactEligible
      .map { item =>
        val exactRepresentative = exactRepresentativeByCauseId(item.ref.id)
        val representative = noveltyRepresentativeByCauseId
          .getOrElse(exactRepresentative.ref.id, exactRepresentative)
        item.ref.id -> representative.ref.id
      }.toMap
    val sameKindResolved = candidates.sortBy(selectionOrder).map { item =>
      if exactConflictIds(item.ref.id) then
        decision(
          item,
          CrossComparisonExposureStatus.DiagnosticComparison,
          CrossComparisonExposureReason.ConflictingRootOwnedEffectTruth,
          item.ref.id
        )
      else representativeByCauseId.get(item.ref.id) match
        case Some(representativeId) if representativeId != item.ref.id =>
          decision(
            item,
            CrossComparisonExposureStatus.RedundantAcrossComparison,
            CrossComparisonExposureReason.HigherAuthorityEquivalentCause,
            representativeId
          )
        case _ =>
          decision(item, item.provisionalStatus, item.provisionalReason, item.ref.id)
    }
    assignComparisonExposureRanks(
      sameKindResolved,
      candidates.map(item => item.ref.id -> item).toMap
    )

  /** Dominance eligibility is resolved before any cross-comparison
    * representative is elected. Every independently selectable Cause remains
    * self-representing here, while exact truth conflicts already fail closed
    * and therefore cannot suppress a fallback.
    */
  private[chessjudgment] def resolveEligibilityForDominance(
      causes: List[(RelativeCauseFact, EvidenceRef)],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): List[CrossComparisonExposureDecision] =
    val candidates = buildCandidates(causes, graph, playedMoves)
    val exactConflictIds = conflictingExactCauseIds(candidates)
    val eligibility = candidates.sortBy(selectionOrder).map { item =>
      if exactConflictIds(item.ref.id) then
        decision(
          item,
          CrossComparisonExposureStatus.DiagnosticComparison,
          CrossComparisonExposureReason.ConflictingRootOwnedEffectTruth,
          item.ref.id
        )
      else decision(item, item.provisionalStatus, item.provisionalReason, item.ref.id)
    }
    assignComparisonExposureRanks(
      eligibility,
      candidates.map(item => item.ref.id -> item).toMap
    )

  /** Assign PVB primary/complementary responsibility tiers only after the
    * single fallback-dominance pass.  A liability Cause that is itself no
    * longer public may not leave an otherwise primary alternative resource
    * orphaned at the complementary tier.
    */
  private[judgment] def resolveRetainedPvbResponsibility(
      retainedDecisions: List[CrossComparisonExposureDecision],
      causes: List[(RelativeCauseFact, EvidenceRef)],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): PvbResponsibilityResolution =
    val normalizedPlayedMoves = playedMoves.map(EvidenceRef.normalizeMove).filter(_.nonEmpty)
    val retainedById = retainedDecisions.map(decision => decision.causeEvidenceId -> decision).toMap
    val retainedCandidates = causes
      .distinctBy(_._2.id)
      .flatMap { case (cause, ref) =>
        retainedById.get(ref.id).filter(_.selected).flatMap { _ =>
          graph.record(ref).collect {
            case EvidenceRecord(_, RelativeCauseFactEvidence(registered), _) if registered == cause =>
              ref.id -> candidate(cause, ref, graph, normalizedPlayedMoves)
          }
        }
      }
      .toMap
    val liabilities = retainedDecisions.flatMap { decision =>
      Option.when(
        decision.status == CrossComparisonExposureStatus.SelectedPrimary &&
          decision.effectMode.contains(PlayerFacingCauseEffectMode.PlayedLiability)
      )(retainedCandidates.get(decision.causeEvidenceId)).flatten
    }
    val uniqueLiabilityByResourceId = retainedDecisions.flatMap { decision =>
      Option.when(
        decision.status == CrossComparisonExposureStatus.SelectedPrimary &&
          decision.effectMode.contains(PlayerFacingCauseEffectMode.AlternativeResource)
      )(retainedCandidates.get(decision.causeEvidenceId)).flatten.flatMap { resource =>
        liabilities.filter(liability => samePvbResponsibilityFrame(liability, resource, graph)) match
          case liability :: Nil => Some(resource.ref.id -> liability.ref.id)
          case _                => None
      }
    }.toMap
    val tiered = retainedDecisions.map { decision =>
      if uniqueLiabilityByResourceId.contains(decision.causeEvidenceId) then
        decision.copy(status = CrossComparisonExposureStatus.SelectedComplementary)
      else decision
    }
    PvbResponsibilityResolution(
      decisions = assignComparisonExposureRanks(tiered, retainedCandidates),
      exactPairs = uniqueLiabilityByResourceId.toList
        .map { case (resourceId, liabilityId) =>
          ExactPvbResponsibilityPair(liabilityId, resourceId)
        }
        .sortBy(pair => (pair.liabilityCauseEvidenceId, pair.resourceCauseEvidenceId))
    )

  /** Sole comparison-exposure rank authority.  The final decision status,
    * rather than the Candidate's earlier provisional status, supplies
    * Primary/Complementary authority.  Both preliminary resolution and the
    * post-dominance PVB tier pass reuse this exact ordering.
    */
  private def assignComparisonExposureRanks(
      decisions: List[CrossComparisonExposureDecision],
      candidatesById: Map[String, Candidate]
  ): List[CrossComparisonExposureDecision] =
    val selected = decisions.filter(_.selected)
    require(
      selected.forall(decision => candidatesById.contains(decision.causeEvidenceId)),
      "selected comparison exposure requires its exact Cause candidate"
    )
    val orderedExposureKeys = selected.map(decision =>
      comparisonExposureOrder(
        candidatesById(decision.causeEvidenceId),
        decision.status
      )
    ).distinct.sorted
    val rankByKey = orderedExposureKeys.zipWithIndex.toMap
    decisions.map { decision =>
      if decision.selected then
        val key = comparisonExposureOrder(
          candidatesById(decision.causeEvidenceId),
          decision.status
        )
        decision.copy(comparisonExposureRank = Some(rankByKey(key)))
      else decision.copy(comparisonExposureRank = None)
    }

  private def decision(
      candidate: Candidate,
      status: CrossComparisonExposureStatus,
      reason: CrossComparisonExposureReason,
      representativeId: String
  ): CrossComparisonExposureDecision =
    CrossComparisonExposureDecision(
      causeEvidenceId = candidate.ref.id,
      status = status,
      reason = reason,
      representativeCauseEvidenceId = representativeId,
      effectMode = candidate.effectMode
    )

  private def candidate(
      cause: RelativeCauseFact,
      ref: EvidenceRef,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Candidate =
    val comparison = graph.comparisonFor(cause).getOrElse(
      throw IllegalArgumentException(
        s"relative cause '${ref.id}' is not bound to a candidate comparison"
      )
    )
    val channels = RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph)
    val (initialStatus, initialReason, effectMode, improvement) =
      provisionalDecision(cause, comparison, channels, graph, playedMoves)
    val compatibleChannels = effectMode.toList.flatMap(mode =>
      PlayerFacingCauseSelectionPolicy.compatibleChannels(mode, channels).map(_._1)
    )
    val incompatibleSelection = initialStatus.selected && compatibleChannels.isEmpty
    val status =
      if incompatibleSelection then CrossComparisonExposureStatus.DiagnosticComparison
      else initialStatus
    val reason =
      if incompatibleSelection then CrossComparisonExposureReason.DirectChangeIncompatible
      else initialReason
    Candidate(
      cause = cause,
      ref = ref,
      comparison = comparison,
      exactIdentity = effectMode.filter(_ => compatibleChannels.nonEmpty)
        .map(mode => exactIdentity(cause, compatibleChannels, mode)),
      novelty = effectMode.filter(_ => compatibleChannels.nonEmpty)
        .map(mode => explanatoryNovelty(cause, compatibleChannels, mode)),
      provisionalStatus = status,
      provisionalReason = reason,
      effectMode = effectMode,
      improvement = improvement,
      channels = compatibleChannels,
      truthView = RootOwnedEffectTruthView.from(compatibleChannels)
    )

  private def causallyAgree(candidates: List[Candidate]): Boolean =
    candidates.headOption.forall(head =>
      candidates.tail.forall(_.truthView.agreesCausallyWith(head.truthView))
    )

  private def provisionalDecision(
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact,
      channels: List[DirectCauseChannel],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): (
      CrossComparisonExposureStatus,
      CrossComparisonExposureReason,
      Option[PlayerFacingCauseEffectMode],
      Double
  ) =
    comparison.kind match
      case CandidateComparisonKind.PlayedVsBest =>
        playedVsBestDecision(cause, comparison, channels, graph, playedMoves)
      case CandidateComparisonKind.PlayedVsAlternative =>
        playedVsAlternativeDecision(cause, comparison, channels, graph, playedMoves)
      case CandidateComparisonKind.BestVsSecond =>
        bestVsSecondDecision(cause, comparison, channels, graph, playedMoves)
      case CandidateComparisonKind.ReferenceVsAlternative =>
        result(
          CrossComparisonExposureStatus.DiagnosticComparison,
          CrossComparisonExposureReason.IndirectComparisonIsDiagnostic
        )

  private def playedVsBestDecision(
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact,
      channels: List[DirectCauseChannel],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ) =
    if !candidatePlayedOrientation(comparison, playedMoves) then
      result(
        CrossComparisonExposureStatus.DiagnosticComparison,
        CrossComparisonExposureReason.ComparisonOrientationMismatch
      )
    else if comparison.comparison.verdict == MoveChoiceVerdict.ImprovesOnReference then
      result(
        CrossComparisonExposureStatus.DiagnosticComparison,
        CrossComparisonExposureReason.PlayedVsBestDirectionInvalid
      )
    else
      val exactLiability = exactCandidateAttribution(
        cause,
        comparison,
        channels,
        graph,
        CauseAttributionKind.CandidateAllowsLiability
      )
      val exactValue = exactCandidateAttribution(
        cause,
        comparison,
        channels,
        graph,
        CauseAttributionKind.CandidateCreatesValue
      )
      val exactResource = exactReferenceResource(cause, comparison, channels, graph)
      comparison.comparison.verdict match
        case verdict if verdict.isActionableLoss && exactLiability =>
          result(
            CrossComparisonExposureStatus.SelectedPrimary,
            CrossComparisonExposureReason.PlayedVsBestOwnsPlayedResponsibility,
            Some(PlayerFacingCauseEffectMode.PlayedLiability),
            comparison.comparison.winPercentLossForMover
          )
        case verdict if verdict.isActionableLoss && exactResource =>
          result(
            CrossComparisonExposureStatus.SelectedPrimary,
            CrossComparisonExposureReason.PlayedVsBestAlternativeResource,
            Some(PlayerFacingCauseEffectMode.AlternativeResource),
            comparison.comparison.winPercentLossForMover
          )
        case MoveChoiceVerdict.MatchesReference if exactValue =>
          result(
            CrossComparisonExposureStatus.SelectedPrimary,
            CrossComparisonExposureReason.PlayedMatchesBestWithExactValue,
            Some(PlayerFacingCauseEffectMode.PlayedValue)
          )
        case MoveChoiceVerdict.PlayableLoss if exactValue =>
          result(
            CrossComparisonExposureStatus.SelectedComplementary,
            CrossComparisonExposureReason.PlayedRetainsExactValueWithPlayableLoss,
            Some(PlayerFacingCauseEffectMode.PlayedValue)
          )
        case _ =>
          result(
            CrossComparisonExposureStatus.DiagnosticComparison,
            CrossComparisonExposureReason.PlayedVsBestOutcomeIsDiagnostic,
            inferredEffectMode(cause, graph, playedMoves)
          )

  private def playedVsAlternativeDecision(
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact,
      channels: List[DirectCauseChannel],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ) =
    if !candidatePlayedOrientation(comparison, playedMoves) then
      result(
        CrossComparisonExposureStatus.DiagnosticComparison,
        CrossComparisonExposureReason.ComparisonOrientationMismatch
      )
    else
      val exactResource = exactReferenceResource(cause, comparison, channels, graph)
      val exactValue = exactCandidateAttribution(
        cause,
        comparison,
        channels,
        graph,
        CauseAttributionKind.CandidateCreatesValue
      )
      val exactLiability = exactCandidateAttribution(
        cause,
        comparison,
        channels,
        graph,
        CauseAttributionKind.CandidateAllowsLiability
      )
      val referenceImprovement = comparison.comparison.winPercentLossForMover
      val playedImprovement = comparison.comparison.candidateWinPercentDeltaForMover.max(0.0)
      if exactResource && referenceImprovement >= JudgmentThresholds.PLAYABLE_LOSS_WP then
        result(
          CrossComparisonExposureStatus.SelectedComplementary,
          CrossComparisonExposureReason.BetterAlternativeCreatesExactResource,
          Some(PlayerFacingCauseEffectMode.AlternativeResource),
          referenceImprovement
        )
      else if exactValue && playedImprovement >= JudgmentThresholds.PLAYABLE_LOSS_WP then
        result(
          CrossComparisonExposureStatus.SelectedComplementary,
          CrossComparisonExposureReason.PlayedMoveCreatesValueAgainstAlternative,
          Some(PlayerFacingCauseEffectMode.PlayedValue),
          playedImprovement
        )
      else if exactLiability && referenceImprovement >= JudgmentThresholds.PLAYABLE_LOSS_WP then
        result(
          CrossComparisonExposureStatus.SelectedComplementary,
          CrossComparisonExposureReason.BetterAlternativeExposesExactPlayedLiability,
          Some(PlayerFacingCauseEffectMode.PlayedLiability),
          referenceImprovement
        )
      else if exactResource || exactValue || exactLiability then
        result(
          CrossComparisonExposureStatus.InferiorAlternative,
          CrossComparisonExposureReason.AlternativeDoesNotImproveOnPlayed,
          inferredEffectMode(cause, graph, playedMoves)
        )
      else
        result(
          CrossComparisonExposureStatus.DiagnosticComparison,
          CrossComparisonExposureReason.UnsupportedAlternativeAttribution,
          inferredEffectMode(cause, graph, playedMoves)
        )

  private def bestVsSecondDecision(
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact,
      channels: List[DirectCauseChannel],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ) =
    if !referencePlayedOrientation(comparison, playedMoves) then
      result(
        CrossComparisonExposureStatus.DiagnosticComparison,
        CrossComparisonExposureReason.ComparisonOrientationMismatch
      )
    else if exactReferenceResource(cause, comparison, channels, graph) &&
      comparison.candidateSet.nonEmpty &&
      comparison.comparison.winPercentLossForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP
    then
      result(
        CrossComparisonExposureStatus.SelectedPrimary,
        CrossComparisonExposureReason.PlayedBestCreatesExactResource,
        Some(PlayerFacingCauseEffectMode.PlayedValue),
        comparison.comparison.winPercentLossForMover
      )
    else
      result(
        CrossComparisonExposureStatus.DiagnosticComparison,
        CrossComparisonExposureReason.CandidateConstraintIsDiagnostic,
        inferredEffectMode(cause, graph, playedMoves)
      )

  private def exactCandidateAttribution(
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact,
      channels: List[DirectCauseChannel],
      graph: TypedEvidenceGraph,
      attributionKind: CauseAttributionKind
  ): Boolean =
    channels.nonEmpty &&
      cause.sourceSide == RelativeCauseSourceSide.Candidate &&
      cause.attribution.kind == attributionKind &&
      RelativeCauseKind.sourceAttributionCompatible(cause.kind, cause.sourceSide, cause.attribution.kind) &&
      graph.relativeCauseBinding(cause).exists(_.eventLine == comparison.candidateLine)

  private def exactReferenceResource(
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact,
      channels: List[DirectCauseChannel],
      graph: TypedEvidenceGraph
  ): Boolean =
    channels.nonEmpty &&
      cause.sourceSide == RelativeCauseSourceSide.Reference &&
      cause.attribution.kind == CauseAttributionKind.ReferenceCreatesResource &&
      RelativeCauseKind.sourceAttributionCompatible(cause.kind, cause.sourceSide, cause.attribution.kind) &&
      graph.relativeCauseBinding(cause).exists(_.eventLine == comparison.referenceLine)

  private def candidatePlayedOrientation(
      comparison: CandidateComparisonFact,
      playedMoves: Set[String]
  ): Boolean =
    comparison.hasDistinctRootMoves &&
      lineIsPlayed(comparison.candidateLine, playedMoves) &&
      !lineIsPlayed(comparison.referenceLine, playedMoves)

  private def referencePlayedOrientation(
      comparison: CandidateComparisonFact,
      playedMoves: Set[String]
  ): Boolean =
    comparison.hasDistinctRootMoves &&
      lineIsPlayed(comparison.referenceLine, playedMoves) &&
      !lineIsPlayed(comparison.candidateLine, playedMoves)

  private def lineIsPlayed(line: LineNodeRef, playedMoves: Set[String]): Boolean =
    playedMoves(EvidenceRef.normalizeMove(line.rootMove))

  private def comparisonRelationKey(comparison: CandidateComparisonFact): CandidateComparisonSemanticKey =
    CandidateComparisonSemanticKey.from(comparison)

  /** A resource becomes complementary only when one played-liability Cause
    * owns a counterpart for every resource channel.  Matches from sibling
    * liability Causes are deliberately not pooled.
    */
  private def samePvbResponsibilityFrame(
      liability: Candidate,
      resource: Candidate,
      graph: TypedEvidenceGraph
  ): Boolean =
    liability.comparison.kind == CandidateComparisonKind.PlayedVsBest &&
      resource.comparison.kind == CandidateComparisonKind.PlayedVsBest &&
      comparisonRelationKey(liability.comparison) == comparisonRelationKey(resource.comparison) &&
      liability.effectMode.contains(PlayerFacingCauseEffectMode.PlayedLiability) &&
      resource.effectMode.contains(PlayerFacingCauseEffectMode.AlternativeResource) &&
      liability.channels.nonEmpty &&
      resource.channels.nonEmpty &&
      resource.channels.forall(resourceChannel =>
        liability.channels.exists(liabilityChannel =>
          samePvbResponsibilityChannel(
            liability.cause,
            liabilityChannel,
            resource.cause,
            resourceChannel,
            graph
          )
        )
      )

  private[judgment] def samePvbResponsibilityChannel(
      liabilityCause: RelativeCauseFact,
      liabilityChannel: DirectCauseChannel,
      resourceCause: RelativeCauseFact,
      resourceChannel: DirectCauseChannel,
      graph: TypedEvidenceGraph
  ): Boolean =
    (for
      liabilityScope <- ComparisonEndpointEffectObservationPolicy
        .scopeFromChannel(liabilityCause, liabilityChannel, graph)
      resourceScope <- ComparisonEndpointEffectObservationPolicy
        .scopeFromChannel(resourceCause, resourceChannel, graph)
      if expectedPvbResponsibilityPolarity(liabilityScope, resourceScope)
      liabilityFrame <- pvbResponsibilityChannelFrame(liabilityChannel, liabilityScope)
      resourceFrame <- pvbResponsibilityChannelFrame(resourceChannel, resourceScope)
    yield liabilityFrame == resourceFrame).contains(true)

  private def expectedPvbResponsibilityPolarity(
      liability: ComparisonEndpointEffectScopeKey,
      resource: ComparisonEndpointEffectScopeKey
  ): Boolean =
    liability.stake == RootOwnedEffectStake.ActorLiability &&
      resource.stake == RootOwnedEffectStake.ActorValue &&
      complementaryPvbDirectChanges(liability.directChange, resource.directChange)

  /** Exhaustive typed complement table.  Being individually compatible with
    * PlayedLiability and AlternativeResource is insufficient: the two changes
    * must describe the opposite outcomes of one exact effect frame.
    */
  private[judgment] def complementaryPvbDirectChanges(
      liability: DirectCausalChange,
      resource: DirectCausalChange
  ): Boolean =
    (liability, resource) match
      case (DirectCausalChange.Occurred, DirectCausalChange.Prevented) => true
      case (DirectCausalChange.Lost, DirectCausalChange.Maintained)    => true
      case (DirectCausalChange.Refuted, DirectCausalChange.Occurred)  => true
      case (DirectCausalChange.Missed, DirectCausalChange.Occurred)   => true
      case _                                                          => false

  private def pvbResponsibilityChannelFrame(
      channel: DirectCauseChannel,
      scope: ComparisonEndpointEffectScopeKey
  ): Option[PvbResponsibilityChannelFrame] =
    for
      _ <- Option.unless(channel.importanceDescriptorAmbiguous)(())
      _ <- Option.unless(channel.proofSegmentAmbiguous)(())
      proof <- channel.rootOwnedProof
      descriptor <- channel.rootOwnedEffectDescriptor
      if descriptor.magnitude.comparisonReady
      materialEvent <- pvbMaterialEvent(descriptor)
      axes = descriptor.identity.strategicAxes
      normalizedScope = pvbPolarityNeutralScope(scope, axes, proof)
    yield PvbResponsibilityChannelFrame(
      rootBoardState = scope.rootBoardState,
      mover = scope.mover,
      endpointTargets = normalizedScope._1,
      endpointMechanisms = normalizedScope._2,
      endpointConsequences = normalizedScope._3,
      endpointHorizon = normalizedPvbHorizon(scope.horizon, proof),
      rawTargets = normalizedObjects(channel.binding.target),
      rawMechanisms = normalizedObjects(channel.binding.mechanism),
      rawConsequences = normalizedPvbConsequences(channel.binding.consequence, axes, proof),
      rawHorizon = normalizedPvbHorizon(channel.binding.horizon, proof),
      primitiveKind = descriptor.identity.primitiveKind,
      primitiveTargets = descriptor.identity.targetSignatures.map(normalizePvbFrameAtom).distinct.sorted,
      planIds = Option.unless(descriptor.identity.planResult.nonEmpty)(descriptor.identity.planIds)
        .getOrElse(Nil)
        .map(normalizePvbFrameAtom)
        .distinct
        .sorted,
      planResult = descriptor.identity.planResult,
      strategicAxes = axes
        .map(axis => PvbResponsibilityStrategicAxis(axis.kind, normalizePvbFrameAtom(axis.label)))
        .distinct
        .sortBy(axis => (axis.kind.toString, axis.label)),
      magnitude = descriptor.magnitude,
      materialEvent = materialEvent
    )

  /** `Some(None)` means the descriptor is not material.  A material
    * descriptor without its exact event salience is not responsibility-frame
    * ready and therefore returns `None` fail-closed.
    */
  private def pvbMaterialEvent(
      descriptor: RootOwnedEffectDescriptor
  ): Option[Option[PvbResponsibilityMaterialEvent]] =
    descriptor.magnitude match
      case DirectEffectMagnitudeKnowledge.Exact(
            DirectCauseImportanceMeasure.MaterialOutcome(durableNetCp, onsetPlyOffset)
          ) =>
        descriptor.materialEventSalience
          .filter(_.plyOffset == onsetPlyOffset)
          .map(event => Some(PvbResponsibilityMaterialEvent(
            plyOffset = event.plyOffset,
            capturedRole = normalizePvbFrameAtom(event.capturedRole.name),
            square = normalizePvbFrameAtom(event.square.key),
            targetValueCp = event.targetValueCp,
            durableNetCp = durableNetCp
          )))
      case _ => Some(None)

  /** Strategic endpoint scopes encode polarity and comparison outcome in
    * their synthetic object signatures.  Normalize only those typed wrapper
    * dimensions; kind, label, primitive targets, raw mechanism, raw horizon,
    * and typed magnitude remain exact.
    */
  private def pvbPolarityNeutralScope(
      scope: ComparisonEndpointEffectScopeKey,
      axes: List[RootOwnedStrategicAxisIdentity],
      proof: RootOwnedEffectProof
  ): (List[String], List[String], List[String]) =
    if axes.isEmpty then
      (
        scope.targetSignatures.map(normalizePvbFrameAtom).distinct.sorted,
        scope.mechanismSignatures.map(normalizePvbFrameAtom).distinct.sorted,
        normalizedPvbConsequenceSignatures(scope.consequenceSignatures, proof)
      )
    else
      val normalizedAxes = axes.map(axis =>
        val kind = normalizePvbFrameAtom(axis.kind.toString)
        val label = normalizePvbFrameAtom(axis.label)
        s"strategic-axis:$kind:$label"
      ).distinct.sorted
      (
        normalizedAxes,
        axes.map(axis => s"strategic-axis-kind:${normalizePvbFrameAtom(axis.kind.toString)}").distinct.sorted,
        normalizedAxes.map(axis => s"strategic-axis-effect:$axis")
      )

  private def normalizedPvbConsequences(
      consequences: List[ConcreteChessObject],
      axes: List[RootOwnedStrategicAxisIdentity],
      proof: RootOwnedEffectProof
  ): List[String] =
    val raw = normalizedPvbConsequenceSignatures(
      consequences.map(_.signaturePart),
      proof
    )
    if axes.isEmpty then raw
    else
      val wrapperSignatures = axes.flatMap { axis =>
        val axisSignature = ConcreteChessObject(
          EvidenceObjectKind.Consequence,
          s"${axis.kind}:${axis.polarity}:${axis.label}"
        ).signaturePart
        val outcomeSignature = axis.comparisonOutcome.toList.map(outcome =>
          ConcreteChessObject(EvidenceObjectKind.Consequence, outcome.toString).signaturePart
        )
        axisSignature :: outcomeSignature
      }.map(normalizePvbFrameAtom).toSet
      val normalizedAxes = axes.map(axis =>
        s"consequence:strategic-axis:${normalizePvbFrameAtom(axis.kind.toString)}:${normalizePvbFrameAtom(axis.label)}"
      )
      (raw.filterNot(wrapperSignatures) ++ normalizedAxes).distinct.sorted

  /** Endgame status is the typed polarity of one otherwise exact technique
    * horizon.  Failed/contradicted and maintained/completed endpoints may be
    * responsibility counterparts; pattern, required squares, entry/terminal
    * offsets, and every other raw object remain exact.
    */
  private def normalizedPvbConsequenceSignatures(
      consequences: List[String],
      proof: RootOwnedEffectProof
  ): List[String] =
    val raw = consequences.map(normalizePvbFrameAtom).distinct.sorted
    pvbEndgameHorizon(proof) match
      case None => raw
      case Some(horizon) =>
        val statusSignature = normalizePvbFrameAtom(
          ConcreteChessObject(EvidenceObjectKind.Consequence, horizon.status.toString).signaturePart
        )
        (raw.filterNot(_ == statusSignature) :+ "consequence:endgame-technique-status").distinct.sorted

  private def normalizedPvbHorizon(
      horizon: Option[String],
      proof: RootOwnedEffectProof
  ): Option[String] =
    pvbEndgameHorizon(proof) match
      case Some(exact) =>
        Some(s"endgame:entry=${exact.entryPlyOffset}:terminal=${exact.terminalPlyOffset}")
      case None => horizon.map(normalizePvbFrameAtom)

  private def pvbEndgameHorizon(
      proof: RootOwnedEffectProof
  ): Option[LineEndgameTechniqueHorizon] =
    proof match
      case RootOwnedEffectProof.EndgameHorizon(_, _, horizon) => Some(horizon)
      case RootOwnedEffectProof.StrategicAxis(primitive, _, _) => pvbEndgameHorizon(primitive)
      case _ => None

  private def normalizedObjects(objects: List[ConcreteChessObject]): List[String] =
    objects.map(objectValue => normalizePvbFrameAtom(objectValue.signaturePart)).distinct.sorted

  private def normalizePvbFrameAtom(value: String): String =
    Option(value).getOrElse("").trim.toLowerCase

  private def inferredEffectMode(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Option[PlayerFacingCauseEffectMode] =
    val binding = graph.requiredRelativeCauseBinding(cause)
    val eventIsPlayed = lineIsPlayed(binding.eventLine, playedMoves)
    cause.attribution.kind match
      case CauseAttributionKind.CandidateAllowsLiability if eventIsPlayed =>
        Some(PlayerFacingCauseEffectMode.PlayedLiability)
      case CauseAttributionKind.CandidateCreatesValue if eventIsPlayed =>
        Some(PlayerFacingCauseEffectMode.PlayedValue)
      case CauseAttributionKind.ReferenceCreatesResource if eventIsPlayed =>
        Some(PlayerFacingCauseEffectMode.PlayedValue)
      case CauseAttributionKind.ReferenceCreatesResource =>
        Some(PlayerFacingCauseEffectMode.AlternativeResource)
      case _ => None

  private def exactIdentity(
      cause: RelativeCauseFact,
      channels: List[DirectCauseChannel],
      effectMode: PlayerFacingCauseEffectMode
  ): ExactCausalIdentity =
    ExactCausalIdentity(
      effectMode,
      cause.kind,
      channels.map(_.causalSignature).distinct.sorted
    )

  private def explanatoryNovelty(
      cause: RelativeCauseFact,
      channels: List[DirectCauseChannel],
      effectMode: PlayerFacingCauseEffectMode
  ): ExplanatoryNovelty =
    ExplanatoryNovelty(
      effectMode,
      cause.kind,
      channels.map(_.explanatoryNoveltySignature).distinct.sorted
    )

  private def result(
      status: CrossComparisonExposureStatus,
      reason: CrossComparisonExposureReason,
      effectMode: Option[PlayerFacingCauseEffectMode] = None,
      improvement: Double = 0.0
  ) =
    (status, reason, effectMode, improvement)

  private def comparisonExposureOrder(
      candidate: Candidate,
      status: CrossComparisonExposureStatus
  ): (Int, Int, Double, Int) =
    val (comparisonQuality, improvementOrder) =
      candidate.effectMode match
        case Some(PlayerFacingCauseEffectMode.PlayedValue) =>
          candidate.comparison.kind match
            case CandidateComparisonKind.BestVsSecond => 0 -> 0.0
            case CandidateComparisonKind.PlayedVsBest => 1 -> 0.0
            case CandidateComparisonKind.PlayedVsAlternative =>
              (2 + candidate.comparison.referenceLine.rank) -> candidate.improvement
            case CandidateComparisonKind.ReferenceVsAlternative => 100 -> candidate.improvement
        case Some(PlayerFacingCauseEffectMode.AlternativeResource) =>
          candidate.comparison.kind match
            case CandidateComparisonKind.PlayedVsBest => 0 -> -candidate.improvement
            case CandidateComparisonKind.PlayedVsAlternative => 1 -> -candidate.improvement
            case _ => 100 -> -candidate.improvement
        case Some(PlayerFacingCauseEffectMode.PlayedLiability) =>
          candidate.comparison.kind match
            case CandidateComparisonKind.PlayedVsBest => 0 -> 0.0
            case CandidateComparisonKind.PlayedVsAlternative => 1 -> -candidate.improvement
            case _ => 100 -> 0.0
        case None => 100 -> 0.0
    (
      -comparisonExposureAuthority(status),
      comparisonQuality,
      improvementOrder,
      candidate.comparison.referenceLine.rank
    )

  private def comparisonExposureAuthority(status: CrossComparisonExposureStatus): Int =
    status match
      case CrossComparisonExposureStatus.SelectedPrimary       => 2
      case CrossComparisonExposureStatus.SelectedComplementary => 1
      case _                                                   => 0

  private def selectionOrder(candidate: Candidate): (Int, Int, Double, Int, String) =
    comparisonExposureOrder(candidate, candidate.provisionalStatus) :* candidate.ref.id

/** Builds the sentence-ready unit above Cause records.  Pairing consumes only
  * exact matches already certified by the PVB responsibility policy; it never
  * re-infers a counterpart from labels, ranks, or channel overlap.
  */
object PlayerFacingIdeaUnitPolicy:

  def resolve(
      selections: List[PlayerFacingCauseSelection],
      exactPairs: List[ExactPvbResponsibilityPair]
  ): List[PlayerFacingIdeaUnit] =
    val selectionsById = selections.map(selection => selection.causeEvidence.id -> selection).toMap
    require(
      selectionsById.size == selections.size,
      "player-facing idea units require one selection per Cause"
    )
    val distinctPairs = exactPairs.distinct
    val resourceIds = distinctPairs.map(_.resourceCauseEvidenceId)
    val liabilityIds = distinctPairs.map(_.liabilityCauseEvidenceId).toSet
    require(
      resourceIds.distinct.size == resourceIds.size,
      "one resource Cause cannot borrow several PVB responsibility owners"
    )
    require(
      liabilityIds.intersect(resourceIds.toSet).isEmpty,
      "a Cause cannot be both responsibility and counterfactual facet"
    )
    distinctPairs.foreach { pair =>
      val liability = selectionsById.getOrElse(
        pair.liabilityCauseEvidenceId,
        throw IllegalArgumentException("an exact PVB pair requires its selected liability Cause")
      )
      val resource = selectionsById.getOrElse(
        pair.resourceCauseEvidenceId,
        throw IllegalArgumentException("an exact PVB pair requires its selected resource Cause")
      )
      require(
        liability.effectMode == PlayerFacingCauseEffectMode.PlayedLiability &&
          liability.exposure == PlayerFacingCauseExposureTier.Primary,
        "an exact PVB idea must be led by the primary played-liability Cause"
      )
      require(
        resource.effectMode == PlayerFacingCauseEffectMode.AlternativeResource &&
          resource.exposure == PlayerFacingCauseExposureTier.Complementary,
        "an exact PVB idea resource must be a complementary counterfactual facet"
      )
    }
    val pairedUnits = distinctPairs
      .groupBy(_.liabilityCauseEvidenceId)
      .toList
      .map { case (liabilityId, pairs) =>
        val resources = pairs.map(_.resourceCauseEvidenceId).distinct.sortBy { resourceId =>
          val selection = selectionsById(resourceId)
          (selection.comparisonExposureRank, resourceId)
        }
        PlayerFacingIdeaUnit(
          id = liabilityId,
          leadCauseEvidenceId = liabilityId,
          memberCauseEvidenceIds = liabilityId :: resources,
          kind = PlayerFacingIdeaUnitKind.ExactPvbResponsibility
        )
      }
    val pairedCauseIds = pairedUnits.flatMap(_.memberCauseEvidenceIds).toSet
    val singleUnits = selections
      .filterNot(selection => pairedCauseIds(selection.causeEvidence.id))
      .map(selection =>
        PlayerFacingIdeaUnit(
          id = selection.causeEvidence.id,
          leadCauseEvidenceId = selection.causeEvidence.id,
          memberCauseEvidenceIds = List(selection.causeEvidence.id),
          kind = PlayerFacingIdeaUnitKind.SingleCause
        )
      )
    val units = (pairedUnits ++ singleUnits).sortBy { unit =>
      val lead = selectionsById(unit.leadCauseEvidenceId)
      (lead.comparisonExposureRank, unit.id)
    }
    require(
      units.flatMap(_.memberCauseEvidenceIds).toSet == selectionsById.keySet &&
        units.flatMap(_.memberCauseEvidenceIds).distinct.size == selections.size,
      "player-facing idea units must partition the selected Cause set"
    )
    units

/** A selected Cause owns one public claim host even when proposal assembly
  * certified the same Cause under several families.
  */
object PlayerFacingCauseClaimOwnershipPolicy:

  def ownerClaimId(
      cause: RelativeCauseFact,
      claims: Iterable[JudgmentClaim]
  ): Option[String] =
    claims.toList.sortBy(claim => ownershipOrder(cause, claim)).headOption.map(_.id)

  private def ownershipOrder(
      cause: RelativeCauseFact,
      claim: JudgmentClaim
  ): (Int, Int, String) =
    val preferredFamily = ClaimFamily.fromCause(cause.kind).orElse(
      cause.kind match
        case RelativeCauseKind.PawnWeaknessTarget | RelativeCauseKind.PawnBreakOpportunity |
            RelativeCauseKind.StructuralImprovement =>
          Some(ClaimFamily.PawnStructure)
        case kind if RelativeCauseKind.strategicContrastBacked(kind) =>
          Some(ClaimFamily.Strategic)
        case _ =>
          None
    )
    (
      if preferredFamily.contains(claim.family) then 0 else 1,
      claim.family.ordinal,
      claim.id
    )
