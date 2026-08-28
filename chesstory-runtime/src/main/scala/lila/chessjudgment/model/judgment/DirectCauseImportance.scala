package lila.chessjudgment.model.judgment

import chess.{ Black, Color, White }
import lila.chessjudgment.model.line.PrincipalVariationEvidence

/** The player-facing stake of one root-owned effect.
  *
  * This is deliberately relative to an explicitly proven side. It is not
  * inferred from a Cause kind or from a comparison score.
  */
enum DirectCauseEffectStake:
  case Benefits(side: Color)
  case Harms(side: Color)
  case Preserves(side: Color)

  def stableKey: String =
    this match
      case Benefits(side)  => s"benefits:${DirectCauseImportancePolicy.colorKey(side)}"
      case Harms(side)     => s"harms:${DirectCauseImportancePolicy.colorKey(side)}"
      case Preserves(side) => s"preserves:${DirectCauseImportancePolicy.colorKey(side)}"

/** Utility of a direct effect for the move being reviewed. This is the only
  * polarity admitted to importance comparison; raw source-side stakes remain
  * provenance in [[DirectCauseImportanceFrame]].
  */
enum PlayerFacingImpact:
  case HarmsReviewedMover(side: Color)
  case BenefitsReviewedMover(side: Color)
  case PreservesReviewedMover(side: Color)

  def stableKey: String =
    this match
      case HarmsReviewedMover(side) =>
        s"harms-reviewed-mover:${DirectCauseImportancePolicy.colorKey(side)}"
      case BenefitsReviewedMover(side) =>
        s"benefits-reviewed-mover:${DirectCauseImportancePolicy.colorKey(side)}"
      case PreservesReviewedMover(side) =>
        s"preserves-reviewed-mover:${DirectCauseImportancePolicy.colorKey(side)}"

object PlayerFacingImpact:

  def from(
      effectMode: PlayerFacingCauseEffectMode,
      actor: Color,
      directChange: DirectCausalChange,
      playedChange: PlayerFacingCausalChange,
      stake: DirectCauseEffectStake
  ): Option[PlayerFacingImpact] =
    if !PlayerFacingCauseSelectionPolicy
        .playedChange(effectMode, directChange)
        .contains(playedChange)
    then None
    else
      val favoredSide = stake match
        case DirectCauseEffectStake.Benefits(side) => Some(side)
        case DirectCauseEffectStake.Harms(side)    => Some(!side)
        case DirectCauseEffectStake.Preserves(_)   => None
      val actorValue =
        favoredSide.contains(actor) || stake == DirectCauseEffectStake.Preserves(actor)
      effectMode match
        case PlayerFacingCauseEffectMode.AlternativeResource =>
          Option.when(actorValue)(PlayerFacingImpact.HarmsReviewedMover(actor))
        case PlayerFacingCauseEffectMode.PlayedLiability =>
          val actorValueDenied = actorValue && Set(
            DirectCausalChange.Lost,
            DirectCausalChange.Refuted,
            DirectCausalChange.Missed
          )(directChange)
          Option.when(favoredSide.contains(!actor) || actorValueDenied)(
            PlayerFacingImpact.HarmsReviewedMover(actor)
          )
        case PlayerFacingCauseEffectMode.PlayedValue =>
          if favoredSide.contains(actor) then
            Some(PlayerFacingImpact.BenefitsReviewedMover(actor))
          else Option.when(stake == DirectCauseEffectStake.Preserves(actor))(
            PlayerFacingImpact.PreservesReviewedMover(actor)
          )

enum DirectCauseStructuralOrigin:
  case PlanResult

/** A domain whose numeric measure has one typed meaning. */
enum DirectCauseImportanceDomain:
  case BoardMate
  case Material
  case Structural(
      origin: DirectCauseStructuralOrigin,
      kind: TransitionConsequenceKind,
      polarity: StructuralSignalPolarity,
      robustness: Option[PlanCausalRobustness]
  )

  def stableKey: String =
    this match
      case BoardMate => "board-mate"
      case Material => "material"
      case Structural(origin, kind, polarity, robustness) =>
        List(
          "structural",
          origin.toString.toLowerCase,
          kind.toString.toLowerCase,
          polarity.toString.toLowerCase,
          robustness.map(_.toString.toLowerCase).getOrElse("none")
        ).mkString(":")

/** Complete provenance for diagnostics. This frame records how a profile was
  * reached, but its equality does not grant or deny magnitude comparison.
  */
final case class DirectCauseImportanceFrame(
    comparison: CandidateComparisonSemanticKey,
    eventLine: SemanticLineKey,
    sourceSide: RelativeCauseSourceSide,
    attribution: CauseAttributionKind,
    exposure: PlayerFacingCauseExposureTier,
    effectMode: PlayerFacingCauseEffectMode,
    actor: Color,
    directChange: DirectCausalChange,
    playedChange: PlayerFacingCausalChange,
    stake: DirectCauseEffectStake
)

/** The complete permission boundary for comparing two direct effects.
  *
  * The root board is semantic FEN (piece placement, side to move, castling,
  * and en-passant only). Comparison relation, source, attribution, exposure,
  * event line, effect mode, direct change, and source-side stake remain
  * provenance in [[DirectCauseImportanceFrame]]. Only their normalized impact
  * on the reviewed mover may grant cross-comparison ordering authority.
  */
final case class DirectCauseImportanceUniverse(
    rootBoardState: String,
    impact: PlayerFacingImpact
):
  require(rootBoardState.nonEmpty, "a direct Cause importance universe requires a semantic root board")

object DirectCauseImportanceUniverse:

  def from(
      rootPosition: PositionNodeRef,
      effectMode: PlayerFacingCauseEffectMode,
      actor: Color,
      directChange: DirectCausalChange,
      playedChange: PlayerFacingCausalChange,
      stake: DirectCauseEffectStake
  ): Option[DirectCauseImportanceUniverse] =
    for
      rootBoardState <- PrincipalVariationEvidence.semanticBoardStateFen(rootPosition.fen)
      if rootPosition.sideToMove.contains(actor)
      impact <- PlayerFacingImpact.from(
        effectMode,
        actor,
        directChange,
        playedChange,
        stake
      )
    yield DirectCauseImportanceUniverse(rootBoardState, impact)

final case class DirectCauseImportanceChannelIdentity(
    channelId: String,
    exactOccurrence: RootOwnedEffectChannelOccurrenceFingerprint
):
  require(channelId.nonEmpty, "a direct Cause importance channel requires an exact channel id")

object DirectCauseImportanceChannelIdentity:
  private[judgment] def from(
      channel: PlayerFacingCauseChannelSelection
  ): DirectCauseImportanceChannelIdentity =
    DirectCauseImportanceChannelIdentity(channel.channelId, channel.exactOccurrence)

final case class DirectCauseImportanceProfile(
    causeEvidenceId: String,
    channelIdentity: DirectCauseImportanceChannelIdentity,
    causalSignature: String,
    frame: DirectCauseImportanceFrame,
    universe: DirectCauseImportanceUniverse,
    domain: DirectCauseImportanceDomain,
    measure: DirectCauseImportanceMeasure,
    effectIdentity: RootOwnedEffectIdentity = RootOwnedEffectIdentity.unscoped
):
  require(
    causalSignature == channelIdentity.exactOccurrence.causalSignature,
    "importance semantic identity must belong to its exact channel occurrence"
  )
  require(
    PlayerFacingImpact
      .from(
        frame.effectMode,
        frame.actor,
        frame.directChange,
        frame.playedChange,
        frame.stake
      )
      .contains(universe.impact),
    "importance provenance and comparison universe must agree on causal semantics"
  )

enum DirectCauseImportanceRelation:
  case Dominates
  case DominatedBy
  case Tied
  case Incomparable

final case class DirectCauseImportanceRelationDecision(
    leftCauseEvidenceId: String,
    leftChannelId: String,
    leftCausalSignature: String,
    rightCauseEvidenceId: String,
    rightChannelId: String,
    rightCausalSignature: String,
    relation: DirectCauseImportanceRelation,
    domainKey: Option[String]
)

final case class DirectCauseImportanceDecision(
    causeEvidenceId: String,
    measuredChannelIds: List[String],
    unmeasuredChannelIds: List[String],
    dominatingCauseEvidenceIds: List[String],
    dominatedWithinDomain: Boolean
):
  def fullyMeasured: Boolean =
    measuredChannelIds.nonEmpty && unmeasuredChannelIds.isEmpty

final case class DirectCauseImportanceResolution(
    selectedCauseEvidenceIds: List[String],
    profiles: List[DirectCauseImportanceProfile],
    relations: List[DirectCauseImportanceRelationDecision],
    decisions: List[DirectCauseImportanceDecision]
):
  def frontierCauseEvidenceIds: List[String] =
    selectedCauseEvidenceIds.filter(id =>
      decisions.find(_.causeEvidenceId == id).exists(decision => !decision.dominatedWithinDomain)
    )

  /** A single top Cause is certified only when every selected channel has a
    * measurable root-owned effect and exactly one Cause remains maximal.
    */
  def uniqueTopCauseEvidenceId: Option[String] =
    val selectedIds = selectedCauseEvidenceIds.distinct
    val decisionIds = decisions.map(_.causeEvidenceId)
    Option.when(
      selectedIds.nonEmpty &&
        selectedIds.size == selectedCauseEvidenceIds.size &&
        decisionIds.size == selectedIds.size &&
        decisionIds.toSet == selectedIds.toSet &&
        decisions.forall(_.fullyMeasured) &&
        frontierCauseEvidenceIds.size == 1
    )(frontierCauseEvidenceIds.head)

object DirectCauseImportanceResolution:
  val empty: DirectCauseImportanceResolution =
    DirectCauseImportanceResolution(Nil, Nil, Nil, Nil)

final case class PlayerFacingIdeaOrderingResult(
    selections: List[PlayerFacingCauseSelection],
    ideaUnits: List[PlayerFacingIdeaUnit],
    causeImportanceResolution: DirectCauseImportanceResolution,
    ideaImportanceResolution: DirectCauseImportanceResolution
)

/** Orders public sentence-ready ideas without pretending that their endpoint
  * Causes are competing ideas.  Cause-level importance remains available for
  * diagnostics, while the lead Cause of each exact idea unit supplies the
  * only importance profile used for unit ordering and unique-top authority.
  */
object PlayerFacingIdeaOrderingPolicy:

  def order(
      selections: List[PlayerFacingCauseSelection],
      ideaUnits: List[PlayerFacingIdeaUnit],
      causeImportanceResolution: DirectCauseImportanceResolution,
      ideaImportanceResolution: DirectCauseImportanceResolution
  ): PlayerFacingIdeaOrderingResult =
    val selectionsById = selections.map(selection => selection.causeEvidence.id -> selection).toMap
    val memberIds = ideaUnits.flatMap(_.memberCauseEvidenceIds)
    val leadIds = ideaUnits.map(_.leadCauseEvidenceId)
    require(
      selectionsById.size == selections.size &&
        memberIds.distinct.size == memberIds.size &&
        memberIds.toSet == selectionsById.keySet,
      "player-facing idea ordering requires an exact Cause partition"
    )
    require(
      ideaUnits.map(_.id).distinct.size == ideaUnits.size &&
        leadIds.distinct.size == leadIds.size &&
        leadIds.forall(selectionsById.contains),
      "player-facing idea ordering requires one selected lead per unit"
    )
    require(
      causeImportanceResolution.selectedCauseEvidenceIds.toSet == selectionsById.keySet,
      "Cause importance must cover every selected Cause"
    )
    require(
      ideaImportanceResolution.selectedCauseEvidenceIds.toSet == leadIds.toSet,
      "idea importance must cover exactly the responsibility-owning leads"
    )
    val layerByLeadId = dominanceLayers(
      leadIds,
      ideaImportanceResolution
    )
    val decisionByLeadId = ideaImportanceResolution.decisions.map(decision =>
      decision.causeEvidenceId -> decision
    ).toMap
    val uniqueTopLeadId = ideaImportanceResolution.uniqueTopCauseEvidenceId
    def priorityStatus(leadId: String): PlayerFacingIdeaPriorityStatus =
      val decision = decisionByLeadId(leadId)
      if !decision.fullyMeasured then PlayerFacingIdeaPriorityStatus.Unmeasured
      else if uniqueTopLeadId.contains(leadId) then PlayerFacingIdeaPriorityStatus.UniqueTop
      else if decision.dominatedWithinDomain then PlayerFacingIdeaPriorityStatus.Dominated
      else PlayerFacingIdeaPriorityStatus.Frontier
    val orderedUnits = ideaUnits
      .sortBy { unit =>
        val lead = selectionsById(unit.leadCauseEvidenceId)
        (
          layerByLeadId.getOrElse(unit.leadCauseEvidenceId, 0),
          lead.comparisonExposureRank,
          unit.id
        )
      }
      .zipWithIndex
      .map { case (unit, order) =>
        unit.copy(
          importanceLayer = layerByLeadId.getOrElse(unit.leadCauseEvidenceId, 0),
          priorityStatus = priorityStatus(unit.leadCauseEvidenceId),
          serializationOrder = order
        )
      }
    val orderedSelections = orderedUnits
      .flatMap { unit =>
        val lead = selectionsById(unit.leadCauseEvidenceId)
        val remaining = unit.memberCauseEvidenceIds
          .filterNot(_ == unit.leadCauseEvidenceId)
          .map(selectionsById)
          .sortBy(selection => (selection.comparisonExposureRank, selection.causeEvidence.id))
        lead :: remaining
      }
      .zipWithIndex
      .map { case (selection, order) => selection.copy(selectionOrder = order) }
    PlayerFacingIdeaOrderingResult(
      selections = orderedSelections,
      ideaUnits = orderedUnits,
      causeImportanceResolution = causeImportanceResolution.copy(
        selectedCauseEvidenceIds = orderedSelections.map(_.causeEvidence.id)
      ),
      ideaImportanceResolution = ideaImportanceResolution.copy(
        selectedCauseEvidenceIds = orderedUnits.map(_.leadCauseEvidenceId)
      )
    )

  /** Peels the proven strict-dominance graph one maximal layer at a time.
    * An idea whose lead is not fully proven dominated (including any
    * unmeasured or partially dominated lead) is always layer zero. Only
    * decisions already certified as dominated may contribute incoming edges
    * to later layers. Invalid or cyclic certified edges violate the upstream
    * contract; silently flattening them would hide corrupt semantics.
    */
  private def dominanceLayers(
      leadIds: List[String],
      importanceResolution: DirectCauseImportanceResolution
  ): Map[String, Int] =
    val selectedIdSet = leadIds.toSet
    val decisions = importanceResolution.decisions
    require(
      decisions.map(_.causeEvidenceId).distinct.size == decisions.size &&
        decisions.map(_.causeEvidenceId).toSet == selectedIdSet,
      "player-facing idea ordering requires one importance decision per idea lead"
    )
    val decisionById = decisions.map(decision => decision.causeEvidenceId -> decision).toMap
    val provenDominatedIds = decisionById.collect {
      case (causeId, decision) if decision.dominatedWithinDomain => causeId
    }.toSet
    val dominatorsByCauseId = provenDominatedIds.map { causeId =>
      val dominators = decisionById(causeId).dominatingCauseEvidenceIds.toSet
      causeId -> dominators
    }.toMap
    val validEdges = dominatorsByCauseId.forall { case (causeId, dominators) =>
      dominators.nonEmpty &&
        dominators.subsetOf(selectedIdSet) &&
        !dominators(causeId)
    }
    require(validEdges, "importance dominance contains an invalid certified edge")
    var remaining = provenDominatedIds
    var layer = 1
    var layers = (selectedIdSet -- provenDominatedIds).map(_ -> 0).toMap
    while remaining.nonEmpty do
      val next = remaining.filter(causeId =>
        dominatorsByCauseId(causeId).intersect(remaining).isEmpty
      )
      require(next.nonEmpty, "importance dominance contains a certified cycle")
      layers = layers ++ next.map(_ -> layer)
      remaining = remaining -- next
      layer += 1
    layers

/** Measured semantics owned by exactly one direct Cause channel.
  *
  * This DTO does not imply that the channel is admissible or player-facing;
  * those remain decisions of the public Cause policies. It only records the
  * typed effect that the channel's own root proof and descriptor can measure.
  */
final case class DirectCauseMeasuredEffect(
    stake: DirectCauseEffectStake,
    domain: DirectCauseImportanceDomain,
    measure: DirectCauseImportanceMeasure,
    effectIdentity: RootOwnedEffectIdentity
)

/** Single accessor for the measurable semantics of one root-owned channel.
  *
  * The accessor deliberately knows nothing about a Cause, an evidence graph,
  * or public admission. A missing proof, an ambiguous descriptor, or a proof
  * family without a typed magnitude therefore yields no measured effect.
  */
object DirectCauseMeasuredEffect:

  def fromChannel(channel: DirectCauseChannel): Option[DirectCauseMeasuredEffect] =
    for
      proof <- channel.rootOwnedProof
      if !channel.importanceDescriptorAmbiguous
      descriptor <- channel.rootOwnedEffectDescriptor
      effect <- fromProof(proof, descriptor)
    yield effect

  private def fromProof(
      proof: RootOwnedEffectProof,
      descriptor: RootOwnedEffectDescriptor
  ): Option[DirectCauseMeasuredEffect] =
    proof match
      case RootOwnedEffectProof.LineEpisode(_, _, episode) =>
        lineEpisodeEffect(episode, descriptor)
      case RootOwnedEffectProof.PlanResult(_, event, assessment, _) =>
        structuralEffect(
          DirectCauseStructuralOrigin.PlanResult,
          event.perspective,
          assessment.consequence,
          Some(assessment.robustness),
          descriptor = descriptor
        )
      case RootOwnedEffectProof.StrategicAxis(primitive, _, _) =>
        fromProof(primitive, descriptor)
      case _ =>
        None

  private def lineEpisodeEffect(
      episode: RootOwnedCausalEpisode,
      descriptor: RootOwnedEffectDescriptor
  ): Option[DirectCauseMeasuredEffect] =
    episode.consequence.kind match
      case LineConsequenceKind.Mate =>
        for
          beneficiary <- episode.consequence.beneficiary
          measure <- descriptor.exactMagnitude.collect {
            case exact @ DirectCauseImportanceMeasure.MateArrival(_) => exact
          }
        yield DirectCauseMeasuredEffect(
          stake = DirectCauseEffectStake.Benefits(beneficiary),
          domain = DirectCauseImportanceDomain.BoardMate,
          measure = measure,
          effectIdentity = descriptor.identity
        )
      case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss =>
        for
          beneficiary <- episode.consequence.beneficiary
          measure <- descriptor.exactMagnitude.collect {
            case exact @ DirectCauseImportanceMeasure.MaterialOutcome(_, _) => exact
          }
        yield DirectCauseMeasuredEffect(
          stake = DirectCauseEffectStake.Benefits(beneficiary),
          domain = DirectCauseImportanceDomain.Material,
          measure = measure,
          effectIdentity = descriptor.identity
        )
      case _ => None

  private def structuralEffect(
      origin: DirectCauseStructuralOrigin,
      perspective: Color,
      consequence: TransitionConsequence,
      robustness: Option[PlanCausalRobustness],
      descriptor: RootOwnedEffectDescriptor,
      forcedStake: Option[DirectCauseEffectStake] = None
  ): Option[DirectCauseMeasuredEffect] =
    val stake = forcedStake.orElse(
      consequence.polarity match
        case StructuralSignalPolarity.Gain => Some(DirectCauseEffectStake.Benefits(perspective))
        case StructuralSignalPolarity.Loss => Some(DirectCauseEffectStake.Harms(perspective))
        case StructuralSignalPolarity.Neutral => None
    )
    for
      exactStake <- stake
      measure <- descriptor.exactMagnitude.collect {
        case exact @ DirectCauseImportanceMeasure.StructuralStrength(_) => exact
      }
    yield DirectCauseMeasuredEffect(
      stake = exactStake,
      domain = DirectCauseImportanceDomain.Structural(
        origin,
        consequence.kind,
        consequence.polarity,
        robustness
      ),
      measure = measure,
      effectIdentity = descriptor.identity
    )

/** Typed partial order over effects already selected by R.
  *
  * This policy cannot generate, retain, suppress, or reorder a Cause. It only
  * describes which selected root-owned effects dominate inside one causal
  * universe and a comparable typed effect scope. Missing measures remain
  * incomparable. An actual board-mate episode is the sole cross-domain
  * exception because it proves a terminal result rather than a mate threat.
  */
object DirectCauseImportancePolicy:

  def resolve(
      selectedCauses: List[(RelativeCauseFact, EvidenceRef)],
      selections: List[PlayerFacingCauseSelection],
      directChannelsByCauseId: Map[String, List[DirectCauseChannel]],
      graph: TypedEvidenceGraph
  ): DirectCauseImportanceResolution =
    val causesById = selectedCauses.map { case (cause, ref) => ref.id -> cause }.toMap
    val selectedChannelsByCauseId = selections.map(selection =>
      selection.causeEvidence.id -> selection.channels.map(DirectCauseImportanceChannelIdentity.from)
    )
    val profiles = selections.flatMap { selection =>
      causesById.get(selection.causeEvidence.id).toList.flatMap { cause =>
        val channels = directChannelsByCauseId.getOrElse(selection.causeEvidence.id, Nil)
        selection.channels.flatMap { selectedChannel =>
          val exactChannels = channels.filter(channel =>
            channel.binding.source == selectedChannel.carrierEvidence &&
              channel.exactOccurrenceFingerprint == selectedChannel.exactOccurrence
          )
          exactChannels match
            case channel :: Nil =>
              profile(cause, selection, selectedChannel, channel, graph).toList
            case Nil => Nil
            case _ =>
              throw IllegalStateException(
                s"multiple direct channels own one exact importance occurrence: ${selectedChannel.channelId}"
              )
        }
      }
    }
    resolveProfiles(selectedChannelsByCauseId, profiles)

  private[judgment] def profile(
      cause: RelativeCauseFact,
      selection: PlayerFacingCauseSelection,
      selectedChannel: PlayerFacingCauseChannelSelection,
      channel: DirectCauseChannel,
      graph: TypedEvidenceGraph
  ): Option[DirectCauseImportanceProfile] =
    for
      effect <- DirectCauseMeasuredEffect.fromChannel(channel)
      if RootOwnedEffectPolicy.admits(cause, graph, channel)
      comparison <- graph.comparisonFor(cause)
      eventLine <- channel.binding.line
      actor <- actorSide(channel.binding)
      universe <- DirectCauseImportanceUniverse.from(
        cause.comparisonEvidence.position,
        selection.effectMode,
        actor,
        selectedChannel.directChange,
        selectedChannel.playedChange,
        effect.stake
      )
    yield DirectCauseImportanceProfile(
      causeEvidenceId = selection.causeEvidence.id,
      channelIdentity = DirectCauseImportanceChannelIdentity.from(selectedChannel),
      causalSignature = selectedChannel.causalSignature,
      frame = DirectCauseImportanceFrame(
        comparison = CandidateComparisonSemanticKey.from(comparison),
        eventLine = SemanticLineKey.from(eventLine),
        sourceSide = cause.sourceSide,
        attribution = cause.attribution.kind,
        exposure = selection.exposure,
        effectMode = selection.effectMode,
        actor = actor,
        directChange = selectedChannel.directChange,
        playedChange = selectedChannel.playedChange,
        stake = effect.stake
      ),
      universe = universe,
      domain = effect.domain,
      measure = effect.measure,
      effectIdentity = effect.effectIdentity
    )

  private[judgment] def compare(
      left: DirectCauseImportanceProfile,
      right: DirectCauseImportanceProfile
  ): DirectCauseImportanceRelation =
    val leftTerminal = terminalMateProfile(left)
    val rightTerminal = terminalMateProfile(right)
    if !typedMeasureComparable(left) || !typedMeasureComparable(right) then
      DirectCauseImportanceRelation.Incomparable
    else if left.universe != right.universe then DirectCauseImportanceRelation.Incomparable
    else if leftTerminal != rightTerminal then
      if leftTerminal then DirectCauseImportanceRelation.Dominates
      else DirectCauseImportanceRelation.DominatedBy
    else if left.domain != right.domain ||
      !sameRequiredEffectScope(left, right)
    then DirectCauseImportanceRelation.Incomparable
    else
      (left.measure, right.measure) match
        case (DirectCauseImportanceMeasure.MateArrival(leftPly), DirectCauseImportanceMeasure.MateArrival(rightPly)) =>
          lowerIsStronger(leftPly, rightPly)
        case (
              DirectCauseImportanceMeasure.MaterialOutcome(leftCp, leftPly),
              DirectCauseImportanceMeasure.MaterialOutcome(rightCp, rightPly)
            ) =>
          pareto(
            leftAtLeast = leftCp >= rightCp && leftPly <= rightPly,
            rightAtLeast = rightCp >= leftCp && rightPly <= leftPly,
            exactlyEqual = leftCp == rightCp && leftPly == rightPly
          )
        case (
              DirectCauseImportanceMeasure.StructuralStrength(leftUnits),
              DirectCauseImportanceMeasure.StructuralStrength(rightUnits)
            ) =>
          higherIsStronger(leftUnits, rightUnits)
        case _ =>
          DirectCauseImportanceRelation.Incomparable

  private[judgment] def resolveProfiles(
      selectedChannelsByCauseId: List[(String, List[DirectCauseImportanceChannelIdentity])],
      profiles: List[DirectCauseImportanceProfile]
  ): DirectCauseImportanceResolution =
    val selectedCauseIds = selectedChannelsByCauseId.map(_._1)
    require(
      selectedCauseIds.distinct.size == selectedCauseIds.size,
      "direct Cause importance requires one selected channel set per Cause"
    )
    val canonicalSelections = selectedChannelsByCauseId.sortBy(_._1).map { case (causeId, channels) =>
      require(channels.nonEmpty, s"selected Cause '$causeId' requires an exact channel occurrence")
      require(
        channels.map(_.channelId).distinct.size == channels.size &&
          channels.map(_.exactOccurrence).distinct.size == channels.size,
        s"selected Cause '$causeId' contains duplicate or conflicting exact channel identities"
      )
      causeId -> channels.sortBy(_.channelId)
    }
    val canonicalSelectedCauseIds = canonicalSelections.map(_._1)
    val selectedChannelsById = canonicalSelections.toMap
    val selectedCauseIdSet = selectedCauseIds.toSet
    val selectedChannelSets = selectedChannelsById.view.mapValues(_.toSet).toMap
    val selectedProfiles = profiles
      .filter(profile => selectedCauseIdSet(profile.causeEvidenceId))
    require(
      selectedProfiles.forall(profile =>
        selectedChannelSets(profile.causeEvidenceId)(profile.channelIdentity)
      ),
      "an importance profile must belong to a selected exact channel occurrence"
    )
    val canonicalProfiles = selectedProfiles
      .groupBy(profile => (profile.causeEvidenceId, profile.channelIdentity))
      .toList
      .map {
        case (_, profile :: Nil) => profile
        case ((causeId, channel), variants) =>
          throw IllegalStateException(
            s"exact channel '${channel.channelId}' of Cause '$causeId' has ${variants.size} importance profiles"
          )
      }
      .sortBy(profile => (profile.causeEvidenceId, profile.channelIdentity.channelId))
    val relations = canonicalProfiles.zipWithIndex.flatMap { case (left, index) =>
      canonicalProfiles.drop(index + 1).collect {
        case right if left.causeEvidenceId != right.causeEvidenceId =>
          val relation = compare(left, right)
          DirectCauseImportanceRelationDecision(
            leftCauseEvidenceId = left.causeEvidenceId,
            leftChannelId = left.channelIdentity.channelId,
            leftCausalSignature = left.causalSignature,
            rightCauseEvidenceId = right.causeEvidenceId,
            rightChannelId = right.channelIdentity.channelId,
            rightCausalSignature = right.causalSignature,
            relation = relation,
            domainKey = Option.when(relation != DirectCauseImportanceRelation.Incomparable)(
              relationDomainKey(left, right)
            )
          )
      }
    }
    val decisions = canonicalSelections.map { case (causeId, selectedChannels) =>
      val ownedProfiles = canonicalProfiles.filter(_.causeEvidenceId == causeId)
      val measuredChannels = ownedProfiles.map(_.channelIdentity)
      val measuredChannelSet = measuredChannels.toSet
      val unmeasuredChannels = selectedChannels.filterNot(measuredChannelSet)
      val dominatorIdsByProfile = ownedProfiles.map { owned =>
        canonicalProfiles
          .filter(other =>
            other.causeEvidenceId != causeId &&
              compare(other, owned) == DirectCauseImportanceRelation.Dominates
          )
          .map(_.causeEvidenceId)
          .toSet
      }
      // Cause-level demotion needs one competing Cause that strictly dominates
      // every measured channel by itself. Independent sibling Causes may not
      // pool their profiles to push a multi-channel Cause into a lower layer.
      val commonDominatorIds = dominatorIdsByProfile match
        case first :: rest => rest.foldLeft(first)(_ intersect _).toList.sorted
        case Nil           => Nil
      DirectCauseImportanceDecision(
        causeEvidenceId = causeId,
        measuredChannelIds = measuredChannels.map(_.channelId),
        unmeasuredChannelIds = unmeasuredChannels.map(_.channelId),
        dominatingCauseEvidenceIds = commonDominatorIds,
        dominatedWithinDomain =
          unmeasuredChannels.isEmpty &&
            commonDominatorIds.nonEmpty
      )
    }
    DirectCauseImportanceResolution(
      selectedCauseEvidenceIds = canonicalSelectedCauseIds,
      profiles = canonicalProfiles,
      relations = relations,
      decisions = decisions
    )

  private def sameRequiredEffectScope(
      left: DirectCauseImportanceProfile,
      right: DirectCauseImportanceProfile
  ): Boolean =
    (left.domain, right.domain) match
      case (
            DirectCauseImportanceDomain.Structural(_, _, _, _),
            DirectCauseImportanceDomain.Structural(_, _, _, _)
          ) =>
        left.effectIdentity != RootOwnedEffectIdentity.unscoped &&
          right.effectIdentity != RootOwnedEffectIdentity.unscoped &&
          left.effectIdentity == right.effectIdentity
      case _ => true

  private def terminalMateProfile(profile: DirectCauseImportanceProfile): Boolean =
    (profile.domain, profile.measure) match
      case (DirectCauseImportanceDomain.BoardMate, DirectCauseImportanceMeasure.MateArrival(_)) => true
      case _ => false

  private def typedMeasureComparable(profile: DirectCauseImportanceProfile): Boolean =
    (profile.domain, profile.measure) match
      case (DirectCauseImportanceDomain.BoardMate, DirectCauseImportanceMeasure.MateArrival(_)) => true
      case (DirectCauseImportanceDomain.Material, DirectCauseImportanceMeasure.MaterialOutcome(_, _)) => true
      case (
            DirectCauseImportanceDomain.Structural(_, _, _, _),
            DirectCauseImportanceMeasure.StructuralStrength(_)
          ) => true
      case _ => false

  private def relationDomainKey(
      left: DirectCauseImportanceProfile,
      right: DirectCauseImportanceProfile
  ): String =
    if left.domain == right.domain then domainKey(left)
    else "terminal:board-mate"

  private def domainKey(profile: DirectCauseImportanceProfile): String =
    profile.domain match
      case DirectCauseImportanceDomain.Structural(_, _, _, _) =>
        s"${profile.domain.stableKey}|effect:${profile.effectIdentity.stableKey}"
      case _ => profile.domain.stableKey

  private def actorSide(binding: EvidenceObjectBinding): Option[Color] =
    binding.actor.collect {
      case ConcreteChessObject(EvidenceObjectKind.Side, side) if side.equalsIgnoreCase("white") => White
      case ConcreteChessObject(EvidenceObjectKind.Side, side) if side.equalsIgnoreCase("black") => Black
    }.distinct match
      case side :: Nil => Some(side)
      case _           => None

  private def lowerIsStronger(left: Int, right: Int): DirectCauseImportanceRelation =
    if left < right then DirectCauseImportanceRelation.Dominates
    else if left > right then DirectCauseImportanceRelation.DominatedBy
    else DirectCauseImportanceRelation.Tied

  private def higherIsStronger(left: Int, right: Int): DirectCauseImportanceRelation =
    if left > right then DirectCauseImportanceRelation.Dominates
    else if left < right then DirectCauseImportanceRelation.DominatedBy
    else DirectCauseImportanceRelation.Tied

  private def pareto(
      leftAtLeast: Boolean,
      rightAtLeast: Boolean,
      exactlyEqual: Boolean
  ): DirectCauseImportanceRelation =
    if exactlyEqual then DirectCauseImportanceRelation.Tied
    else if leftAtLeast && !rightAtLeast then DirectCauseImportanceRelation.Dominates
    else if rightAtLeast && !leftAtLeast then DirectCauseImportanceRelation.DominatedBy
    else DirectCauseImportanceRelation.Incomparable

  private[judgment] def colorKey(color: Color): String =
    if color.white then "white" else "black"
