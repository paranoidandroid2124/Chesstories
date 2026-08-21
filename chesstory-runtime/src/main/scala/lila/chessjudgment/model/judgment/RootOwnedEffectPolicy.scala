package lila.chessjudgment.model.judgment

import chess.*
import lila.chessjudgment.model.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.position.{ PawnTopology, PositionFeatures }
import lila.chessjudgment.model.{ ActivePlans, BranchReplyProbeBinding, Fact, Motif, MotifCategory, PlanEventIdentity, PlanId, PlanMatch, PlanScoringResult, PlanSequenceSummary, TransitionType }
import lila.chessjudgment.model.structure.{ PlanAlignment, StructureId, StructureProfile }
import lila.chessjudgment.model.strategic.{ EngineLine, PlanContinuity }
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }
private[judgment] object RootOwnedEffectDescriptorPolicy:

  private final case class IdentityParts(
      primitiveKind: RootOwnedEffectPrimitiveKind,
      planIds: List[String] = Nil,
      strategicAxes: List[RootOwnedStrategicAxisIdentity] = Nil,
      planResult: Option[PlanResultSemanticIdentity] = None
  )

  def describe(
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof
  ): RootOwnedEffectDescriptor =
    val parts = identityParts(proof)
    val materialOutcome = rootOwnedMaterialOutcome(proof)
    RootOwnedEffectDescriptor(
      identity = RootOwnedEffectIdentity(
        primitiveKind = parts.primitiveKind,
        targetSignatures = binding.target.map(_.signaturePart).distinct.sorted,
        planIds = parts.planIds.map(normalize).filter(_.nonEmpty).distinct.sorted,
        strategicAxes = parts.strategicAxes.distinct.sortBy(_.stableKey),
        planResult = parts.planResult
      ),
      magnitude = magnitudeKnowledge(proof, binding, materialOutcome),
      materialEventSalience = materialOutcome.map(_.event)
    )

  private def identityParts(proof: RootOwnedEffectProof): IdentityParts =
    proof match
      case _: RootOwnedEffectProof.LineEpisode =>
        IdentityParts(RootOwnedEffectPrimitiveKind.LineEpisode)
      case _: RootOwnedEffectProof.RootLineEvent =>
        IdentityParts(RootOwnedEffectPrimitiveKind.RootLineEvent)
      case _: RootOwnedEffectProof.EndgameHorizon =>
        IdentityParts(RootOwnedEffectPrimitiveKind.EndgameHorizon)
      case _: RootOwnedEffectProof.StructuralTransition =>
        IdentityParts(RootOwnedEffectPrimitiveKind.StructuralTransition)
      case _: RootOwnedEffectProof.RootMoveMotif =>
        IdentityParts(RootOwnedEffectPrimitiveKind.RootMoveMotif)
      case _: RootOwnedEffectProof.RootRelation =>
        IdentityParts(RootOwnedEffectPrimitiveKind.RootRelation)
      case _: RootOwnedEffectProof.ThreatCreation =>
        IdentityParts(RootOwnedEffectPrimitiveKind.ThreatCreation)
      case _: RootOwnedEffectProof.ThreatDefense =>
        IdentityParts(RootOwnedEffectPrimitiveKind.ThreatDefense)
      case RootOwnedEffectProof.PlanResult(_, event, assessment, selectedInducedResponse) =>
        IdentityParts(
          RootOwnedEffectPrimitiveKind.PlanResult,
          planIds = List(event.planId.id),
          planResult = Some(
            PlanResultSemanticIdentity.from(event, assessment, selectedInducedResponse)
          )
        )
      case RootOwnedEffectProof.PlanRestriction(_, event, _, _) =>
        IdentityParts(
          RootOwnedEffectPrimitiveKind.PlanRestriction,
          planIds = List(event.planId.id)
        )
      case _: RootOwnedEffectProof.DefensiveRecaptureResource =>
        IdentityParts(RootOwnedEffectPrimitiveKind.DefensiveRecaptureResource)
      case RootOwnedEffectProof.StrategicAxis(primitive, axis, outcome) =>
        val base = identityParts(primitive)
        base.copy(
          strategicAxes = base.strategicAxes :+ RootOwnedStrategicAxisIdentity(
            kind = axis.kind,
            polarity = axis.polarity,
            label = normalize(axis.label),
            comparisonOutcome = outcome
          )
        )

  private def magnitudeKnowledge(
      proof: RootOwnedEffectProof,
      binding: EvidenceObjectBinding,
      materialOutcome: Option[RootOwnedMaterialOutcome]
  ): DirectEffectMagnitudeKnowledge =
    proof match
      case RootOwnedEffectProof.LineEpisode(_, line, episode) =>
        episode.consequence.kind match
          case LineConsequenceKind.Mate =>
            Option
              .when(
                episode.consequence.beneficiary.nonEmpty && episode.eventPlyOffset >= 0
              )(DirectCauseImportanceMeasure.MateArrival(episode.eventPlyOffset))
              .map(DirectEffectMagnitudeKnowledge.Exact.apply)
              .getOrElse(DirectEffectMagnitudeKnowledge.ExpectedButMissing)
          case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss =>
            (for
              beneficiary <- episode.consequence.beneficiary
              outcome <- materialOutcome
              if outcome.beneficiary == beneficiary
              capture <- exactEpisodeCapture(line, episode)
              if capture.side == beneficiary
              if outcome.event.matches(capture)
              if outcome.durableNetCp > 0
            yield DirectCauseImportanceMeasure.MaterialOutcome(
              outcome.durableNetCp,
              outcome.event.plyOffset
            )).map(DirectEffectMagnitudeKnowledge.Exact.apply)
              .getOrElse(DirectEffectMagnitudeKnowledge.ExpectedButMissing)
          case _ => DirectEffectMagnitudeKnowledge.NotApplicable
      case RootOwnedEffectProof.ThreatCreation(_, threat) =>
        expectedMagnitude(threatMagnitude(threat, binding))
      case RootOwnedEffectProof.ThreatDefense(_, threat, _) =>
        expectedMagnitude(threatMagnitude(threat, binding))
      case RootOwnedEffectProof.StructuralTransition(_, _, consequence) =>
        expectedMagnitude(structuralMagnitude(consequence))
      case RootOwnedEffectProof.PlanResult(_, _, assessment, _) =>
        expectedMagnitude(structuralMagnitude(assessment.consequence))
      case RootOwnedEffectProof.PlanRestriction(_, _, consequence, _) =>
        expectedMagnitude(structuralMagnitude(consequence))
      case _: RootOwnedEffectProof.DefensiveRecaptureResource =>
        DirectEffectMagnitudeKnowledge.NotApplicable
      case RootOwnedEffectProof.StrategicAxis(primitive, _, _) =>
        magnitudeKnowledge(primitive, binding, materialOutcome)
      case _: RootOwnedEffectProof.RootLineEvent | _: RootOwnedEffectProof.EndgameHorizon |
          _: RootOwnedEffectProof.RootMoveMotif | _: RootOwnedEffectProof.RootRelation =>
        DirectEffectMagnitudeKnowledge.NotApplicable

  private def expectedMagnitude(
      magnitude: Option[DirectCauseImportanceMeasure]
  ): DirectEffectMagnitudeKnowledge =
    magnitude
      .map(DirectEffectMagnitudeKnowledge.Exact.apply)
      .getOrElse(DirectEffectMagnitudeKnowledge.ExpectedButMissing)

  private def exactEpisodeCapture(
      line: LineFactEvidence,
      episode: RootOwnedCausalEpisode
  ): Option[LineMaterialCapture] =
    line.lineReplaySteps.lift(episode.eventPlyOffset).flatMap { eventStep =>
      line.materialCaptures.filter(capture =>
        capture.plyOffset == episode.eventPlyOffset &&
          EvidenceRef.sameMove(capture.moveUci, eventStep.moveUci)
      ) match
        case capture :: Nil => Some(capture)
        case _              => None
    }

  private def rootOwnedMaterialOutcome(
      proof: RootOwnedEffectProof
  ): Option[RootOwnedMaterialOutcome] =
    proof match
      case RootOwnedEffectProof.LineEpisode(_, _, episode) =>
        episode.consequence.materialOutcome
      case RootOwnedEffectProof.StrategicAxis(primitive, _, _) =>
        rootOwnedMaterialOutcome(primitive)
      case _ => None

  private def threatMagnitude(
      threat: ThreatEpisodeEvidence,
      binding: EvidenceObjectBinding
  ): Option[DirectCauseImportanceMeasure] =
    Option.when(threat.episode.turnsToImpact > 0 && binding.target.nonEmpty)(
      DirectCauseImportanceMeasure.ThreatHorizon(threat.episode.turnsToImpact)
    )

  private def structuralMagnitude(
      consequence: TransitionConsequence
  ): Option[DirectCauseImportanceMeasure] =
    Option.when(consequence.strength > 0)(
      DirectCauseImportanceMeasure.StructuralStrength(consequence.strength)
    )

  private def normalize(value: String): String =
    Option(value).getOrElse("").trim.toLowerCase

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

  /** A replayed consequence belongs to the root move only when its causal
    * chain starts with a typed root bridge. Moving the same piece again later
    * is a witness of identity, not proof that the root move caused the later
    * result. Material continuation links may transport an already-rooted
    * episode, but cannot seed one.
    */
  private[chessjudgment] def admitsLineEpisode(
      line: LineFactEvidence,
      episode: RootOwnedCausalEpisode
  ): Boolean =
    val rootMove = EvidenceRef.normalizeMove(line.line.rootMove)
    val chain = episode.chainMoves.map(EvidenceRef.normalizeMove)
    val replay = line.lineReplayMoves.map(EvidenceRef.normalizeMove)
    val eventMove = chain.lastOption
    val exactReplayPrefix =
      episode.line == line.line &&
        EvidenceRef.sameMove(episode.actor.moveUci, rootMove) &&
        chain.nonEmpty &&
        chain == replay.take(chain.size) &&
        episode.eventPlyOffset == chain.size - 1 &&
        line.lineReplaySteps
          .lift(episode.eventPlyOffset)
          .exists(step => eventMove.exists(EvidenceRef.sameMove(step.moveUci, _)))
    val typedEpisodeWasDerived =
      RootOwnedCausalEpisode.from(line, rootMove).contains(episode)

    exactReplayPrefix &&
      typedEpisodeWasDerived &&
      causalPathStartsAtRoot(line, episode, rootMove)

  private def causalPathStartsAtRoot(
      line: LineFactEvidence,
      episode: RootOwnedCausalEpisode,
      rootMove: String
  ): Boolean =
    val chain = episode.chainMoves.map(EvidenceRef.normalizeMove)
    val eventMove = chain.last
    val indexedMoves = chain.zipWithIndex
    def occursBefore(causeMove: String, effectMove: String): Boolean =
      indexedMoves.exists { case (cause, causeIndex) =>
        EvidenceRef.sameMove(cause, causeMove) &&
          indexedMoves.exists { case (effect, effectIndex) =>
            effectIndex > causeIndex && EvidenceRef.sameMove(effect, effectMove)
          }
      }
    val transports = episode.links.filter(link =>
      link.kind == RootCausalLinkKind.MaterialActorContinuation ||
        link.kind == RootCausalLinkKind.MaterialCaptureResponse
    )
    def reachesEvent(currentMove: String, visited: Set[String]): Boolean =
      EvidenceRef.sameMove(currentMove, eventMove) ||
        transports.exists(link =>
          EvidenceRef.sameMove(link.causeMove, currentMove) &&
            occursBefore(link.causeMove, link.effectMove) &&
            !visited(EvidenceRef.normalizeMove(link.effectMove)) &&
            reachesEvent(
              link.effectMove,
              visited + EvidenceRef.normalizeMove(link.effectMove)
            )
        )

    episode.links.exists { link =>
      val rootSeed = EvidenceRef.sameMove(link.causeMove, rootMove)
      link.kind match
        case RootCausalLinkKind.ImmediateRootAction =>
          chain.size == 1 && rootSeed &&
            EvidenceRef.sameMove(link.effectMove, rootMove)
        case RootCausalLinkKind.ContinuousLineAccess =>
          rootSeed &&
            occursBefore(link.causeMove, link.effectMove) &&
            episode.chainMoves.zipWithIndex.exists { case (move, plyOffset) =>
              EvidenceRef.sameMove(move, link.effectMove) &&
                RootOwnedCausalEpisode
                  .continuousLineAccessSeedLink(line, plyOffset)
                  .contains(link)
            } &&
            reachesEvent(
              link.effectMove,
              Set(EvidenceRef.normalizeMove(link.effectMove))
            )
        case RootCausalLinkKind.ForcedCaptureResponse =>
          rootSeed &&
            occursBefore(link.causeMove, link.effectMove) &&
            episode.chainMoves.zipWithIndex.exists { case (move, plyOffset) =>
              EvidenceRef.sameMove(move, link.effectMove) &&
                (for
                  rootStep <- line.lineReplaySteps.headOption
                  eventStep <- line.lineReplaySteps.lift(plyOffset)
                  seed <- RootOwnedCausalEpisode
                    .forcedCaptureResponseLink(line, rootStep, eventStep, plyOffset)
                yield seed).contains(link)
            } &&
            reachesEvent(
              link.effectMove,
              Set(EvidenceRef.normalizeMove(link.effectMove))
            )
        case RootCausalLinkKind.ForcedCheckResponse =>
          rootSeed &&
            occursBefore(link.causeMove, link.effectMove) &&
            episode.chainMoves.zipWithIndex.exists { case (move, plyOffset) =>
              EvidenceRef.sameMove(move, link.effectMove) &&
                (for
                  rootStep <- line.lineReplaySteps.headOption
                  eventStep <- line.lineReplaySteps.lift(plyOffset)
                  seed <- RootOwnedCausalEpisode
                    .forcedCheckResponseLink(line, rootStep, eventStep, plyOffset)
                yield seed).contains(link)
            } &&
            reachesEvent(
              link.effectMove,
              Set(EvidenceRef.normalizeMove(link.effectMove))
            )
        case RootCausalLinkKind.RootActorCaptured =>
          rootSeed &&
            occursBefore(link.causeMove, link.effectMove) &&
            episode.chainMoves.zipWithIndex.exists { case (move, plyOffset) =>
              EvidenceRef.sameMove(move, link.effectMove) &&
                RootOwnedCausalEpisode
                  .rootActorCapturedSeedLink(line, episode.actor, plyOffset)
                  .contains(link)
            } &&
            reachesEvent(
              link.effectMove,
              Set(EvidenceRef.normalizeMove(link.effectMove))
            )
        case RootCausalLinkKind.RootActorContinuation |
            RootCausalLinkKind.MaterialActorContinuation |
            RootCausalLinkKind.MaterialCaptureResponse =>
          false
    }

  def structuralProofs(
      cause: RelativeCauseFact,
      source: EvidenceRef,
      delta: StructuralDeltaEvidence
  ): List[(TransitionConsequence, RootOwnedEffectProof)] =
    RelativeCauseKind
      .structuralConsequences(cause.kind, delta)
      .map(consequence =>
        consequence -> RootOwnedEffectProof.StructuralTransition(source, delta, consequence)
      )

  def threatProof(
      kind: RelativeCauseKind,
      source: EvidenceRef,
      threat: ThreatEpisodeEvidence
  ): Option[RootOwnedEffectProof] =
    kind match
      case RelativeCauseKind.OnlyDefenseNecessity =>
        Some(RootOwnedEffectProof.ThreatDefense(source, threat, onlyDefense = true))
      case RelativeCauseKind.DefensiveResource =>
        Some(RootOwnedEffectProof.ThreatDefense(source, threat, onlyDefense = false))
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability | RelativeCauseKind.KingForcing =>
        Some(RootOwnedEffectProof.ThreatCreation(source, threat))
      case _ => None

  def planResultProof(
      cause: RelativeCauseFact,
      source: EvidenceRef,
      event: PlanCausalEventEvidence
  ): Option[(PlanCausalResultAssessment, RootOwnedEffectProof)] =
    exactPlanAssessment(cause.kind, event).map(assessment =>
      assessment -> RootOwnedEffectProof.PlanResult(source, event, assessment)
    )

  def planRestrictionProofs(
      source: EvidenceRef,
      event: PlanCausalEventEvidence,
      graph: TypedEvidenceGraph
  ): List[(TransitionConsequence, RootOwnedEffectProof)] =
    event.opponentResourceDeterrence.toList.flatMap { deterrence =>
      canonicalPlanRestrictionConsequence(event, deterrence, graph).toList.map(consequence =>
        consequence -> RootOwnedEffectProof.PlanRestriction(
          source,
          event,
          consequence,
          deterrence
        )
      )
    }

  def certify(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof,
      primitiveCausalSignature: Option[String] = None
  ): Option[RootOwnedEffect] =
    expectedDirectChange(cause, proof).flatMap { change =>
      val effect = DirectCauseChannel(
        binding = binding,
        directChange = change,
        primitiveCausalSignature = primitiveCausalSignature,
        rootOwnedProof = Some(proof)
      )
      Option.when(admits(cause, graph, effect))(effect)
    }

  def admits(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      effect: RootOwnedEffect
  ): Boolean =
    effect.rootOwnedProof.exists { proof =>
      graph.relativeCauseBinding(cause).exists { causeBinding =>
        val eventLine = causeBinding.eventLine
        RootCausalActor
          .fromPosition(cause.comparisonEvidence.position, eventLine.rootMove)
          .exists { actor =>
            RelativeCauseKind.sourceAttributionCompatible(
              cause.kind,
              cause.sourceSide,
              cause.attribution.kind
            ) &&
              requiredExactPlanResultAuthority(cause, graph, proof) &&
              sameCausalRootBoard(cause, effect.binding, proof) &&
              expectedDirectChange(cause, proof).contains(effect.directChange) &&
              effect.binding.line.contains(eventLine) &&
              exactRootActor(effect.binding, actor) &&
              specificTargetReady(effect.binding.target) &&
              effect.binding.mechanism.nonEmpty &&
              effect.binding.consequence.nonEmpty &&
              proofSourceCarried(effect.binding, proof.primitiveSource) &&
              primitiveSourceRegistered(graph, proof) &&
              proofOwnsEventRoot(proof, eventLine, actor, cause, graph, effect.binding) &&
              attributionOwnsEffect(cause, proof, actor)
          }
      }
    }

  def strategicProof(
      primitive: RootOwnedEffectProof,
      axis: StrategicAxisDetail,
      outcome: Option[StrategicAxisComparisonOutcome] = None
  ): Option[RootOwnedEffectProof] =
    primitive match
      case RootOwnedEffectProof.StrategicAxis(inner, existingAxis, existingOutcome)
          if existingAxis.stableKey == axis.stableKey =>
        Some(RootOwnedEffectProof.StrategicAxis(
          inner,
          existingAxis,
          outcome.orElse(existingOutcome)
        ))
      case RootOwnedEffectProof.StrategicAxis(_, _, _) =>
        None
      case _ =>
        Some(RootOwnedEffectProof.StrategicAxis(primitive, axis, outcome))

  /** Narrow self-contained strategic authority.  This is not a substitute for
    * a sustained axis in general: it accepts only a verified root structural
    * transition whose actor occupies an opposing pawn's exact one-step advance
    * square.  Strategic wrappers may refine that primitive but cannot invent it.
    */
  private[chessjudgment] def exactRootPawnAdvanceRestrictionPrimitive(
      proof: RootOwnedEffectProof
  ): Option[(EvidenceRef, StructuralDeltaEvidence, TransitionConsequence)] =
    proof match
      case RootOwnedEffectProof.StructuralTransition(source, delta, consequence) =>
        Option.when(
          StructuralDeltaEvidence
            .exactRootOccupiedPawnAdvanceRestrictions(delta, consequence)
            .nonEmpty
        )((source, delta, consequence))
      case RootOwnedEffectProof.StrategicAxis(primitive, axis, _) =>
        Option
          .when(
            axis.kind == StrategicAxisKind.Counterplay &&
              axis.polarity == StrategicAxisPolarity.Restrain &&
              strategicAxisOwnsPrimitive(primitive, axis)
          )(primitive)
          .flatMap(exactRootPawnAdvanceRestrictionPrimitive)
      case _ =>
        None

  /** Extracts only a bare exact plan result. Strategic wrappers belong to the
    * strategic comparison inventory and cannot substitute for this primitive.
    */
  private[chessjudgment] def exactPlanResultPrimitive(
      proof: RootOwnedEffectProof
  ): Option[(
      EvidenceRef,
      PlanCausalEventEvidence,
      PlanCausalResultAssessment,
      Option[PlanCausalResponse]
  )] =
    proof match
      case RootOwnedEffectProof.PlanResult(source, event, assessment, selectedInducedResponse) =>
        Some((source, event, assessment, selectedInducedResponse))
      case _ =>
        None

  private def requiredExactPlanResultAuthority(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      proof: RootOwnedEffectProof
  ): Boolean =
    exactPlanResultPrimitive(proof) match
      case Some((source, event, assessment, Some(selectedInducedResponse)))
          if cause.kind == RelativeCauseKind.WrongMoveOrder =>
        graph
          .comparisonFor(cause)
          .flatMap(comparison =>
            ComparisonEndpointEffectObservationPolicy
              .exactInducedResponseMoveOrder(
                comparison,
                cause.sourceSide,
                source,
                event,
                graph
              )
          )
          .contains(assessment -> selectedInducedResponse)
      case Some((_, _, _, None)) if cause.kind == RelativeCauseKind.WrongMoveOrder =>
        false
      case Some((_, _, _, Some(_))) =>
        false
      case exactPlanResult =>
        !RelativeCauseKind.requiresExactPlanResult(cause.kind) ||
          exactPlanResult.exists { case (_, event, assessment, selectedInducedResponse) =>
            selectedInducedResponse.isEmpty &&
            exactPlanAssessment(cause.kind, event).contains(assessment)
          }

  /** Common root-ownership predicates shared by Cause generation and public
    * projection. Callers may add kind-specific semantics, but may not weaken
    * these legal-root and exact-line requirements.
    */
  def lineRecordOwnsEventRoot(
      source: EvidenceRef,
      line: LineFactEvidence,
      eventLine: LineNodeRef
  ): Boolean =
    source.line.contains(eventLine) &&
      line.line == eventLine &&
      RootCausalActor.fromLineFact(line, eventLine.rootMove).nonEmpty

  def motifRecordOwnsEventRoot(
      source: EvidenceRef,
      motif: MoveMotifEvidence,
      eventLine: LineNodeRef
  ): Boolean =
    source.line.contains(eventLine) &&
      RootCausalActor.fromPosition(source.position, eventLine.rootMove).nonEmpty &&
      motif.recordLineBound(source) &&
      motif.isRootEvent &&
      EvidenceRef.sameMove(motif.rootMove, eventLine.rootMove) &&
      motif.eventMove.forall(EvidenceRef.sameMove(_, eventLine.rootMove))

  def relationRecordOwnsEventRoot(
      source: EvidenceRef,
      relation: RelationFactEvidence,
      eventLine: LineNodeRef
  ): Boolean =
    source.line.contains(eventLine) &&
      RootCausalActor.fromPosition(source.position, eventLine.rootMove).nonEmpty &&
      relation.hasConcreteRelationProof &&
      relation.mentionsLineMove(eventLine.rootMove) &&
      relation.rootGeometryConnected(eventLine.rootMove)

  def tacticalCarrierOwnsEventRoot(
      source: EvidenceRef,
      mechanism: TacticalMechanismEvidence,
      eventLine: LineNodeRef
  ): Boolean =
    source.line.contains(eventLine) &&
      RootCausalActor.fromPosition(source.position, eventLine.rootMove).nonEmpty &&
      mechanism.line.contains(eventLine) &&
      mechanism.moveUci.exists(EvidenceRef.sameMove(_, eventLine.rootMove))

  /** A MaterialGain carrier may only summarize an admitted primitive that
    * owns the same event root. A carrier cannot turn a continuation-only line
    * result into root causality; an independently root-owned motif or relation
    * remains sufficient.
    */
  private[chessjudgment] def materialGainPrimitiveOwnsEventRoot(
      signal: TacticalMechanismSignal,
      record: EvidenceRecord,
      eventLine: LineNodeRef
  ): Boolean =
    record.payload match
      case line: LineFactEvidence =>
        signal.kind == TacticalMechanismSignalKind.LineConsequence &&
          signal.label.equalsIgnoreCase(LineConsequenceKind.MaterialGain.toString) &&
          RootOwnedEffectPolicy.lineRecordOwnsEventRoot(record.ref, line, eventLine) &&
          line
            .rootOwnedCausalEpisodes(eventLine.rootMove)
            .exists(_.consequence.kind == LineConsequenceKind.MaterialGain)
      case motif: MoveMotifEvidence =>
        signal.kind == TacticalMechanismSignalKind.Motif &&
          RootOwnedEffectPolicy.motifRecordOwnsEventRoot(record.ref, motif, eventLine) &&
          TacticalMechanismKind.fromMotif(motif.motif).contains(TacticalMechanismKind.MaterialGain)
      case relation: RelationFactEvidence =>
        signal.kind == TacticalMechanismSignalKind.Relation &&
          RootOwnedEffectPolicy.relationRecordOwnsEventRoot(record.ref, relation, eventLine) &&
          TacticalMechanismKind.fromRelation(relation.kind) == TacticalMechanismKind.MaterialGain
      case _ =>
        false

  private def sameCausalRootBoard(
      cause: RelativeCauseFact,
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof
  ): Boolean =
    val rootPosition = cause.comparisonEvidence.position
    (binding.source :: (binding.provenance :+ proof.primitiveSource)).forall(ref =>
      sameCausalRootOccurrence(ref.position, rootPosition)
    )

  private def attributionOwnsEffect(
      cause: RelativeCauseFact,
      proof: RootOwnedEffectProof,
      actor: RootCausalActor
  ): Boolean =
    val required = cause.attribution.kind match
      case CauseAttributionKind.ReferenceCreatesResource | CauseAttributionKind.CandidateCreatesValue =>
        Some(RootOwnedEffectStake.ActorValue)
      case CauseAttributionKind.CandidateAllowsLiability =>
        Some(RootOwnedEffectStake.ActorLiability)
      case CauseAttributionKind.SharedContext | CauseAttributionKind.ContextOnly |
          CauseAttributionKind.Unattributed =>
        None
    required.exists(stake => effectStake(proof, actor).contains(stake))

  private[chessjudgment] def effectStake(
      proof: RootOwnedEffectProof,
      actor: RootCausalActor
  ): Option[RootOwnedEffectStake] =
    proof match
      case RootOwnedEffectProof.LineEpisode(_, _, episode) =>
        episode.consequence.beneficiary.map(beneficiary =>
          if beneficiary == actor.color then RootOwnedEffectStake.ActorValue
          else RootOwnedEffectStake.ActorLiability
        )
      case RootOwnedEffectProof.RootLineEvent(_, _, event) =>
        Some(
          if event.kind == LineEventKind.Tempo then RootOwnedEffectStake.ActorLiability
          else RootOwnedEffectStake.ActorValue
        )
      case RootOwnedEffectProof.EndgameHorizon(_, _, horizon) =>
        horizon.status match
          case LineEndgameTechniqueHorizonStatus.Failed |
              LineEndgameTechniqueHorizonStatus.ContradictedByTerminalProof =>
            Some(RootOwnedEffectStake.ActorLiability)
          case LineEndgameTechniqueHorizonStatus.Active |
              LineEndgameTechniqueHorizonStatus.Transitioned |
              LineEndgameTechniqueHorizonStatus.Completed =>
            Some(RootOwnedEffectStake.ActorValue)
          case LineEndgameTechniqueHorizonStatus.SupersededByTactic =>
            None
      case RootOwnedEffectProof.StructuralTransition(_, _, consequence) =>
        if consequence.kind == TransitionConsequenceKind.PawnTensionResolution then
          Some(RootOwnedEffectStake.ActorValue)
        else if consequence.positive then Some(RootOwnedEffectStake.ActorValue)
        else if consequence.negative then Some(RootOwnedEffectStake.ActorLiability)
        else None
      case RootOwnedEffectProof.RootMoveMotif(_, _) |
          RootOwnedEffectProof.RootRelation(_, _) |
          RootOwnedEffectProof.ThreatCreation(_, _) |
          RootOwnedEffectProof.ThreatDefense(_, _, _) =>
        Some(RootOwnedEffectStake.ActorValue)
      case RootOwnedEffectProof.PlanResult(_, _, assessment, _) =>
        assessment.robustness match
          case PlanCausalRobustness.Refuted => Some(RootOwnedEffectStake.ActorLiability)
          case PlanCausalRobustness.Robust | PlanCausalRobustness.Conditional =>
            Some(RootOwnedEffectStake.ActorValue)
          case PlanCausalRobustness.Untested | PlanCausalRobustness.Deferred |
              PlanCausalRobustness.Superseded =>
            None
      case RootOwnedEffectProof.PlanRestriction(_, _, _, _) =>
        Some(RootOwnedEffectStake.ActorValue)
      case _: RootOwnedEffectProof.DefensiveRecaptureResource =>
        Some(RootOwnedEffectStake.ActorValue)
      case RootOwnedEffectProof.StrategicAxis(primitive, _, _) =>
        effectStake(primitive, actor)

  private[chessjudgment] def strategicAxisStake(
      axis: StrategicAxisDetail
  ): Option[RootOwnedEffectStake] =
    axis.polarity match
      case StrategicAxisPolarity.Gain | StrategicAxisPolarity.Preserve |
          StrategicAxisPolarity.Support | StrategicAxisPolarity.Restrain =>
        Some(RootOwnedEffectStake.ActorValue)
      case StrategicAxisPolarity.Loss | StrategicAxisPolarity.Concede =>
        Some(RootOwnedEffectStake.ActorLiability)
      case StrategicAxisPolarity.Release if axis.kind == StrategicAxisKind.PawnBreak =>
        Some(RootOwnedEffectStake.ActorValue)
      case StrategicAxisPolarity.Release
          if axis.kind == StrategicAxisKind.Target || axis.kind == StrategicAxisKind.Activity =>
        Some(RootOwnedEffectStake.ActorLiability)
      case StrategicAxisPolarity.Release => None

  private[chessjudgment] def strategicAxisChange(
      axis: StrategicAxisDetail
  ): Option[DirectCausalChange] =
    axis.polarity match
      case StrategicAxisPolarity.Gain => Some(DirectCausalChange.Occurred)
      case StrategicAxisPolarity.Restrain => Some(DirectCausalChange.Prevented)
      case StrategicAxisPolarity.Preserve | StrategicAxisPolarity.Support =>
        Some(DirectCausalChange.Maintained)
      case StrategicAxisPolarity.Loss | StrategicAxisPolarity.Concede =>
        Some(DirectCausalChange.Lost)
      case StrategicAxisPolarity.Release if axis.kind == StrategicAxisKind.PawnBreak =>
        Some(DirectCausalChange.Occurred)
      case StrategicAxisPolarity.Release
          if axis.kind == StrategicAxisKind.Target || axis.kind == StrategicAxisKind.Activity =>
        Some(DirectCausalChange.Lost)
      case StrategicAxisPolarity.Release => None

  private def canonicalPlanRestrictionConsequence(
      event: PlanCausalEventEvidence,
      deterrence: OpponentResourceDeterrenceProof,
      graph: TypedEvidenceGraph
  ): Option[TransitionConsequence] =
    val lines = graph.canonicalCandidateLinesFromEvidence
    Option
      .when(
        event.opponentResourceDeterrence.contains(deterrence) &&
          event.opponentResourceDeterrenceProofReady(lines, graph)
      )(deterrence)
      .flatMap(_.consequence(event.perspective, lines, graph))
      .filter(event.structuralConsequences.contains)

  private def strategicAxisOwnsPrimitive(
      primitive: RootOwnedEffectProof,
      axis: StrategicAxisDetail
  ): Boolean =
    primitive match
      case RootOwnedEffectProof.StructuralTransition(_, delta, consequence) =>
        StrategicMechanismEvidence
          .structuralAxesForConsequence(delta, consequence)
          .exists(_.stableKey == axis.stableKey)
      case RootOwnedEffectProof.PlanResult(_, event, assessment, _) =>
        val expectedPolarity =
          if event.exactRobustPublicResultAssessment.contains(assessment) then
            Some(StrategicAxisPolarity.Gain)
          else if event.exactRefutedPublicResultAssessment.contains(assessment) then
            Some(StrategicAxisPolarity.Concede)
          else None
        expectedPolarity.exists(polarity =>
          axis.kind == StrategicAxisKind.PlanCoherence &&
            axis.polarity == polarity &&
            axis.label == event.planId.id
        )
      case RootOwnedEffectProof.PlanRestriction(_, _, _, _) =>
        axis == StrategicAxisDetail(
          StrategicAxisKind.Counterplay,
          StrategicAxisPolarity.Restrain,
          "opponent-resource-deterrence"
        )
      case RootOwnedEffectProof.StrategicAxis(_, _, _) =>
        false
      case _ =>
        false

  private def proofSourceCarried(binding: EvidenceObjectBinding, source: EvidenceRef): Boolean =
    binding.source == source || binding.provenance.contains(source)

  private def primitiveSourceRegistered(
      graph: TypedEvidenceGraph,
      proof: RootOwnedEffectProof
  ): Boolean =
    proof match
      case RootOwnedEffectProof.LineEpisode(source, line, _) =>
        graph.record(source).exists(_.payload == line)
      case RootOwnedEffectProof.RootLineEvent(source, line, _) =>
        graph.record(source).exists(_.payload == line)
      case RootOwnedEffectProof.EndgameHorizon(source, line, _) =>
        graph.record(source).exists(_.payload == line)
      case RootOwnedEffectProof.StructuralTransition(source, delta, _) =>
        graph.record(source).exists(_.payload == delta)
      case RootOwnedEffectProof.RootMoveMotif(source, motif) =>
        graph.record(source).exists(_.payload == motif)
      case RootOwnedEffectProof.RootRelation(source, relation) =>
        graph.record(source).exists(_.payload == relation)
      case RootOwnedEffectProof.ThreatCreation(source, threat) =>
        graph.record(source).exists(_.payload == threat)
      case RootOwnedEffectProof.ThreatDefense(source, threat, _) =>
        graph.record(source).exists(_.payload == threat)
      case RootOwnedEffectProof.PlanResult(source, event, _, _) =>
        graph.record(source).exists(_.payload == event)
      case RootOwnedEffectProof.PlanRestriction(source, event, _, _) =>
        graph.record(source).exists(_.payload == event)
      case RootOwnedEffectProof.DefensiveRecaptureResource(source, comparison, _) =>
        graph.record(source).exists(_.payload == CandidateComparisonEvidence(comparison))
      case RootOwnedEffectProof.StrategicAxis(primitive, _, _) =>
        primitiveSourceRegistered(graph, primitive)

  private def proofOwnsEventRoot(
      proof: RootOwnedEffectProof,
      eventLine: LineNodeRef,
      actor: RootCausalActor,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      binding: EvidenceObjectBinding
  ): Boolean =
    proof match
      case RootOwnedEffectProof.LineEpisode(source, line, episode) =>
        lineRecordOwnsEventRoot(source, line, eventLine) &&
          episode.line == eventLine &&
          episode.actor == actor &&
          admitsLineEpisode(line, episode) &&
          line.rootOwnedCausalEpisodes(eventLine.rootMove).contains(episode)
      case RootOwnedEffectProof.RootLineEvent(source, line, event) =>
        lineRecordOwnsEventRoot(source, line, eventLine) &&
          line.eventsForRootMove(eventLine.rootMove).contains(event) &&
          event.side.forall(_ == actor.color)
      case RootOwnedEffectProof.EndgameHorizon(source, line, horizon) =>
        lineRecordOwnsEventRoot(source, line, eventLine) &&
          horizon.techniqueSide == actor.color
      case RootOwnedEffectProof.StructuralTransition(source, delta, _) =>
        source.line.contains(eventLine) &&
          delta.line.contains(eventLine) &&
          EvidenceRef.sameMove(delta.moveUci, eventLine.rootMove) &&
          delta.perspective == actor.color &&
          PrincipalVariationEvidence.sameBoardState(delta.from.fen, cause.comparisonEvidence.position.fen) &&
          PrincipalVariationEvidence
            .legalFenAfter(delta.from.fen, eventLine.rootMove)
            .exists(PrincipalVariationEvidence.sameBoardState(_, delta.to.fen))
      case RootOwnedEffectProof.RootMoveMotif(source, motif) =>
        motifRecordOwnsEventRoot(source, motif, eventLine)
      case RootOwnedEffectProof.RootRelation(source, relation) =>
        relationRecordOwnsEventRoot(source, relation, eventLine)
      case RootOwnedEffectProof.ThreatCreation(source, threat) =>
        source.line.contains(eventLine) &&
          threat.episode.hasConcreteThreatProof &&
          threat.episode.threatActor == actor.color &&
          rootCreatedThreat(threat, eventLine.rootMove, actor.color)
      case RootOwnedEffectProof.ThreatDefense(source, threat, onlyDefense) =>
        source.line.contains(eventLine) &&
          threat.episode.hasConcreteThreatProof &&
          threat.episode.sideUnderPressure == actor.color &&
          rootDefendsThreat(threat, eventLine.rootMove, onlyDefense)
      case RootOwnedEffectProof.PlanResult(source, event, _, _) =>
        planEventOwnsRoot(source, event, eventLine, actor.color)
      case RootOwnedEffectProof.PlanRestriction(source, event, consequence, deterrence) =>
        source.line.contains(eventLine) &&
          planTransitionRootOwned(event, source, eventLine, actor) &&
          event.opponentResourceDeterrence.contains(deterrence) &&
          canonicalPlanRestrictionConsequence(event, deterrence, graph).contains(consequence)
      case RootOwnedEffectProof.DefensiveRecaptureResource(source, comparison, resource) =>
        val exactCandidateLineRefs = graph.record(source).toList.flatMap(_.parents).flatMap(graph.record).collect {
          case EvidenceRecord(ref, line: LineFactEvidence, _)
              if line.line == comparison.candidateLine => ref
        }
        comparison.kind == CandidateComparisonKind.PlayedVsBest &&
          cause.kind == RelativeCauseKind.DefensiveResource &&
          cause.sourceSide == RelativeCauseSourceSide.Reference &&
          cause.attribution.kind == CauseAttributionKind.ReferenceCreatesResource &&
          comparison.kind == CandidateComparisonKind.PlayedVsBest &&
          comparison.comparison.verdict.isActionableLoss &&
          comparison.referenceLine == eventLine &&
          EvidenceRef.sameMove(comparison.referenceLine.rootMove, actor.moveUci) &&
          graph.comparisonFor(cause).contains(comparison) &&
          exactCandidateLineRefs.size == 1 &&
          binding.provenance == exactCandidateLineRefs &&
          graph.record(exactCandidateLineRefs.head).exists {
            case EvidenceRecord(_, line: LineFactEvidence, _) =>
              PlayedVsBestDefensiveRecaptureResource.proves(
                  comparison,
                  cause.comparisonEvidence.position,
                  line,
                  resource
                )
            case _ => false
          }
      case RootOwnedEffectProof.StrategicAxis(primitive, _, _) =>
        proofOwnsEventRoot(primitive, eventLine, actor, cause, graph, binding)

  private def planTransitionRootOwned(
      event: PlanCausalEventEvidence,
      source: EvidenceRef,
      eventLine: LineNodeRef,
      actor: RootCausalActor
  ): Boolean =
    planEventOwnsRoot(source, event, eventLine, actor.color)

  /** Exact root ownership shared by C proof transport and final root-owned
    * effect certification. This verifies only the actor/root relation; the
    * Cause-specific result (robust, refuted, or restriction) remains a
    * separate typed proof obligation.
    */
  private[chessjudgment] def planEventOwnsRoot(
      source: EvidenceRef,
      event: PlanCausalEventEvidence,
      eventLine: LineNodeRef,
      mover: Color
  ): Boolean =
    source.line.contains(eventLine) &&
      event.rootLine == eventLine &&
      EvidenceRef.sameMove(event.rootMove, eventLine.rootMove) &&
      event.perspective == mover &&
      sameCausalRootOccurrence(source.position, event.rootTransition.from) &&
      PrincipalVariationEvidence
        .legalFenAfter(source.position.fen, eventLine.rootMove)
        .exists(PrincipalVariationEvidence.sameBoardState(_, event.rootTransition.to.fen))

  private def expectedDirectChange(
      cause: RelativeCauseFact,
      proof: RootOwnedEffectProof,
      inheritedAxis: Option[StrategicAxisDetail] = None
  ): Option[DirectCausalChange] =
    proof match
      case RootOwnedEffectProof.LineEpisode(_, line, episode) =>
        Option
          .when(
            admitsLineEpisode(line, episode) &&
              RelativeCauseKind.acceptsDirectLineConsequence(
                cause.kind,
                line,
                line.line.rootMove,
                episode.consequence
              ) && lineBeneficiaryCompatible(cause, episode.consequence, episode.actor.color)
          )(episode)
          .flatMap(owned => lineConsequenceChange(cause, owned.consequence, owned.actor.color))
      case RootOwnedEffectProof.RootLineEvent(_, _, event) =>
        Option
          .when(rootLocalEventAccepted(cause.kind, event.kind))(event.kind)
          .flatMap(lineEventChange(cause, _))
      case RootOwnedEffectProof.EndgameHorizon(_, line, horizon) =>
        Option
          .when(
            line
              .endgameTechniquesTriggeredByRootMove(line.line.rootMove, cause.kind)
              .contains(horizon)
          )(horizon.status)
          .flatMap(endgameHorizonChange(cause, _))
      case RootOwnedEffectProof.StructuralTransition(_, delta, consequence) =>
        Option
          .when(structuralConsequenceCompatible(cause, delta, consequence, inheritedAxis))(
            consequence
          )
          .map(transitionConsequenceChange(cause, _))
      case RootOwnedEffectProof.RootMoveMotif(_, motif) =>
        Option
          .when(moveMotifCanProjectCause(motif, cause.kind))(motif)
          .flatMap(rootMoveMotifChange)
          .map(normalizeCauseChange(cause, _))
      case RootOwnedEffectProof.RootRelation(_, relation) =>
        Option.when(relationCanProjectCause(relation, cause.kind))(relation.kind).flatMap(relationChange(cause, _))
      case RootOwnedEffectProof.ThreatCreation(_, threat) =>
        Option.when(threatCreationCauseCompatible(cause.kind, threat))(
          normalizeCauseChange(cause, DirectCausalChange.Occurred)
        )
      case RootOwnedEffectProof.ThreatDefense(_, _, onlyDefense) =>
        Option.when(threatDefenseCauseCompatible(cause.kind, onlyDefense))(
          normalizeCauseChange(cause, DirectCausalChange.Prevented)
        )
      case RootOwnedEffectProof.PlanResult(_, event, assessment, _) =>
        val exactAssessment =
          if cause.kind == RelativeCauseKind.WrongMoveOrder then
            event.exactRobustPublicResultAssessment
          else exactPlanAssessment(cause.kind, event)
        exactAssessment
          .filter(_ == assessment)
          .flatMap { exact =>
            cause.kind match
              case RelativeCauseKind.PlanImprovement | RelativeCauseKind.WrongMoveOrder =>
                Some(transitionConsequenceChange(cause, exact.consequence))
              case RelativeCauseKind.PlanContradiction => Some(DirectCausalChange.Refuted)
              case _ => None
          }
      case RootOwnedEffectProof.PlanRestriction(_, _, consequence, _) =>
        Option.when(planRestrictionCompatible(cause, consequence))(
          DirectCausalChange.Prevented
        )
      case _: RootOwnedEffectProof.DefensiveRecaptureResource =>
        Option.when(cause.kind == RelativeCauseKind.DefensiveResource)(DirectCausalChange.Occurred)
      case RootOwnedEffectProof.StrategicAxis(primitive, axis, outcome) =>
        val sameAxis = inheritedAxis.forall(existing =>
          existing.kind == axis.kind && existing.polarity == axis.polarity
        )
        for
          _ <- Option.when(strategicAxisOwnsPrimitive(primitive, axis)) { () }
          primitiveChange <- expectedDirectChange(cause, primitive, Some(axis))
          if RelativeCauseKind.strategicAxisCanProveCause(cause.kind, axis, cause.sourceSide)
          expectedAxisChange <- Option.when(sameAxis)(axisChange(cause, axis)).flatten
          if primitiveChange == expectedAxisChange
          if outcome.forall(strategicComparisonOutcomeCompatible(_, cause.sourceSide, primitiveChange))
        yield primitiveChange

  private def exactPlanAssessment(
      kind: RelativeCauseKind,
      event: PlanCausalEventEvidence
  ): Option[PlanCausalResultAssessment] =
    kind match
      case RelativeCauseKind.PlanImprovement => event.exactRobustPublicResultAssessment
      case RelativeCauseKind.PlanContradiction => event.exactRefutedPublicResultAssessment
      case _ => None

  private def structuralConsequenceCompatible(
      cause: RelativeCauseFact,
      delta: StructuralDeltaEvidence,
      consequence: TransitionConsequence,
      axis: Option[StrategicAxisDetail]
  ): Boolean =
    consequence.strength > 0 &&
      delta.consequences.contains(consequence) &&
      RelativeCauseKind.structuralConsequences(cause.kind, delta, axis).contains(consequence)

  private def planRestrictionCompatible(
      cause: RelativeCauseFact,
      consequence: TransitionConsequence
  ): Boolean =
    cause.kind == RelativeCauseKind.OpponentRestriction &&
      consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction &&
      consequence.strength > 0 &&
      consequence.subjects.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)

  private def exactRootActor(binding: EvidenceObjectBinding, actor: RootCausalActor): Boolean =
    val expected = Set(
      EvidenceObjectKind.Move -> actor.moveUci,
      EvidenceObjectKind.Side -> (if actor.color.white then "white" else "black"),
      EvidenceObjectKind.Piece -> actor.role.name,
      EvidenceObjectKind.Square -> actor.from.key,
      EvidenceObjectKind.Square -> actor.to.key
    ).map { case (kind, key) => s"$kind:${key.trim.toLowerCase}" }
    binding.actor.map(_.signaturePart).toSet == expected

  private def specificTargetReady(targets: List[ConcreteChessObject]): Boolean =
    targets.exists(target =>
      target.key.trim.nonEmpty &&
        Set(EvidenceObjectKind.Square, EvidenceObjectKind.File, EvidenceObjectKind.Pawn)(target.kind)
    )

  private def lineBeneficiaryCompatible(
      cause: RelativeCauseFact,
      consequence: LineConsequence,
      actor: Color
  ): Boolean =
    val expected = cause.attribution.kind match
      case CauseAttributionKind.ReferenceCreatesResource | CauseAttributionKind.CandidateCreatesValue => Some(actor)
      case CauseAttributionKind.CandidateAllowsLiability => Some(!actor)
      case CauseAttributionKind.SharedContext | CauseAttributionKind.ContextOnly |
          CauseAttributionKind.Unattributed => None
    expected.exists(expectedBeneficiary => consequence.beneficiary.contains(expectedBeneficiary)) &&
      consequence.kind != LineConsequenceKind.ForcedTheme &&
      consequence.kind != LineConsequenceKind.Sacrifice

  private def rootCreatedThreat(
      threat: ThreatEpisodeEvidence,
      rootMove: String,
      actor: Color
  ): Boolean =
    threat.episode.motifs.exists(motif =>
      motif.plyIndex == 0 &&
        motif.color == actor &&
        motif.move.exists(EvidenceRef.sameMove(_, rootMove))
    )

  private def threatCreationCauseCompatible(
      kind: RelativeCauseKind,
      threat: ThreatEpisodeEvidence
  ): Boolean =
    kind match
      case RelativeCauseKind.KingForcing => threat.episode.kind == ThreatKind.Mate
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability => true
      case _ => false

  private def threatDefenseCauseCompatible(
      kind: RelativeCauseKind,
      onlyDefense: Boolean
  ): Boolean =
    kind match
      case RelativeCauseKind.OnlyDefenseNecessity =>
        onlyDefense
      case RelativeCauseKind.DefensiveResource =>
        !onlyDefense
      case _ => false

  private def rootDefendsThreat(
      threat: ThreatEpisodeEvidence,
      rootMove: String,
      onlyDefense: Boolean
  ): Boolean =
    if onlyDefense then threat.onlyDefense.exists(EvidenceRef.sameMove(_, rootMove))
    else threat.episode.bestDefense.exists(EvidenceRef.sameMove(_, rootMove))

  private def moveMotifCanProjectCause(
      payload: MoveMotifEvidence,
      kind: RelativeCauseKind
  ): Boolean =
    val mechanismKinds = TacticalMechanismKind.fromMotif(payload.motif).toSet
    kind match
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability => mechanismKinds.nonEmpty
      case RelativeCauseKind.KingForcing => mechanismKinds(TacticalMechanismKind.KingForcing)
      case RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss =>
        mechanismKinds(TacticalMechanismKind.Tempo)
      case RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow =>
        mechanismKinds(TacticalMechanismKind.RecaptureChoice)
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        mechanismKinds(TacticalMechanismKind.Conversion)
      case RelativeCauseKind.DrawResource => mechanismKinds(TacticalMechanismKind.DrawResource)
      case RelativeCauseKind.MaterialSwing => mechanismKinds(TacticalMechanismKind.MaterialGain)
      case _ => false

  private def relationCanProjectCause(
      payload: RelationFactEvidence,
      kind: RelativeCauseKind
  ): Boolean =
    kind match
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability => true
      case RelativeCauseKind.WrongMoveOrder => payload.kind == RelationFactKind.Zwischenzug
      case RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow =>
        payload.kind == RelationFactKind.DefenderTrade || payload.kind == RelationFactKind.Zwischenzug
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        payload.kind == RelationFactKind.BadPieceLiquidation
      case RelativeCauseKind.KingForcing =>
        Set(
          RelationFactKind.DoubleCheck,
          RelationFactKind.BackRankMate,
          RelationFactKind.MateNet,
          RelationFactKind.GreekGift
        )(payload.kind)
      case RelativeCauseKind.MaterialSwing =>
        Set(
          RelationFactKind.HangingPiece,
          RelationFactKind.TrappedPiece,
          RelationFactKind.Domination
        )(payload.kind)
      case _ => false

  private def rootLocalEventAccepted(
      causeKind: RelativeCauseKind,
      eventKind: LineEventKind
  ): Boolean =
    causeKind match
      case RelativeCauseKind.WrongMoveOrder => eventKind == LineEventKind.Tempo
      case RelativeCauseKind.KingForcing =>
        eventKind == LineEventKind.Check || eventKind == LineEventKind.Mate
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability =>
        eventKind == LineEventKind.Capture ||
          eventKind == LineEventKind.Recapture ||
          eventKind == LineEventKind.Check ||
          eventKind == LineEventKind.Mate ||
          eventKind == LineEventKind.Promotion
      case _ => false

  private def normalizeCauseChange(
      cause: RelativeCauseFact,
      raw: DirectCausalChange
  ): DirectCausalChange =
    if cause.kind == RelativeCauseKind.ConversionMiss &&
      cause.sourceSide == RelativeCauseSourceSide.Candidate &&
      cause.attribution.kind == CauseAttributionKind.CandidateAllowsLiability
    then DirectCausalChange.Missed
    else raw

  private def lineConsequenceChange(
      cause: RelativeCauseFact,
      consequence: LineConsequence,
      actor: Color
  ): Option[DirectCausalChange] =
    val raw = consequence.kind match
      case LineConsequenceKind.MaterialLoss => Some(DirectCausalChange.Lost)
      case LineConsequenceKind.MaterialGain =>
        consequence.beneficiary.map(beneficiary =>
          if beneficiary == actor then DirectCausalChange.Occurred else DirectCausalChange.Lost
        )
      case LineConsequenceKind.ImmediateReplyCheck | LineConsequenceKind.Mate |
          LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow |
          LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
        Some(DirectCausalChange.Occurred)
      case LineConsequenceKind.DrawResource => Some(DirectCausalChange.Maintained)
      case LineConsequenceKind.ForcedTheme | LineConsequenceKind.Sacrifice => None
    raw.map(normalizeCauseChange(cause, _))

  private def lineEventChange(
      cause: RelativeCauseFact,
      kind: LineEventKind
  ): Option[DirectCausalChange] =
    val raw = kind match
      case LineEventKind.Capture | LineEventKind.Recapture | LineEventKind.Check |
          LineEventKind.Mate | LineEventKind.Promotion | LineEventKind.Tempo |
          LineEventKind.DefenderMove => Some(DirectCausalChange.Occurred)
      case _ => None
    raw.map(normalizeCauseChange(cause, _))

  private def endgameHorizonChange(
      cause: RelativeCauseFact,
      status: LineEndgameTechniqueHorizonStatus
  ): Option[DirectCausalChange] =
    val raw = status match
      case LineEndgameTechniqueHorizonStatus.Active => Some(DirectCausalChange.Maintained)
      case LineEndgameTechniqueHorizonStatus.Transitioned | LineEndgameTechniqueHorizonStatus.Completed =>
        Some(DirectCausalChange.Occurred)
      case LineEndgameTechniqueHorizonStatus.Failed => Some(DirectCausalChange.Lost)
      case LineEndgameTechniqueHorizonStatus.ContradictedByTerminalProof =>
        Some(DirectCausalChange.Refuted)
      case LineEndgameTechniqueHorizonStatus.SupersededByTactic => None
    raw.map(normalizeCauseChange(cause, _))

  private[chessjudgment] def rootMoveMotifChange(
      payload: MoveMotifEvidence
  ): Option[DirectCausalChange] =
    val mechanisms = TacticalMechanismKind.fromMotif(payload.motif).toSet
    if mechanisms(TacticalMechanismKind.DrawResource) then Some(DirectCausalChange.Maintained)
    else Option.when(mechanisms.nonEmpty)(DirectCausalChange.Occurred)

  private def relationChange(
      cause: RelativeCauseFact,
      kind: RelationFactKind
  ): Option[DirectCausalChange] =
    val raw = kind match
      case RelationFactKind.StalemateTrap | RelationFactKind.PerpetualCheck => DirectCausalChange.Maintained
      case _ => DirectCausalChange.Occurred
    Some(normalizeCauseChange(cause, raw))

  private def transitionConsequenceChange(
      cause: RelativeCauseFact,
      consequence: TransitionConsequence
  ): DirectCausalChange =
    val raw = consequence.kind match
      case TransitionConsequenceKind.OpponentMobilityRestriction => DirectCausalChange.Prevented
      case TransitionConsequenceKind.PawnTensionResolution => DirectCausalChange.Occurred
      case TransitionConsequenceKind.TargetPressureRelease => DirectCausalChange.Lost
      case _ if consequence.negative => DirectCausalChange.Lost
      case _ => DirectCausalChange.Occurred
    normalizeCauseChange(cause, raw)

  private def axisChange(
      cause: RelativeCauseFact,
      axis: StrategicAxisDetail
  ): Option[DirectCausalChange] =
    strategicAxisChange(axis).map(normalizeCauseChange(cause, _))

  private def strategicComparisonOutcomeCompatible(
      outcome: StrategicAxisComparisonOutcome,
      sourceSide: RelativeCauseSourceSide,
      change: DirectCausalChange
  ): Boolean =
    val resourceChange = Set(
      DirectCausalChange.Occurred,
      DirectCausalChange.Prevented,
      DirectCausalChange.Maintained
    )(change)
    outcome match
      case StrategicAxisComparisonOutcome.ReferenceOnly |
          StrategicAxisComparisonOutcome.ReferenceStronger =>
        sourceSide == RelativeCauseSourceSide.Reference && resourceChange
      case StrategicAxisComparisonOutcome.CandidateOnly |
          StrategicAxisComparisonOutcome.CandidateStronger =>
        sourceSide == RelativeCauseSourceSide.Candidate && resourceChange
      case StrategicAxisComparisonOutcome.SharedSustained =>
        change == DirectCausalChange.Maintained
      case StrategicAxisComparisonOutcome.CandidateConcession =>
        sourceSide == RelativeCauseSourceSide.Candidate && change == DirectCausalChange.Lost
      case StrategicAxisComparisonOutcome.ReferencePreservesPlan =>
        sourceSide == RelativeCauseSourceSide.Reference && change == DirectCausalChange.Maintained
