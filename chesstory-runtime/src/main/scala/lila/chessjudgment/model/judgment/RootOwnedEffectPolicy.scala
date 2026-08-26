package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.model.line.PrincipalVariationEvidence
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
      case _: RootOwnedEffectProof.RootRelation =>
        IdentityParts(RootOwnedEffectPrimitiveKind.RootRelation)
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
              capture <- line.uniqueMaterialCaptureFor(episode)
              if capture.side == beneficiary
              if outcome.event.matches(capture)
              if outcome.durableNetCp > 0
            yield DirectCauseImportanceMeasure.MaterialOutcome(
              outcome.durableNetCp,
              outcome.event.plyOffset
            )).map(DirectEffectMagnitudeKnowledge.Exact.apply)
              .getOrElse(DirectEffectMagnitudeKnowledge.ExpectedButMissing)
          case _ => DirectEffectMagnitudeKnowledge.NotApplicable
      case RootOwnedEffectProof.PlanResult(_, _, assessment, _) =>
        expectedMagnitude(structuralMagnitude(assessment.consequence))
      case RootOwnedEffectProof.PlanRestriction(_, _, consequence, _) =>
        expectedMagnitude(structuralMagnitude(consequence))
      case _: RootOwnedEffectProof.DefensiveRecaptureResource =>
        DirectEffectMagnitudeKnowledge.NotApplicable
      case RootOwnedEffectProof.StrategicAxis(primitive, _, _) =>
        magnitudeKnowledge(primitive, binding, materialOutcome)
      case _: RootOwnedEffectProof.RootLineEvent | _: RootOwnedEffectProof.RootRelation =>
        DirectEffectMagnitudeKnowledge.NotApplicable

  private def expectedMagnitude(
      magnitude: Option[DirectCauseImportanceMeasure]
  ): DirectEffectMagnitudeKnowledge =
    magnitude
      .map(DirectEffectMagnitudeKnowledge.Exact.apply)
      .getOrElse(DirectEffectMagnitudeKnowledge.ExpectedButMissing)

  private def rootOwnedMaterialOutcome(
      proof: RootOwnedEffectProof
  ): Option[RootOwnedMaterialOutcome] =
    proof match
      case RootOwnedEffectProof.LineEpisode(_, _, episode) =>
        episode.consequence.materialOutcome
      case RootOwnedEffectProof.StrategicAxis(primitive, _, _) =>
        rootOwnedMaterialOutcome(primitive)
      case _ => None

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
    exactReplayPrefix &&
      causalPathStartsAtRoot(episode, rootMove)

  private def causalPathStartsAtRoot(
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
        case RootCausalLinkKind.ContinuousLineAccess |
            RootCausalLinkKind.ForcedCaptureResponse |
            RootCausalLinkKind.ForcedCheckResponse |
            RootCausalLinkKind.RootActorCaptured =>
          rootSeed &&
            occursBefore(link.causeMove, link.effectMove) &&
            reachesEvent(
              link.effectMove,
              Set(EvidenceRef.normalizeMove(link.effectMove))
            )
        case RootCausalLinkKind.RootActorContinuation |
            RootCausalLinkKind.MaterialActorContinuation |
            RootCausalLinkKind.MaterialCaptureResponse =>
          false
    }

  def planResultProofs(
      cause: RelativeCauseFact,
      source: EvidenceRef,
      event: PlanCausalEventEvidence
  ): List[(PlanCausalResultAssessment, RootOwnedEffectProof)] =
    exactPlanAssessments(cause.kind, event).map(assessment =>
      assessment -> RootOwnedEffectProof.PlanResult(source, event, assessment)
    )

  def planRestrictionProofs(
      source: EvidenceRef,
      event: PlanCausalEventEvidence
  ): List[(TransitionConsequence, RootOwnedEffectProof)] =
    event.opponentResourceDeterrence.toList.flatMap { deterrence =>
      canonicalPlanRestrictionConsequence(event, deterrence).toList.map(consequence =>
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
      primitiveCausalSignature: Option[String] = None,
      selectedProofSegment: Option[DirectCauseProofSegment] = None
  ): List[RootOwnedEffect] =
    expectedDirectChange(cause, proof).toList.flatMap { change =>
      val availableSegments = DirectCauseProofSegment.allFrom(proof)
      val exactOccurrences: List[Option[DirectCauseProofSegment]] =
        selectedProofSegment match
          case Some(selected) if availableSegments.contains(selected) => List(Some(selected))
          case Some(_)                                                => Nil
          case None if availableSegments.nonEmpty                     => availableSegments.map(Some(_))
          case None                                                   => List(None)
      exactOccurrences
        .map(segment =>
          DirectCauseChannel(
            binding = binding,
            directChange = change,
            primitiveCausalSignature = primitiveCausalSignature,
            rootOwnedProof = Some(proof),
            proofSegmentOccurrence = segment
          )
        )
        .filter(admits(cause, graph, _))
    }

  def admits(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      effect: RootOwnedEffect
  ): Boolean =
    effect.rootOwnedProof.exists { proof =>
      graph.relativeCauseBinding(cause).exists { causeBinding =>
        val eventLine = causeBinding.eventLine
        graph
          .certifiedRootActorFor(eventLine)
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
      outcome: Option[StrategicAxisComparisonOutcome],
      authorizedPlanResults: List[StrategicAxisPlanResultBinding]
  ): Option[RootOwnedEffectProof] =
    val exactPrimitive = primitive.primitiveProof
    if !strategicPrimitiveAuthorized(exactPrimitive, axis, authorizedPlanResults) then None
    else primitive match
      case RootOwnedEffectProof.StrategicAxis(inner, existingAxis, existingOutcome)
          if existingAxis.stableKey == axis.stableKey =>
        val exactOutcome: Option[Option[StrategicAxisComparisonOutcome]] =
          (existingOutcome, outcome) match
            case (Some(existing), Some(requested)) if existing != requested => None
            case (Some(existing), _)                                        => Some(Some(existing))
            case (None, requested)                                         => Some(requested)
        exactOutcome.map(resolved =>
            RootOwnedEffectProof.StrategicAxis(
              inner,
              existingAxis,
              resolved
            )
          )
      case RootOwnedEffectProof.StrategicAxis(_, _, _) =>
        None
      case _ =>
        Some(RootOwnedEffectProof.StrategicAxis(primitive, axis, outcome))

  /** A strategic wrapper owns one primitive only through its exact typed
    * producer/result binding. Empty result authority is valid solely for
    * non-PlanResult primitives such as an exact opponent-resource restriction.
    */
  private[chessjudgment] def strategicPrimitiveAuthorized(
      primitive: RootOwnedEffectProof,
      axis: StrategicAxisDetail,
      authorizedPlanResults: List[StrategicAxisPlanResultBinding]
  ): Boolean =
    val exactPrimitive = primitive.primitiveProof
    val exactResultAuthority = exactPrimitive match
      case RootOwnedEffectProof.PlanResult(source, _, assessment, selectedInducedResponse) =>
        selectedInducedResponse.isEmpty &&
          authorizedPlanResults.contains(StrategicAxisPlanResultBinding(source, assessment))
      case _ =>
        authorizedPlanResults.isEmpty
    exactResultAuthority && strategicAxisOwnsPrimitive(exactPrimitive, axis)

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
          .toList
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
            exactPlanAssessments(cause.kind, event).contains(assessment)
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

  def relationRecordOwnsEventRoot(
      graph: TypedEvidenceGraph,
      source: EvidenceRef,
      relation: RelationFactEvidence,
      eventLine: LineNodeRef
  ): Boolean =
    source.line.contains(eventLine) &&
      graph.certifiedRootActorFor(eventLine).nonEmpty &&
      graph.record(source).exists(record => record.payload == relation && graph.relationProofEligible(record)) &&
      relation.mentionsLineMove(eventLine.rootMove) &&
      relation.rootGeometryConnected(eventLine.rootMove)

  def tacticalCarrierOwnsEventRoot(
      graph: TypedEvidenceGraph,
      source: EvidenceRef,
      mechanism: TacticalMechanismEvidence,
      eventLine: LineNodeRef
  ): Boolean =
    source.line.contains(eventLine) &&
      graph.certifiedRootActorFor(eventLine).nonEmpty &&
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
      case RootOwnedEffectProof.RootRelation(_, _) =>
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
      case StrategicAxisPolarity.Gain | StrategicAxisPolarity.Support |
          StrategicAxisPolarity.Restrain =>
        Some(RootOwnedEffectStake.ActorValue)
      case StrategicAxisPolarity.Concede =>
        Some(RootOwnedEffectStake.ActorLiability)

  private[chessjudgment] def strategicAxisChange(
      axis: StrategicAxisDetail
  ): Option[DirectCausalChange] =
    axis.polarity match
      case StrategicAxisPolarity.Gain => Some(DirectCausalChange.Occurred)
      case StrategicAxisPolarity.Restrain => Some(DirectCausalChange.Prevented)
      case StrategicAxisPolarity.Support =>
        Some(DirectCausalChange.Maintained)
      case StrategicAxisPolarity.Concede =>
        Some(DirectCausalChange.Lost)

  private def canonicalPlanRestrictionConsequence(
      event: PlanCausalEventEvidence,
      deterrence: OpponentResourceDeterrenceProof
  ): Option[TransitionConsequence] =
    Option
      .when(
        event.opponentResourceDeterrence.contains(deterrence) &&
          event.opponentResourceDeterrenceProofReady
      )(deterrence)
      .flatMap(_.consequence)
      .filter(event.structuralConsequences.contains)

  private def strategicAxisOwnsPrimitive(
      primitive: RootOwnedEffectProof,
      axis: StrategicAxisDetail
  ): Boolean =
    primitive match
      case RootOwnedEffectProof.PlanResult(_, event, assessment, _) =>
        val expectedPolarity =
          if event.exactRobustPublicResultAssessments.contains(assessment) then
            Some(StrategicAxisPolarity.Gain)
          else if event.exactRefutedPublicResultAssessments.contains(assessment) then
            Some(StrategicAxisPolarity.Concede)
          else if
            assessment.robustness == PlanCausalRobustness.Conditional &&
              event.positiveCausalResultAssessments.contains(assessment)
          then Some(StrategicAxisPolarity.Support)
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
    def eligiblePayload(source: EvidenceRef, payload: EvidencePayload): Boolean =
      graph.record(source).exists(record => graph.proofEligible(record) && record.payload == payload)

    proof match
      case RootOwnedEffectProof.LineEpisode(source, line, _) =>
        eligiblePayload(source, line)
      case RootOwnedEffectProof.RootLineEvent(source, line, _) =>
        eligiblePayload(source, line)
      case RootOwnedEffectProof.RootRelation(source, relation) =>
        eligiblePayload(source, relation)
      case RootOwnedEffectProof.PlanResult(source, event, _, _) =>
        eligiblePayload(source, event)
      case RootOwnedEffectProof.PlanRestriction(source, event, _, _) =>
        eligiblePayload(source, event)
      case RootOwnedEffectProof.DefensiveRecaptureResource(source, comparison, _) =>
        eligiblePayload(source, CandidateComparisonEvidence(comparison))
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
      case RootOwnedEffectProof.RootRelation(source, relation) =>
        relationRecordOwnsEventRoot(graph, source, relation, eventLine)
      case RootOwnedEffectProof.PlanResult(source, event, _, _) =>
        planEventOwnsRoot(source, event, eventLine, actor.color)
      case RootOwnedEffectProof.PlanRestriction(source, event, consequence, deterrence) =>
        source.line.contains(eventLine) &&
          planTransitionRootOwned(event, source, eventLine, actor) &&
          event.opponentResourceDeterrence.contains(deterrence) &&
          canonicalPlanRestrictionConsequence(event, deterrence).contains(consequence)
      case RootOwnedEffectProof.DefensiveRecaptureResource(source, comparison, resource) =>
        val parentLineRecords = graph.record(source).toList.flatMap(_.parents).flatMap(graph.record).collect {
          case EvidenceRecord(ref, line: LineFactEvidence, _)
              if line.line == comparison.candidateLine || line.line == comparison.referenceLine => ref -> line
        }
        val exactCandidateLines = parentLineRecords.filter(_._2.line == comparison.candidateLine)
        val exactReferenceLines = parentLineRecords.filter(_._2.line == comparison.referenceLine)
        comparison.kind == CandidateComparisonKind.PlayedVsBest &&
          cause.kind == RelativeCauseKind.DefensiveResource &&
          cause.sourceSide == RelativeCauseSourceSide.Reference &&
          cause.attribution.kind == CauseAttributionKind.ReferenceCreatesResource &&
          comparison.kind == CandidateComparisonKind.PlayedVsBest &&
          comparison.comparison.verdict.isActionableLoss &&
          comparison.referenceLine == eventLine &&
          EvidenceRef.sameMove(comparison.referenceLine.rootMove, actor.moveUci) &&
          graph.comparisonFor(cause).contains(comparison) &&
          exactCandidateLines.size == 1 &&
          exactReferenceLines.size == 1 &&
          binding.provenance == exactCandidateLines.map(_._1) &&
          PlayedVsBestDefensiveRecaptureResource.proves(
            comparison,
            cause.comparisonEvidence.position,
            exactCandidateLines.head._2,
            exactReferenceLines.head._2,
            resource
          )
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
      event.rootTransitionIsCertified

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
      case RootOwnedEffectProof.RootRelation(_, relation) =>
        Option
          .when(relationDirectlyProvesCause(relation, cause.kind))(
            normalizeCauseChange(cause, DirectCausalChange.Occurred)
          )
      case RootOwnedEffectProof.PlanResult(_, event, assessment, _) =>
        val exactAssessment =
          if cause.kind == RelativeCauseKind.WrongMoveOrder then
            event.exactRobustPublicResultAssessments
          else exactPlanAssessments(cause.kind, event)
        exactAssessment
          .find(_ == assessment)
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

  private def exactPlanAssessments(
      kind: RelativeCauseKind,
      event: PlanCausalEventEvidence
  ): List[PlanCausalResultAssessment] =
    kind match
      case RelativeCauseKind.PlanImprovement => event.exactRobustPublicResultAssessments
      case RelativeCauseKind.PlanContradiction => event.exactRefutedPublicResultAssessments
      case _ => Nil

  private def planRestrictionCompatible(
      cause: RelativeCauseFact,
      consequence: TransitionConsequence
  ): Boolean =
    cause.kind == RelativeCauseKind.OpponentRestriction &&
      consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction &&
      consequence.strength > 0 &&
      consequence.subjectFacts.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)

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

  private[chessjudgment] def relationDirectlyProvesCause(
      payload: RelationFactEvidence,
      kind: RelativeCauseKind
  ): Boolean =
    kind match
      case RelativeCauseKind.MissedTacticalResource =>
        payload.kind == RelationFactKind.DoubleCheck
      case RelativeCauseKind.KingForcing =>
        payload.kind == RelationFactKind.DoubleCheck ||
          payload.kind == RelationFactKind.CheckingEnemyControlBundle
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

  private def transitionConsequenceChange(
      cause: RelativeCauseFact,
      consequence: TransitionConsequence
  ): DirectCausalChange =
    val raw = consequence.kind match
      case TransitionConsequenceKind.OpponentMobilityRestriction => DirectCausalChange.Prevented
      case TransitionConsequenceKind.PawnTensionResolution => DirectCausalChange.Occurred
      case _ if consequence.removesState => DirectCausalChange.Lost
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
      case StrategicAxisComparisonOutcome.ReferenceOnly =>
        sourceSide == RelativeCauseSourceSide.Reference && resourceChange
      case StrategicAxisComparisonOutcome.CandidateOnly =>
        sourceSide == RelativeCauseSourceSide.Candidate && resourceChange
      case StrategicAxisComparisonOutcome.SharedSustained =>
        change == DirectCausalChange.Maintained
      case StrategicAxisComparisonOutcome.CandidateConcession =>
        sourceSide == RelativeCauseSourceSide.Candidate && change == DirectCausalChange.Lost
