package lila.chessjudgment.model.judgment

import chess.*
import lila.chessjudgment.model.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.position.{ PawnTopology, PositionFeatures }
import lila.chessjudgment.model.{ ActivePlans, BranchReplyProbeBinding, Fact, Motif, MotifCategory, PlanEventIdentity, PlanId, PlanMatch, PlanScoringResult, PlanSequenceSummary, TransitionType }
import lila.chessjudgment.model.structure.{ PlanAlignment, StructureId, StructureProfile }
import lila.chessjudgment.model.strategic.{ EngineLine, PlanContinuity }
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }
private[chessjudgment] object ComparisonEndpointEffectObservationPolicy:

  enum MagnitudeRelation:
    case Equal
    case LeftStrictlyStronger
    case RightStrictlyStronger
    case Incomparable

  /** Comparison-only projection for exact PlanResults. Ownership keeps the
    * complete descriptor (including endpoint-local moves and causal route),
    * while a Played-vs-Best comparison matches the endpoint-neutral result
    * semantics here and compares strength only as magnitude.
    */
  private[chessjudgment] final case class PlanResultComparisonKey(
      rootBoardState: String,
      mover: ComparisonEndpointMoverIdentity,
      consequenceKind: TransitionConsequenceKind,
      polarity: StructuralSignalPolarity,
      goalTargetSubjects: List[String],
      robustness: PlanCausalRobustness,
      branchSemantics: List[(
          PlanCausalBranchOutcome,
          Int,
          Option[Int],
          Option[PlanCausalTerminalOutcome],
          Option[Int]
      )],
      causalRouteSemantics: List[(PlanCausalDependencyKind, String, Int, List[String])],
      mechanismSignatures: List[String],
      consequenceSignatures: List[String],
      horizon: Option[String],
      directChange: DirectCausalChange,
      stake: RootOwnedEffectStake
  )

  /** Common fail-closed membership gate for every retained and differential
    * admission branch. Membership is necessary but never sufficient: it
    * cannot itself admit a channel.
    */
  def uniqueNeutralWitnessFor(
      snapshot: ComparisonEndpointEvidenceSnapshot,
      sourceSide: RelativeCauseSourceSide,
      channel: DirectCauseChannel,
      graph: TypedEvidenceGraph
  ): Option[ComparisonEndpointEvidenceWitness] =
    val expectedLine = sourceSide match
      case RelativeCauseSourceSide.Reference => Some(snapshot.comparison.referenceLine)
      case RelativeCauseSourceSide.Candidate => Some(snapshot.comparison.candidateLine)
      case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed => None
    snapshot
      .forSide(sourceSide)
      .toList
      .filter(side =>
        side.sourceSide == sourceSide &&
          expectedLine.exists(line => sameSemanticLine(Some(side.line), Some(line)))
      )
      .flatMap(side =>
        side.witnesses.filter(witness =>
          witness.sourceSide == sourceSide &&
            sameSemanticLine(Some(side.line), Some(witness.line))
        )
      )
      .filter(neutralWitnessMatchesChannel(_, channel, graph)) match
        case exact :: Nil => Some(exact)
        case _            => None

  private def neutralWitnessMatchesChannel(
      witness: ComparisonEndpointEvidenceWitness,
      channel: DirectCauseChannel,
      graph: TypedEvidenceGraph
  ): Boolean =
    channel.rootOwnedProof.exists { proof =>
      val binding = channel.binding
      proof == witness.rootOwnedProof &&
        proof.primitiveSource == witness.primitiveProofSource &&
        carrierAndProvenanceMatch(witness, channel, graph) &&
        sameSemanticLine(binding.line, Some(witness.line)) &&
        sameObjects(binding.actor, witness.binding.actor) &&
        sameObjects(binding.target, witness.binding.target) &&
        sameObjects(binding.mechanism, witness.binding.mechanism) &&
        sameObjects(binding.consequence, witness.binding.consequence) &&
        sameObjects(binding.witness, witness.binding.witness) &&
        normalized(binding.horizon) == normalized(witness.binding.horizon) &&
        channel.proofSegment.contains(witness.proofSegment) &&
        channel.rootOwnedEffectDescriptor.contains(witness.effectDescriptor)
    }

  private def carrierAndProvenanceMatch(
      witness: ComparisonEndpointEvidenceWitness,
      channel: DirectCauseChannel,
      graph: TypedEvidenceGraph
  ): Boolean =
    val actual = channel.binding
    val expected = witness.binding
    actual.source == expected.source &&
      actual.provenance == expected.provenance &&
      graph
        .record(actual.source)
        .toList
        .flatMap(graph.parentClosure)
        .map(_.ref.id)
        .distinct
        .sorted == witness.carrierAncestorSourceIds

  private def sameSemanticLine(
      left: Option[LineNodeRef],
      right: Option[LineNodeRef]
  ): Boolean =
    (left, right) match
      case (Some(leftLine), Some(rightLine)) =>
        leftLine.role == rightLine.role &&
          leftLine.rank == rightLine.rank &&
          EvidenceRef.sameMove(leftLine.rootMove, rightLine.rootMove)
      case (None, None) => true
      case _            => false

  private def sameObjects(
      left: List[ConcreteChessObject],
      right: List[ConcreteChessObject]
  ): Boolean =
    left.map(_.signaturePart).sorted == right.map(_.signaturePart).sorted

  private def normalized(value: Option[String]): Option[String] =
    value.map(normalize).filter(_.nonEmpty)

  /** Public admission needs exactly one fact for a queried scope. Multiple
    * distinct magnitudes are not averaged or selected by evidence order.
    */
  def uniqueObservationFor(
      inventory: ComparisonEndpointEffectInventory,
      scope: ComparisonEndpointEffectScopeKey
  ): Option[Option[ComparisonEndpointEffectObservation]] =
    inventory match
      case ComparisonEndpointEffectInventory.Incomplete => None
      case ComparisonEndpointEffectInventory.Complete(observations) =>
        observations.filter(_.scope == scope).toList match
          case Nil          => Some(None)
          case exact :: Nil => Some(Some(exact))
          case _            => None

  /** Removes endpoint-local source/target identity, Plan taxonomy, concrete
    * branch and route moves/squares, and the comparison magnitude from an
    * exact PlanResult. Endpoint-neutral goal, mechanism, response outcome and
    * timing semantics remain. Projection is unavailable unless the exact
    * ownership identity and its StructuralStrength magnitude agree.
    */
  private[chessjudgment] def planResultComparisonKey(
      observation: ComparisonEndpointEffectObservation
  ): Option[PlanResultComparisonKey] =
    val scope = observation.scope
    for
      identity <- scope.effectIdentity.planResult
      _ <- Option.when(
        scope.effectIdentity.primitiveKind == RootOwnedEffectPrimitiveKind.PlanResult &&
          scope.effectIdentity.strategicAxes.isEmpty
      )(())
      strength <- observation.magnitude match
        case ComparisonEndpointEffectMagnitude.Exact(
              DirectCauseImportanceMeasure.StructuralStrength(units)
            ) if units > 0 => Some(units)
        case _ => None
      if identity.strength == strength
      goalTargets <- normalizedRequiredSignatures(identity.goalTargetSubjects)
      mechanisms <- normalizedRequiredSignatures(scope.mechanismSignatures)
      baseConsequences <- normalizedRequiredSignatures(
        scope.consequenceSignatures.filterNot(signature =>
          normalize(signature).startsWith(s"${EvidenceObjectKind.Move.toString.toLowerCase}:")
        )
      )
    yield PlanResultComparisonKey(
      rootBoardState = scope.rootBoardState,
      mover = scope.mover,
      consequenceKind = identity.consequenceKind,
      polarity = identity.polarity,
      goalTargetSubjects = goalTargets,
      robustness = identity.robustness,
      branchSemantics = identity.branches
        .map(branch =>
          (
            branch.outcome,
            branch.observedThroughPlyOffset,
            branch.realizationPlyOffset,
            branch.terminalOutcome,
            branch.terminalPlyOffset
          )
        )
        .sortBy { case (outcome, observedThrough, realization, terminal, terminalPly) =>
          List(
            outcome.toString.toLowerCase,
            observedThrough.toString,
            realization.map(_.toString).getOrElse("none"),
            terminal.map(_.toString.toLowerCase).getOrElse("none"),
            terminalPly.map(_.toString).getOrElse("none")
          ).mkString("|")
        },
      causalRouteSemantics = identity.causalRoute
        .map(route =>
          (
            route.dependencyKind,
            normalize(route.proofKind),
            route.plyOffset,
            route.proofPieceRoles.map(normalize).filter(_.nonEmpty).distinct.sorted
          )
        )
        .sortBy { case (kind, proofKind, plyOffset, proofRoles) =>
          List(
            kind.toString.toLowerCase,
            proofKind,
            plyOffset.toString,
            proofRoles.mkString("[", ",", "]")
          ).mkString("|")
        },
      mechanismSignatures = mechanisms,
      consequenceSignatures = baseConsequences,
      horizon = scope.horizon.map(normalize).filter(_.nonEmpty),
      directChange = scope.directChange,
      stake = scope.stake
    )

  def compareMagnitude(
      left: ComparisonEndpointEffectMagnitude,
      right: ComparisonEndpointEffectMagnitude
  ): MagnitudeRelation =
    import MagnitudeRelation.*
    (left, right) match
      case (ComparisonEndpointEffectMagnitude.QualitativePresence,
            ComparisonEndpointEffectMagnitude.QualitativePresence) => Equal
      case (ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.MaterialOutcome(leftValue, leftPly)),
            ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.MaterialOutcome(rightValue, rightPly))) =>
        pareto(
          leftNoWorse = leftValue >= rightValue && leftPly <= rightPly,
          rightNoWorse = rightValue >= leftValue && rightPly <= leftPly,
          equal = leftValue == rightValue && leftPly == rightPly
        )
      case (ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.MateArrival(leftPly)),
            ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.MateArrival(rightPly))) =>
        orderedSmallerIsStronger(leftPly, rightPly)
      case (ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.ThreatHorizon(leftTurns)),
            ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.ThreatHorizon(rightTurns))) =>
        orderedSmallerIsStronger(leftTurns, rightTurns)
      case (ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.StructuralStrength(leftUnits)),
            ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.StructuralStrength(rightUnits))) =>
        orderedLargerIsStronger(leftUnits, rightUnits)
      case (ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.StrategicStrength(leftUnits)),
            ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.StrategicStrength(rightUnits))) =>
        orderedLargerIsStronger(leftUnits, rightUnits)
      case _ => Incomparable

  def fromChannel(
      cause: RelativeCauseFact,
      channel: DirectCauseChannel,
      graph: TypedEvidenceGraph
  ): Option[ComparisonEndpointEffectObservation] =
    for
      _ <- Option.unless(channel.importanceDescriptorAmbiguous)(())
      observation <- EvidenceObjectBinding.comparisonEndpointPrimitiveObservation(cause, channel, graph)
      if attributionStake(cause.attribution.kind).contains(observation.scope.stake)
    yield observation

  private[chessjudgment] def fromOwnedProof(
      rootPosition: PositionNodeRef,
      eventLine: LineNodeRef,
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof,
      directChange: DirectCausalChange,
      stake: RootOwnedEffectStake
  ): Option[ComparisonEndpointEffectObservation] =
    val descriptor = RootOwnedEffectDescriptorPolicy.describe(binding, proof)
    for
      magnitude <- comparisonMagnitude(descriptor.magnitude)
      scope <- scopeFrom(
        rootPosition,
        eventLine,
        binding,
        directChange,
        descriptor,
        stake
      )
    yield ComparisonEndpointEffectObservation(scope, magnitude)

  private[chessjudgment] def fromLineEpisode(
      rootPosition: PositionNodeRef,
      eventLine: LineNodeRef,
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof,
      episode: RootOwnedCausalEpisode
  ): Option[ComparisonEndpointEffectObservation] =
    for
      actor <- RootCausalActor.fromPosition(rootPosition, eventLine.rootMove)
      stake <- RootOwnedEffectPolicy.effectStake(proof, actor)
      change <- episode.consequence.kind match
        case LineConsequenceKind.MaterialGain if stake == RootOwnedEffectStake.ActorValue =>
          Some(DirectCausalChange.Occurred)
        case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss =>
          Some(DirectCausalChange.Lost)
        case LineConsequenceKind.ImmediateReplyCheck | LineConsequenceKind.Mate |
            LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow |
            LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
          Some(DirectCausalChange.Occurred)
        case LineConsequenceKind.DrawResource => Some(DirectCausalChange.Maintained)
        case _ => None
      observation <- fromOwnedProof(rootPosition, eventLine, binding, proof, change, stake)
    yield observation

  private[chessjudgment] def fromRootLineEvent(
      rootPosition: PositionNodeRef,
      eventLine: LineNodeRef,
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof,
      event: LineMoveEvent
  ): Option[ComparisonEndpointEffectObservation] =
    for
      actor <- RootCausalActor.fromPosition(rootPosition, eventLine.rootMove)
      stake <- RootOwnedEffectPolicy.effectStake(proof, actor)
      if Set(
        LineEventKind.Capture,
        LineEventKind.Recapture,
        LineEventKind.Check,
        LineEventKind.Mate,
        LineEventKind.Promotion,
        LineEventKind.Tempo,
        LineEventKind.DefenderMove
      )(event.kind)
      observation <- fromOwnedProof(
        rootPosition,
        eventLine,
        binding,
        proof,
        DirectCausalChange.Occurred,
        stake
      )
    yield observation

  private[chessjudgment] def fromEndgameHorizon(
      rootPosition: PositionNodeRef,
      eventLine: LineNodeRef,
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof,
      horizon: LineEndgameTechniqueHorizon
  ): Option[ComparisonEndpointEffectObservation] =
    for
      actor <- RootCausalActor.fromPosition(rootPosition, eventLine.rootMove)
      stake <- RootOwnedEffectPolicy.effectStake(proof, actor)
      change <- horizon.status match
        case LineEndgameTechniqueHorizonStatus.Active => Some(DirectCausalChange.Maintained)
        case LineEndgameTechniqueHorizonStatus.Transitioned |
            LineEndgameTechniqueHorizonStatus.Completed => Some(DirectCausalChange.Occurred)
        case LineEndgameTechniqueHorizonStatus.Failed => Some(DirectCausalChange.Lost)
        case LineEndgameTechniqueHorizonStatus.ContradictedByTerminalProof =>
          Some(DirectCausalChange.Refuted)
        case LineEndgameTechniqueHorizonStatus.SupersededByTactic => None
      observation <- fromOwnedProof(rootPosition, eventLine, binding, proof, change, stake)
    yield observation

  private[chessjudgment] def fromStructuralConsequence(
      rootPosition: PositionNodeRef,
      eventLine: LineNodeRef,
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof,
      consequence: TransitionConsequence
  ): Option[ComparisonEndpointEffectObservation] =
    for
      actor <- RootCausalActor.fromPosition(rootPosition, eventLine.rootMove)
      stake <- RootOwnedEffectPolicy.effectStake(proof, actor)
      change <- consequence.kind match
        case TransitionConsequenceKind.OpponentMobilityRestriction =>
          Some(DirectCausalChange.Prevented)
        case TransitionConsequenceKind.PawnTensionResolution =>
          Some(DirectCausalChange.Occurred)
        case TransitionConsequenceKind.TargetPressureRelease =>
          Some(DirectCausalChange.Lost)
        case _ if consequence.positive => Some(DirectCausalChange.Occurred)
        case _ if consequence.negative => Some(DirectCausalChange.Lost)
        case _ => None
      observation <- fromOwnedProof(rootPosition, eventLine, binding, proof, change, stake)
    yield observation

  /** Rebuilds the endpoint fact from the same typed plan-result primitive used
    * by the public channel. Only the selected exact robust/refuted assessment
    * is observable; heuristic or unresolved plan carriers remain incomplete.
    */
  private[chessjudgment] def fromExactPlanResult(
      rootPosition: PositionNodeRef,
      eventLine: LineNodeRef,
      source: EvidenceRef,
      event: PlanCausalEventEvidence,
      assessment: PlanCausalResultAssessment,
      exactBinding: Option[EvidenceObjectBinding] = None
  ): Option[ComparisonEndpointEffectObservation] =
    val exactChange =
      if event.exactRefutedPublicResultAssessment.contains(assessment) then
        Some(DirectCausalChange.Refuted)
      else if event.exactRobustPublicResultAssessment.contains(assessment) then
        assessment.consequence.kind match
          case TransitionConsequenceKind.OpponentMobilityRestriction =>
            Some(DirectCausalChange.Prevented)
          case TransitionConsequenceKind.PawnTensionResolution =>
            Some(DirectCausalChange.Occurred)
          case TransitionConsequenceKind.TargetPressureRelease =>
            Some(DirectCausalChange.Lost)
          case _ if assessment.consequence.positive =>
            Some(DirectCausalChange.Occurred)
          case _ if assessment.consequence.negative =>
            Some(DirectCausalChange.Lost)
          case _ =>
            None
      else None
    val proof = RootOwnedEffectProof.PlanResult(source, event, assessment)
    for
      _ <- Option.when(source.confidence != EvidenceConfidence.Heuristic)(())
      actor <- RootCausalActor.fromPosition(rootPosition, eventLine.rootMove)
      if RootOwnedEffectPolicy.planEventOwnsRoot(source, event, eventLine, actor.color)
      change <- exactChange
      stake <- RootOwnedEffectPolicy.effectStake(proof, actor)
      observation <- fromOwnedProof(
        rootPosition,
        eventLine,
        exactBinding.getOrElse(
          EvidenceObjectBinding.planAssessmentBinding(source, event, actor, assessment, eventLine)
        ),
        proof,
        change,
        stake
      )
    yield observation

  /** Selects an exact root -> induced reply -> opposite endpoint realization
    * already proved by one F-stage plan episode. This observes comparison
    * order only; it neither constructs nor admits a Cause.
    */
  private[chessjudgment] def exactInducedResponseMoveOrder(
      comparison: CandidateComparisonFact,
      sourceSide: RelativeCauseSourceSide,
      source: EvidenceRef,
      event: PlanCausalEventEvidence
  ): Option[(PlanCausalResultAssessment, PlanCausalResponse)] =
    val endpointLines = sourceSide match
      case RelativeCauseSourceSide.Reference =>
        Some(comparison.referenceLine -> comparison.candidateLine)
      case RelativeCauseSourceSide.Candidate =>
        Some(comparison.candidateLine -> comparison.referenceLine)
      case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed =>
        None
    for
      (sourceLine, oppositeLine) <- endpointLines
      if !EvidenceRef.sameMove(sourceLine.rootMove, oppositeLine.rootMove)
      if sameSemanticLine(source.line, Some(sourceLine))
      if sameSemanticLine(Some(event.rootLine), Some(sourceLine))
      if source.confidence != EvidenceConfidence.Heuristic
      if RootOwnedEffectPolicy.planEventOwnsRoot(
        source,
        event,
        sourceLine,
        comparison.comparison.mover
      )
      assessment <- event.exactRobustPublicResultAssessment
      episode <- event.episode
      if assessment.sourceEvent != episode.root
      if EvidenceRef.sameMove(assessment.sourceEvent.moveUci, oppositeLine.rootMove)
      path <- episode.enablingPathTo(assessment.sourceEvent)
      dependencies = episode.enablingDependenciesTo(assessment.sourceEvent)
      if path.headOption.contains(episode.root) &&
        path.lastOption.contains(assessment.sourceEvent) &&
        dependencies.size == path.size - 1 &&
        dependencies.zip(path.zip(path.drop(1))).forall {
          case (dependency, (from, to)) =>
            dependency.from == from &&
              dependency.to == to &&
              dependency.planConnectionProven &&
              dependency.enablesContinuation
        }
      exactResponses = episode.responses
        .filter(response =>
          response.trigger == episode.root &&
            response.proven &&
            response.plyOffset == 1 &&
            response.step.ply < assessment.sourceEvent.step.ply &&
            PrincipalVariationEvidence.sameBoardState(
              episode.root.step.fenAfter,
              response.step.fenBefore
            )
        )
        .distinctBy(response =>
          (
            response.trigger.moveUci,
            response.step.moveUci,
            response.step.ply,
            response.step.fenBefore,
            response.step.fenAfter,
            response.plyOffset
          )
        )
      response <- exactResponses match
        case exact :: Nil => Some(exact)
        case _            => None
    yield assessment -> response

  /** Resolves only the comparison-specific neutral witness assembled by the
    * endpoint snapshot. A generic PlanResult witness cannot become a
    * move-order producer input merely by sharing its primitive source.
    */
  private[chessjudgment] def exactInducedResponseMoveOrderRecords(
      snapshot: ComparisonEndpointEvidenceSnapshot,
      sourceSide: RelativeCauseSourceSide,
      graph: TypedEvidenceGraph
  ): List[EvidenceRecord] =
    val expectedLine = sourceSide match
      case RelativeCauseSourceSide.Reference => Some(snapshot.comparison.referenceLine)
      case RelativeCauseSourceSide.Candidate => Some(snapshot.comparison.candidateLine)
      case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed => None
    snapshot
      .forSide(sourceSide)
      .toList
      .filter(side =>
        side.sourceSide == sourceSide &&
          expectedLine.exists(line => sameSemanticLine(Some(side.line), Some(line)))
      )
      .flatMap(side =>
        side.witnesses.filter(witness =>
          witness.sourceSide == sourceSide &&
            sameSemanticLine(Some(witness.line), Some(side.line))
        )
      )
      .flatMap { witness =>
        RootOwnedEffectPolicy.exactPlanResultPrimitive(witness.rootOwnedProof).toList.flatMap {
          case (source, event, assessment) =>
            for
              (selectedAssessment, response) <-
                exactInducedResponseMoveOrder(
                  snapshot.comparison,
                  sourceSide,
                  source,
                  event
                ).toList
              if selectedAssessment == assessment
              if RootOwnedEffectPolicy.sameCausalRootOccurrence(
                source.position,
                snapshot.comparisonEvidence.position
              )
              actor <- RootCausalActor
                .fromPosition(source.position, witness.line.rootMove)
                .toList
              expectedBinding <- EvidenceObjectBinding
                .inducedResponseMoveOrderBinding(
                  snapshot.comparison,
                  sourceSide,
                  source,
                  event,
                  actor,
                  assessment,
                  response,
                  witness.line
                )
                .toList
              if witness.binding == expectedBinding
              expectedProof = RootOwnedEffectProof.PlanResult(source, event, assessment)
              expectedSegment <- DirectCauseProofSegment.from(expectedProof).toList
              if witness.proofSegment == expectedSegment
              if witness.effectDescriptor ==
                RootOwnedEffectDescriptorPolicy.describe(expectedBinding, expectedProof)
              expectedObservation = fromExactPlanResult(
                source.position,
                witness.line,
                source,
                event,
                assessment,
                Some(expectedBinding)
              )
              if witness.observation == expectedObservation
              record <- graph.record(source).toList
              if record.ref == source && record.payload == event
              if graph
                .parentClosure(record)
                .map(_.ref.id)
                .distinct
                .sorted == witness.carrierAncestorSourceIds
            yield record
        }
      }
      .distinctBy(_.ref.id)

  def retainedPlayedValueReady(
      cause: RelativeCauseFact,
      channel: DirectCauseChannel,
      comparison: CandidateComparisonFact,
      graph: TypedEvidenceGraph
  ): Boolean =
    comparison.kind == CandidateComparisonKind.PlayedVsBest &&
      graph.comparisonFor(cause).contains(comparison) &&
      Set(MoveChoiceVerdict.MatchesReference, MoveChoiceVerdict.PlayableLoss)(
        comparison.comparison.verdict
      ) &&
      !channel.importanceDescriptorAmbiguous &&
      channel.rootOwnedProof.nonEmpty &&
      channel.binding.line.contains(comparison.candidateLine) &&
      channel.rootOwnedEffectDescriptor.exists(_.magnitude.comparisonReady) &&
      channel.binding.actor.nonEmpty &&
      channel.binding.target.nonEmpty &&
      channel.binding.mechanism.nonEmpty &&
      channel.binding.consequence.nonEmpty &&
      RootOwnedEffectPolicy.admits(cause, graph, channel) &&
      (for
        line <- channel.binding.line
        actor <- RootCausalActor.fromPosition(cause.comparisonEvidence.position, line.rootMove)
        proof <- channel.rootOwnedProof
        stake <- RootOwnedEffectPolicy.effectStake(proof, actor)
      yield stake).contains(RootOwnedEffectStake.ActorValue) &&
      cause.sourceSide == RelativeCauseSourceSide.Candidate &&
      cause.attribution.kind == CauseAttributionKind.CandidateCreatesValue

  /** A PlayedVsBest liability belongs to the played move even when the best
    * endpoint did not enumerate the same plan id. This exception is narrower
    * than endpoint-differential admission: only the candidate's bare exact
    * refuted plan result may cross it.
    */
  def retainedPlayedLiabilityReady(
      cause: RelativeCauseFact,
      channel: DirectCauseChannel,
      comparison: CandidateComparisonFact,
      graph: TypedEvidenceGraph
  ): Boolean =
    comparison.kind == CandidateComparisonKind.PlayedVsBest &&
      graph.comparisonFor(cause).contains(comparison) &&
      comparison.comparison.verdict.isActionableLoss &&
      cause.kind == RelativeCauseKind.PlanContradiction &&
      cause.sourceSide == RelativeCauseSourceSide.Candidate &&
      cause.attribution.kind == CauseAttributionKind.CandidateAllowsLiability &&
      !channel.importanceDescriptorAmbiguous &&
      channel.directChange == DirectCausalChange.Refuted &&
      channel.binding.line.contains(comparison.candidateLine) &&
      channel.rootOwnedEffectDescriptor.exists(_.magnitude.comparisonReady) &&
      channel.binding.actor.nonEmpty &&
      channel.binding.target.nonEmpty &&
      channel.binding.mechanism.nonEmpty &&
      channel.binding.consequence.nonEmpty &&
      RootOwnedEffectPolicy.admits(cause, graph, channel) &&
      fromChannel(cause, channel, graph).nonEmpty &&
      (for
        proof <- channel.rootOwnedProof
        actor <- RootCausalActor.fromPosition(
          cause.comparisonEvidence.position,
          comparison.candidateLine.rootMove
        )
        stake <- RootOwnedEffectPolicy.effectStake(proof, actor)
        if proof match
          case RootOwnedEffectProof.PlanResult(source, event, assessment) =>
            source.confidence != EvidenceConfidence.Heuristic &&
              event.exactRefutedPublicResultAssessment.contains(assessment) &&
              RootOwnedEffectPolicy.planEventOwnsRoot(
                source,
                event,
                comparison.candidateLine,
                actor.color
              )
          case _ => false
      yield stake).contains(RootOwnedEffectStake.ActorLiability)

  /** Strategic contrast is itself the complete comparison fact. Its semantic
    * axis is the target/mechanism/consequence scope; primitive carrier objects
    * remain mandatory on the Cause channel but are not reconstructed from a
    * sibling Cause or support id here.
    */
  private[chessjudgment] def fromStrategicAxis(
      rootPosition: PositionNodeRef,
      eventLine: LineNodeRef,
      axis: StrategicAxisDetail,
      strength: Int
  ): Option[ComparisonEndpointEffectObservation] =
    for
      actor <- RootCausalActor.fromPosition(rootPosition, eventLine.rootMove)
      rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(rootPosition.fen)
      stake <- RootOwnedEffectPolicy.strategicAxisStake(axis)
      change <- RootOwnedEffectPolicy.strategicAxisChange(axis)
      if strength > 0
    yield
      val axisKey = normalize(axis.stableKey)
      ComparisonEndpointEffectObservation(
        scope = ComparisonEndpointEffectScopeKey(
          rootBoardState = rootBoard,
          mover = ComparisonEndpointMoverIdentity(actor.color),
          targetSignatures = List(s"strategic-axis:$axisKey"),
          mechanismSignatures = List(s"strategic-axis-kind:${normalize(axis.kind.toString)}"),
          consequenceSignatures = List(s"strategic-axis-polarity:${normalize(axis.polarity.toString)}"),
          horizon = None,
          directChange = change,
          effectIdentity = RootOwnedEffectIdentity(
            primitiveKind = RootOwnedEffectPrimitiveKind.Unspecified,
            targetSignatures = List(s"strategic-axis:$axisKey"),
            planIds = Nil,
            strategicAxes = List(
              RootOwnedStrategicAxisIdentity(
                kind = axis.kind,
                polarity = axis.polarity,
                label = normalize(axis.label),
                comparisonOutcome = None
              )
            )
          ),
          stake = stake
        ),
        magnitude = ComparisonEndpointEffectMagnitude.Exact(
          DirectCauseImportanceMeasure.StrategicStrength(strength)
        )
      )

  private def comparisonMagnitude(
      knowledge: DirectEffectMagnitudeKnowledge
  ): Option[ComparisonEndpointEffectMagnitude] =
    knowledge match
      case DirectEffectMagnitudeKnowledge.Exact(measure) =>
        Some(ComparisonEndpointEffectMagnitude.Exact(measure))
      case DirectEffectMagnitudeKnowledge.NotApplicable =>
        Some(ComparisonEndpointEffectMagnitude.QualitativePresence)
      case DirectEffectMagnitudeKnowledge.ExpectedButMissing |
          DirectEffectMagnitudeKnowledge.Ambiguous => None

  private[chessjudgment] def strategicScopeFromChannel(
      cause: RelativeCauseFact,
      channel: DirectCauseChannel,
      graph: TypedEvidenceGraph
  ): Option[ComparisonEndpointEffectScopeKey] =
    for
      _ <- Option.unless(channel.importanceDescriptorAmbiguous)(())
      eventLine <- channel.binding.line
      actor <- RootCausalActor.fromPosition(cause.comparisonEvidence.position, eventLine.rootMove)
      proof <- channel.rootOwnedProof
      if RootOwnedEffectPolicy.admits(cause, graph, channel)
      descriptor <- channel.rootOwnedEffectDescriptor
      axis <- descriptor.identity.strategicAxes match
        case exact :: Nil => Some(exact)
        case _            => None
      stake <- RootOwnedEffectPolicy.effectStake(proof, actor)
      axisStake <- RootOwnedEffectPolicy.strategicAxisStake(
        StrategicAxisDetail(axis.kind, axis.polarity, axis.label)
      )
      if stake == axisStake
      if attributionStake(cause.attribution.kind).contains(stake)
      rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(cause.comparisonEvidence.position.fen)
      change <- RootOwnedEffectPolicy.strategicAxisChange(
        StrategicAxisDetail(axis.kind, axis.polarity, axis.label)
      )
    yield
      val axisKey = normalize(s"${axis.kind}:${axis.polarity}:${axis.label}")
      ComparisonEndpointEffectScopeKey(
        rootBoardState = rootBoard,
        mover = ComparisonEndpointMoverIdentity(actor.color),
        targetSignatures = List(s"strategic-axis:$axisKey"),
        mechanismSignatures = List(s"strategic-axis-kind:${normalize(axis.kind.toString)}"),
        consequenceSignatures = List(s"strategic-axis-polarity:${normalize(axis.polarity.toString)}"),
        horizon = None,
        directChange = change,
        effectIdentity = RootOwnedEffectIdentity(
          primitiveKind = RootOwnedEffectPrimitiveKind.Unspecified,
          targetSignatures = List(s"strategic-axis:$axisKey"),
          planIds = Nil,
          strategicAxes = List(axis.copy(comparisonOutcome = None))
        ),
        stake = stake
      )

  def scopeFromChannel(
      cause: RelativeCauseFact,
      channel: DirectCauseChannel,
      graph: TypedEvidenceGraph
  ): Option[ComparisonEndpointEffectScopeKey] =
    if channel.rootOwnedEffectDescriptor.exists(_.identity.strategicAxes.nonEmpty) then
      strategicScopeFromChannel(cause, channel, graph)
    else fromChannel(cause, channel, graph).map(_.scope)

  private def scopeFrom(
      rootPosition: PositionNodeRef,
      eventLine: LineNodeRef,
      binding: EvidenceObjectBinding,
      directChange: DirectCausalChange,
      descriptor: RootOwnedEffectDescriptor,
      stake: RootOwnedEffectStake
  ): Option[ComparisonEndpointEffectScopeKey] =
    for
      actor <- RootCausalActor.fromPosition(rootPosition, eventLine.rootMove)
      rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(rootPosition.fen)
      targets <- normalizedTargetSignatures(descriptor, binding.target)
      mechanisms <- normalizedMechanismSignatures(descriptor, binding.mechanism)
      consequences <- normalizedConsequenceSignatures(descriptor, binding.consequence)
    yield ComparisonEndpointEffectScopeKey(
      rootBoardState = rootBoard,
      mover = ComparisonEndpointMoverIdentity(actor.color),
      targetSignatures = targets,
      mechanismSignatures = mechanisms,
      consequenceSignatures = consequences,
      horizon = normalizedScopeHorizon(descriptor, binding.horizon),
      directChange = directChange,
      effectIdentity = normalizedEffectIdentity(descriptor),
      stake = stake
    )

  private def attributionStake(
      attribution: CauseAttributionKind
  ): Option[RootOwnedEffectStake] =
    attribution match
      case CauseAttributionKind.ReferenceCreatesResource | CauseAttributionKind.CandidateCreatesValue =>
        Some(RootOwnedEffectStake.ActorValue)
      case CauseAttributionKind.CandidateAllowsLiability =>
        Some(RootOwnedEffectStake.ActorLiability)
      case CauseAttributionKind.SharedContext | CauseAttributionKind.ContextOnly |
          CauseAttributionKind.Unattributed => None

  private def normalizedScopeHorizon(
      descriptor: RootOwnedEffectDescriptor,
      horizon: Option[String]
  ): Option[String] =
    descriptor.magnitude match
      case DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.MaterialOutcome(_, _)) |
          DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.MateArrival(_)) |
          DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.ThreatHorizon(_)) =>
        None
      case _ => horizon.map(normalize).filter(_.nonEmpty)

  private def normalizedConsequenceSignatures(
      descriptor: RootOwnedEffectDescriptor,
      consequences: List[ConcreteChessObject]
  ): Option[List[String]] =
    descriptor.magnitude match
      case DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.MaterialOutcome(_, _)) =>
        Some(List("consequence:material-transfer"))
      case DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.MateArrival(_)) =>
        Some(List("consequence:mate"))
      case _ => requiredSignatures(consequences)

  private def normalizedTargetSignatures(
      descriptor: RootOwnedEffectDescriptor,
      targets: List[ConcreteChessObject]
  ): Option[List[String]] =
    descriptor.magnitude match
      case DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.MaterialOutcome(_, _)) =>
        Some(List("outcome:material-transfer"))
      case DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.MateArrival(_)) =>
        Some(List("outcome:mate"))
      case _ => requiredSignatures(targets)

  private def normalizedMechanismSignatures(
      descriptor: RootOwnedEffectDescriptor,
      mechanisms: List[ConcreteChessObject]
  ): Option[List[String]] =
    descriptor.magnitude match
      case DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.MaterialOutcome(_, _)) =>
        Some(List("outcome:material-transfer"))
      case DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.MateArrival(_)) =>
        Some(List("outcome:mate"))
      case _ => requiredSignatures(mechanisms)

  private def normalizedEffectIdentity(
      descriptor: RootOwnedEffectDescriptor
  ): RootOwnedEffectIdentity =
    descriptor.magnitude match
      case DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.MaterialOutcome(_, _)) =>
        RootOwnedEffectIdentity(
          RootOwnedEffectPrimitiveKind.LineEpisode,
          List("outcome:material-transfer"),
          Nil,
          Nil
        )
      case DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.MateArrival(_)) =>
        RootOwnedEffectIdentity(
          RootOwnedEffectPrimitiveKind.LineEpisode,
          List("outcome:mate"),
          Nil,
          Nil
        )
      case _ => descriptor.identity

  private def pareto(
      leftNoWorse: Boolean,
      rightNoWorse: Boolean,
      equal: Boolean
  ): MagnitudeRelation =
    import MagnitudeRelation.*
    if equal then Equal
    else if leftNoWorse && !rightNoWorse then LeftStrictlyStronger
    else if rightNoWorse && !leftNoWorse then RightStrictlyStronger
    else Incomparable

  private def orderedLargerIsStronger(left: Int, right: Int): MagnitudeRelation =
    import MagnitudeRelation.*
    if left == right then Equal
    else if left > right then LeftStrictlyStronger
    else RightStrictlyStronger

  private def orderedSmallerIsStronger(left: Int, right: Int): MagnitudeRelation =
    import MagnitudeRelation.*
    if left == right then Equal
    else if left < right then LeftStrictlyStronger
    else RightStrictlyStronger

  private def requiredSignatures(
      objects: List[ConcreteChessObject]
  ): Option[List[String]] =
    Option.when(objects.nonEmpty)(objects.map(_.signaturePart).distinct.sorted)

  private def normalizedRequiredSignatures(
      signatures: List[String]
  ): Option[List[String]] =
    val normalized = signatures.map(normalize).filter(_.nonEmpty).distinct.sorted
    Option.when(normalized.nonEmpty)(normalized)

  private def normalize(value: String): String =
    Option(value).getOrElse("").trim.toLowerCase
