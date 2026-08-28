package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.PrincipalVariationEvidence
private[chessjudgment] object ComparisonEndpointEffectObservationPolicy:

  enum MagnitudeRelation:
    case Equal
    case LeftStrictlyStronger
    case RightStrictlyStronger
    case Incomparable

  private[chessjudgment] final case class PlanResultCounterfactualResponseMapping(
      sourceInducedResponse: PlanResultSourceOccurrence
  )

  private[chessjudgment] final case class PlanResultCounterfactualDependencyMapping(
      source: PlanCausalDependencyFunctionIdentity,
      counterpart: PlanCausalDependencyFunctionIdentity
  )

  /** Exact anti-isomorphism for a two-move ordering comparison. It records
    * which root occurrence becomes the opposite result, which result becomes
    * the opposite root, the induced response, and the full dependency facts.
    * No independently normalized or sorted route can construct this proof.
    */
  private[chessjudgment] final case class PlanResultCounterfactualCorrespondence private[chessjudgment] (
      sourceRootToCounterpartResult: (
          PlanResultSourceOccurrence,
          PlanResultSourceOccurrence
      ),
      sourceResultToCounterpartRoot: (
          PlanResultSourceOccurrence,
          PlanResultSourceOccurrence
      ),
      inducedResponse: PlanResultCounterfactualResponseMapping,
      dependencyMappings: List[PlanResultCounterfactualDependencyMapping],
      sourceGoalFunction: PlanCausalGoalFunctionIdentity,
      counterpartGoalFunction: PlanCausalGoalFunctionIdentity
  )

  private final case class PlanResultOutcomeSemantics(
      rootBoardState: String,
      mover: ComparisonEndpointMoverIdentity,
      consequenceKind: TransitionConsequenceKind,
      polarity: StructuralSignalPolarity,
      goalTargetSubjects: List[String],
      robustness: PlanCausalRobustness,
      branchSemantics: List[String],
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
          expectedLine.exists(line => SemanticLineKey.same(side.line, line))
      )
      .flatMap(side =>
        side.witnesses.filter(witness =>
          witness.sourceSide == sourceSide &&
            SemanticLineKey.same(side.line, witness.line)
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
        SemanticLineKey.sameOptional(binding.line, Some(witness.line)) &&
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

  private[chessjudgment] def planResultCounterfactualCorrespondence(
      comparison: CandidateComparisonFact,
      sourceSide: RelativeCauseSourceSide,
      sourceObservation: ComparisonEndpointEffectObservation,
      counterpartObservation: ComparisonEndpointEffectObservation
  ): Option[PlanResultCounterfactualCorrespondence] =
    val endpointLines = sourceSide match
      case RelativeCauseSourceSide.Reference =>
        Some(comparison.referenceLine -> comparison.candidateLine)
      case RelativeCauseSourceSide.Candidate =>
        Some(comparison.candidateLine -> comparison.referenceLine)
      case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed => None
    for
      (sourceLine, counterpartLine) <- endpointLines
      sourceIdentity <- exactPlanResultIdentity(sourceObservation)
      counterpartIdentity <- exactPlanResultIdentity(counterpartObservation)
      sourceResponse <- sourceIdentity.selectedInducedResponse
      if counterpartIdentity.selectedInducedResponse.isEmpty
      if sourceIdentity.root.plyOffset == 0 && counterpartIdentity.root.plyOffset == 0
      if sourceIdentity.source.plyOffset == 2 && counterpartIdentity.source.plyOffset == 2
      if sourceResponse.plyOffset == 1
      if sameMove(sourceIdentity.root.moveUci, sourceLine.rootMove)
      if sameMove(sourceIdentity.source.moveUci, counterpartLine.rootMove)
      if sameMove(counterpartIdentity.root.moveUci, counterpartLine.rootMove)
      if sameMove(counterpartIdentity.source.moveUci, sourceLine.rootMove)
      if sameActorRoleTransition(sourceIdentity.root, counterpartIdentity.source)
      if sameActorRoleTransition(sourceIdentity.source, counterpartIdentity.root)
      sourceOutcome <- planResultOutcomeSemantics(sourceObservation, sourceIdentity)
      counterpartOutcome <- planResultOutcomeSemantics(counterpartObservation, counterpartIdentity)
      if sourceOutcome == counterpartOutcome
      sourceDependency <- sourceIdentity.causalRoute match
        case exact :: Nil => Some(exact)
        case _            => None
      counterpartDependency <- counterpartIdentity.causalRoute match
        case exact :: Nil => Some(exact)
        case _            => None
      if dependencyCounterfactuallyCorresponds(
        sourceIdentity,
        counterpartIdentity,
        sourceDependency,
        counterpartDependency,
        sourceResponse
      )
      if goalFunctionsCounterfactuallyCorrespond(
        sourceIdentity,
        counterpartIdentity,
        sourceDependency,
        counterpartDependency
      )
    yield
      PlanResultCounterfactualCorrespondence(
        sourceRootToCounterpartResult = sourceIdentity.root -> counterpartIdentity.source,
        sourceResultToCounterpartRoot = sourceIdentity.source -> counterpartIdentity.root,
        inducedResponse = PlanResultCounterfactualResponseMapping(
          sourceResponse
        ),
        dependencyMappings = List(
          PlanResultCounterfactualDependencyMapping(sourceDependency, counterpartDependency)
        ),
        sourceGoalFunction = sourceIdentity.goalFunction,
        counterpartGoalFunction = counterpartIdentity.goalFunction
      )

  private def exactPlanResultIdentity(
      observation: ComparisonEndpointEffectObservation
  ): Option[PlanResultSemanticIdentity] =
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
    yield identity

  private def planResultOutcomeSemantics(
      observation: ComparisonEndpointEffectObservation,
      identity: PlanResultSemanticIdentity
  ): Option[PlanResultOutcomeSemantics] =
    val scope = observation.scope
    for
      goalTargets <- normalizedRequiredSignatures(identity.goalTargetSubjects)
      mechanisms <- normalizedRequiredSignatures(scope.mechanismSignatures)
      baseConsequences <- normalizedRequiredSignatures(
        scope.consequenceSignatures.filterNot(signature =>
          normalize(signature).startsWith(s"${EvidenceObjectKind.Move.toString.toLowerCase}:")
        )
      )
    yield PlanResultOutcomeSemantics(
      rootBoardState = scope.rootBoardState,
      mover = scope.mover,
      consequenceKind = identity.consequenceKind,
      polarity = identity.polarity,
      goalTargetSubjects = goalTargets,
      robustness = identity.robustness,
      branchSemantics = identity.branches.map(branch =>
        List(
          branch.outcome.toString.toLowerCase,
          branch.observedThroughPlyOffset.toString,
          branch.realizations
            .map(realization => s"${realization.matchKind.toString.toLowerCase}:${realization.plyOffset}")
            .distinct
            .sorted
            .mkString("[", ",", "]"),
          branch.terminalOutcome.map(_.toString.toLowerCase).getOrElse("none"),
          branch.terminalPlyOffset.map(_.toString).getOrElse("none")
        ).mkString("|")
      ).distinct.sorted,
      mechanismSignatures = mechanisms,
      consequenceSignatures = baseConsequences,
      horizon = scope.horizon.map(normalize).filter(_.nonEmpty),
      directChange = scope.directChange,
      stake = scope.stake
    )

  private def goalFunctionsCounterfactuallyCorrespond(
      sourceIdentity: PlanResultSemanticIdentity,
      counterpartIdentity: PlanResultSemanticIdentity,
      sourceDependency: PlanCausalDependencyFunctionIdentity,
      counterpartDependency: PlanCausalDependencyFunctionIdentity
  ): Boolean =
    sourceIdentity.goalFunction.mechanism == counterpartIdentity.goalFunction.mechanism &&
      ((sourceIdentity.goalFunction.supportingDependency, counterpartIdentity.goalFunction.supportingDependency) match
        case (None, None) => true
        case (Some(source), Some(counterpart)) =>
          source == sourceDependency && counterpart == counterpartDependency
        case _ => false)

  private def dependencyCounterfactuallyCorresponds(
      sourceIdentity: PlanResultSemanticIdentity,
      counterpartIdentity: PlanResultSemanticIdentity,
      source: PlanCausalDependencyFunctionIdentity,
      counterpart: PlanCausalDependencyFunctionIdentity,
      sourceResponse: PlanResultSourceOccurrence
  ): Boolean =
    source.fromPlyOffset == 0 &&
      source.toPlyOffset == sourceIdentity.source.plyOffset &&
      counterpart.fromPlyOffset == 0 &&
      counterpart.toPlyOffset == counterpartIdentity.source.plyOffset &&
      source.plyOffset == sourceIdentity.source.plyOffset &&
      counterpart.plyOffset == counterpartIdentity.source.plyOffset &&
      source.dependencyKind == counterpart.dependencyKind &&
      sameMove(source.fromMoveUci, sourceIdentity.root.moveUci) &&
      sameMove(source.toMoveUci, sourceIdentity.source.moveUci) &&
      sameMove(counterpart.fromMoveUci, counterpartIdentity.root.moveUci) &&
      sameMove(counterpart.toMoveUci, counterpartIdentity.source.moveUci) &&
      sameMove(source.fromMoveUci, counterpart.toMoveUci) &&
      sameMove(source.toMoveUci, counterpart.fromMoveUci) &&
      dependencyProofCounterfactuallyCorresponds(
        sourceIdentity,
        counterpartIdentity,
        source.proof,
        counterpart.proof,
        sourceResponse
      )

  private def dependencyProofCounterfactuallyCorresponds(
      sourceIdentity: PlanResultSemanticIdentity,
      counterpartIdentity: PlanResultSemanticIdentity,
      source: PlanCausalDependencyFunctionProof,
      counterpart: PlanCausalDependencyFunctionProof,
      sourceResponse: PlanResultSourceOccurrence
  ): Boolean =
    (source, counterpart) match
      case (
            left: PlanCausalDependencyFunctionProof.ObjectState,
            right: PlanCausalDependencyFunctionProof.ObjectState
          ) =>
        left.color == right.color &&
          roleMatches(left.rootBeforeRole, sourceIdentity.root) &&
          roleMatches(left.pieceRole, sourceIdentity.source) &&
          roleMatches(right.rootBeforeRole, counterpartIdentity.root) &&
          roleMatches(right.pieceRole, counterpartIdentity.source) &&
          moveSquaresMatch(sourceIdentity.root, left.rootFrom, left.rootTo) &&
          moveSquaresMatch(sourceIdentity.source, left.futureFrom, left.futureTo) &&
          moveSquaresMatch(counterpartIdentity.root, right.rootFrom, right.rootTo) &&
          moveSquaresMatch(counterpartIdentity.source, right.futureFrom, right.futureTo)
      case (
            left: PlanCausalDependencyFunctionProof.LineAccess,
            right: PlanCausalDependencyFunctionProof.LineAccess
          ) =>
        left.color == right.color &&
          roleMatches(left.enabledPieceRole, sourceIdentity.source) &&
          roleMatches(right.enabledPieceRole, counterpartIdentity.source) &&
          moveSquaresMatch(sourceIdentity.source, left.enabledFrom, left.enabledTo) &&
          moveSquaresMatch(counterpartIdentity.source, right.enabledFrom, right.enabledTo) &&
          left.vacatedSquares.exists(moveOriginMatches(sourceIdentity.root, _)) &&
          right.vacatedSquares.exists(moveOriginMatches(counterpartIdentity.root, _))
      case (
            left: PlanCausalDependencyFunctionProof.ResponseContinuation,
            right: PlanCausalDependencyFunctionProof.ResponseContinuation
          ) =>
        left.kind == right.kind &&
          sameMove(left.replyMoveUci, sourceResponse.moveUci) &&
          moveSquaresMatch(sourceResponse, left.replyFrom, left.replyTo) &&
          moveSquaresMatch(sourceIdentity.source, left.followUpFrom, left.followUpTo) &&
          moveSquaresMatch(counterpartIdentity.source, right.followUpFrom, right.followUpTo) &&
          left.proofPieceRoles.map(_.name.toLowerCase).distinct.sorted ==
            right.proofPieceRoles.map(_.name.toLowerCase).distinct.sorted &&
          left.proofSquares.drop(4).map(_.key.toLowerCase) ==
            right.proofSquares.drop(4).map(_.key.toLowerCase)
      case _ => false

  private def roleMatches(
      role: EvidencePieceRole,
      occurrence: PlanResultSourceOccurrence
  ): Boolean =
    occurrence.actor.beforeRole.equalsIgnoreCase(role.name)

  private def sameActorRoleTransition(
      left: PlanResultSourceOccurrence,
      right: PlanResultSourceOccurrence
  ): Boolean =
    left.actor.beforeRole.equalsIgnoreCase(right.actor.beforeRole) &&
      left.actor.afterRole.equalsIgnoreCase(right.actor.afterRole)

  private def moveSquaresMatch(
      occurrence: PlanResultSourceOccurrence,
      from: EvidenceSquare,
      to: EvidenceSquare
  ): Boolean =
    occurrence.actor.from.equalsIgnoreCase(from.key) &&
      occurrence.actor.to.equalsIgnoreCase(to.key)

  private def moveOriginMatches(
      occurrence: PlanResultSourceOccurrence,
      square: EvidenceSquare
  ): Boolean =
    occurrence.actor.from.equalsIgnoreCase(square.key)

  private def sameMove(left: String, right: String): Boolean =
    EvidenceRef.sameMove(left, right)

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
      case (ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.StructuralStrength(leftUnits)),
            ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.StructuralStrength(rightUnits))) =>
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
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof,
      directChange: DirectCausalChange,
      stake: RootOwnedEffectStake,
      actor: RootCausalActor
  ): Option[ComparisonEndpointEffectObservation] =
    val descriptor = RootOwnedEffectDescriptorPolicy.describe(binding, proof)
    for
      magnitude <- comparisonMagnitude(descriptor.magnitude)
      scope <- scopeFrom(
        rootPosition,
        binding,
        directChange,
        descriptor,
        stake,
        actor
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
      actor <- proof match
        case RootOwnedEffectProof.LineEpisode(_, line, _) =>
          RootCausalActor.fromLineFact(line, eventLine.rootMove)
        case _ => None
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
      observation <- fromOwnedProof(rootPosition, binding, proof, change, stake, actor)
    yield observation

  private[chessjudgment] def fromRootLineEvent(
      rootPosition: PositionNodeRef,
      eventLine: LineNodeRef,
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof,
      event: LineMoveEvent
  ): Option[ComparisonEndpointEffectObservation] =
    for
      actor <- proof match
        case RootOwnedEffectProof.RootLineEvent(_, line, _) =>
          RootCausalActor.fromLineFact(line, eventLine.rootMove)
        case _ => None
      stake <- RootOwnedEffectPolicy.effectStake(proof, actor)
      if Set(
        LineEventKind.Capture,
        LineEventKind.Recapture,
        LineEventKind.Check,
        LineEventKind.Mate,
        LineEventKind.Promotion,
        LineEventKind.CheckEvasion
      )(event.kind)
      observation <- fromOwnedProof(
        rootPosition,
        binding,
        proof,
        DirectCausalChange.Occurred,
        stake,
        actor
      )
    yield observation

  /** Rebuilds the endpoint fact from the same typed plan-result primitive used
    * by the public channel. Only the selected exact robust/refuted assessment
    * is observable; unresolved plan carriers remain incomplete.
    */
  private[chessjudgment] def fromExactPlanResult(
      rootPosition: PositionNodeRef,
      eventLine: LineNodeRef,
      source: EvidenceRef,
      event: PlanCausalEventEvidence,
      assessment: PlanCausalResultAssessment,
      graph: TypedEvidenceGraph,
      exactBinding: Option[EvidenceObjectBinding] = None,
      selectedInducedResponse: Option[PlanCausalResponse] = None
  ): Option[ComparisonEndpointEffectObservation] =
    val exactChange =
      if event.exactRefutedPublicResultAssessments.contains(assessment) then
        Some(DirectCausalChange.Refuted)
      else if event.exactRobustPublicResultAssessments.contains(assessment) then
        assessment.consequence.kind match
          case TransitionConsequenceKind.PawnTensionResolution =>
            Some(DirectCausalChange.Occurred)
          case _ if assessment.consequence.establishesState =>
            Some(DirectCausalChange.Occurred)
          case _ if assessment.consequence.removesState =>
            Some(DirectCausalChange.Lost)
          case _ =>
            None
      else None
    val proof = RootOwnedEffectProof.PlanResult(
      source,
      event,
      assessment,
      selectedInducedResponse
    )
    (for
      sourceRecord <- graph.record(source)
      _ <- Option.when(sourceRecord.payload == event && graph.proofEligible(sourceRecord))(() )
      actor <- RootCausalActor.fromPlanEvent(event)
      if RootOwnedEffectPolicy.planEventOwnsRoot(source, event, eventLine, actor.color)
      change <- exactChange
      stake <- RootOwnedEffectPolicy.effectStake(proof, actor)
    yield (actor, change, stake)).flatMap { case (actor, change, stake) =>
      val bindings = exactBinding.toList match
        case exact @ (_ :: _) => exact
        case Nil =>
          EvidenceObjectBinding
            .planAssessmentRouteBindings(source, event, actor, assessment, eventLine, proof)
            .map(_._1)
      val observations = bindings.flatMap(binding =>
        fromOwnedProof(rootPosition, binding, proof, change, stake, actor)
      )
      Option.when(
        bindings.nonEmpty &&
          observations.size == bindings.size &&
          observations.distinct.size == 1
      )(observations.head)
    }

  /** Selects an exact root -> induced reply -> opposite endpoint realization
    * already proved by one F-stage plan episode. This observes comparison
    * order only; it neither constructs nor admits a Cause.
    */
  private[chessjudgment] def exactInducedResponseMoveOrder(
      comparison: CandidateComparisonFact,
      sourceSide: RelativeCauseSourceSide,
      source: EvidenceRef,
      event: PlanCausalEventEvidence,
      graph: TypedEvidenceGraph
  ): List[(PlanCausalResultAssessment, PlanCausalResponse)] =
    event.exactRobustPublicResultAssessments.flatMap(assessment =>
      exactInducedResponseMoveOrderForAssessment(
        comparison,
        sourceSide,
        source,
        event,
        assessment,
        graph
      ).toList
    )

  private def exactInducedResponseMoveOrderForAssessment(
      comparison: CandidateComparisonFact,
      sourceSide: RelativeCauseSourceSide,
      source: EvidenceRef,
      event: PlanCausalEventEvidence,
      assessment: PlanCausalResultAssessment,
      graph: TypedEvidenceGraph
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
      sourceRecord <- graph.record(source)
      if sourceRecord.payload == event && graph.proofEligible(sourceRecord)
      canonicalLineParent <- sourceRecord.parents.filter(parentRef =>
        parentRef.producer == EvidenceProducer.LegalLineProducer &&
          parentRef.layer == EvidenceLayer.Line &&
          parentRef.position == source.position &&
          parentRef.line.contains(event.rootLine) &&
          parentRef.scope == event.rootLine.role.scope
      ) match
        case exact :: Nil => Some(exact)
        case _            => None
      canonicalLineRecord <- graph.record(canonicalLineParent)
      canonicalLine <- canonicalLineRecord match
        case EvidenceRecord(parentRef, payload: LineFactEvidence, _)
            if parentRef == canonicalLineParent && payload.line == event.rootLine &&
              graph.proofEligible(canonicalLineRecord) =>
          Some(payload)
        case _ => None
      if !EvidenceRef.sameMove(sourceLine.rootMove, oppositeLine.rootMove)
      if SemanticLineKey.sameOptional(source.line, Some(sourceLine))
      if SemanticLineKey.same(event.rootLine, sourceLine)
      if RootOwnedEffectPolicy.planEventOwnsRoot(
        source,
        event,
        sourceLine,
        comparison.comparison.mover
      )
      episode <- event.episode
      if assessment.sourceEvent != episode.root
      if assessment.sourcePlyOffset == 2
      if EvidenceRef.sameMove(assessment.sourceEvent.moveUci, oppositeLine.rootMove)
      dependencies = assessment.causalPath
      if dependencies.nonEmpty &&
        dependencies.exists(_.from == episode.root) &&
        dependencies.exists(_.to == assessment.sourceEvent) &&
        dependencies.forall(dependency =>
          dependency.planConnectionProven && dependency.enablesContinuation
        )
      rawEligibleResponses = episode.responses
        .filter(response =>
          response.trigger == episode.root &&
            response.proven &&
            response.plyOffset == 1 &&
            PrincipalVariationEvidence.sameBoardState(
              episode.root.step.fenAfter,
              response.step.fenBefore
            )
        )
      response <- rawEligibleResponses match
        case exact :: Nil => Some(exact)
        case _            => None
      if assessment.sourceEvent.step.ply == response.step.ply + 1
      if PrincipalVariationEvidence.sameBoardState(
        response.step.fenAfter,
        assessment.sourceEvent.step.fenBefore
      )
      canonicalOccurrences <- canonicalLine.lineReplaySteps.take(3) match
        case root :: induced :: result :: Nil => Some((root, induced, result))
        case _                                => None
      (canonicalRoot, canonicalResponse, canonicalResult) = canonicalOccurrences
      exactOccurrences = List(
        canonicalRoot -> episode.root.step,
        canonicalResponse -> response.step,
        canonicalResult -> assessment.sourceEvent.step
      )
      if exactOccurrences.forall { case (canonical, selected) =>
        EvidenceRef.normalizeMove(canonical.moveUci) == EvidenceRef.normalizeMove(selected.moveUci) &&
        canonical.ply == selected.ply &&
        PrincipalVariationEvidence.sameBoardState(canonical.fenBefore, selected.fenBefore) &&
        PrincipalVariationEvidence.sameBoardState(canonical.fenAfter, selected.fenAfter)
      }
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
          expectedLine.exists(line => SemanticLineKey.same(side.line, line))
      )
      .flatMap(side =>
        side.witnesses.filter(witness =>
          witness.sourceSide == sourceSide &&
            SemanticLineKey.same(witness.line, side.line)
        )
      )
      .flatMap { witness =>
        RootOwnedEffectPolicy.exactPlanResultPrimitive(witness.rootOwnedProof).toList.flatMap {
          case (source, event, assessment, selectedInducedResponse) =>
            for
              (selectedAssessment, response) <-
                exactInducedResponseMoveOrder(
                  snapshot.comparison,
                  sourceSide,
                  source,
                  event,
                  graph
                )
              if selectedAssessment == assessment
              if selectedInducedResponse.contains(response)
              if RootOwnedEffectPolicy.sameCausalRootOccurrence(
                source.position,
                snapshot.comparisonEvidence.position
              )
              actor <- RootCausalActor.fromPlanEvent(event).toList
              (expectedBinding, expectedSegment) <- EvidenceObjectBinding
                .inducedResponseMoveOrderBindings(
                  snapshot.comparison,
                  sourceSide,
                  source,
                  event,
                  actor,
                  assessment,
                  response,
                  witness.line
                )
              if witness.binding == expectedBinding
              expectedProof = RootOwnedEffectProof.PlanResult(
                source,
                event,
                assessment,
                selectedInducedResponse = Some(response)
              )
              if witness.proofSegment == expectedSegment
              if witness.effectDescriptor ==
                RootOwnedEffectDescriptorPolicy.describe(expectedBinding, expectedProof)
              expectedObservation = fromExactPlanResult(
                source.position,
                witness.line,
                source,
                event,
                assessment,
                graph,
                Some(expectedBinding),
                selectedInducedResponse = Some(response)
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
        actor <- graph.certifiedRootActorFor(line)
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
        actor <- graph.certifiedRootActorFor(comparison.candidateLine)
        stake <- RootOwnedEffectPolicy.effectStake(proof, actor)
        if proof match
          case RootOwnedEffectProof.PlanResult(source, event, assessment, selectedInducedResponse) =>
            graph.record(source).exists(record => record.payload == event && graph.proofEligible(record)) &&
              selectedInducedResponse.isEmpty &&
              event.exactRefutedPublicResultAssessments.contains(assessment) &&
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
      actor: RootCausalActor,
      axis: StrategicAxisDetail
  ): Option[ComparisonEndpointEffectObservation] =
    for
      rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(rootPosition.fen)
      stake <- RootOwnedEffectPolicy.strategicAxisStake(axis)
      change <- RootOwnedEffectPolicy.strategicAxisChange(axis)
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
        magnitude = ComparisonEndpointEffectMagnitude.QualitativePresence
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
      actor <- graph.certifiedRootActorFor(eventLine)
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
      binding: EvidenceObjectBinding,
      directChange: DirectCausalChange,
      descriptor: RootOwnedEffectDescriptor,
      stake: RootOwnedEffectStake,
      actor: RootCausalActor
  ): Option[ComparisonEndpointEffectScopeKey] =
    for
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
          DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.MateArrival(_)) =>
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
