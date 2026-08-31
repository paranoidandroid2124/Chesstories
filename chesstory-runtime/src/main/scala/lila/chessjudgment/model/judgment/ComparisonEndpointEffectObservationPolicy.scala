package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.PrincipalVariationEvidence
private[chessjudgment] object ComparisonEndpointEffectObservationPolicy:

  enum MagnitudeRelation:
    case Equal
    case LeftStrictlyStronger
    case RightStrictlyStronger
    case Incomparable

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
        channel.proofSegment == witness.proofSegment &&
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
      case _ => Incomparable

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
            LineConsequenceKind.Promotion =>
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

  /** Rebuilds the endpoint fact from the same typed passed-pawn-result primitive used
    * by the public channel. Only the selected exact robust assessment is
    * observable; unresolved passed-pawn-result carriers remain incomplete.
    */
  private[chessjudgment] def fromExactPassedPawnResult(
      rootPosition: PositionNodeRef,
      eventLine: LineNodeRef,
      source: EvidenceRef,
      result: PassedPawnResultProofEvidence,
      graph: TypedEvidenceGraph,
      exactBinding: Option[EvidenceObjectBinding] = None
  ): Option[ComparisonEndpointEffectObservation] =
    val exactChange =
      if RelativeCauseKind.passedPawnResultProofCanProveCause(RelativeCauseKind.PassedPawnResult, result) then
        result.assessment.consequence.kind match
          case TransitionConsequenceKind.PawnTensionResolution =>
            Some(DirectCausalChange.Occurred)
          case _ if result.assessment.consequence.establishesState =>
            Some(DirectCausalChange.Occurred)
          case _ if result.assessment.consequence.removesState =>
            Some(DirectCausalChange.Lost)
          case _ =>
            None
      else None
    val proof = RootOwnedEffectProof.PassedPawnResult(source, result)
    (for
      sourceRecord <- graph.record(source)
      _ <- Option.when(sourceRecord.payload == result && graph.proofEligible(sourceRecord))(() )
      actor <- RootCausalActor.fromPassedPawnResultEvent(result.event)
      if RootOwnedEffectPolicy.passedPawnResultOwnsEventRoot(graph, source, result, eventLine, actor)
      change <- exactChange
      stake <- RootOwnedEffectPolicy.effectStake(proof, actor)
    yield (actor, change, stake)).flatMap { case (actor, change, stake) =>
      val bindings = exactBinding.toList match
        case exact @ (_ :: _) => exact
        case Nil =>
          EvidenceObjectBinding
            .passedPawnResultRouteBindings(
              source,
              result.event,
              actor,
              result.assessment,
              eventLine,
              proof
            )
            .map { case (binding, _) =>
              binding.copy(provenance = result.proofParentSources)
            }
      val observations = bindings.flatMap(binding =>
        fromOwnedProof(rootPosition, binding, proof, change, stake, actor)
      )
      Option.when(
        bindings.nonEmpty &&
          observations.size == bindings.size &&
          observations.distinct.size == 1
      )(observations.head)
    }

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
          Nil
        )
      case DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.MateArrival(_)) =>
        RootOwnedEffectIdentity(
          RootOwnedEffectPrimitiveKind.LineEpisode,
          List("outcome:mate"),
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
