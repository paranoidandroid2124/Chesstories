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
      assessment: PlanCausalResultAssessment
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
        EvidenceObjectBinding.planAssessmentBinding(source, event, actor, assessment, eventLine),
        proof,
        change,
        stake
      )
    yield observation

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

  private def normalize(value: String): String =
    Option(value).getOrElse("").trim.toLowerCase
