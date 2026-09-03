package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.PassedPawnResultKind
import lila.chessjudgment.model.judgment.*

/** One changed-dependency seed owns one exact structural consequence and the
  * one direct transition proof issued for it. Occurrence/path ownership is
  * added by the result-event demand; the episode builder only consumes this
  * certified lower-layer unit and never rediscovers result candidates.
  */
private[assembly] final case class PassedPawnResultTransitionSeed private (
    observation: GraphOwnedLineTransitionOccurrence,
    consequence: TransitionConsequence,
    resultProof: PassedPawnResultTransitionProof
):
  require(
    observation.consequences.count(_ == consequence) == 1,
    "a passed-pawn result seed needs one exact structural consequence"
  )
  require(
    resultProof.consequence == consequence &&
      resultProof.sourceStructuralOccurrenceOwner == observation.structuralOccurrenceOwner &&
      resultProof.sourceTransition == observation.transition &&
      resultProof.supportingDependency.isEmpty,
    "a passed-pawn result seed needs one exact direct transition proof"
  )

  def step: LineReplayStep = observation.step
  def resultKind: PassedPawnResultKind = resultProof.resultKind

  def belongsTo(trace: CausalLineTrace): Boolean =
    trace.observation(step).contains(observation) && trace.replay.contains(step)

  def proves(event: PassedPawnResultEventNode): Boolean =
    event.identity.kind == resultKind &&
      event.graphOccurrence == observation &&
      event.structuralConsequences.contains(consequence) &&
      resultProof.binds(event, consequence, Nil)

  def stableKey: String =
    BoundedCausalIdentity.digest(
      List(
        "passed-pawn-result-transition-seed:v1",
        BoundedCausalIdentity.evidenceRecordKey(observation.lineOccurrenceRecord),
        BoundedCausalIdentity.evidenceRecordKey(observation.transitionOccurrenceRecord),
        BoundedCausalIdentity.evidenceRecordKey(observation.structuralOccurrenceRecord),
        BoundedCausalIdentity.stepKey(step),
        consequence.stableKey,
        resultProof.stableKey
      )
    )

private[assembly] object PassedPawnResultTransitionSeed:
  def direct(
      observation: GraphOwnedLineTransitionOccurrence,
      consequence: TransitionConsequence
  ): List[PassedPawnResultTransitionSeed] =
    require(
      observation.consequences.count(_ == consequence) == 1,
      "one changed occurrence cannot duplicate an exact passed-pawn consequence"
    )
    val proofs = PassedPawnResultTransitionProof.directCandidates(
      observation,
      consequence
    )
    require(
      proofs.map(_.stableKey).distinct.size == proofs.size,
      "one exact passed-pawn transition proof may have only one seed producer"
    )
    proofs.map(PassedPawnResultTransitionSeed(observation, consequence, _))

private[assembly] final case class CausalStepRelation(
    from: LineReplayStep,
    to: LineReplayStep,
    between: List[LineReplayStep],
    objectStates: List[LineObjectTrajectory],
    lineAccess: Option[LineAccessTrajectory],
    responseContinuations: Map[LineReplayStep, List[CausalResponseContinuationTrajectory]]
):
  def exactlyEnables: Boolean =
    objectStates.nonEmpty ||
      lineAccess.nonEmpty ||
      responseContinuations.valuesIterator.exists(_.nonEmpty)

  def continuationsFor(response: LineReplayStep): List[CausalResponseContinuationTrajectory] =
    responseContinuations.getOrElse(response, Nil)

private[assembly] final case class CausalLineTrace(
    observations: Map[LineReplayStep, GraphOwnedLineTransitionOccurrence],
    canonicalReplay: CanonicalLineReplay
):
  def replay: List[LineReplayStep] = canonicalReplay.replaySteps
  private lazy val replayIndex = replay.zipWithIndex.toMap
  private val relationCache =
    scala.collection.mutable.Map.empty[
      (LineReplayStep, LineReplayStep),
      Option[CausalStepRelation]
    ]

  def observation(step: LineReplayStep): Option[GraphOwnedLineTransitionOccurrence] = observations.get(step)
  def lineOwnerRecord(step: LineReplayStep): Option[EvidenceRecord] =
    observations.get(step).map(_.lineOccurrenceRecord)
  def relation(from: LineReplayStep, to: LineReplayStep): Option[CausalStepRelation] =
    relationCache.getOrElseUpdate(from -> to, relationOnDemand(from, to))
  def legalStep(step: LineReplayStep): Option[lila.chessjudgment.model.line.LegalReplayStep] =
    canonicalReplay.legalStep(step)

  def transition(step: LineReplayStep): Option[CanonicalReplayTransition] =
    canonicalReplay.transition(step)
  private def relationOnDemand(
      from: LineReplayStep,
      to: LineReplayStep
  ): Option[CausalStepRelation] =
    for
      _ <- observations.get(from)
      _ <- observations.get(to)
      fromIndex <- replayIndex.get(from)
      toIndex <- replayIndex.get(to)
      if toIndex > fromIndex
    yield
      val between = replay.slice(fromIndex + 1, toIndex)
      val responseContinuations = between.flatMap { response =>
        val continuations: List[CausalResponseContinuationTrajectory] = List(
          PawnBreakFollowUpTrajectory.find(
            from,
            response,
            to,
            between,
            canonicalReplay,
            lineOwnerRecord
          ),
          CaptureResponseFollowUpTrajectory.find(
            from,
            response,
            to,
            between,
            canonicalReplay,
            lineOwnerRecord
          )
        ).flatten
        Option.when(continuations.nonEmpty)(response -> continuations)
      }.toMap
      CausalStepRelation(
        from = from,
        to = to,
        between = between,
        objectStates = LineObjectTrajectory
          .findAll(
            from,
            between :+ to,
            canonicalReplay,
            step => lineOwnerRecord(step).flatMap(CertifiedLineReplayRecord.from),
            None
          )
          .filter(_.futureStep == to),
        lineAccess = LineAccessTrajectory.findRootClearanceBeforeUse(
          from,
          to,
          between,
          canonicalReplay,
          lineOwnerRecord
        ),
        responseContinuations = responseContinuations
      )

private[assembly] object CausalLineTrace:
  def from(
      certifiedObservations: List[GraphOwnedLineTransitionOccurrence],
      admittedReplay: CanonicalLineReplay
  ): Option[CausalLineTrace] =
    val replay = admittedReplay.replaySteps
    val exactObservations = certifiedObservations.map(_.step) == replay &&
      certifiedObservations.forall(observation =>
        admittedReplay.transition(observation.step).contains(observation.canonicalTransition)
      )
    Option.when(exactObservations)(
      CausalLineTrace(
        certifiedObservations.map(value => value.step -> value).toMap,
        admittedReplay
      )
    )

private[assembly] object PassedPawnResultEpisodeBuilder:

  def fromLine(
      rootIdentity: lila.chessjudgment.model.PassedPawnResultEventIdentity,
      rootConsequences: List[TransitionConsequence],
      trace: CausalLineTrace,
      resultSeeds: List[PassedPawnResultTransitionSeed]
  ): Option[PassedPawnResultEpisode] =
    for
      rootStep <- trace.replay.headOption
      observation <- trace.observation(rootStep)
    yield
      val root = PassedPawnResultEventNode(
        identity = rootIdentity,
        perspective = observation.transition.perspective,
        structuralConsequences = rootConsequences,
        graphOccurrence = observation
      )
      fromContinuation(
        root = root,
        trace = trace,
        resultSeeds = resultSeeds
      )

  private def fromContinuation(
      root: PassedPawnResultEventNode,
      trace: CausalLineTrace,
      resultSeeds: List[PassedPawnResultTransitionSeed]
  ): PassedPawnResultEpisode =
    val resultKind = root.identity.kind
    val replay = trace.replay
    val continuation = replay.drop(1)
    require(
      replay.headOption.contains(root.step) &&
        replay.map(_.ply) == (root.step.ply to root.step.ply + replay.size - 1).toList,
      "a passed-pawn continuation must retain the exact ordered graph-owned line occurrences"
    )
    require(
      resultSeeds.map(_.stableKey).distinct.size == resultSeeds.size,
      "one exact passed-pawn result seed may be consumed only once per occurrence"
    )
    require(
      resultSeeds.forall(seed =>
        seed.resultKind == resultKind && replay.contains(seed.step) && seed.belongsTo(trace)
      ),
      "passed-pawn result seeds must belong to the exact replay, owner, and result kind"
    )
    val resultSeedsByStep = resultSeeds.groupMap(_.step)(identity)
    resultSeedsByStep.values.foreach(seeds =>
      require(
        seeds.map(_.consequence).distinct.size == seeds.size,
        "one result occurrence cannot consume duplicate passed-pawn consequences"
      )
    )
    val observedCandidates = root :: continuation.map(step =>
      eventNode(resultKind, step, trace).copy(
        structuralConsequences = resultSeedsByStep
          .getOrElse(step, Nil)
          .map(_.consequence)
      )
    )
    val demandedResultSteps = resultSeedsByStep.keySet - root.step
    val demandedPrefix = observedCandidates.lastIndexWhere(event => demandedResultSteps(event.step)) match
      case -1        => List(root)
      case lastIndex => observedCandidates.take(lastIndex + 1)
    val responsesByTrigger =
      scala.collection.mutable.Map.empty[PassedPawnResultEventNode, List[PassedPawnResultReply]]
    val (reachableCandidates, possibleDependencies) = demandedPrefix.drop(1).foldLeft(
      Set(root) -> List.empty[PassedPawnResultDependency]
    ) { case ((reachable, accepted), to) =>
      val dependenciesTo = demandedPrefix.takeWhile(_ != to).filter(reachable).flatMap { from =>
        val responses = responsesByTrigger.getOrElseUpdate(
          from,
          responsesFor(from, replay, trace)
        )
        dependencies(
          from,
          to,
          trace,
          responses,
          resultSeedsByStep
        )
          .filter(_.causalConnectionProven)
      }
      require(
        dependenciesTo.distinct.size == dependenciesTo.size,
        "one exact passed-pawn-result dependency may be produced only once"
      )
      if dependenciesTo.nonEmpty then
        (reachable + to) -> (accepted ++ dependenciesTo)
      else reachable -> accepted
    }
    val observedResponses = observedCandidates.flatMap(responsesByTrigger.getOrElse(_, Nil))
    val possibleEpisode = PassedPawnResultEpisode(
      root = root,
      continuations = observedCandidates.filter(event => event != root && reachableCandidates(event)),
      dependencies = possibleDependencies,
      responses = observedResponses.filter(_.proven)
    )
    val exactResultRoutes = resultRoutes(
      resultKind,
      possibleEpisode,
      resultSeeds
    )
    val routedDependencies = exactResultRoutes.iterator.flatMap(_.causalPath).toSet
    val observedDependencies = possibleDependencies.filter(routedDependencies)
    val routeEvents = (
      root :: exactResultRoutes.flatMap(route =>
        route.causalPath.flatMap(dependency => List(dependency.from, dependency.to)) :+ route.sourceEvent
      )
    ).toSet
    val routedConsequencesByEvent = exactResultRoutes
      .groupMap(_.sourceEvent)(_.consequence)
      .view
      .mapValues(_.toSet)
      .toMap
    val provenConsequencesByEvent = observedCandidates.map(event =>
      event -> event.structuralConsequences.filter(
        routedConsequencesByEvent.getOrElse(event, Set.empty)
      )
    ).toMap
    val selectedByObservation = observedCandidates
      .filter(routeEvents)
      .map { event =>
        val provenConsequences = provenConsequencesByEvent.getOrElse(event, Nil)
        val selected = selectResults(
          resultKind,
          event,
          if event == root then root.structuralConsequences else provenConsequences
        )
        event -> (if event == root then selected.copy(identity = root.identity) else selected)
      }
      .toMap
    val selectedDependencies = observedDependencies
      .filter(dependency => selectedByObservation.contains(dependency.from) && selectedByObservation.contains(dependency.to))
      .map(dependency => dependency.copy(
        from = selectedByObservation(dependency.from),
        to = selectedByObservation(dependency.to)
      ))
      .filter(_.causalConnectionProven)
    val selectedResponses = observedResponses
      .filter(response => selectedByObservation.contains(response.trigger))
      .map(response => response.copy(
        trigger = selectedByObservation(response.trigger)
      ))
      .filter(_.proven)
    val selectedRoot = selectedByObservation.getOrElse(root, root)
    def selectedEvent(event: PassedPawnResultEventNode): PassedPawnResultEventNode =
      selectedByObservation.getOrElse(event, event)
    val selectedResultRoutes = exactResultRoutes.flatMap(_.withMappedEvents(selectedEvent))
    require(
      selectedResultRoutes.size == exactResultRoutes.size,
      "a selected result route must retain its exact supporting dependency"
    )
    PassedPawnResultEpisode(
      root = selectedRoot,
      continuations = selectedByObservation.values
        .filterNot(_ == selectedRoot)
        .toList
        .sortBy(_.step.ply),
      dependencies = selectedDependencies,
      responses = selectedResponses,
      resultRoutes = selectedResultRoutes
    )

  private def eventNode(
      resultKind: PassedPawnResultKind,
      step: LineReplayStep,
      trace: CausalLineTrace
  ): PassedPawnResultEventNode =
    val observed = trace.observation(step).getOrElse(
      throw IllegalArgumentException("a passed-pawn event needs its graph-owned structural observation")
    )
    PassedPawnResultEventNode(
      identity = PassedPawnResultEventIdentityBuilder.from(
        rootMove = step.moveUci,
        actor = observed.movement,
        kind = resultKind
      ),
      perspective = observed.transition.perspective,
      structuralConsequences = observed.consequences,
      graphOccurrence = observed
    )

  private def dependencies(
      from: PassedPawnResultEventNode,
      to: PassedPawnResultEventNode,
      trace: CausalLineTrace,
      responses: List[PassedPawnResultReply],
      resultSeedsByStep: Map[LineReplayStep, List[PassedPawnResultTransitionSeed]]
  ): List[PassedPawnResultDependency] =
    val relation = trace.relation(from.step, to.step)
    val plyOffset = to.step.ply - from.step.ply
    val hasPassedPawnResult =
      resultSeedsByStep.getOrElse(to.step, Nil).exists(_.proves(to))
    val structuralDependencies = relation.toList.flatMap(relation =>
      relation.objectStates.map(objectStateDependency(from, to, _, trace)) ++
        relation.lineAccess.map(lineAccessDependency(from, to, _, trace)).toList
    )
    val responseContinuations = responses
      .filter(response =>
        response.trigger == from &&
          response.step.ply > from.step.ply &&
          response.step.ply < to.step.ply
      )
      .flatMap(response => relation.toList.flatMap(_.continuationsFor(response.step)))
      .filter {
        case _: PawnBreakFollowUpTrajectory => hasPassedPawnResult
        case _: CaptureResponseFollowUpTrajectory => true
      }
      .map(trajectory =>
        PassedPawnResultDependency(
          from,
          to,
          PassedPawnResultDependencyKind.ResponseContinuationPrecondition,
          PassedPawnResultDependencyProof.ResponseContinuation(trajectory),
          plyOffset,
          graphOccurrencesFor(from, to, trajectory.interveningSteps, trace)
        )
      )
    structuralDependencies ++ responseContinuations

  private def objectStateDependency(
      from: PassedPawnResultEventNode,
      to: PassedPawnResultEventNode,
      trajectory: LineObjectTrajectory,
      trace: CausalLineTrace
  ): PassedPawnResultDependency =
    val plyOffset = to.step.ply - from.step.ply
    PassedPawnResultDependency(
      from,
      to,
      PassedPawnResultDependencyKind.ObjectStatePrecondition,
      PassedPawnResultDependencyProof.ObjectState(trajectory),
      plyOffset,
      graphOccurrencesFor(from, to, trajectory.interveningSteps, trace)
    )

  private def lineAccessDependency(
      from: PassedPawnResultEventNode,
      to: PassedPawnResultEventNode,
      trajectory: LineAccessTrajectory,
      trace: CausalLineTrace
  ): PassedPawnResultDependency =
    val plyOffset = to.step.ply - from.step.ply
    PassedPawnResultDependency(
      from,
      to,
      PassedPawnResultDependencyKind.LineAccessPrecondition,
      PassedPawnResultDependencyProof.LineAccess(trajectory),
      plyOffset,
      graphOccurrencesFor(from, to, trajectory.interveningSteps, trace)
    )

  private def graphOccurrencesFor(
      from: PassedPawnResultEventNode,
      to: PassedPawnResultEventNode,
      intervening: List[LineReplayStep],
      trace: CausalLineTrace
  ): List[GraphOwnedLineTransitionOccurrence] =
    ((from.step :: intervening) :+ to.step).distinct.map(step =>
      trace.observation(step).getOrElse(
        throw IllegalArgumentException("a passed-pawn dependency needs every graph-owned proof occurrence")
      )
    )

  private def responsesFor(
      trigger: PassedPawnResultEventNode,
      replay: List[LineReplayStep],
      trace: CausalLineTrace
  ): List[PassedPawnResultReply] =
    val triggerIndex = replay.indexOf(trigger.step)
    if triggerIndex < 0 then Nil
    else
      (for
        triggerTransition <- trace.transition(trigger.step).toList
        triggerDestination = triggerTransition.relationDelta.rootMove.to
        candidates = replay.drop(triggerIndex + 1)
        whilePresent = candidates.foldLeft((true, List.empty[(LineReplayStep, CanonicalReplayTransition)])) {
          case ((false, admitted), _) => false -> admitted
          case ((true, admitted), step) =>
            trace.transition(step) match
              case None => false -> admitted
              case Some(transition) =>
                val originalActorStillPresent = !transition.boardFootprint.changedSquares.exists(
                  _.key.equalsIgnoreCase(triggerDestination.key)
                )
                originalActorStillPresent -> ((step -> transition) :: admitted)
        }._2.reverse
        (step, _) <- whilePresent
        occurrence <- trace.observation(step).toList
        response <- PassedPawnResultReply
          .certified(trigger, occurrence, triggerTransition, trace.canonicalReplay)
          .toList
      yield response)

  private def selectResults(
      resultKind: PassedPawnResultKind,
      event: PassedPawnResultEventNode,
      provenConsequences: List[TransitionConsequence]
  ): PassedPawnResultEventNode =
    require(
      provenConsequences.distinct.size == provenConsequences.size,
      "one exact result event cannot select a consequence more than once"
    )
    event.copy(
      identity = event.identity.copy(kind = resultKind),
      structuralConsequences = provenConsequences
    )

  private def resultRoutes(
      resultKind: PassedPawnResultKind,
      episode: PassedPawnResultEpisode,
      resultSeeds: List[PassedPawnResultTransitionSeed]
  ): List[PassedPawnResultRoute] =
    episode.continuationsEnabledByRoot.flatMap { sourceEvent =>
      PassedPawnResultEpisode.resultConsequences(sourceEvent).flatMap { consequence =>
        val causalPaths = episode.enablingDependencyPathsTo(sourceEvent)
        val sourceProofs = resultSeeds
          .filter(seed => seed.consequence == consequence && seed.proves(sourceEvent))
          .map(_.resultProof)
        require(
          sourceProofs.nonEmpty,
          "a passed-pawn result route cannot promote an unseeded consequence"
        )
        val dependencyProofs = episode
          .enablingDependenciesTo(sourceEvent)
          .flatMap(dependency =>
            PassedPawnResultTransitionProof.certifyDependency(
              resultKind,
              sourceEvent,
              dependency,
              consequence
            )
          )
        val exactProofs = sourceProofs ++ dependencyProofs
        require(
          exactProofs.distinct.size == exactProofs.size,
          "one exact passed-pawn-result transition proof may be produced only once"
        )
        causalPaths.flatMap { causalPath =>
          exactProofs.flatMap(proof =>
            PassedPawnResultRoute.certified(sourceEvent, consequence, causalPath, proof)
          )
        }
      }
    }.sortBy(_.stableKey)
