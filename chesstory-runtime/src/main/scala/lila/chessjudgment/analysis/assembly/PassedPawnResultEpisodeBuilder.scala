package lila.chessjudgment.analysis.assembly

import chess.Pawn
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.PassedPawnResultKind
import lila.chessjudgment.model.judgment.*

private[assembly] final case class CausalStepObservation(
    step: LineReplayStep,
    transition: StructuralTransitionBinding,
    consequences: List[TransitionConsequence],
    movement: CanonicalRootLegalMove,
    lineOccurrenceRecord: EvidenceRecord,
    structuralOccurrence: ReplayStructuralOccurrence
):
  def lineOccurrenceOwner: EvidenceRef = lineOccurrenceRecord.ref

/** One changed-dependency seed owns one exact structural consequence and the
  * one direct transition proof issued for it. Occurrence/path ownership is
  * added by the result-event demand; the episode builder only consumes this
  * certified lower-layer unit and never rediscovers result candidates.
  */
private[assembly] final case class PassedPawnResultTransitionSeed private (
    observation: CausalStepObservation,
    consequence: TransitionConsequence,
    resultProof: PassedPawnResultTransitionProof
):
  require(
    observation.consequences.count(_ == consequence) == 1,
    "a passed-pawn result seed needs one exact structural consequence"
  )
  require(
    resultProof.consequence == consequence &&
      resultProof.sourceLineOccurrenceOwner == observation.lineOccurrenceOwner &&
      resultProof.sourceOccurrenceId == observation.structuralOccurrence.occurrenceId &&
      resultProof.sourcePremiseKeys == observation.structuralOccurrence.sourcePremiseKeys &&
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
      event.step == step &&
      event.lineOccurrenceRecord == observation.lineOccurrenceRecord &&
      event.structuralOccurrence == observation.structuralOccurrence &&
      event.structuralTransition == observation.transition &&
      event.canonicalMovement.contains(observation.movement) &&
      event.structuralConsequences.contains(consequence) &&
      resultProof.binds(event, consequence, Nil)

  def stableKey: String =
    BoundedCausalIdentity.digest(
      List(
        "passed-pawn-result-transition-seed:v1",
        BoundedCausalIdentity.evidenceRecordKey(observation.lineOccurrenceRecord),
        BoundedCausalIdentity.stepKey(step),
        consequence.stableKey,
        resultProof.stableKey
      )
    )

private[assembly] object PassedPawnResultTransitionSeed:
  def direct(
      observation: CausalStepObservation,
      consequence: TransitionConsequence
  ): List[PassedPawnResultTransitionSeed] =
    require(
      observation.consequences.count(_ == consequence) == 1,
      "one changed occurrence cannot duplicate an exact passed-pawn consequence"
    )
    val proofs = PassedPawnResultTransitionProof.directCandidates(
      observation.lineOccurrenceOwner,
      observation.structuralOccurrence,
      observation.transition,
      consequence,
      observation.movement
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
    replay: List[LineReplayStep],
    observations: Map[LineReplayStep, CausalStepObservation],
    canonicalReplay: Option[CanonicalLineReplay]
):
  private lazy val replayIndex = replay.zipWithIndex.toMap
  private val relationCache =
    scala.collection.mutable.Map.empty[
      (LineReplayStep, LineReplayStep),
      Option[CausalStepRelation]
    ]

  def observation(step: LineReplayStep): Option[CausalStepObservation] = observations.get(step)
  def lineOwnerRecord(step: LineReplayStep): Option[EvidenceRecord] =
    observations.get(step).map(_.lineOccurrenceRecord)
  def relation(from: LineReplayStep, to: LineReplayStep): Option[CausalStepRelation] =
    relationCache.getOrElseUpdate(from -> to, relationOnDemand(from, to))
  def legalStep(step: LineReplayStep): Option[lila.chessjudgment.model.line.LegalReplayStep] =
    canonicalReplay.flatMap(_.legalStep(step))

  def transition(step: LineReplayStep): Option[CanonicalReplayTransition] =
    canonicalReplay.flatMap(_.transition(step))
  def indexOf(step: LineReplayStep): Option[Int] = replayIndex.get(step)

  private[assembly] def computedRelationCount: Int = relationCache.size

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
        val continuations: List[CausalResponseContinuationTrajectory] = canonicalReplay.toList.flatMap { admitted =>
          List(
            PawnBreakFollowUpTrajectory.find(
              from,
              response,
              to,
              between,
              admitted,
              lineOwnerRecord
            ),
            CaptureResponseFollowUpTrajectory.find(from, response, to, between, admitted, lineOwnerRecord)
          ).flatten
        }
        Option.when(continuations.nonEmpty)(response -> continuations)
      }.toMap
      CausalStepRelation(
        from = from,
        to = to,
        between = between,
        objectStates = canonicalReplay.toList
          .flatMap(replay =>
            LineObjectTrajectory.findAll(
              from,
              between :+ to,
              replay,
              lineOwnerRecord
            )
          )
          .filter(_.futureStep == to),
        lineAccess = canonicalReplay.flatMap(admitted =>
          LineAccessTrajectory.findRootClearanceBeforeUse(
            from,
            to,
            between,
            admitted,
            lineOwnerRecord
          )
        ),
        responseContinuations = responseContinuations
      )

private[assembly] object CausalLineTrace:
  def from(
      replay: List[LineReplayStep],
      certifiedObservations: List[CausalStepObservation],
      admittedReplay: Option[CanonicalLineReplay] = None
  ): CausalLineTrace =
    val canonicalReplay = admittedReplay.filter(_.matches(replay))
    val observationsByStep = certifiedObservations.groupBy(_.step)
    require(
      observationsByStep.values.forall(_.size == 1),
      "one causal replay step cannot have multiple observation producers"
    )
    val observed = observationsByStep
      .valuesIterator
      .map(_.head)
      .filter(observation =>
        replay.contains(observation.step) && canonicalReplay.exists(_.transition(observation.step).exists(
          canonical =>
            canonical.relationDelta.rootMove == observation.movement &&
              canonical.structuralOccurrence == observation.structuralOccurrence &&
              EvidenceRef.sameMove(canonical.legal.uci, observation.transition.moveUci) &&
              canonical.legal.ply == observation.step.ply
        ))
      )
      .toList
    CausalLineTrace(
      replay,
      observed.map(value => value.step -> value).toMap,
      canonicalReplay
    )

private[assembly] object PassedPawnResultEpisodeBuilder:

  def fromLine(
      rootTransition: StructuralTransitionBinding,
      rootLineRecord: EvidenceRecord,
      rootStructuralOccurrence: ReplayStructuralOccurrence,
      rootIdentity: lila.chessjudgment.model.PassedPawnResultEventIdentity,
      rootConsequences: List[TransitionConsequence],
      line: LineFactEvidence,
      trace: CausalLineTrace,
      resultSeeds: List[PassedPawnResultTransitionSeed]
  ): PassedPawnResultEpisode =
    val rootStep = line.lineReplaySteps.headOption
      .filter(step => EvidenceRef.sameMove(step.moveUci, rootTransition.moveUci))
      .getOrElse(
        LineReplayStep(
          ply = rootTransition.to.ply,
          moveUci = rootTransition.moveUci,
          fenBefore = rootTransition.from.fen,
          fenAfter = rootTransition.to.fen
        )
      )
    val root = PassedPawnResultEventNode(
      identity = rootIdentity,
      step = rootStep,
      perspective = rootTransition.perspective,
      structuralConsequences = rootConsequences,
      lineOccurrenceRecord = rootLineRecord,
      structuralOccurrence = rootStructuralOccurrence,
      structuralTransition = rootTransition,
      canonicalStep = trace.legalStep(rootStep),
      canonicalMovement = trace.transition(rootStep).map(_.relationDelta.rootMove)
    )
    fromContinuation(
      root = root,
      continuation = line.lineReplaySteps.dropWhile(_ != rootStep).drop(1),
      trace = Some(trace),
      resultSeeds = resultSeeds
    )

  def fromContinuation(
      root: PassedPawnResultEventNode,
      continuation: List[LineReplayStep],
      trace: Option[CausalLineTrace] = None,
      admittedReplay: Option[CanonicalLineReplay] = None,
      resultSeeds: List[PassedPawnResultTransitionSeed] = Nil
  ): PassedPawnResultEpisode =
    val resultKind = root.identity.kind
    val lineOnlyContinuation =
      continuation match
        case head :: tail if sameStructuralStep(head, root.step) => tail
        case steps                                               => steps
    val rebasedContinuation = lineOnlyContinuation.zipWithIndex.map { case (step, index) =>
      step.copy(ply = root.step.ply + index + 1)
    }
    val replay = root.step :: rebasedContinuation
    val activeTrace = trace.getOrElse {
      val exactAdmittedReplay = admittedReplay.flatMap(replayValue =>
        replayValue.subset(replay).orElse(Option.when(replayValue.matches(replay))(replayValue))
      )
      CausalLineTrace.from(
        replay,
        root.canonicalMovement.map(movement =>
          CausalStepObservation(
            root.step,
            root.structuralTransition,
            root.structuralConsequences,
            movement,
            root.lineOccurrenceRecord,
            root.structuralOccurrence
          )
        ).toList,
        admittedReplay = exactAdmittedReplay
      )
    }
    val certifiedRoot = root.copy(
      canonicalStep = root.certifiedLegalStep.orElse(activeTrace.legalStep(root.step)),
      canonicalMovement = root.canonicalMovement.orElse(
        activeTrace.transition(root.step).map(_.relationDelta.rootMove)
      )
    )
    require(
      resultSeeds.map(_.stableKey).distinct.size == resultSeeds.size,
      "one exact passed-pawn result seed may be consumed only once per occurrence"
    )
    require(
      resultSeeds.forall(seed =>
        seed.resultKind == resultKind && replay.contains(seed.step) && seed.belongsTo(activeTrace)
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
    val observedCandidates = certifiedRoot :: rebasedContinuation.flatMap(step =>
      eventNode(resultKind, step, Some(activeTrace)).map(event =>
        event.copy(
          structuralConsequences = resultSeedsByStep
            .getOrElse(step, Nil)
            .map(_.consequence)
        )
      )
    )
    val demandedResultSteps = resultSeedsByStep.keySet - certifiedRoot.step
    val demandedPrefix = observedCandidates.lastIndexWhere(event => demandedResultSteps(event.step)) match
      case -1        => List(certifiedRoot)
      case lastIndex => observedCandidates.take(lastIndex + 1)
    val responsesByTrigger =
      scala.collection.mutable.Map.empty[PassedPawnResultEventNode, List[PassedPawnResultReply]]
    val (reachableCandidates, possibleDependencies) = demandedPrefix.drop(1).foldLeft(
      Set(certifiedRoot) -> List.empty[PassedPawnResultDependency]
    ) { case ((reachable, accepted), to) =>
      val dependenciesTo = demandedPrefix.takeWhile(_ != to).filter(reachable).flatMap { from =>
        val responses = responsesByTrigger.getOrElseUpdate(
          from,
          responsesFor(from, replay, activeTrace)
        )
        dependencies(
          from,
          to,
          activeTrace,
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
      root = certifiedRoot,
      continuations = observedCandidates.filter(event => event != certifiedRoot && reachableCandidates(event)),
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
      certifiedRoot :: exactResultRoutes.flatMap(route =>
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
          if event == certifiedRoot then certifiedRoot.structuralConsequences else provenConsequences
        )
        event -> (if event == certifiedRoot then selected.copy(identity = certifiedRoot.identity) else selected)
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
    val selectedRoot = selectedByObservation.getOrElse(certifiedRoot, certifiedRoot)
    def selectedEvent(event: PassedPawnResultEventNode): PassedPawnResultEventNode =
      selectedByObservation.getOrElse(event, event)
    val selectedResultRoutes = exactResultRoutes.map(_.withMappedEvents(selectedEvent))
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

  private def sameStructuralStep(left: LineReplayStep, right: LineReplayStep): Boolean =
    EvidenceRef.sameMove(left.moveUci, right.moveUci) &&
      PrincipalVariationEvidence.sameBoardState(left.fenBefore, right.fenBefore) &&
      PrincipalVariationEvidence.sameBoardState(left.fenAfter, right.fenAfter)

  private def eventNode(
      resultKind: PassedPawnResultKind,
      step: LineReplayStep,
      trace: Option[CausalLineTrace] = None
  ): Option[PassedPawnResultEventNode] =
    val resolved = trace
      .flatMap(traceValue =>
        for
          observed <- traceValue.observation(step)
          transition <- traceValue.transition(step)
        yield (observed, transition)
      )
    resolved.map { case (observed, transition) =>
      PassedPawnResultEventNode(
        identity = PassedPawnResultEventIdentityBuilder.from(
          rootMove = step.moveUci,
          actor = transition.relationDelta.rootMove,
          kind = resultKind
        ),
        step = step,
        perspective = observed.transition.perspective,
        structuralConsequences = observed.consequences,
        lineOccurrenceRecord = observed.lineOccurrenceRecord,
        structuralOccurrence = observed.structuralOccurrence,
        structuralTransition = observed.transition,
        canonicalStep = Some(transition.legal),
        canonicalMovement = Some(transition.relationDelta.rootMove)
      )
    }

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
      relation.objectStates.map(objectStateDependency(from, to, _)) ++
        relation.lineAccess.map(lineAccessDependency(from, to, _)).toList
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
          plyOffset
        )
      )
    structuralDependencies ++ responseContinuations

  private def objectStateDependency(
      from: PassedPawnResultEventNode,
      to: PassedPawnResultEventNode,
      trajectory: LineObjectTrajectory
  ): PassedPawnResultDependency =
    val plyOffset = to.step.ply - from.step.ply
    PassedPawnResultDependency(
      from,
      to,
      PassedPawnResultDependencyKind.ObjectStatePrecondition,
      PassedPawnResultDependencyProof.ObjectState(trajectory),
      plyOffset
    )

  private def lineAccessDependency(
      from: PassedPawnResultEventNode,
      to: PassedPawnResultEventNode,
      trajectory: LineAccessTrajectory
  ): PassedPawnResultDependency =
    val plyOffset = to.step.ply - from.step.ply
    PassedPawnResultDependency(
      from,
      to,
      PassedPawnResultDependencyKind.LineAccessPrecondition,
      PassedPawnResultDependencyProof.LineAccess(trajectory),
      plyOffset
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
        canonicalReplay <- trace.canonicalReplay.toList
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
                originalActorStillPresent -> (admitted :+ (step -> transition))
        }._2
        (step, responseTransition) <- whilePresent
        response <- PassedPawnResultReply
          .certified(trigger, step, triggerTransition, responseTransition, canonicalReplay)
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
      val transition = sourceEvent.structuralTransition
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
              sourceEvent.lineOccurrenceOwner,
              sourceEvent.structuralOccurrence,
              transition,
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
