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
    lineOccurrenceOwner: EvidenceRef,
    structuralOccurrence: ReplayStructuralOccurrence
)

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
            PawnBreakFollowUpTrajectory.find(from, response, to, between, admitted),
            CaptureResponseFollowUpTrajectory.find(from, response, to, between, admitted),
            CheckResponseFollowUpTrajectory.find(from, response, to, between, admitted)
          ).flatten
        }
        Option.when(continuations.nonEmpty)(response -> continuations)
      }.toMap
      CausalStepRelation(
        from = from,
        to = to,
        between = between,
        objectStates = canonicalReplay.toList
          .flatMap(LineObjectTrajectory.findAll(from, between :+ to, _))
          .filter(_.futureStep == to),
        lineAccess = canonicalReplay.flatMap(
          LineAccessTrajectory.findRootClearanceBeforeUse(from, to, between, _)
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
      rootLine: LineNodeRef,
      rootTransition: StructuralTransitionBinding,
      rootLineOwner: EvidenceRef,
      rootStructuralOccurrence: ReplayStructuralOccurrence,
      rootIdentity: lila.chessjudgment.model.PassedPawnResultEventIdentity,
      rootConsequences: List[TransitionConsequence],
      line: LineFactEvidence,
      trace: CausalLineTrace
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
      lineOccurrenceOwner = rootLineOwner,
      structuralOccurrence = rootStructuralOccurrence,
      structuralTransition = rootTransition,
      canonicalStep = trace.legalStep(rootStep),
      canonicalMovement = trace.transition(rootStep).map(_.relationDelta.rootMove)
    )
    fromContinuation(
      rootLine = rootLine,
      role = rootTransition.role,
      root = root,
      continuation = line.lineReplaySteps.dropWhile(_ != rootStep).drop(1),
      trace = Some(trace)
    )

  def fromContinuation(
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      root: PassedPawnResultEventNode,
      continuation: List[LineReplayStep],
      observedResultMove: Option[String] = None,
      trace: Option[CausalLineTrace] = None,
      admittedReplay: Option[CanonicalLineReplay] = None
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
            root.lineOccurrenceOwner,
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
    val observedCandidates = certifiedRoot :: rebasedContinuation.flatMap(step =>
      eventNode(resultKind, step, Some(activeTrace))
    )
    val demandedResults = observedCandidates.filter(event =>
      event != certifiedRoot && (
        observedResultMove.exists(EvidenceRef.sameMove(_, event.moveUci)) ||
          hasRootPassedPawnResult(resultKind, rootLine, role, event)
      )
    ).toSet
    val demandedPrefix = observedCandidates.lastIndexWhere(demandedResults) match
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
        dependencies(resultKind, rootLine, role, from, to, activeTrace, responses, observedResultMove)
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
    val exactResultRoutes = resultRoutes(resultKind, rootLine, role, possibleEpisode)
    val routedDependencies = exactResultRoutes.iterator.flatMap(_.causalPath).toSet
    val observedDependencies = possibleDependencies.filter(routedDependencies)
    val routeEvents = (
      certifiedRoot :: exactResultRoutes.flatMap(route =>
        route.causalPath.flatMap(dependency => List(dependency.from, dependency.to)) :+ route.sourceEvent
      )
    ).toSet
    val provenConsequencesByEvent = exactResultRoutes
      .groupMap(_.sourceEvent)(_.consequence)
      .view
      .mapValues(_.distinct)
      .toMap
    val selectedByObservation = observedCandidates
      .filter(routeEvents)
      .map { event =>
        val provenConsequences = provenConsequencesByEvent.getOrElse(event, Nil)
        val selected = selectResults(
          resultKind,
          event,
          if event == certifiedRoot then
            (certifiedRoot.structuralConsequences ++ provenConsequences).distinct
          else provenConsequences
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
        lineOccurrenceOwner = observed.lineOccurrenceOwner,
        structuralOccurrence = observed.structuralOccurrence,
        structuralTransition = observed.transition,
        canonicalStep = Some(transition.legal),
        canonicalMovement = Some(transition.relationDelta.rootMove)
      )
    }

  private def dependencies(
      resultKind: PassedPawnResultKind,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      from: PassedPawnResultEventNode,
      to: PassedPawnResultEventNode,
      trace: CausalLineTrace,
      responses: List[PassedPawnResultReply],
      observedResultMove: Option[String]
  ): List[PassedPawnResultDependency] =
    val relation = trace.relation(from.step, to.step)
    val plyOffset = to.step.ply - from.step.ply
    val exactObservedResult = observedResultMove.exists(EvidenceRef.sameMove(_, to.moveUci))
    val hasPassedPawnResult = exactObservedResult || hasRootPassedPawnResult(resultKind, rootLine, role, to)
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
        case pawn: PawnBreakFollowUpTrajectory =>
          hasPassedPawnResult ||
            to.structuralConsequences.exists(_.kind == TransitionConsequenceKind.PassedPawnProgress)
        case _: CaptureResponseFollowUpTrajectory => true
        case _: CheckResponseFollowUpTrajectory => hasPassedPawnResult
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

  private def hasRootPassedPawnResult(
      resultKind: PassedPawnResultKind,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      event: PassedPawnResultEventNode
  ): Boolean =
    event.canonicalMovement.exists(movement =>
      event.structuralConsequences.exists(consequence =>
        PassedPawnResultEventProof.candidateConsequenceForKind(
          resultKind,
          consequence,
          event.lineOccurrenceOwner,
          event.structuralOccurrence,
          event.structuralTransition,
          movement
        ).nonEmpty
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
        canonicalReplay <- trace.canonicalReplay.toList
        triggerTransition <- trace.transition(trigger.step).toList
        triggerDestination = triggerTransition.legal.move.dest
        candidates = replay.drop(triggerIndex + 1)
        whilePresent = candidates.foldLeft((true, List.empty[(LineReplayStep, CanonicalReplayTransition)])) {
          case ((false, admitted), _) => false -> admitted
          case ((true, admitted), step) =>
            trace.transition(step) match
              case None => false -> admitted
              case Some(transition) =>
                val originalActorStillPresent = !transition.boardFootprint.changedSquareSet(triggerDestination)
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
    val consequences = provenConsequences.distinct
    event.copy(
      identity = event.identity.copy(kind = resultKind),
      structuralConsequences = consequences
    )

  private def resultRoutes(
      resultKind: PassedPawnResultKind,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      episode: PassedPawnResultEpisode
  ): List[PassedPawnResultRoute] =
    episode.continuationsEnabledByRoot.flatMap { sourceEvent =>
      val transition = sourceEvent.structuralTransition
      PassedPawnResultEpisode.resultConsequences(sourceEvent).flatMap { consequence =>
        val causalPaths = episode.enablingDependencyPathsTo(sourceEvent)
        val sourceProofs = PassedPawnResultTransitionProof
          .certify(
            sourceEvent.identity,
            sourceEvent.lineOccurrenceOwner,
            sourceEvent.structuralOccurrence,
            transition,
            consequence
          )
          .toList
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
