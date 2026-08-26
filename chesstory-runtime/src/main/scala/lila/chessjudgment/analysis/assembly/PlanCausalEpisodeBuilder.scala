package lila.chessjudgment.analysis.assembly

import chess.{ Color, Pawn, Position, Square }
import lila.chessjudgment.analysis.structure.StructuralDeltaContracts
import lila.chessjudgment.model.Plan
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PrincipalVariationEvidence }
import lila.chessjudgment.model.position.PositionFeatures
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }
import lila.chessjudgment.model.judgment.*

private[assembly] final case class CausalStepObservation(
    step: LineReplayStep,
    transition: StructuralTransitionBinding,
    consequences: List[TransitionConsequence],
    actorRole: Option[chess.Role]
)

private[assembly] final case class CausalStepRelation(
    from: LineReplayStep,
    to: LineReplayStep,
    between: List[LineReplayStep],
    objectState: Option[LineObjectTrajectory],
    lineAccess: Option[LineAccessTrajectory],
    responseContinuations: Map[LineReplayStep, List[PlanResponseContinuationTrajectory]]
):
  def exactlyEnables: Boolean =
    objectState.nonEmpty ||
      lineAccess.nonEmpty ||
      responseContinuations.valuesIterator.exists(_.nonEmpty)

  def continuationsFor(response: LineReplayStep): List[PlanResponseContinuationTrajectory] =
    responseContinuations.getOrElse(response, Nil)

private[assembly] final case class ReplayOccurrenceKey private (
    canonicalFen: String,
    ply: Int
)

private[assembly] object ReplayOccurrenceKey:
  def from(fen: String, ply: Int): ReplayOccurrenceKey =
    ReplayOccurrenceKey(
      PrincipalVariationEvidence.normalizeFen(fen),
      ply
    )

private[assembly] final case class CausalLineTrace(
    replay: List[LineReplayStep],
    observations: Map[LineReplayStep, CausalStepObservation],
    relations: Map[(LineReplayStep, LineReplayStep), CausalStepRelation],
    positionFacts: Map[ReplayOccurrenceKey, CausalPositionFacts],
    canonicalReplay: Option[CanonicalLineReplay]
):
  private lazy val replayIndex = replay.zipWithIndex.toMap

  def observation(step: LineReplayStep): Option[CausalStepObservation] = observations.get(step)
  def relation(from: LineReplayStep, to: LineReplayStep): Option[CausalStepRelation] = relations.get(from -> to)
  def legalStep(step: LineReplayStep): Option[lila.chessjudgment.model.line.LegalReplayStep] =
    canonicalReplay.flatMap(_.legalStep(step))
  def indexOf(step: LineReplayStep): Option[Int] = replayIndex.get(step)
  def positionFeatures(fen: String, ply: Int): Option[PositionFeatures] =
    positionFacts.get(ReplayOccurrenceKey.from(fen, ply)).map(_.features)
  def position(fen: String, ply: Int): Option[Position] =
    positionFacts.get(ReplayOccurrenceKey.from(fen, ply)).map(_.position)
  def positionAnalysis(
      fen: String,
      ply: Int
  ): Option[lila.chessjudgment.analysis.position.PositionAnalysis] =
    positionFacts.get(ReplayOccurrenceKey.from(fen, ply)).map(_.analysis)

private[assembly] final case class CausalPositionFacts(
    analysis: lila.chessjudgment.analysis.position.PositionAnalysis
):
  def position: Position = analysis.position
  def features: PositionFeatures = analysis.features

private[assembly] object CausalLineTrace:
  def from(
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      perspective: Color,
      replay: List[LineReplayStep],
      rootObservation: Option[CausalStepObservation] = None,
      admittedReplay: Option[CanonicalLineReplay] = None
  ): CausalLineTrace =
    val canonicalReplay = admittedReplay.filter(_.matches(replay))
    val positionFacts = canonicalReplay.map(positionFactsFor).getOrElse(Map.empty)
    val observed = replay.flatMap { step =>
      rootObservation.filter(_.step == step).orElse(
        canonicalReplay.flatMap(observeWithCanonicalTransition(rootLine, role, perspective, step, _))
      )
    }
    val replayIndex = replay.zipWithIndex.toMap
    val relations = observed.zipWithIndex.flatMap { case (from, observedIndex) =>
      observed.drop(observedIndex + 1).flatMap(to =>
        for
          fromIndex <- replayIndex.get(from.step)
          toIndex <- replayIndex.get(to.step)
          if toIndex > fromIndex
        yield
          val between = replay.slice(fromIndex + 1, toIndex)
          val responseContinuations = between.flatMap { response =>
            val continuations: List[PlanResponseContinuationTrajectory] = canonicalReplay.toList.flatMap { admitted =>
              List(
                PawnBreakFollowUpTrajectory.find(from.step, response, to.step, between, admitted),
                CaptureResponseFollowUpTrajectory.find(from.step, response, to.step, between, admitted),
                CheckResponseFollowUpTrajectory.find(from.step, response, to.step, between, admitted)
              ).flatten
            }
            Option.when(continuations.nonEmpty)(response -> continuations)
          }.toMap
          val relation = CausalStepRelation(
            from = from.step,
            to = to.step,
            between = between,
            objectState = canonicalReplay
              .flatMap(LineObjectTrajectory.find(from.step, between :+ to.step, (between.size + 1).max(1), _))
              .filter(_.futureStep == to.step),
            lineAccess = canonicalReplay.flatMap(
              LineAccessTrajectory.findRootClearanceBeforeUse(from.step, to.step, between, _)
            ),
            responseContinuations = responseContinuations
          )
          (from.step -> to.step) -> relation
      )
    }.toMap
    CausalLineTrace(
      replay,
      observed.map(value => value.step -> value).toMap,
      relations,
      positionFacts,
      canonicalReplay
    )

  private def positionFactsFor(replay: CanonicalLineReplay): Map[ReplayOccurrenceKey, CausalPositionFacts] =
    replay.replaySteps
      .flatMap(step =>
        List(
          replay.analysisBefore(step).map(analysis => ((step.fenBefore, step.ply - 1), analysis)),
          replay.analysisAfter(step).map(analysis => ((step.fenAfter, step.ply), analysis))
        ).flatten
      )
      .foldLeft(Map.empty[ReplayOccurrenceKey, CausalPositionFacts]) {
        case (facts, ((fen, ply), analysis)) =>
          val key = ReplayOccurrenceKey.from(fen, ply)
          facts.get(key) match
            case None => facts.updated(key, CausalPositionFacts(analysis))
            case Some(existing) if existing.analysis.asInstanceOf[AnyRef] eq analysis.asInstanceOf[AnyRef] =>
              facts
            case Some(_) =>
              throw IllegalArgumentException(
                s"canonical replay supplied conflicting analyses for occurrence $key"
              )
      }

  private[assembly] def observe(
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      perspective: Color,
      step: LineReplayStep,
      replay: CanonicalLineReplay
  ): Option[CausalStepObservation] =
    Option
      .when(replay.matches(List(step)))(replay)
      .flatMap(observeWithCanonicalTransition(rootLine, role, perspective, step, _))

  private def observeWithCanonicalTransition(
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      perspective: Color,
      step: LineReplayStep,
      canonicalReplay: CanonicalLineReplay
  ): Option[CausalStepObservation] =
    for
      replayTransition <- canonicalReplay.transition(step)
      legalStep = replayTransition.legal
      beforeFacts = CausalPositionFacts(replayTransition.beforeAnalysis)
      if beforeFacts.position.color == perspective
      afterFacts = CausalPositionFacts(replayTransition.afterAnalysis)
      delta = replayTransition.structuralDelta
    yield CausalStepObservation(
      step = step,
      transition = StructuralTransitionBinding(
        moveUci = step.moveUci,
        role = role,
        from = PositionNodeRef(step.fenBefore, step.ply - 1, Some(beforeFacts.position.color)),
        to = PositionNodeRef(step.fenAfter, step.ply, Some(afterFacts.position.color)),
        line = Some(rootLine),
        perspective = perspective,
        actorRole = Square.fromKey(EvidenceRef.normalizeMove(step.moveUci).take(2))
          .flatMap(beforeFacts.position.board.roleAt)
          .map(role => EvidencePieceRole(role.name))
      ),
      consequences = StructuralDeltaContracts
        .consequences(delta)
        .filter(consequence => consequence.establishesState && consequence.strength > 0),
      actorRole = Square.fromKey(EvidenceRef.normalizeMove(step.moveUci).take(2))
        .flatMap(beforeFacts.position.board.roleAt)
    )

private[assembly] object PlanCausalEpisodeBuilder:

  def fromLine(
      plan: Plan,
      rootLine: LineNodeRef,
      rootTransition: StructuralTransitionBinding,
      rootIdentity: lila.chessjudgment.model.PlanEventIdentity,
      rootConsequences: List[TransitionConsequence],
      line: LineFactEvidence,
      positionHistory: CanonicalPositionHistory,
      trace: CausalLineTrace
  ): PlanCausalEpisode =
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
    val root = PlanCausalEventNode(
      identity = rootIdentity,
      step = rootStep,
      perspective = rootTransition.perspective,
      structuralConsequences = rootConsequences,
      canonicalStep = trace.legalStep(rootStep)
    )
    fromContinuation(
      plan = plan,
      rootLine = rootLine,
      role = rootTransition.role,
      root = root,
      continuation = line.lineReplaySteps.dropWhile(_ != rootStep).drop(1),
      positionHistory = positionHistory,
      trace = Some(trace)
    )

  def fromContinuation(
      plan: Plan,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      root: PlanCausalEventNode,
      continuation: List[LineReplayStep],
      positionHistory: CanonicalPositionHistory,
      observedResultMove: Option[String] = None,
      trace: Option[CausalLineTrace] = None,
      admittedReplay: Option[CanonicalLineReplay] = None
  ): PlanCausalEpisode =
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
        rootLine,
        role,
        root.perspective,
        replay,
        Some(
          CausalStepObservation(
            root.step,
            StructuralTransitionBinding(
              root.moveUci,
              role,
              PositionNodeRef(root.step.fenBefore, root.step.ply - 1, Some(root.perspective)),
              PositionNodeRef(root.step.fenAfter, root.step.ply, Some(!root.perspective)),
              Some(rootLine),
              root.perspective,
              root.identity.actorRole.map(EvidencePieceRole.apply)
            ),
            root.structuralConsequences,
            root.certifiedLegalStep.map(_.move.piece.role)
          )
        ),
        admittedReplay = exactAdmittedReplay
      )
    }
    val materialSummary = activeTrace.canonicalReplay
      .filter(_.matches(replay))
      .flatMap(CandidateLineAssembler.lineMaterialSummary(positionHistory, _))
    val certifiedRoot = root.copy(
      canonicalStep = root.certifiedLegalStep.orElse(activeTrace.legalStep(root.step))
    )
    val observedCandidates = certifiedRoot :: rebasedContinuation.flatMap(step =>
      eventNode(plan, rootLine, role, root.perspective, step, Some(activeTrace))
    )
    val observedResponses = observedCandidates.flatMap(trigger => responsesFor(trigger, replay, activeTrace))
    val rawBaseDependencies = observedCandidates.zipWithIndex.flatMap { case (from, fromIndex) =>
      observedCandidates.drop(fromIndex + 1).flatMap(to =>
        dependency(plan, rootLine, role, from, to, activeTrace, observedResponses, observedResultMove, materialSummary)
      )
    }
    val baseDependencies = rawBaseDependencies
    val routeCandidates = observedCandidates.zipWithIndex.flatMap { case (from, fromIndex) =>
      observedCandidates.drop(fromIndex + 1).flatMap(to =>
        routeDependencies(from, to, activeTrace)
      )
    }
    val possibleDependencies = (baseDependencies ++ routeCandidates)
      .distinct
      .filter(_.planConnectionProven)
    val possibleEpisode = PlanCausalEpisode(
      root = certifiedRoot,
      continuations = observedCandidates.filterNot(_ == certifiedRoot),
      dependencies = possibleDependencies,
      responses = observedResponses.filter(_.proven)
    )
    val exactResultRoutes = resultRoutes(plan, rootLine, role, possibleEpisode)
    val observedDependencies = exactResultRoutes.flatMap(_.causalPath).distinct
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
        val selected = selectResults(
          plan,
          event,
          provenConsequencesByEvent.getOrElse(event, Nil)
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
      .filter(_.planConnectionProven)
    val selectedResponses = observedResponses
      .filter(response => selectedByObservation.contains(response.trigger))
      .map(response => response.copy(
        trigger = selectedByObservation(response.trigger)
      ))
      .filter(_.proven)
    val selectedRoot = selectedByObservation.getOrElse(certifiedRoot, certifiedRoot)
    def selectedEvent(event: PlanCausalEventNode): PlanCausalEventNode =
      selectedByObservation.getOrElse(event, event)
    val selectedResultRoutes = exactResultRoutes.map(_.withMappedEvents(selectedEvent))
    PlanCausalEpisode(
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

  def withHistory(
      plan: Plan,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      episode: PlanCausalEpisode,
      historyReplay: Option[CanonicalLineReplay],
      trace: CausalLineTrace
  ): PlanCausalEpisode =
    val combined = for
      history <- historyReplay
      continuation <- trace.canonicalReplay
      replay <- CanonicalLineReplay.concatenate(history, continuation)
    yield replay
    combined.fold(episode) { replay =>
      val history = replay.replaySteps.filter(_.ply < episode.root.step.ply).sortBy(_.ply)
      val (antecedents, dependencies) = historicalPath(plan, rootLine, role, episode.root, history, replay)
      episode.copy(
        dependencies = (episode.dependencies ++ dependencies).distinct,
        antecedents = antecedents
      )
    }

  private def historicalPath(
      plan: Plan,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      root: PlanCausalEventNode,
      history: List[LineReplayStep],
      replay: CanonicalLineReplay
  ): (List[PlanCausalEventNode], List[PlanCausalEventDependency]) =
    if history.isEmpty || !hasRootPlanResult(plan, rootLine, role, root) then Nil -> Nil
    else
      val direct = history.zipWithIndex.flatMap { case (step, index) =>
        historicalDependencies(
          plan,
          rootLine,
          role,
          step,
          root,
          history.drop(index + 1),
          includePersistentPreconditions = true,
          replay = replay
        )
      }
      val directDependencies = direct.filter(_.planConnectionProven).distinct
      val priorObjectDependencies = allPriorObjectDependencies(
        plan,
        rootLine,
        role,
        directDependencies.filter(_.kind == PlanCausalDependencyKind.ObjectStatePrecondition),
        history,
        replay
      )
      val dependencies = (directDependencies ++ priorObjectDependencies).distinct
      val antecedents = dependencies
        .flatMap(dependency => List(dependency.from, dependency.to))
        .filter(_.step.ply < root.step.ply)
        .distinct
        .sortBy(_.step.ply)
      antecedents -> dependencies

  private def allPriorObjectDependencies(
      plan: Plan,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      seeds: List[PlanCausalEventDependency],
      history: List[LineReplayStep],
      replay: CanonicalLineReplay
  ): List[PlanCausalEventDependency] =
    @annotation.tailrec
    def expand(
        frontier: List[PlanCausalEventDependency],
        found: Set[PlanCausalEventDependency]
    ): Set[PlanCausalEventDependency] =
      if frontier.isEmpty then found
      else
        val discovered = frontier.flatMap { dependency =>
          val earlier = history.filter(_.ply < dependency.from.step.ply)
          earlier.zipWithIndex.flatMap { case (step, index) =>
            historicalDependencies(
              plan,
              rootLine,
              role,
              step,
              dependency.from,
              earlier.drop(index + 1),
              includePersistentPreconditions = false,
              replay = replay
            ).filter(candidate =>
              candidate.kind == PlanCausalDependencyKind.ObjectStatePrecondition &&
                candidate.planConnectionProven
            )
          }
        }.distinct
        val unseen = discovered.filterNot(found)
        expand(unseen, found ++ unseen)

    expand(seeds, seeds.toSet).toList.sortBy(dependency =>
      (dependency.from.step.ply, dependency.to.step.ply, dependency.from.moveUci, dependency.to.moveUci)
    )

  private def historicalDependencies(
      plan: Plan,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      sourceStep: LineReplayStep,
      target: PlanCausalEventNode,
      between: List[LineReplayStep],
      includePersistentPreconditions: Boolean,
      replay: CanonicalLineReplay
  ): List[PlanCausalEventDependency] =
    val sourceMove = EvidenceRef.normalizeMove(sourceStep.moveUci)
    val targetMove = EvidenceRef.normalizeMove(target.moveUci)
    val samePerspective = replay.before(sourceStep).exists(_.color == target.perspective)
    val consecutiveActorTurns = between.forall(step =>
      replay.before(step).exists(_.color != target.perspective)
    )
    val objectState = Option
      .when(
        samePerspective &&
          consecutiveActorTurns &&
          sourceMove.slice(2, 4) == targetMove.take(2)
      )(
        LineObjectTrajectory
          .find(sourceStep, between :+ target.step, between.size + 1, replay)
          .filter(_.futureStep == target.step)
      )
      .flatten
      .map(PlanCausalDependencyKind.ObjectStatePrecondition -> PlanCausalDependencyProof.ObjectState(_))
    val persistent =
      if samePerspective && includePersistentPreconditions then
        LineAccessTrajectory
          .findRootClearanceBeforeUse(sourceStep, target.step, between, replay)
          .map(PlanCausalDependencyKind.LineAccessPrecondition -> PlanCausalDependencyProof.LineAccess(_))
          .toList
      else Nil
    val proofs = objectState.toList ++ persistent
    Option
      .when(proofs.nonEmpty)(eventNode(plan, rootLine, role, target.perspective, sourceStep, admittedReplay = Some(replay)))
      .flatten
      .toList
      .flatMap(from => proofs.map { case (kind, proof) =>
        PlanCausalEventDependency(from, target, kind, proof, target.step.ply - sourceStep.ply)
      })
      .filter(_.planConnectionProven)

  private def eventNode(
      plan: Plan,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      perspective: Color,
      step: LineReplayStep,
      trace: Option[CausalLineTrace] = None,
      admittedReplay: Option[CanonicalLineReplay] = None
  ): Option[PlanCausalEventNode] =
    val resolved = trace
      .flatMap(traceValue =>
        traceValue.observation(step).map(observed => observed -> traceValue.legalStep(step))
      )
      .orElse(
        admittedReplay
          .flatMap(_.subset(List(step)))
          .flatMap(replay =>
            CausalLineTrace
              .observe(rootLine, role, perspective, step, replay)
              .map(observed => observed -> replay.legalStep(step))
          )
      )
    resolved.map { case (observed, legalStep) =>
      PlanCausalEventNode(
        identity = PlanEventIdentityBuilder.from(
          rootMove = step.moveUci,
          actorRole = legalStep.map(_.move.piece.role.name),
          plan = plan,
          consequences = observed.consequences,
        ),
        step = step,
        perspective = perspective,
        structuralConsequences = observed.consequences,
        canonicalStep = legalStep
      )
    }

  private def dependency(
      plan: Plan,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      from: PlanCausalEventNode,
      to: PlanCausalEventNode,
      trace: CausalLineTrace,
      responses: List[PlanCausalResponse],
      observedResultMove: Option[String],
      materialSummary: Option[LineMaterialSummary]
  ): List[PlanCausalEventDependency] =
    val replay = trace.replay
    val relation = trace.relation(from.step, to.step)
    val fromIndex = trace.indexOf(from.step).getOrElse(-1)
    val between = relation.map(_.between).getOrElse(Nil)
    val plyOffset = to.step.ply - from.step.ply
    val exactObservedResult = observedResultMove.exists(EvidenceRef.sameMove(_, to.moveUci))
    val hasObservedResult = exactObservedResult || to.structuralConsequences.nonEmpty
    val hasPlanResult = exactObservedResult || hasRootPlanResult(plan, rootLine, role, to)
    val inducedResponseBetween = responses.exists(response =>
      response.trigger == from && response.step.ply > from.step.ply && response.step.ply < to.step.ply
    )
    val objectState =
      Option
        .when(hasPlanResult || inducedResponseBetween)(
          relation.flatMap(_.objectState).map(objectStateDependency(from, to, _))
        )
        .flatten
    val lineAccessCandidate = relation
      .flatMap(_.lineAccess)
      .map(lineAccessDependency(from, to, _))
    val lineAccess =
      Option
        .when(hasPlanResult)(lineAccessCandidate)
        .flatten
    val responseContinuations = responses
      .filter(response =>
        response.trigger == from &&
          response.step.ply > from.step.ply &&
          response.step.ply < to.step.ply
      )
      .flatMap(response =>
        val exchangeConversion =
          Option
            .when(advantageTransformationPlan(plan) && promotedPassedPawnResult(to))(
              materialSummary.flatMap(summary =>
                trace.canonicalReplay.flatMap(admitted =>
                  ExchangeConversionTrajectory.find(
                    planRootStep = replay.head,
                    exchangeStep = from.step,
                    replyStep = response.step,
                    promotionStep = to.step,
                    interveningSteps = between,
                    planPrefix = replay.slice(1, fromIndex),
                    materialSummary = summary,
                    replay = admitted
                  )
                )
              )
            )
            .flatten
        exchangeConversion.toList ++ relation.toList.flatMap(_.continuationsFor(response.step))
      )
      .filter {
        case pawn: PawnBreakFollowUpTrajectory =>
          hasPlanResult ||
            (
              pawn.kind == PawnBreakFollowUpKind.ReleasedPassedPawn &&
                responseTransformationPlan(plan) &&
                to.structuralConsequences.exists(_.kind == TransitionConsequenceKind.PassedPawnProgress)
            )
        case _: CaptureResponseFollowUpTrajectory => true
        case _: CheckResponseFollowUpTrajectory => hasPlanResult
        case _: ExchangeConversionTrajectory    => hasPlanResult && promotedPassedPawnResult(to)
      }
      .map(trajectory =>
        PlanCausalEventDependency(
          from,
          to,
          PlanCausalDependencyKind.ResponseContinuationPrecondition,
          PlanCausalDependencyProof.ResponseContinuation(trajectory),
          plyOffset
        )
      )
    List(
      objectState,
      lineAccess,
    ).flatten ++ responseContinuations

  private def routeDependencies(
      from: PlanCausalEventNode,
      to: PlanCausalEventNode,
      trace: CausalLineTrace
  ): List[PlanCausalEventDependency] =
    trace.relation(from.step, to.step).toList.flatMap(relation =>
      List(
        relation.objectState.map(objectStateDependency(from, to, _)),
        relation.lineAccess
          .map(lineAccessDependency(from, to, _))
      ).flatten
    )

  private def objectStateDependency(
      from: PlanCausalEventNode,
      to: PlanCausalEventNode,
      trajectory: LineObjectTrajectory
  ): PlanCausalEventDependency =
    val plyOffset = to.step.ply - from.step.ply
    PlanCausalEventDependency(
      from,
      to,
      PlanCausalDependencyKind.ObjectStatePrecondition,
      PlanCausalDependencyProof.ObjectState(trajectory),
      plyOffset
    )

  private def lineAccessDependency(
      from: PlanCausalEventNode,
      to: PlanCausalEventNode,
      trajectory: LineAccessTrajectory
  ): PlanCausalEventDependency =
    val plyOffset = to.step.ply - from.step.ply
    PlanCausalEventDependency(
      from,
      to,
      PlanCausalDependencyKind.LineAccessPrecondition,
      PlanCausalDependencyProof.LineAccess(trajectory),
      plyOffset
    )

  private def hasRootPlanResult(
      plan: Plan,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      event: PlanCausalEventNode
  ): Boolean =
    event.structuralConsequences.exists(consequence =>
      PlanCausalEventProof.candidateConsequenceForPlan(
        plan,
        consequence,
        transitionFor(rootLine, role, event)
      ).nonEmpty
    )

  private def responseTransformationPlan(plan: Plan): Boolean =
    Set(PlanTheme.PawnBreakPreparation, PlanTheme.AdvantageTransformation)(plan.theme)

  private def advantageTransformationPlan(plan: Plan): Boolean =
    plan.theme == PlanTheme.AdvantageTransformation

  private def promotedPassedPawnResult(event: PlanCausalEventNode): Boolean =
    val move = EvidenceRef.normalizeMove(event.moveUci)
    event.structuralConsequences.exists(consequence =>
      consequence.kind == TransitionConsequenceKind.PassedPawnProgress &&
        consequence.subjectFacts.exists {
          case StructuralSubject.PassedPawnPromoted(_, from, to) =>
            move.length == 5 && move.take(2) == from.key.toLowerCase && move.slice(2, 4) == to.key.toLowerCase
          case _ => false
        }
    )

  private def responsesFor(
      trigger: PlanCausalEventNode,
      replay: List[LineReplayStep],
      trace: CausalLineTrace
  ): List[PlanCausalResponse] =
    val triggerIndex = replay.indexOf(trigger.step)
    if triggerIndex < 0 then Nil
    else
      replay
        .drop(triggerIndex + 1)
        .takeWhile(step => PlanCausalResponse.planPiecePresent(trigger, step, trace.legalStep(step)))
        .flatMap { step =>
          for
            legalStep <- trace.legalStep(step).toList
            beforeAnalysis <- trace.positionAnalysis(step.fenBefore, step.ply - 1).toList
            afterAnalysis <- trace.positionAnalysis(step.fenAfter, step.ply).toList
            response <- PlanCausalResponse
              .certified(trigger, step, legalStep, beforeAnalysis, afterAnalysis)
              .toList
          yield response
        }

  private def selectResults(
      plan: Plan,
      event: PlanCausalEventNode,
      provenConsequences: List[TransitionConsequence]
  ): PlanCausalEventNode =
    val consequences = provenConsequences.distinct
    event.copy(
      identity = PlanEventIdentityBuilder.from(
        rootMove = event.step.moveUci,
        actorRole = event.certifiedLegalStep.map(_.move.piece.role.name),
        plan = plan,
        consequences = consequences
      ),
      structuralConsequences = consequences
    )

  private def resultRoutes(
      plan: Plan,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      episode: PlanCausalEpisode
  ): List[PlanCausalResultRoute] =
    episode.continuationsEnabledByRoot.flatMap { sourceEvent =>
      val transition = transitionFor(rootLine, role, sourceEvent)
      PlanCausalEpisode.resultConsequences(sourceEvent).flatMap { consequence =>
        val causalPaths = episode.enablingDependencyPathsTo(sourceEvent)
        val sourceProofs = PlanCausalGoalProof
          .certify(plan.theme, Some(plan.kind), transition, consequence)
          .toList
        val dependencyProofs = episode
          .enablingDependenciesTo(sourceEvent)
          .flatMap(dependency =>
            PlanCausalGoalProof.certifyDependency(plan, transition, dependency, consequence)
          )
        val exactProofs = (sourceProofs ++ dependencyProofs).distinct
        causalPaths.flatMap { causalPath =>
          exactProofs.flatMap(proof =>
            PlanCausalResultRoute.certified(sourceEvent, consequence, causalPath, proof)
          )
        }
      }
    }.distinct.sortBy(_.stableKey)

  private def transitionFor(
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      event: PlanCausalEventNode
  ): StructuralTransitionBinding =
    StructuralTransitionBinding(
      moveUci = event.step.moveUci,
      role = role,
      from = PositionNodeRef(event.step.fenBefore, event.step.ply - 1, Some(event.perspective)),
      to = PositionNodeRef(event.step.fenAfter, event.step.ply, Some(!event.perspective)),
      line = Some(rootLine),
      perspective = event.perspective,
      actorRole = event.identity.actorRole.map(EvidencePieceRole.apply)
    )
