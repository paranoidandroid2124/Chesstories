package lila.chessjudgment.analysis.assembly

import chess.{ Color, Pawn }
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.structure.{ StructuralDeltaAnalyzer, StructuralDeltaContracts }
import lila.chessjudgment.model.{ PlanMatch, PlanSupport }
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PrincipalVariationEvidence }
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }
import lila.chessjudgment.model.judgment.*

private[assembly] object PlanCausalEpisodeBuilder:

  def fromLine(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      rootTransition: StructuralTransitionBinding,
      rootIdentity: lila.chessjudgment.model.PlanEventIdentity,
      rootConsequences: List[TransitionConsequence],
      rootDevelopmentChoices: List[StructuralDevelopmentChoice],
      line: LineFactEvidence,
      positionHistory: CanonicalPositionHistory
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
      developmentChoices = rootDevelopmentChoices
    )
    fromContinuation(
      plan = plan,
      rootLine = rootLine,
      role = rootTransition.role,
      root = root,
      continuation = line.lineReplaySteps.dropWhile(_ != rootStep).drop(1),
      positionHistory = positionHistory
    )

  def fromContinuation(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      root: PlanCausalEventNode,
      continuation: List[LineReplayStep],
      positionHistory: CanonicalPositionHistory,
      observedResultMove: Option[String] = None,
      observedReplyBranch: Boolean = false
  ): PlanCausalEpisode =
    val lineOnlyContinuation =
      continuation match
        case head :: tail if sameStructuralStep(head, root.step) => tail
        case steps                                               => steps
    val rebasedContinuation = lineOnlyContinuation.zipWithIndex.map { case (step, index) =>
      step.copy(ply = root.step.ply + index + 1)
    }
    val replay = root.step :: rebasedContinuation
    val materialSummary =
      Option
        .when(
          positionHistory.currentFen == root.step.fenBefore &&
            positionHistory.currentPly + 1 == root.step.ply
        )(positionHistory)
        .flatMap(_.extend(replay.map(_.moveUci)).toOption)
        .filter { lineHistory =>
          lineHistory.segmentReplaySteps
            .drop(positionHistory.segmentReplaySteps.size)
            .map(step => LineReplayStep(step.ply, step.uci, step.beforeFen, step.afterFen)) == replay
        }
        .flatMap(lineHistory => CandidateLineAssembler.lineMaterialSummary(positionHistory, lineHistory))
    val rawCandidates = root :: rebasedContinuation.flatMap(step => eventNode(plan, rootLine, role, root.perspective, step))
    val observedCandidates = withAcceptedExchangeCompletions(plan, rawCandidates, replay)
    val observedResponses = observedCandidates.flatMap(trigger => responsesFor(plan, trigger, replay))
    val rawBaseDependencies = observedCandidates.zipWithIndex.flatMap { case (from, fromIndex) =>
      observedCandidates.drop(fromIndex + 1).flatMap(to =>
        dependency(plan, rootLine, role, from, to, replay, observedResponses, observedResultMove, materialSummary)
      )
    }
    val exchangeConversionTriggers = rawBaseDependencies.collect {
      case PlanCausalEventDependency(
            from,
            _,
            PlanCausalDependencyKind.ResponseContinuationPrecondition,
            PlanCausalDependencyProof.ResponseContinuation(_: ExchangeConversionTrajectory),
            _
          ) => from
    }.toSet
    val baseDependencies = rawBaseDependencies.filterNot {
      case PlanCausalEventDependency(
            from,
            _,
            PlanCausalDependencyKind.ResponseContinuationPrecondition,
            PlanCausalDependencyProof.ResponseContinuation(_: CheckResponseFollowUpTrajectory),
            _
          ) => exchangeConversionTriggers(from)
      case _ => false
    }
    val routeCandidates = observedCandidates.zipWithIndex.flatMap { case (from, fromIndex) =>
      observedCandidates.drop(fromIndex + 1).flatMap(to =>
        List(objectStateDependency(from, to, replay), lineAccessDependency(from, to, replay)).flatten
      )
    }
    val possibleDependencies = (baseDependencies ++ routeCandidates).distinct
    val possibleRootEnabled = continuationEnabledByRoot(root, possibleDependencies)
    val futureGoalEvents = observedCandidates.filter(event =>
      val incoming = possibleDependencies.filter(dependency => dependency.to == event && dependency.enablesContinuation)
      val observedConsequences = (
        event.structuralConsequences ++
          observedReplyBranchRouteGain(plan, root, event, incoming, observedReplyBranch)
      ).distinct
      event != root &&
        possibleRootEnabled(event) &&
        (
          observedResultMove.exists(EvidenceRef.sameMove(_, event.moveUci)) ||
          (
            hasRootPlanResult(plan, rootLine, role, event) &&
              incoming.exists(dependency => !captureResponseContinuation(dependency))
          ) ||
            incoming.exists(dependency => observedConsequences.exists(
              dependencySupportsResult(plan, rootLine, role, dependency, _)
            ))
        )
    ).toSet
    val goalPathNodes = nodesReaching(futureGoalEvents, possibleDependencies)
    val goalDirectedRoutes = routeCandidates.filter(dependency =>
      possibleRootEnabled(dependency.from) && goalPathNodes(dependency.to)
    )
    val observedDependencies = (baseDependencies ++ goalDirectedRoutes).distinct
    val observedConnected = connectedEvents(root, observedDependencies)
    val rootEnabled = continuationEnabledByRoot(root, observedDependencies)
    val selectedByObservation = observedCandidates
      .filter(observedConnected)
      .map { event =>
        val selected = selectResults(
          plan,
          rootLine,
          role,
          root,
          event,
          observedDependencies.filter(dependency =>
            dependency.to == event && rootEnabled(dependency.from) && dependency.enablesContinuation
          ),
          observedReplyBranch,
          preserveDirectExchangeLine = event == root
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
      .filter(_.planConnectionProven)
    val selectedResponses = observedResponses
      .filter(response => selectedByObservation.contains(response.trigger))
      .map(response => response.copy(
        trigger = selectedByObservation(response.trigger),
        structuralConsequences = selectResponseResults(plan, rootLine, role, response)
      ))
      .filter(_.proven)
    val selectedRoot = selectedByObservation.getOrElse(root, root)
    val connected = connectedEvents(selectedRoot, selectedDependencies)
    val observedVacatedSquareResult = Option
      .when(observedReplyBranch && pieceRedeploymentPlan(plan))(
        selectedDependencies.collectFirst {
          case PlanCausalEventDependency(
                from,
                to,
                PlanCausalDependencyKind.LineAccessPrecondition,
                PlanCausalDependencyProof.LineAccess(trajectory),
                _
              )
              if from == selectedRoot &&
                trajectory.enabledTo == trajectory.vacatedSquare &&
                !trajectory.placesPieceBeforeClearance &&
                to.structuralConsequences.exists(consequence =>
                  consequence.kind == TransitionConsequenceKind.MobilityGain &&
                    consequence.positive &&
                    consequence.strength > 0
                ) =>
            to
        }
      )
      .flatten
    val episodeEvents = observedVacatedSquareResult.map(result => Set(selectedRoot, result)).getOrElse(connected)
    val connectedDependencies = selectedDependencies.filter(dependency =>
      episodeEvents(dependency.from) && episodeEvents(dependency.to)
    )
    val connectedResponses = selectedResponses.filter(response => episodeEvents(response.trigger))
    PlanCausalEpisode(
      root = selectedRoot,
      continuations = selectedByObservation.values
        .filterNot(_ == selectedRoot)
        .filter(episodeEvents)
        .toList
        .sortBy(_.step.ply),
      dependencies = connectedDependencies,
      responses = connectedResponses
    )

  private def sameStructuralStep(left: LineReplayStep, right: LineReplayStep): Boolean =
    EvidenceRef.sameMove(left.moveUci, right.moveUci) &&
      PrincipalVariationEvidence.sameBoardState(left.fenBefore, right.fenBefore) &&
      PrincipalVariationEvidence.sameBoardState(left.fenAfter, right.fenAfter)

  def withHistory(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      episode: PlanCausalEpisode,
      history: List[LineReplayStep]
  ): PlanCausalEpisode =
    val (antecedents, dependencies) = historicalPath(
      plan,
      rootLine,
      role,
      episode.root,
      history.filter(_.ply < episode.root.step.ply).sortBy(_.ply)
    )
    episode.copy(
      dependencies = (episode.dependencies ++ dependencies).distinct,
      antecedents = antecedents
    )

  private def historicalPath(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      root: PlanCausalEventNode,
      history: List[LineReplayStep]
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
          includePersistentPreconditions = true
        )
      }
      val directObject = direct
        .filter(_.kind == PlanCausalDependencyKind.ObjectStatePrecondition)
        .maxByOption(_.from.step.ply)
      val objectRoute = directObject.toList.flatMap(dependency =>
        priorObjectRoute(plan, rootLine, role, dependency, history.takeWhile(_.ply < dependency.from.step.ply))
      )
      val principalPersistent = direct
        .filterNot(_.kind == PlanCausalDependencyKind.ObjectStatePrecondition)
        .groupBy(_.kind)
        .values
        .flatMap(_.maxByOption(_.from.step.ply))
        .toList
      val dependencies = (
        principalPersistent ++ objectRoute
      ).distinct.filter(_.planConnectionProven)
      val antecedents = dependencies
        .flatMap(dependency => List(dependency.from, dependency.to))
        .filter(_.step.ply < root.step.ply)
        .distinct
        .sortBy(_.step.ply)
      antecedents -> dependencies

  private def priorObjectRoute(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      dependency: PlanCausalEventDependency,
      history: List[LineReplayStep]
  ): List[PlanCausalEventDependency] =
    val previous = history.zipWithIndex.reverseIterator
      .flatMap { case (step, index) =>
        historicalDependencies(
          plan,
          rootLine,
          role,
          step,
          dependency.from,
          history.drop(index + 1),
          includePersistentPreconditions = false
        ).find(_.kind == PlanCausalDependencyKind.ObjectStatePrecondition)
      }
      .take(1)
      .toList
      .headOption
    previous.toList.flatMap(candidate =>
      priorObjectRoute(plan, rootLine, role, candidate, history.takeWhile(_.ply < candidate.from.step.ply))
    ) :+ dependency

  private def historicalDependencies(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      sourceStep: LineReplayStep,
      target: PlanCausalEventNode,
      between: List[LineReplayStep],
      includePersistentPreconditions: Boolean
  ): List[PlanCausalEventDependency] =
    val sourceMove = EvidenceRef.normalizeMove(sourceStep.moveUci)
    val targetMove = EvidenceRef.normalizeMove(target.moveUci)
    val samePerspective = Fen.read(Standard, Fen.Full(sourceStep.fenBefore)).exists(_.color == target.perspective)
    val consecutiveActorTurns = between.forall(step =>
      Fen.read(Standard, Fen.Full(step.fenBefore)).forall(_.color != target.perspective)
    )
    val objectState = Option
      .when(
        samePerspective &&
          consecutiveActorTurns &&
          sourceMove.slice(2, 4) == targetMove.take(2)
      )(
        LineObjectTrajectory
          .find(sourceStep, between :+ target.step, between.size + 1)
          .filter(_.futureStep == target.step)
      )
      .flatten
      .map(PlanCausalDependencyKind.ObjectStatePrecondition -> PlanCausalDependencyProof.ObjectState(_))
    val targetIsPawn = target.identity.actorRole.exists(_.equalsIgnoreCase(Pawn.toString))
    val persistent = Option.when(samePerspective && includePersistentPreconditions)(
      List(
        LineAccessTrajectory
          .find(sourceStep, target.step, between)
          .filterNot(_.placesPieceBeforeClearance)
          .map(PlanCausalDependencyKind.LineAccessPrecondition -> PlanCausalDependencyProof.LineAccess(_)),
        Option
          .when(targetIsPawn)(PawnAdvanceSupportTrajectory.find(sourceStep, target.step, between))
          .flatten
          .map(PlanCausalDependencyKind.PawnAdvanceSupport -> PlanCausalDependencyProof.PawnAdvanceSupport(_)),
        Option
          .when(targetIsPawn)(RetreatControlTrajectory.find(sourceStep, target.step, between))
          .flatten
          .map(PlanCausalDependencyKind.RetreatControlPrecondition -> PlanCausalDependencyProof.RetreatControl(_))
      ).flatten
    ).getOrElse(Nil)
    val proofs = objectState.toList ++ persistent
    Option
      .when(proofs.nonEmpty)(eventNode(plan, rootLine, role, target.perspective, sourceStep))
      .flatten
      .toList
      .flatMap(from => proofs.map { case (kind, proof) =>
        PlanCausalEventDependency(from, target, kind, proof, target.step.ply - sourceStep.ply)
      })
      .filter(dependency => dependency.planConnectionProven && historyConnectionFitsTarget(dependency))

  private def historyConnectionFitsTarget(dependency: PlanCausalEventDependency): Boolean =
    dependency.kind match
      case PlanCausalDependencyKind.LineAccessPrecondition =>
        dependency.to.identity.actorRole.exists(_.equalsIgnoreCase(Pawn.toString))
      case _ =>
        true

  private def eventNode(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      perspective: Color,
      step: LineReplayStep
  ): Option[PlanCausalEventNode] =
    for
      before <- Fen.read(Standard, Fen.Full(step.fenBefore))
      if before.color == perspective
      after <- Fen.read(Standard, Fen.Full(step.fenAfter))
      delta <- StructuralDeltaAnalyzer.delta(
        beforeFen = step.fenBefore,
        beforeBoard = before.board,
        afterFen = step.fenAfter,
        afterBoard = after.board,
        side = perspective,
        files = ('a' to 'h').toList,
        targets = Nil,
        createdTensionFrom = Option(EvidenceRef.normalizeMove(step.moveUci).slice(2, 4)).filter(_.matches("[a-h][1-8]")),
        moveUci = Some(step.moveUci)
      )
    yield
      val transition = StructuralTransitionBinding(
        moveUci = step.moveUci,
        role = role,
        from = PositionNodeRef(step.fenBefore, step.ply - 1, Some(before.color)),
        to = PositionNodeRef(step.fenAfter, step.ply, Some(after.color)),
        line = Some(rootLine),
        perspective = perspective
      )
      val consequences = StructuralDeltaContracts
        .consequences(delta)
        .filter(consequence => consequence.positive && consequence.strength > 0)
      val developmentChoices = StructuralDeltaContracts.developmentChoices(delta)
      PlanCausalEventNode(
        identity = PlanEventIdentityBuilder.from(
          rootMove = step.moveUci,
          beforeFen = step.fenBefore,
          plan = plan,
          consequences = consequences,
          developmentChoices = developmentChoices
        ),
        step = step,
        perspective = perspective,
        structuralConsequences = consequences,
        developmentChoices = developmentChoices
      )

  private def dependency(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      from: PlanCausalEventNode,
      to: PlanCausalEventNode,
      replay: List[LineReplayStep],
      responses: List[PlanCausalResponse],
      observedResultMove: Option[String],
      materialSummary: Option[LineMaterialSummary]
  ): List[PlanCausalEventDependency] =
    val fromIndex = replay.indexOf(from.step)
    val toIndex = replay.indexOf(to.step)
    val between = Option.when(fromIndex >= 0 && toIndex > fromIndex)(replay.slice(fromIndex + 1, toIndex)).getOrElse(Nil)
    val plyOffset = to.step.ply - from.step.ply
    val exactObservedResult = observedResultMove.exists(EvidenceRef.sameMove(_, to.moveUci))
    val hasObservedResult = exactObservedResult || to.structuralConsequences.nonEmpty || to.developmentChoices.nonEmpty
    val hasPlanResult = exactObservedResult || hasRootPlanResult(plan, rootLine, role, to)
    val inducedResponseBetween = responses.exists(response =>
      response.trigger == from && response.step.ply > from.step.ply && response.step.ply < to.step.ply
    )
    val objectState =
      Option
        .when(hasPlanResult || inducedResponseBetween)(objectStateDependency(from, to, replay))
        .flatten
    val lineAccessCandidate = lineAccessDependency(from, to, replay)
    val lineAccess =
      Option
        .when(hasPlanResult)(lineAccessCandidate)
        .flatten
    val pawnAdvanceSupport =
      Option
        .when(hasObservedResult)(PawnAdvanceSupportTrajectory.find(from.step, to.step, between))
        .flatten
        .map(trajectory =>
          PlanCausalEventDependency(
            from,
            to,
            PlanCausalDependencyKind.PawnAdvanceSupport,
            PlanCausalDependencyProof.PawnAdvanceSupport(trajectory),
            plyOffset
          )
        )
    val retreatControl =
      Option
        .when(hasObservedResult)(RetreatControlTrajectory.find(from.step, to.step, between))
        .flatten
        .map(trajectory =>
          PlanCausalEventDependency(
            from,
            to,
            PlanCausalDependencyKind.RetreatControlPrecondition,
            PlanCausalDependencyProof.RetreatControl(trajectory),
            plyOffset
          )
        )
    val responseContinuation = responses
      .filter(response =>
        response.trigger == from &&
          response.step.ply > from.step.ply &&
          response.step.ply < to.step.ply
      )
      .flatMap(response =>
        List(
          Option
            .when(advantageTransformationPlan(plan) && promotedPassedPawnResult(to))(
              materialSummary.flatMap(summary =>
                ExchangeConversionTrajectory.find(
                  planRootStep = replay.head,
                  exchangeStep = from.step,
                  replyStep = response.step,
                  promotionStep = to.step,
                  interveningSteps = between,
                  planPrefix = replay.slice(1, fromIndex),
                  materialSummary = summary
                )
              )
            )
            .flatten,
          PawnBreakFollowUpTrajectory.find(from.step, response.step, to.step, between),
          CaptureResponseFollowUpTrajectory.find(from.step, response.step, to.step, between),
          CheckResponseFollowUpTrajectory.find(from.step, response.step, to.step, between)
        ).flatten
      )
      .find {
        case pawn: PawnBreakFollowUpTrajectory =>
          hasPlanResult ||
            (
              pawn.kind == PawnBreakFollowUpKind.ReleasedPassedPawn &&
                responseTransformationPlan(plan) &&
                to.structuralConsequences.exists(_.kind == TransitionConsequenceKind.PassedPawnProgress)
            )
        case _: CaptureResponseFollowUpTrajectory =>
          transitionFor(rootLine, role, from).exists(transition =>
            to.structuralConsequences.exists(
              PlanCausalGoalProof.provesAfterInducedResponse(from.identity, transition, _)
            )
          ) || lineAccessCandidate.exists(_.planConnectionProven)
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
    val sharedTargets =
      PlanCausalEpisode.pressureTargetSquares(from).intersect(PlanCausalEpisode.pressureTargetSquares(to)).toList.sorted
    val sharedTarget = Option.when(sharedTargets.nonEmpty)(
      PlanCausalEventDependency(
        from,
        to,
        PlanCausalDependencyKind.SharedTargetCoordination,
        PlanCausalDependencyProof.SharedTarget(sharedTargets.map(EvidenceSquare(_))),
        plyOffset
      )
    )
    val flankAdvance = flankAdvanceDependency(from, to, plyOffset)
    List(
      objectState,
      lineAccess,
      pawnAdvanceSupport,
      retreatControl,
      responseContinuation,
      sharedTarget,
      flankAdvance
    ).flatten

  private def deferredLineAccessResultProven(
      trajectory: LineAccessTrajectory,
      result: PlanCausalEventNode
  ): Boolean =
    val openedPlacedPiece = result.structuralConsequences.exists(consequence =>
      consequence.kind == TransitionConsequenceKind.LineUnlockGain &&
        consequence
          .subjectsForPieceAt(trajectory.enabledPieceRole.name, trajectory.enabledFrom.key)
          .nonEmpty
    )
    val reachedConcreteTarget = result.structuralConsequences.exists(consequence =>
      consequence.kind == TransitionConsequenceKind.TargetPressureGain &&
        consequence.subjectsAt(trajectory.enabledTo.key).nonEmpty
    )
    openedPlacedPiece && reachedConcreteTarget

  private def objectStateDependency(
      from: PlanCausalEventNode,
      to: PlanCausalEventNode,
      replay: List[LineReplayStep]
  ): Option[PlanCausalEventDependency] =
    val fromIndex = replay.indexOf(from.step)
    val toIndex = replay.indexOf(to.step)
    val between = Option.when(fromIndex >= 0 && toIndex > fromIndex)(replay.slice(fromIndex + 1, toIndex)).getOrElse(Nil)
    val plyOffset = to.step.ply - from.step.ply
    LineObjectTrajectory
      .find(from.step, between :+ to.step, (between.size + 1).max(1))
      .filter(_.futureStep == to.step)
      .map(trajectory =>
        PlanCausalEventDependency(
          from,
          to,
          PlanCausalDependencyKind.ObjectStatePrecondition,
          PlanCausalDependencyProof.ObjectState(trajectory),
          plyOffset
        )
      )

  private def lineAccessDependency(
      from: PlanCausalEventNode,
      to: PlanCausalEventNode,
      replay: List[LineReplayStep]
  ): Option[PlanCausalEventDependency] =
    val fromIndex = replay.indexOf(from.step)
    val toIndex = replay.indexOf(to.step)
    val between = Option.when(fromIndex >= 0 && toIndex > fromIndex)(replay.slice(fromIndex + 1, toIndex)).getOrElse(Nil)
    val plyOffset = to.step.ply - from.step.ply
    LineAccessTrajectory
      .find(from.step, to.step, between)
      .filter(trajectory =>
        !trajectory.placesPieceBeforeClearance || deferredLineAccessResultProven(trajectory, to)
      )
      .map(trajectory =>
        PlanCausalEventDependency(
          from,
          to,
          PlanCausalDependencyKind.LineAccessPrecondition,
          PlanCausalDependencyProof.LineAccess(trajectory),
          plyOffset
        )
      )

  private def hasRootPlanResult(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      event: PlanCausalEventNode
  ): Boolean =
    transitionFor(rootLine, role, event).exists(transition =>
      event.structuralConsequences.exists(consequence =>
        PlanCausalEventProof.candidateConsequenceForPlan(plan, consequence, transition).nonEmpty
      )
    ) ||
      event.developmentChoices.nonEmpty && PlanCausalEventProof.developmentSupportsPlan(plan)

  private def responseTransformationPlan(plan: PlanMatch): Boolean =
    plan.support.exists {
      case PlanSupport.Theme(PlanTheme.PawnBreakPreparation | PlanTheme.AdvantageTransformation) => true
      case _                                                                                     => false
    }

  private def advantageTransformationPlan(plan: PlanMatch): Boolean =
    plan.support.exists {
      case PlanSupport.Theme(PlanTheme.AdvantageTransformation) => true
      case _                                                    => false
    }

  private def promotedPassedPawnResult(event: PlanCausalEventNode): Boolean =
    event.structuralConsequences.exists(consequence =>
      consequence.kind == TransitionConsequenceKind.PassedPawnProgress &&
        consequence.subjects.exists(_.trim.toLowerCase.startsWith("passed-pawn-promoted:"))
    )

  private def nodesReaching(
      destinations: Set[PlanCausalEventNode],
      dependencies: List[PlanCausalEventDependency]
  ): Set[PlanCausalEventNode] =
    @annotation.tailrec
    def loop(reaching: Set[PlanCausalEventNode]): Set[PlanCausalEventNode] =
      val next = reaching ++ dependencies.collect {
        case dependency if reaching(dependency.to) && dependency.enablesContinuation => dependency.from
      }
      if next == reaching then reaching else loop(next)
    loop(destinations)

  private def flankAdvanceDependency(
      from: PlanCausalEventNode,
      to: PlanCausalEventNode,
      plyOffset: Int
  ): Option[PlanCausalEventDependency] =
    for
      position <- Fen.read(Standard, Fen.Full(from.step.fenBefore))
      king <- position.board.kingPosOf(!from.perspective)
      advanceFile <- from.moveUci.headOption
      targets = PlanCausalEpisode.pressureTargetSquares(to).toList.sorted.flatMap(target =>
        _root_.chess.Square.fromKey(target).filter(square =>
          PlanCausalGoalProof.sameBoardWing(advanceFile, square.key.head) &&
            math.max(
              (square.file.value - king.file.value).abs,
              (square.rank.value - king.rank.value).abs
            ) <= 2
        ).map(square => EvidenceSquare(square.key))
      )
      proof = PlanCausalDependencyProof.FlankAdvance(EvidenceSquare(king.key), targets)
      dependency = PlanCausalEventDependency(
        from,
        to,
        PlanCausalDependencyKind.FlankAdvanceCoordination,
        proof,
        plyOffset
      )
      if dependency.planConnectionProven
    yield dependency

  private def responsesFor(
      plan: PlanMatch,
      trigger: PlanCausalEventNode,
      replay: List[LineReplayStep]
  ): List[PlanCausalResponse] =
    val triggerIndex = replay.indexOf(trigger.step)
    if triggerIndex < 0 then Nil
    else
      replay
        .drop(triggerIndex + 1)
        .takeWhile(step => PlanCausalEpisode.planPiecePresent(trigger, step.fenBefore))
        .map { step =>
          PlanCausalResponse(
            trigger,
            step,
            step.ply - trigger.step.ply
          )
        }
        .filter(_.proven)
        .map(response => response.copy(structuralConsequences = responseConsequences(plan, trigger, response.step)))

  private def responseConsequences(
      plan: PlanMatch,
      trigger: PlanCausalEventNode,
      step: LineReplayStep
  ): List[TransitionConsequence] =
    val completedExchange =
      PlanCausalEpisode.triggerMoveCapturesPiece(trigger) &&
        PlanCausalEpisode.responseCapturesPlanPiece(trigger, step)
    val beforeFen = if completedExchange then trigger.step.fenBefore else step.fenBefore
    val observedMove = Option.unless(completedExchange)(step.moveUci)
    val structural = (for
      before <- Fen.read(Standard, Fen.Full(beforeFen))
      after <- Fen.read(Standard, Fen.Full(step.fenAfter))
      delta <- StructuralDeltaAnalyzer.delta(
        beforeFen = beforeFen,
        beforeBoard = before.board,
        afterFen = step.fenAfter,
        afterBoard = after.board,
        side = trigger.perspective,
        files = ('a' to 'h').toList,
        targets = Nil,
        createdTensionFrom = observedMove
          .map(EvidenceRef.normalizeMove)
          .map(_.slice(2, 4))
          .filter(_.matches("[a-h][1-8]")),
        moveUci = observedMove
      )
    yield StructuralDeltaContracts
      .consequences(delta)
      .filter(consequence => consequence.positive && consequence.strength > 0 && consequence.subjects.nonEmpty)
    ).getOrElse(Nil)
    val exchangeCompletion = Option
      .when(
        completedExchange && PlanCausalEventProof.planTheme(plan).contains(PlanTheme.FavorableExchange)
      )(
        trigger.structuralConsequences
          .find(_.kind == TransitionConsequenceKind.PieceExchangeAvailable)
          .map(completedExchangeConsequence)
      )
      .flatten
      .toList
    (structural ++ exchangeCompletion).distinct

  private def withAcceptedExchangeCompletions(
      plan: PlanMatch,
      candidates: List[PlanCausalEventNode],
      replay: List[LineReplayStep]
  ): List[PlanCausalEventNode] =
    if !PlanCausalEventProof.planTheme(plan).contains(PlanTheme.FavorableExchange) then candidates
    else
      candidates.map { candidate =>
        val completions = candidates.flatMap { trigger =>
          val triggerIndex = replay.indexOf(trigger.step)
          for
            response <- replay.lift(triggerIndex + 1).toList
            followUp <- replay.lift(triggerIndex + 2).filter(_ == candidate.step).toList
            trajectory <- CaptureResponseFollowUpTrajectory
              .find(trigger.step, response, followUp, List(response))
              .toList
            exchange <- trigger.structuralConsequences
              .find(consequence => exchangeTargetMatchesResponder(consequence, trajectory))
              .toList
          yield completedExchangeConsequence(exchange)
        }.distinct
        if completions.isEmpty then candidate
        else
          val consequences = (candidate.structuralConsequences ++ completions).distinct
          candidate.copy(
            identity = PlanEventIdentityBuilder.from(
              rootMove = candidate.moveUci,
              beforeFen = candidate.step.fenBefore,
              plan = plan,
              consequences = consequences,
              developmentChoices = candidate.developmentChoices
            ),
            structuralConsequences = consequences
          )
      }

  private def exchangeTargetMatchesResponder(
      consequence: TransitionConsequence,
      trajectory: CaptureResponseFollowUpTrajectory
  ): Boolean =
    consequence.kind == TransitionConsequenceKind.PieceExchangeAvailable &&
      consequence
        .subjectsForPieceAt(
          trajectory.responderRole.name,
          trajectory.replyFrom.key
        )
        .exists(consequence.goalSubjects.contains)

  private def completedExchangeConsequence(exchange: TransitionConsequence): TransitionConsequence =
    TransitionConsequence(
      kind = TransitionConsequenceKind.PieceExchangeCompleted,
      polarity = StructuralSignalPolarity.Gain,
      strength = exchange.strength.max(1),
      subjects = exchange.subjects,
      targetSubjects = exchange.goalSubjects
    )

  private def selectResponseResults(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      response: PlanCausalResponse
  ): List[TransitionConsequence] =
    transitionFor(rootLine, role, response.trigger).toList.flatMap { transition =>
      response.structuralConsequences.flatMap(consequence =>
        PlanCausalEventProof.responseConsequenceForPlan(plan, transition, response, consequence)
      )
    }.distinct

  private def connectedEvents(
      root: PlanCausalEventNode,
      dependencies: List[PlanCausalEventDependency]
  ): Set[PlanCausalEventNode] =
    @annotation.tailrec
    def loop(connected: Set[PlanCausalEventNode]): Set[PlanCausalEventNode] =
      val next = connected ++ dependencies.flatMap { dependency =>
        if connected(dependency.from) || connected(dependency.to) then List(dependency.from, dependency.to)
        else Nil
      }
      if next == connected then connected else loop(next)
    loop(Set(root))

  private def continuationEnabledByRoot(
      root: PlanCausalEventNode,
      dependencies: List[PlanCausalEventDependency]
  ): Set[PlanCausalEventNode] =
    @annotation.tailrec
    def loop(enabled: Set[PlanCausalEventNode]): Set[PlanCausalEventNode] =
      val next = enabled ++ dependencies.collect {
        case dependency if enabled(dependency.from) && dependency.enablesContinuation => dependency.to
      }
      if next == enabled then enabled else loop(next)
    loop(Set(root))

  private def selectResults(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      root: PlanCausalEventNode,
      event: PlanCausalEventNode,
      enablingDependencies: List[PlanCausalEventDependency],
      observedReplyBranch: Boolean,
      preserveDirectExchangeLine: Boolean
  ): PlanCausalEventNode =
    val transition = transitionFor(rootLine, role, event)
    val rookTransferMobility = enablingDependencies
      .filter(dependency => rookTransferDependency(dependency) && !lineAccessPlacesPieceBeforeClearance(dependency))
      .flatMap(_ =>
        StructuralDeltaAnalyzer.movedPieceMobilityGain(
          event.step.fenBefore,
          event.step.fenAfter,
          event.perspective,
          event.moveUci
        )
      )
    val observedVacatedSquareRouteGain =
      observedReplyBranchRouteGain(plan, root, event, enablingDependencies, observedReplyBranch)
    val observedConsequences = (
        event.structuralConsequences ++
        rookTransferMobility ++
        observedVacatedSquareRouteGain
    ).distinct
    val dependencyProvenConsequences = observedConsequences.flatMap(consequence =>
      enablingDependencies.collectFirst(Function.unlift(dependency =>
        dependencyProvenResult(plan, rootLine, role, dependency, consequence)
      ))
    ).distinct
    val planSupportedConsequences = observedConsequences.flatMap { consequence =>
      transition.flatMap(PlanCausalEventProof.candidateConsequenceForPlan(plan, consequence, _))
    }
    val primaryGoalConsequences = transition
      .map(PlanCausalEventProof.goalConsequences(plan, _, planSupportedConsequences))
      .getOrElse(planSupportedConsequences)
    val exchangeLineConsequences = Option
      .when(preserveDirectExchangeLine)(
        PlanCausalEventProof.planTheme(plan).toList.flatMap(theme =>
          observedConsequences.filter(consequence =>
            PlanCausalGoalProof.lineAccessAdvancesExchangeGoal(
              theme,
              primaryGoalConsequences,
              consequence
            )
          )
        )
      )
      .getOrElse(Nil)
    val planGoalConsequences = (primaryGoalConsequences ++ exchangeLineConsequences).distinct
    val planGoalKinds = planGoalConsequences.map(_.kind).distinct
    val orderedDependencyConsequences =
      (
        planGoalKinds.flatMap(kind => dependencyProvenConsequences.filter(_.kind == kind)) ++
          dependencyProvenConsequences.filterNot(consequence => planGoalKinds.contains(consequence.kind))
      ).distinct
    val consequences =
      if observedVacatedSquareRouteGain.nonEmpty then observedVacatedSquareRouteGain
      else if orderedDependencyConsequences.nonEmpty then orderedDependencyConsequences
      else planGoalConsequences
    val developmentChoices =
      if PlanCausalEventProof.developmentSupportsPlan(plan) then event.developmentChoices
      else Nil
    event.copy(
      identity = PlanEventIdentityBuilder.from(
        rootMove = event.step.moveUci,
        beforeFen = event.step.fenBefore,
        plan = plan,
        consequences = consequences,
        developmentChoices = developmentChoices
      ),
      structuralConsequences = consequences,
      developmentChoices = developmentChoices
    )

  private def observedReplyBranchRouteGain(
      plan: PlanMatch,
      root: PlanCausalEventNode,
      event: PlanCausalEventNode,
      dependencies: List[PlanCausalEventDependency],
      observedReplyBranch: Boolean
  ): List[TransitionConsequence] =
    Option
      .when(observedReplyBranch && pieceRedeploymentPlan(plan))(
        dependencies.flatMap {
            case PlanCausalEventDependency(
                  from,
                  to,
                  PlanCausalDependencyKind.LineAccessPrecondition,
                  PlanCausalDependencyProof.LineAccess(trajectory),
                  _
                )
                if from == root &&
                to == event &&
                !trajectory.enabledPieceRole.name.equalsIgnoreCase(_root_.chess.Rook.toString) &&
                trajectory.enabledTo == trajectory.vacatedSquare &&
                !trajectory.placesPieceBeforeClearance =>
              Some(TransitionConsequence(
                kind = TransitionConsequenceKind.MobilityGain,
                polarity = StructuralSignalPolarity.Gain,
                strength = 1,
                subjects = List(
                  s"${trajectory.enabledPieceRole.name}:${trajectory.enabledFrom.key}-${trajectory.enabledTo.key}"
                )
              ))
            case _ => None
          }
          .distinct
      )
      .getOrElse(Nil)

  private def rookTransferDependency(dependency: PlanCausalEventDependency): Boolean =
    dependency.proof match
      case PlanCausalDependencyProof.LineAccess(trajectory) =>
        trajectory.enabledPieceRole.name.equalsIgnoreCase(_root_.chess.Rook.toString) &&
          (
            dependency.from.identity.goalKind.contains(PlanKind.RookFileTransfer) ||
              trajectory.placesPieceBeforeClearance
          )
      case _ => false

  private def lineAccessPlacesPieceBeforeClearance(dependency: PlanCausalEventDependency): Boolean =
    dependency.proof match
      case PlanCausalDependencyProof.LineAccess(trajectory) => trajectory.placesPieceBeforeClearance
      case _                                                => false

  private def dependencyProvenResult(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      dependency: PlanCausalEventDependency,
      consequence: TransitionConsequence
  ): Option[TransitionConsequence] =
    Option
      .when(dependencySupportsResult(plan, rootLine, role, dependency, consequence))(
        dependency.proof match
          case PlanCausalDependencyProof.ObjectState(_) if consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction =>
            val subjects = StructuralDeltaEvidence.directlyRestrictedOpponentSubjects(consequence)
            consequence.copy(
              strength = consequence.strength.min(subjects.size).max(1),
              subjects = subjects
            )
          case PlanCausalDependencyProof.LineAccess(trajectory)
              if trajectory.placesPieceBeforeClearance &&
                consequence.kind == TransitionConsequenceKind.TargetPressureGain =>
            val matchingSubjects = consequence.subjectsAt(trajectory.enabledTo.key)
            consequence.copy(
              strength = consequence.strength.min(matchingSubjects.size).max(1),
              subjects = matchingSubjects,
              targetSubjects = Nil
            )
          case _ => consequence
      )
      .flatMap(PlanCausalGoalProof.consequenceOnNamedAttackWing(plan.plan.id, _))

  private def dependencySupportsResult(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      dependency: PlanCausalEventDependency,
      consequence: TransitionConsequence
  ): Boolean =
    dependency.proof match
      case PlanCausalDependencyProof.ObjectState(trajectory) =>
        val sameMovedPiece =
          dependency.to.identity.actorRole.exists(_.equalsIgnoreCase(trajectory.pieceRole.name)) &&
            EvidenceRef.sameMove(dependency.to.moveUci, trajectory.futureStep.moveUci)
        val pieceRouteResult =
          pieceRedeploymentPlan(plan) &&
            sameMovedPiece &&
            !PlanCausalEpisode.triggerMoveCapturesPiece(dependency.to) &&
            PlanCausalGoalProof.movedPieceCreatesRouteResult(dependency.to, consequence)
        pieceRouteResult
      case PlanCausalDependencyProof.LineAccess(trajectory) if rookTransferDependency(dependency) =>
        if trajectory.placesPieceBeforeClearance then
          consequence.kind match
            case TransitionConsequenceKind.TargetPressureGain =>
              consequence.subjectsAt(trajectory.enabledTo.key).nonEmpty
            case TransitionConsequenceKind.LineUnlockGain =>
              consequence
                .subjectsForPieceAt(trajectory.enabledPieceRole.name, trajectory.enabledFrom.key)
                .nonEmpty
            case _ => false
        else
          Set(
              TransitionConsequenceKind.MobilityGain,
              TransitionConsequenceKind.TargetPressureGain,
              TransitionConsequenceKind.FileOccupationGain,
              TransitionConsequenceKind.RookLiftActivation,
              TransitionConsequenceKind.BatteryPressureGain
            )(consequence.kind) &&
            consequence.positive &&
            consequence.strength > 0 &&
            resultBoundToFutureMove(
              dependency.to,
              trajectory.enabledPieceRole,
              trajectory.enabledFrom,
              trajectory.enabledTo,
              consequence
            )
      case PlanCausalDependencyProof.LineAccess(trajectory) if pieceRedeploymentPlan(plan) =>
        dependency.to.identity.actorRole.exists(_.equalsIgnoreCase(trajectory.enabledPieceRole.name)) &&
          EvidenceRef.sameMove(dependency.to.moveUci, trajectory.enabledStep.moveUci) &&
          !PlanCausalEpisode.triggerMoveCapturesPiece(dependency.to) &&
          PlanCausalGoalProof.movedPieceCreatesRouteResult(dependency.to, consequence)
      case PlanCausalDependencyProof.RetreatControl(trajectory) =>
        val resultSquares = PlanCausalEpisode.consequenceSquares(consequence).map(_.key.toLowerCase).toSet
        resultSquares(trajectory.pressuredSquare.key.toLowerCase) ||
          resultSquares(trajectory.controlledRetreatSquare.key.toLowerCase)
      case PlanCausalDependencyProof.PawnAdvanceSupport(trajectory) =>
        dependency.to.identity.actorRole.exists(_.equalsIgnoreCase(Pawn.toString)) &&
          EvidenceRef.sameMove(dependency.to.moveUci, trajectory.pawnAdvanceStep.moveUci) &&
          consequence.positive &&
          consequence.strength > 0 &&
          resultBoundToFutureMove(
            dependency.to,
            EvidencePieceRole(Pawn.toString),
            trajectory.pawnFrom,
            trajectory.pawnTo,
            consequence
          )
      case PlanCausalDependencyProof.ResponseContinuation(pawn: PawnBreakFollowUpTrajectory)
          if responseTransformationPlan(plan) =>
        val resultSquares = PlanCausalEpisode.consequenceSquares(consequence).map(_.key.toLowerCase).toSet
        pawn.kind == PawnBreakFollowUpKind.ReleasedPassedPawn &&
          consequence.kind == TransitionConsequenceKind.PassedPawnProgress &&
          pawn.releasedPassedPawn.exists(square =>
            resultSquares(square.key.toLowerCase) ||
              resultSquares(pawn.followUpFrom.key.toLowerCase) ||
              resultSquares(pawn.followUpTo.key.toLowerCase)
          )
      case PlanCausalDependencyProof.ResponseContinuation(_: CaptureResponseFollowUpTrajectory) =>
        transitionFor(rootLine, role, dependency.from).exists(transition =>
          PlanCausalGoalProof.provesAfterInducedResponse(dependency.from.identity, transition, consequence)
        )
      case _ =>
        false

  private def resultBoundToFutureMove(
      event: PlanCausalEventNode,
      role: EvidencePieceRole,
      from: EvidenceSquare,
      to: EvidenceSquare,
      consequence: TransitionConsequence
  ): Boolean =
    val move = EvidenceRef.normalizeMove(event.moveUci)
    val actorMatches =
      event.identity.actorRole.exists(_.equalsIgnoreCase(role.name)) &&
        move.take(2).equalsIgnoreCase(from.key) &&
        move.slice(2, 4).equalsIgnoreCase(to.key)
    val targets = EvidenceObjectBinding.goalTargetObjects(consequence)
    val routeTarget = targets.exists(target =>
      target.kind == EvidenceObjectKind.Square && target.key.equalsIgnoreCase(to.key) ||
        target.kind == EvidenceObjectKind.File && target.key.equalsIgnoreCase(to.key.take(1))
    )
    val attacksTarget = (for
      position <- Fen.read(Standard, Fen.Full(event.step.fenAfter))
      actorSquare <- _root_.chess.Square.fromKey(to.key)
      actor <- position.board.pieceAt(actorSquare)
      if actor.color == event.perspective && actor.role.toString.equalsIgnoreCase(role.name)
    yield targets.exists(target =>
      target.kind == EvidenceObjectKind.Square &&
        _root_.chess.Square.fromKey(target.key).exists(targetSquare =>
          position.board.attackers(targetSquare, event.perspective).squares.contains(actorSquare)
        )
    )).contains(true)
    actorMatches && targets.nonEmpty && (routeTarget || attacksTarget)

  private def pieceRedeploymentPlan(plan: PlanMatch): Boolean =
    plan.support.exists {
      case PlanSupport.Theme(PlanTheme.PieceRedeployment) => true
      case _                                              => false
    }

  private def captureResponseContinuation(dependency: PlanCausalEventDependency): Boolean =
    dependency.proof match
      case PlanCausalDependencyProof.ResponseContinuation(_: CaptureResponseFollowUpTrajectory) => true
      case _                                                                                      => false

  private def transitionFor(
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      event: PlanCausalEventNode
  ): Option[StructuralTransitionBinding] =
    for
      before <- Fen.read(Standard, Fen.Full(event.step.fenBefore))
      after <- Fen.read(Standard, Fen.Full(event.step.fenAfter))
    yield StructuralTransitionBinding(
      moveUci = event.step.moveUci,
      role = role,
      from = PositionNodeRef(event.step.fenBefore, event.step.ply - 1, Some(before.color)),
      to = PositionNodeRef(event.step.fenAfter, event.step.ply, Some(after.color)),
      line = Some(rootLine),
      perspective = event.perspective
    )
