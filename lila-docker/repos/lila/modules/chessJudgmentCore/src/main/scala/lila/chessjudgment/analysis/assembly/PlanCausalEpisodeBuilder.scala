package lila.chessjudgment.analysis.assembly

import chess.Color
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.structure.{ StructuralDeltaAnalyzer, StructuralDeltaContracts }
import lila.chessjudgment.model.PlanMatch
import lila.chessjudgment.model.judgment.*

private[assembly] object PlanCausalEpisodeBuilder:

  def fromLine(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      rootTransition: StructuralTransitionBinding,
      rootIdentity: lila.chessjudgment.model.PlanEventIdentity,
      rootConsequences: List[TransitionConsequence],
      rootDevelopmentChoices: List[StructuralDevelopmentChoice],
      line: LineFactEvidence
  ): PlanCausalEpisode =
    val rootStep = line.lineReplaySteps.headOption
      .filter(step => EvidenceRef.sameMove(step.moveUci, rootTransition.moveUci))
      .getOrElse(
        LineReplayStep(
          ply = rootTransition.from.ply,
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
      continuation = line.lineReplaySteps.dropWhile(_ != rootStep).drop(1)
    )

  def fromContinuation(
      plan: PlanMatch,
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      root: PlanCausalEventNode,
      continuation: List[LineReplayStep]
  ): PlanCausalEpisode =
    val replay = root.step :: continuation
    val candidates = root :: continuation.flatMap(step => eventNode(plan, rootLine, role, root.perspective, step))
    val candidateResponses = candidates.flatMap(trigger => responsesFor(trigger, replay))
    val dependencies = candidates.zipWithIndex.flatMap { case (from, fromIndex) =>
      candidates.drop(fromIndex + 1).flatMap(to =>
        dependency(from, to, replay, candidateResponses)
      )
    }
    val connected = connectedEvents(root, dependencies)
    val connectedDependencies = dependencies.filter(dependency => connected(dependency.from) && connected(dependency.to))
    val connectedResponses = candidateResponses.filter(response => connected(response.trigger))
    PlanCausalEpisode(
      root = root,
      continuations = candidates.filterNot(_ == root).filter(connected).sortBy(_.step.ply),
      dependencies = connectedDependencies,
      responses = connectedResponses
    )

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
        from = PositionNodeRef(step.fenBefore, step.ply, Some(before.color)),
        to = PositionNodeRef(step.fenAfter, step.ply + 1, Some(after.color)),
        line = Some(rootLine),
        perspective = perspective
      )
      val consequences = StructuralDeltaContracts
        .consequences(delta)
        .filter(consequence => consequence.positive && consequence.strength > 0)
        .filter(PlanCausalEventProof.consequenceSupportsPlan(plan, _))
        .flatMap(PlanCausalEventProof.ownedConsequence(plan, _, transition))
      val developmentChoices =
        Option
          .when(PlanCausalEventProof.developmentSupportsPlan(plan))(StructuralDeltaContracts.developmentChoices(delta))
          .getOrElse(Nil)
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
      from: PlanCausalEventNode,
      to: PlanCausalEventNode,
      replay: List[LineReplayStep],
      responses: List[PlanCausalResponse]
  ): List[PlanCausalEventDependency] =
    val fromIndex = replay.indexOf(from.step)
    val toIndex = replay.indexOf(to.step)
    val between = Option.when(fromIndex >= 0 && toIndex > fromIndex)(replay.slice(fromIndex + 1, toIndex)).getOrElse(Nil)
    val plyOffset = to.step.ply - from.step.ply
    val hasPlanResult = to.structuralConsequences.nonEmpty || to.developmentChoices.nonEmpty
    val inducedResponseBetween = responses.exists(response =>
      response.trigger == from && response.step.ply > from.step.ply && response.step.ply < to.step.ply
    )
    val objectState =
      Option
        .when(hasPlanResult || inducedResponseBetween)(
          LineObjectTrajectory.find(from.step, between :+ to.step, (between.size + 1).max(1))
        )
        .flatten
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
    val lineAccess =
      Option
        .when(hasPlanResult)(LineAccessTrajectory.find(from.step, to.step, between))
        .flatten
        .map(trajectory =>
          PlanCausalEventDependency(
            from,
            to,
            PlanCausalDependencyKind.LineAccessPrecondition,
            PlanCausalDependencyProof.LineAccess(trajectory),
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
    List(objectState, lineAccess, sharedTarget, flankAdvance).flatten

  private def flankAdvanceDependency(
      from: PlanCausalEventNode,
      to: PlanCausalEventNode,
      plyOffset: Int
  ): Option[PlanCausalEventDependency] =
    for
      position <- Fen.read(Standard, Fen.Full(from.step.fenBefore))
      king <- position.board.kingPosOf(!from.perspective)
      targets = PlanCausalEpisode.pressureTargetSquares(to).toList.sorted.map(EvidenceSquare(_))
      proof = PlanCausalDependencyProof.FlankAdvance(EvidenceSquare(king.key), targets)
      dependency = PlanCausalEventDependency(
        from,
        to,
        PlanCausalDependencyKind.FlankAdvanceCoordination,
        proof,
        plyOffset
      )
      if dependency.dependencyProven
    yield dependency

  private def responsesFor(
      trigger: PlanCausalEventNode,
      replay: List[LineReplayStep]
  ): List[PlanCausalResponse] =
    val triggerIndex = replay.indexOf(trigger.step)
    if triggerIndex < 0 || PlanCausalEpisode.pressureTargetSquares(trigger).isEmpty then Nil
    else
      replay
        .drop(triggerIndex + 1)
        .takeWhile(step => Fen.read(Standard, Fen.Full(step.fenBefore)).forall(_.color != trigger.perspective))
        .flatMap { step =>
          val origin = EvidenceRef.normalizeMove(step.moveUci).take(2)
          Option
            .when(PlanCausalEpisode.pressureTargetSquares(trigger)(origin))(
              PlanCausalResponse(trigger, step, EvidenceSquare(origin), step.ply - trigger.step.ply)
            )
            .filter(_.proven)
        }

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
