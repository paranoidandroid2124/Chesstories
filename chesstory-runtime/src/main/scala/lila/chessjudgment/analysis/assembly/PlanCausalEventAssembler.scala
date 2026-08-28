package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.line.{
  AutomaticTerminal,
  CandidateLineEvaluation,
  CanonicalPositionHistory,
  PrincipalVariationEvidence
}
import lila.chessjudgment.model.{ Plan, PlanActorOccurrence, PlanEventIdentity, PlanEventOccurrence }
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanKind
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.judgment.{
  NormalizedCandidateLine as CanonicalNormalizedCandidateLine,
  NormalizedMoveReviewInput as CanonicalNormalizedMoveReviewInput
}

object PlanCausalEventAssembler:

  final private case class ObservedBranchEpisode(
      line: LineNodeRef,
      continuation: List[LineReplayStep],
      episode: PlanCausalEpisode,
      trace: CausalLineTrace
  )

  final private case class ObservedBranchLine(
      line: LineNodeRef,
      continuation: List[LineReplayStep],
      trace: CausalLineTrace
  )

  final private case class PlanCausalEventDraft(
      suffix: String,
      position: PositionNodeRef,
      line: LineNodeRef,
      scope: EvidenceScope,
      payload: PlanCausalEventEvidence,
      parents: List[EvidenceRef]
  )

  def fromAssembly(
      input: CanonicalNormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val graph = context.evidenceGraph
    context.lines
      .filter(_.role != LineNodeRole.Threat)
      .flatMap { candidateLine =>
        val rootLine = candidateLine.ref
        val drafts = for
          transition <- uniqueRootTransition(context, rootLine).toList
          lineRecordAndPayload <- uniqueLineRecord(graph, rootLine).toList
          structuralRecordAndPayload <- uniqueStructuralRecord(graph, rootLine).toList
          (lineRecord, linePayload) = lineRecordAndPayload
          (structuralRecord, structural) = structuralRecordAndPayload
          lineTrace = PlanCausalEventProof.causalTrace(
            rootLine,
            structural,
            linePayload,
            linePayload.certifiedReplay
          )
          rootReplayStep <- linePayload.lineReplaySteps.headOption.toList
          rootCanonicalTransition <- lineTrace.transition(rootReplayStep).toList
          rootMovement = rootCanonicalTransition.relationDelta.rootMove
          observedBranchLines = observedBranchLinesFor(
            input,
            context,
            rootLine,
            structural
          )
          observedBranchPlans = observedBranchLines.flatMap(branch =>
            PlanCausalEventProof.eventCandidatePlans(branch.trace, structural.perspective)
          )
          rootPlanCandidates = (
            PlanCausalEventProof.eventCandidatePlans(lineTrace, structural.perspective) ++
              observedBranchPlans
          ).distinctBy(_.kind)
          rootPlan <- rootPlanCandidates
          directFunctionDurable = linePayload.rootActorSurvivesLine.contains(true)
          structuralBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, List(structuralRecord.ref))
          supportedConsequences = structural.consequences
            .filter(consequence => consequence.establishesState && consequence.strength > 0)
              .flatMap(
                PlanCausalEventProof.candidateConsequenceForPlan(
                  rootPlan,
                  _,
                  structural.transition,
                  rootMovement
                )
              )
              .filter(consequence =>
                val durabilityProven = directFunctionDurable || !TransitionConsequenceKind
                  .requiresRootActorSurvival(consequence.kind)
                durabilityProven &&
                PlanCausalEventProof.consequenceProvenForRootMove(
                  rootLine,
                  rootLine.rootMove,
                  consequence,
                  PlanCausalEventProof.structuralConsequenceEstablishesPlanGoal(
                    rootPlan,
                    structural,
                    consequence
                  ),
                  Nil,
                  structuralBindings
                )
              )
          rootGoalProofs = PlanCausalEventProof.goalProofs(
            rootPlan,
            structural.transition,
            supportedConsequences,
            rootMovement
          )
          establishedConsequences = rootGoalProofs.map(_.consequence).distinct
          rootIdentity = PlanEventIdentityBuilder.from(
            rootMove = rootLine.rootMove,
            actor = rootMovement,
            plan = rootPlan
          )
          mainLineEpisode = PlanCausalEpisodeBuilder.fromLine(
            plan = rootPlan,
            rootLine = rootLine,
            rootTransition = structural.transition,
            rootIdentity = rootIdentity,
            rootConsequences = establishedConsequences,
            line = linePayload,
            trace = lineTrace
          )
          observedBranchEpisodes = observedBranchEpisodesFor(
            rootLine = rootLine,
            transition = structural.transition,
            plan = rootPlan,
            root = mainLineEpisode.root,
            branches = observedBranchLines,
            positionHistory = input.positionHistory
          )
          observedBranchEpisode <-
            Option.empty[ObservedBranchEpisode] :: observedBranchEpisodes.map(Some(_))
          initialEpisode = observedBranchEpisode.map(_.episode).getOrElse(mainLineEpisode)
          episode = PlanCausalEpisodeBuilder.withHistory(
            plan = rootPlan,
            rootLine = rootLine,
            role = structural.transition.role,
            episode = initialEpisode,
            historyReplay = input.historyReplay,
            trace = lineTrace
          )
          if establishedConsequences.nonEmpty || episode.rootEnablesContinuation
        yield
          val provisionalPayload = PlanCausalEventEvidence(
            rootTransition = structural.transition,
            causalEpisode = episode,
            directGoalProofs = rootGoalProofs,
            branchWitnesses = Nil,
            continuationSourceLine = observedBranchEpisode.map(_.line),
            canonicalRootTransitionProof = structural.canonicalTransitionProof
          )
          val branchWitnesses = Option
            .when(
              episode.rootEnablesContinuation && provisionalPayload.observedGoalResultRoutes.nonEmpty
            )(provisionalPayload)
            .toList
            .flatMap(event =>
              branchWitnessesFor(input, context, rootLine, structural.transition, rootPlan, event)
            )
          val payload = provisionalPayload.copy(branchWitnesses = branchWitnesses)
          val futureKey = if payload.observedGoalResultRoutes.nonEmpty then "causal" else "direct"
          val occurrenceKey = allocator.key(episodeOccurrenceKey(payload.causalEpisode, observedBranchEpisode.map(_.line)))
          PlanCausalEventDraft(
            suffix =
              s"plan-causal-event:${allocator.key(rootLine.role)}:${rootLine.rootMove}:${allocator.key(payload.planId.id)}:$futureKey:$occurrenceKey",
            position = transition.from,
            line = rootLine,
            scope = transition.role.scope,
            payload = payload,
            parents = (
              List(structuralRecord.ref, lineRecord.ref, transition.evidence) ++
                observedBranchEpisode.toList.flatMap(observed =>
                  uniqueLineRecord(graph, observed.line).map(_._1.ref)
                ) ++
                branchWitnesses.flatMap(witness =>
                  uniqueLineRecord(graph, witness.line).map(_._1.ref)
                )
            ).distinctBy(_.id)
          )
        uniqueDraftsById(
          drafts
          .filter(draft =>
            PlanCausalEventProof.decisiveGoalProof(draft.payload)
          )
          .map(draft =>
            EvidenceRecord(
              ref = allocator.evidenceRef(
                suffix = draft.suffix,
                producer = EvidenceProducer.PlanCausalEventProducer,
                layer = EvidenceLayer.PlanCausalEvent,
                position = draft.position,
                line = Some(draft.line),
                scope = draft.scope,
                confidence = EvidenceConfidence.Mixed
              ),
              payload = draft.payload,
              parents = draft.parents
            )
          )
        )
      }

  private def episodeOccurrenceKey(
      episode: PlanCausalEpisode,
      continuationSource: Option[LineNodeRef]
  ): String =
    def exact(values: Iterable[String]): String =
      values.iterator.map(value => s"${value.length}:$value").mkString
    val events = episode.chronologicalSteps.map(event =>
      exact(List(
        PlanEventOccurrence.from(
          event = event.identity,
          moveUci = event.moveUci,
          ply = event.step.ply,
          fenBefore = event.step.fenBefore,
          fenAfter = event.step.fenAfter
        ).stableKey,
        exact(event.structuralConsequences.map(_.stableKey).sorted)
      ))
    )
    val dependencies = episode.dependencies.map(_.stableKey).sorted
    val resultRoutes = episode.resultRoutes.map(_.stableKey).sorted
    exact(List(
      continuationSource.map(_.id).getOrElse("main-line"),
      exact(events),
      exact(dependencies),
      exact(resultRoutes)
    ))

  private def uniqueDraftsById(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.foldLeft(List.empty[EvidenceRecord]) { (accepted, record) =>
      accepted.find(_.ref.id == record.ref.id) match
        case None                           => accepted :+ record
        case Some(existing) if existing == record => accepted
        case Some(_) =>
          throw IllegalArgumentException(
            s"plan-causal evidence id collision for '${record.ref.id}'"
          )
    }


  private[assembly] def exactThreatLineFor(
      context: JudgmentAssemblyContext,
      branch: NormalizedThreatBranch,
      normalized: CanonicalNormalizedCandidateLine
  ): Option[CandidateLineNode] =
    context.lines.filter(line =>
      line.ref.role == LineNodeRole.Threat &&
        line.ref.rank == normalized.rank &&
        normalized.rootMove.exists(EvidenceRef.sameMove(_, line.ref.rootMove)) &&
        PrincipalVariationEvidence.sameBoardState(line.evidence.position.fen, branch.branchFen)
    ) match
      case line :: Nil => Some(line)
      case _           => None


  private[assembly] def lineReplaySteps(positionHistory: CanonicalPositionHistory): List[LineReplayStep] =
    positionHistory.segmentReplaySteps.map(step =>
      LineReplayStep(step.ply, step.uci, step.beforeFen, step.afterFen)
    )

  private def branchWitnessesFor(
      input: CanonicalNormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      rootLine: LineNodeRef,
      transition: StructuralTransitionBinding,
      plan: Plan,
      expectedEvent: PlanCausalEventEvidence
  ): List[PlanCausalBranchWitness] =
    expectedEvent.episode.toList.flatMap(expectedEpisode =>
      val requiredHorizonPlyOffset = expectedEvent.requiredHorizonPlyOffset
      replyBranchLines(input, transition, requiredHorizonPlyOffset)
        .flatMap { case (branch, normalized) =>
          exactThreatLineFor(context, branch, normalized)
            .flatMap(line => linePayload(context, line.ref).map(line -> _))
            .flatMap { case (line, payload) =>
              context.continuationReplay(transition, line.ref).map { combinedReplay =>
                PlanCausalEventProof.branchWitness(
                  sourceProbeId = branch.sourceProbeId,
                  line = line.ref,
                  linePayload = payload,
                  rootLine = rootLine,
                  rootTransition = transition,
                  plan = plan,
                  expectedEpisode = expectedEpisode,
                  requiredHorizonPlyOffset = requiredHorizonPlyOffset,
                  evaluation = normalized.evaluation,
                  admittedReplay = Some(combinedReplay)
                )
              }
            }
        }
    )

  private[assembly] def replyBranchLines(
      input: CanonicalNormalizedMoveReviewInput,
      transition: StructuralTransitionBinding,
      requiredHorizonPlyOffset: Int
  ): List[(NormalizedThreatBranch, NormalizedCandidateLine)] =
    val covering = input.threatBranches
      .filter { branch =>
        EvidenceRef.sameMove(branch.probedMoveUci, transition.moveUci) &&
        PrincipalVariationEvidence.sameBoardState(branch.branchFen, transition.to.fen) &&
        branch.certifiedHorizonPlyOffset >= requiredHorizonPlyOffset
      }
      .flatMap(branch => branch.lines.map(branch -> _))
    val minimumCoveringHorizonByReply = covering
      .flatMap { case (branch, line) =>
        line.rootMove.map(move => EvidenceRef.normalizeMove(move) -> branch.certifiedHorizonPlyOffset)
      }
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.min)
      .toMap
    covering
      .filter { case (branch, line) =>
        line.rootMove.exists(move =>
          minimumCoveringHorizonByReply(EvidenceRef.normalizeMove(move)) == branch.certifiedHorizonPlyOffset
        )
      }
      .sortBy { case (branch, line) =>
        (
          branch.sourceProbeId,
          branch.certifiedHorizonPlyOffset,
          line.rank,
          line.rootMove.getOrElse(""),
          PrincipalVariationEvidence.normalizeFen(branch.branchFen)
        )
      }

  private def observedBranchLinesFor(
      input: CanonicalNormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      rootLine: LineNodeRef,
      structural: StructuralDeltaEvidence
  ): List[ObservedBranchLine] =
    val transition = structural.transition
    val rootStep = LineReplayStep(
      ply = transition.to.ply,
      moveUci = transition.moveUci,
      fenBefore = transition.from.fen,
      fenAfter = transition.to.fen
    )
    val rootObservation = structural.certifiedRootMovement.map(movement =>
      CausalStepObservation(
        rootStep,
        transition,
        structural.consequences,
        movement
      )
    )
    planObservationBranches(input, transition)
      .flatMap(branch =>
        branch.lines.flatMap { normalized =>
          exactThreatLineFor(context, branch, normalized)
            .flatMap(line => linePayload(context, line.ref).map(line -> _))
            .flatMap { case (line, payload) =>
              for
                combinedReplay <- context.continuationReplay(transition, line.ref)
                combinedSteps = combinedReplay.replaySteps
                if combinedSteps.headOption.contains(rootStep)
                continuation = combinedSteps.drop(1)
                if continuation.map(_.moveUci) == payload.lineReplaySteps.map(_.moveUci)
                trace = CausalLineTrace.from(
                  rootLine,
                  transition.role,
                  transition.perspective,
                  combinedSteps,
                  rootObservation,
                  admittedReplay = Some(combinedReplay)
                )
              yield ObservedBranchLine(line.ref, continuation, trace)
            }
        }
      )
      .distinct

  private def observedBranchEpisodesFor(
      rootLine: LineNodeRef,
      transition: StructuralTransitionBinding,
      plan: Plan,
      root: PlanCausalEventNode,
      branches: List[ObservedBranchLine],
      positionHistory: CanonicalPositionHistory
  ): List[ObservedBranchEpisode] =
    branches
      .map { branch =>
        val episode = PlanCausalEpisodeBuilder.fromContinuation(
          plan = plan,
          rootLine = rootLine,
          role = transition.role,
          root = root,
          continuation = branch.continuation,
          trace = Some(branch.trace)
        )
        ObservedBranchEpisode(branch.line, branch.continuation, episode, branch.trace)
      }
      .filter(observed =>
        observed.episode.rootEnablesContinuation &&
          observed.episode.resultRoutes.nonEmpty
      )
      .sortBy(observed =>
        (
          branchDependencySpecificity(observed.episode),
          observed.episode.requiredPlyOffset,
          observed.line.rank,
          observed.line.id
        )
      )

  private def planObservationBranches(
      input: CanonicalNormalizedMoveReviewInput,
      transition: StructuralTransitionBinding
  ): List[NormalizedThreatBranch] =
    input.threatBranches.filter(branch =>
        EvidenceRef.sameMove(branch.probedMoveUci, transition.moveUci) &&
        PrincipalVariationEvidence.sameBoardState(branch.branchFen, transition.to.fen)
    ).distinct

  private def branchDependencySpecificity(episode: PlanCausalEpisode): Int =
    episode.dependencies
      .filter(dependency => dependency.from == episode.root && dependency.enablesContinuation)
      .map {
        case PlanCausalEventDependency(
              _,
              _,
              PlanCausalDependencyKind.LineAccessPrecondition,
              PlanCausalDependencyProof.LineAccess(trajectory),
              _
            ) if trajectory.enabledTo == trajectory.vacatedSquare =>
          0
        case dependency if dependency.kind == PlanCausalDependencyKind.LineAccessPrecondition => 1
        case _ => 2
      }
      .minOption
      .getOrElse(Int.MaxValue)

  private def linePayload(context: JudgmentAssemblyContext, line: LineNodeRef): Option[LineFactEvidence] =
    uniqueLineRecord(context.evidenceGraph, line).map(_._2)

  private def uniqueRootTransition(
      context: JudgmentAssemblyContext,
      line: LineNodeRef
  ): Option[MoveTransitionEdge] =
    context.transitions.filter(edge =>
      edge.role.lineRole == line.role &&
        EvidenceRef.sameMove(edge.moveUci, line.rootMove)
    ) match
      case transition :: Nil => Some(transition)
      case _                 => None

  private def uniqueLineRecord(
      graph: TypedEvidenceGraph,
      line: LineNodeRef
  ): Option[(EvidenceRecord, LineFactEvidence)] =
    graph.records.collect {
      case record @ EvidenceRecord(ref, payload: LineFactEvidence, _) if ref.line.contains(line) =>
        record -> payload
    } match
      case record :: Nil => Some(record)
      case _             => None

  private def uniqueStructuralRecord(
      graph: TypedEvidenceGraph,
      line: LineNodeRef
  ): Option[(EvidenceRecord, StructuralDeltaEvidence)] =
    graph.records.collect {
      case record @ EvidenceRecord(_, payload: StructuralDeltaEvidence, _)
          if payload.line.contains(line) && EvidenceRef.sameMove(payload.moveUci, line.rootMove) =>
        record -> payload
    } match
      case record :: Nil => Some(record)
      case _             => None

private[assembly] object PlanCausalEventProof:
  def structuralConsequenceEstablishesPlanGoal(
      plan: Plan,
      structural: StructuralDeltaEvidence,
      consequence: TransitionConsequence
  ): Boolean =
    structural.consequences.exists(observed =>
      observed.kind == consequence.kind &&
        observed.polarity == consequence.polarity &&
        consequence.subjectFacts.toSet.subsetOf(observed.subjectFacts.toSet)
    ) &&
    consequence.establishesState &&
    consequence.strength > 0 &&
    structural.certifiedRootMovement
      .flatMap(goalConsequenceForPlan(plan, consequence, structural.transition, _))
      .nonEmpty

  def causalTrace(
      rootLine: LineNodeRef,
      structural: StructuralDeltaEvidence,
      line: LineFactEvidence,
      replay: Option[CanonicalLineReplay] = None
  ): CausalLineTrace =
    val rootObservation = for
      step <- line.lineReplaySteps.headOption
      if EvidenceRef.sameMove(step.moveUci, rootLine.rootMove)
      movement <- structural.certifiedRootMovement
    yield
        CausalStepObservation(
          step,
          structural.transition,
          structural.consequences,
          movement
        )
    CausalLineTrace.from(
      rootLine,
      structural.transition.role,
      structural.perspective,
      line.lineReplaySteps,
      rootObservation,
      replay
    )

  def eventCandidatePlans(
      trace: CausalLineTrace,
      perspective: chess.Color
  ): List[Plan] =
    candidateHypothesisPlans(trace, perspective)

  private def candidateHypothesisPlans(
      trace: CausalLineTrace,
      perspective: chess.Color
  ): List[Plan] =
    val replay = trace.replay
    val rootStep = replay.headOption
    val observed = replay.flatMap(trace.observation)
    val reachable =
      rootStep.map(step => reachableSteps(step, observed.map(_.step), trace)).getOrElse(Set.empty)
    val kinds = observed
      .filter(event => rootStep.contains(event.step) || reachable(event.step))
      .flatMap(candidatePlanKindsAt)
      .distinct
    kinds.map(kind => Plan(kind, perspective))

  /** Candidate prior only.  Public plan authority is granted later by the
    * causal dependency and branch contracts; matching a result vocabulary
    * here is never proof of intent or durability.
    */
  private[assembly] def candidatePlanKindsAt(observation: CausalStepObservation): List[PlanKind] =
    observation.consequences
      .flatMap(consequence =>
        PlanCausalGoalProof.directCandidates(
          observation.transition,
          consequence,
          observation.movement
        ).map(_.goalKind)
      )
      .distinct

  /** A newly opened developing square proves that the future move develops a
    * piece, but not that the root move intended that square among all legal
    * development choices.  Proactive forced work therefore requires a named
    * structural result beyond generic opening development. Existing PV moves
    * remain eligible for normal development commentary.
    */
  private[assembly] def latentCandidatePlanKindsAt(observation: CausalStepObservation): List[PlanKind] =
    candidatePlanKindsAt(observation)

  private[assembly] def hasLatentContinuationCandidate(trace: CausalLineTrace): Boolean =
    trace.replay.headOption.exists(root =>
      trace.replay.drop(1).exists(step =>
        trace.relation(root, step).exists(_.exactlyEnables) &&
          trace.observation(step).exists(latentCandidatePlanKindsAt(_).nonEmpty)
      )
    )

  private def reachableSteps(
      root: LineReplayStep,
      observed: List[LineReplayStep],
      trace: CausalLineTrace
  ): Set[LineReplayStep] =
    val edges = observed.zipWithIndex.flatMap { case (from, index) =>
      observed.drop(index + 1).filter { to =>
        trace.relation(from, to).exists(_.exactlyEnables)
      }.map(from -> _)
    }
    @annotation.tailrec
    def expand(reached: Set[LineReplayStep]): Set[LineReplayStep] =
      val next = reached ++ edges.collect { case (from, to) if reached(from) => to }
      if next == reached then reached else expand(next)
    expand(Set(root)) - root

  def rootActorIsPawn(rootLine: LineNodeRef, structural: StructuralDeltaEvidence): Boolean =
    rootActor(rootLine, structural).exists(_._2.role == _root_.chess.Pawn)

  private def rootActor(
      rootLine: LineNodeRef,
      structural: StructuralDeltaEvidence
  ): Option[(_root_.chess.Square, _root_.chess.Piece)] =
    structural.certifiedRootStep
      .filter(step => EvidenceRef.sameMove(step.uci, rootLine.rootMove))
      .map(step => step.move.orig -> step.move.piece)

  def decisiveGoalProof(
      event: PlanCausalEventEvidence
  ): Boolean =
    val futureRoutes = event.observedGoalResultRoutes
    event.directGoalConsequences.nonEmpty || futureRoutes.nonEmpty

  def goalProofs(
      plan: Plan,
      transition: StructuralTransitionBinding,
      consequences: List[TransitionConsequence],
      movement: CanonicalRootLegalMove
  ): List[PlanCausalGoalProof] =
    consequences.flatMap(consequence =>
      PlanCausalGoalProof.certify(
        plan.theme,
        Some(plan.kind),
        transition,
        consequence,
        movement
      )
    ).distinct

  def goalConsequenceForPlan(
      plan: Plan,
      consequence: TransitionConsequence,
      transition: StructuralTransitionBinding,
      movement: CanonicalRootLegalMove
  ): Option[TransitionConsequence] =
    Option.when(
      PlanCausalGoalProof.proves(plan.theme, Some(plan.kind), transition, consequence, movement)
    )(consequence)

  def candidateConsequenceForPlan(
      plan: Plan,
      consequence: TransitionConsequence,
      transition: StructuralTransitionBinding,
      movement: CanonicalRootLegalMove
  ): Option[TransitionConsequence] =
    goalConsequenceForPlan(plan, consequence, transition, movement)

  def consequenceProvenForRootMove(
      rootLine: LineNodeRef,
      rootMove: String,
      consequence: TransitionConsequence,
      structuralConsequenceEstablishesPlanGoal: Boolean,
      planBindings: List[EvidenceObjectBinding],
      structuralBindings: List[EvidenceObjectBinding]
  ): Boolean =
    def matchesRootMove(binding: EvidenceObjectBinding): Boolean =
      binding.line.contains(rootLine) &&
        (binding.actor ++ binding.witness).exists(obj =>
          obj.kind == EvidenceObjectKind.Move && EvidenceRef.sameMove(obj.key, rootMove)
        )
    val planMatchesRootMove = planBindings.exists(matchesRootMove)
    (planMatchesRootMove || structuralConsequenceEstablishesPlanGoal) &&
    consequenceBoundToRootMove(rootLine, rootMove, consequence, structuralBindings)

  def consequenceBoundToRootMove(
      rootLine: LineNodeRef,
      rootMove: String,
      consequence: TransitionConsequence,
      structuralBindings: List[EvidenceObjectBinding]
  ): Boolean =
    val normalizedKind = consequence.kind.toString.trim.toLowerCase
    def matchesRootMove(binding: EvidenceObjectBinding): Boolean =
      binding.line.contains(rootLine) &&
        (binding.actor ++ binding.witness).exists(obj =>
          obj.kind == EvidenceObjectKind.Move && EvidenceRef.sameMove(obj.key, rootMove)
        )
    val consequenceBindings = structuralBindings.filter(binding =>
      matchesRootMove(binding) &&
        binding.mechanism.exists(obj =>
          obj.kind == EvidenceObjectKind.Mechanism && obj.key.trim.toLowerCase == normalizedKind
        )
    )
    val concreteTargetRequired =
      Set(
        TransitionConsequenceKind.OpenFileEstablished,
        TransitionConsequenceKind.SemiOpenFileEstablished,
        TransitionConsequenceKind.PawnTensionCreated,
        TransitionConsequenceKind.PassedPawnProgress,
        TransitionConsequenceKind.BatteryFormation
      )(consequence.kind)
    consequenceBindings.nonEmpty &&
    (!concreteTargetRequired ||
      consequence.subjectFacts.nonEmpty && consequenceBindings.exists(
        _.target.exists(EvidenceObjectBinding.specificSurfaceTargetObject)
      ))

  def branchWitness(
      sourceProbeId: String,
      line: LineNodeRef,
      linePayload: LineFactEvidence,
      rootLine: LineNodeRef,
      rootTransition: StructuralTransitionBinding,
      plan: Plan,
      expectedEpisode: PlanCausalEpisode,
      requiredHorizonPlyOffset: Int,
      evaluation: CandidateLineEvaluation,
      admittedReplay: Option[CanonicalLineReplay] = None
  ): PlanCausalBranchWitness =
    val proofThroughPlyOffset = linePayload.lineReplayCount
      .min(requiredHorizonPlyOffset)
    val continuation = linePayload.lineReplaySteps.take(proofThroughPlyOffset)
    val canonicalPrefix = linePayload.certifiedReplay.flatMap(_.subset(continuation))
    val observedCandidate = PlanCausalEpisodeBuilder.fromContinuation(
      plan = plan,
      rootLine = rootLine,
      role = rootTransition.role,
      root = expectedEpisode.root,
      continuation = continuation,
      admittedReplay = admittedReplay
    )
    val observed = Option.when(observedCandidate.rootEnablesContinuation)(observedCandidate)
    val exactTerminalOutcome = evaluation match
      case CandidateLineEvaluation.ExactAutomaticTerminal(_, AutomaticTerminal.Checkmate(winner)) =>
        Some(
          if winner == expectedEpisode.root.perspective then PlanCausalTerminalOutcome.Victory
          else PlanCausalTerminalOutcome.Defeat
        )
      case CandidateLineEvaluation.ExactAutomaticTerminal(
            _,
            AutomaticTerminal.Stalemate |
            AutomaticTerminal.InsufficientMaterial |
            AutomaticTerminal.FivefoldRepetition |
            AutomaticTerminal.SeventyFiveMoveRule
          ) =>
        Some(PlanCausalTerminalOutcome.Draw)
      case CandidateLineEvaluation.EngineSearch(_) => None
    val terminalOutcome =
      Option
        .when(proofThroughPlyOffset == linePayload.lineReplayCount)(continuation.lastOption)
        .flatten
        .flatMap(step => canonicalPrefix.flatMap(_.legalStep(step)))
        .flatMap(_ => exactTerminalOutcome)
    val terminalStep = Option.when(terminalOutcome.nonEmpty)(continuation.lastOption).flatten
    PlanCausalBranchWitness(
      sourceProbeId = sourceProbeId,
      line = line,
      observedEpisode = observed,
      certifiedHorizonPlyOffset = requiredHorizonPlyOffset,
      observedThroughPlyOffset = proofThroughPlyOffset,
      terminalOutcome = terminalOutcome,
      terminalPlyOffset = Option.when(terminalOutcome.nonEmpty)(continuation.size),
      terminalStep = terminalStep,
      canonicalReplay = canonicalPrefix
    )

private[assembly] object PlanEventIdentityBuilder:
  def from(
      rootMove: String,
      actor: CanonicalRootLegalMove,
      plan: Plan
  ): PlanEventIdentity =
    require(
      EvidenceRef.sameMove(rootMove, actor.moveUci),
      "a plan identity must consume the same canonical legal move"
    )
    PlanEventIdentity.fromCanonical(
      rootMove = rootMove,
      kind = plan.kind,
      actor = PlanActorOccurrence.certified(
        side = actor.side,
        beforeRole = actor.beforeRole.name,
        afterRole = actor.afterRole.name,
        from = actor.from.key,
        to = actor.to.key,
        legalMoveSemanticId = actor.fact.semanticId
      )
    )
