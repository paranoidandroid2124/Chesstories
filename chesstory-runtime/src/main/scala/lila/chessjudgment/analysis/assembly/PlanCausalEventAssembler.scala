package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PrincipalVariationEvidence }
import lila.chessjudgment.model.{ Plan, PlanEventIdentity, PlanEventOccurrence }
import lila.chessjudgment.model.ProbeObjective
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }
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

  final private case class ResourceDeterrenceDraft(
      rootLine: LineNodeRef,
      proof: OpponentResourceDeterrenceProof,
      continuation: List[LineReplayStep],
      replay: CanonicalLineReplay,
      perspective: chess.Color,
      resourceMove: String,
      resourceRole: String,
      materialGain: LineMaterialCapture
  ):
    def plan: Plan =
      Plan(PlanKind.ProphylaxisRestraint, perspective)

    def consequence: TransitionConsequence =
      OpponentResourceDeterrenceProof.consequence(resourceMove, resourceRole)

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
          observedBranchLines = observedBranchLinesFor(
            input,
            context,
            rootLine,
            structural
          )
          observedBranchPlans = observedBranchLines.flatMap(branch =>
            PlanCausalEventProof.eventCandidatePlans(branch.trace, structural.perspective)
          )
          deterrenceDrafts = resourceDeterrenceDrafts(input, context, rootLine, structural.transition)
          rootPlanCandidates = (
            PlanCausalEventProof.eventCandidatePlans(lineTrace, structural.perspective) ++
              observedBranchPlans ++
              deterrenceDrafts.map(_.plan)
          ).distinctBy(_.kind)
          rootPlan <- rootPlanCandidates
          ownedDeterrenceCandidates = deterrenceDrafts.filter(_.plan.kind == rootPlan.kind)
          ownedDeterrence <-
            if ownedDeterrenceCandidates.nonEmpty then ownedDeterrenceCandidates.map(Some(_))
            else List(None)
          rootAlreadyOwned = ownedDeterrence.nonEmpty
          directFunctionDurable = linePayload.rootActorSurvivesLine.contains(true)
          rootMoveIsPromotion = rootLine.rootMove.length == 5
          structuralBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, List(structuralRecord.ref))
          supportedConsequences = structural.consequences
            .filter(consequence => consequence.establishesState && consequence.strength > 0)
              .flatMap(
                PlanCausalEventProof.candidateConsequenceForPlan(rootPlan, _, structural.transition)
              )
              .filter(consequence =>
                val durabilityProven = directFunctionDurable || !TransitionConsequenceKind
                  .requiresRootActorSurvival(consequence.kind)
                !rootMoveIsPromotion &&
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
          rootEstablishedConsequences = PlanCausalEventProof.goalConsequences(
            rootPlan,
            structural.transition,
            supportedConsequences
          )
          establishedConsequences = Option
            .when(rootAlreadyOwned)(
              (
                rootEstablishedConsequences ++
                  ownedDeterrence.map(_.consequence)
              ).distinct
            )
            .getOrElse(Nil)
          rootIdentity = PlanEventIdentityBuilder.from(
            rootMove = rootLine.rootMove,
            actorRole = linePayload.lineReplaySteps.headOption
              .flatMap(lineTrace.legalStep)
              .map(_.move.piece.role.name),
            plan = rootPlan,
            consequences = establishedConsequences
          )
          mainLineEpisode = ownedDeterrence
            .map(draft =>
              deterrenceEpisode(draft, structural.transition, rootIdentity, input.positionHistory)
            )
            .getOrElse(
              PlanCausalEpisodeBuilder.fromLine(
                plan = rootPlan,
                rootLine = rootLine,
                rootTransition = structural.transition,
                rootIdentity = rootIdentity,
                rootConsequences = establishedConsequences,
                line = linePayload,
                positionHistory = input.positionHistory,
                trace = lineTrace
              )
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
          resolvedIdentity = PlanCausalEventProof.goalEstablishedByContinuation(
            rootIdentity,
            establishedConsequences,
            initialEpisode
          )
          resolvedEpisode =
            if resolvedIdentity == rootIdentity then initialEpisode
            else
              (ownedDeterrence, observedBranchEpisode) match
                case (Some(deterrence), _) =>
                  deterrenceEpisode(
                    deterrence,
                    structural.transition,
                    resolvedIdentity,
                    input.positionHistory
                  )
                case (None, Some(observed)) =>
                  PlanCausalEpisodeBuilder.fromContinuation(
                    plan = rootPlan,
                    rootLine = rootLine,
                    role = structural.transition.role,
                    root = observed.episode.root.copy(identity = resolvedIdentity),
                    continuation = observed.continuation,
                    positionHistory = input.positionHistory,
                    trace = Some(observed.trace)
                  )
                case (None, None) =>
                  PlanCausalEpisodeBuilder.fromLine(
                    plan = rootPlan,
                    rootLine = rootLine,
                    rootTransition = structural.transition,
                    rootIdentity = resolvedIdentity,
                    rootConsequences = establishedConsequences,
                    line = linePayload,
                    positionHistory = input.positionHistory,
                    trace = lineTrace
                  )
          episode = PlanCausalEpisodeBuilder.withHistory(
            plan = rootPlan,
            rootLine = rootLine,
            role = structural.transition.role,
            episode = resolvedEpisode,
            historyReplay = input.historyReplay,
            trace = lineTrace
          )
          deterrenceProof = ownedDeterrence
            .map(_.proof)
            .filter(_.certifiedFor(rootLine, structural.transition, episode).nonEmpty)
          if (ownedDeterrence.nonEmpty && deterrenceProof.nonEmpty) ||
            (ownedDeterrence.isEmpty && (
              (rootAlreadyOwned &&
                (establishedConsequences.nonEmpty || episode.causalEpisodeProven)) ||
                (!rootAlreadyOwned && episode.rootEnablesContinuation)
            ))
        yield
          val provisionalPayload = PlanCausalEventEvidence(
            rootTransition = structural.transition,
            causalEpisode = episode.withRootIdentity(resolvedIdentity),
            branchWitnesses = Nil,
            opponentResourceDeterrence = deterrenceProof,
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
          val deterrenceKey = ownedDeterrence
            .map(draft => s":resource:${allocator.key(draft.proof.resourceLine.id)}:${allocator.key(draft.resourceMove)}")
            .getOrElse("")
          PlanCausalEventDraft(
            suffix =
              s"plan-causal-event:${allocator.key(rootLine.role)}:${rootLine.rootMove}:${allocator.key(payload.planId.id)}:$futureKey:$occurrenceKey$deterrenceKey",
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
                ) ++
                deterrenceProof.toList.flatMap(proof =>
                  (proof.resourceLine :: proof.comparisons.map(_.resourceLine)).flatMap(line =>
                    uniqueLineRecord(graph, line).map(_._1.ref)
                  )
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

  private def resourceDeterrenceDrafts(
      input: CanonicalNormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      rootLine: LineNodeRef,
      transition: StructuralTransitionBinding
  ): List[ResourceDeterrenceDraft] =
    val rootBranches =
      input.threatBranches.filter(branch => EvidenceRef.sameMove(branch.probedMoveUci, rootLine.rootMove))
    val drafts = for
      branch <- rootBranches
      (resourceMove, normalizedLine) <- resourceLines(branch)
      resourceLine <- exactThreatLineFor(context, branch, normalizedLine).toList
      payload <- linePayload(context, resourceLine.ref).toList
      combinedReplay <- context.continuationReplay(transition, resourceLine.ref).toList
      continuation = combinedReplay.replaySteps.drop(1)
      continuationLegal = combinedReplay.legalSteps.drop(1)
      _ <- continuation.headOption
        .filter(step => EvidenceRef.sameMove(step.moveUci, resourceMove))
        .toList
      resourceLegal <- continuationLegal.headOption
        .filter(step =>
          EvidenceRef.sameMove(step.uci, resourceMove) &&
            step.move.piece.color == !transition.perspective
        )
        .toList
      resourceRole = resourceLegal.move.piece.role.toString.toLowerCase
      _ <- continuation.lift(1)
        .filter(_ => continuationLegal.lift(1).exists(_.move.piece.color == transition.perspective))
        .toList
      materialGain <- payload
        .opponentResourcePunishmentCapturesFor(transition.perspective)
        .sortBy(_.plyOffset)
      _ <- continuation
        .lift(materialGain.plyOffset)
        .filter(step => EvidenceRef.sameMove(step.moveUci, materialGain.moveUci))
        .toList
      comparisons = input.threatBranches
        .filter(comparison => !EvidenceRef.sameMove(comparison.probedMoveUci, rootLine.rootMove))
        .flatMap(comparison =>
          resourceLines(comparison)
            .collect {
              case (move, normalized) if EvidenceRef.sameMove(move, resourceMove) => normalized
            }
            .flatMap(normalized =>
              exactThreatLineFor(context, comparison, normalized).map(line =>
                OpponentResourceComparison(
                  rootMove = comparison.probedMoveUci,
                  sourceProbeId = comparison.sourceProbeId,
                  resourceLine = line.ref
                )
              )
            )
        )
        .distinct
      if comparisons.nonEmpty
      resourceSequence = continuation.take(materialGain.plyOffset + 1)
      if resourceSequence.size >= 2
      proof = OpponentResourceDeterrenceProof(
        sourceProbeId = branch.sourceProbeId,
        resourceLine = resourceLine.ref,
        comparisons = comparisons,
        materialGainPlyOffset = materialGain.plyOffset
      )
      draft = ResourceDeterrenceDraft(
        rootLine = rootLine,
        proof = proof,
        continuation = continuation,
        replay = combinedReplay,
        perspective = transition.perspective,
        resourceMove = resourceMove,
        resourceRole = resourceRole,
        materialGain = materialGain
      )
    yield draft
    drafts
      .flatMap { draft =>
        val identity = PlanEventIdentityBuilder.from(
          rootMove = rootLine.rootMove,
          actorRole = draft.replay.legalSteps.headOption.map(_.move.piece.role.name),
          plan = draft.plan,
          consequences = List(draft.consequence)
        )
        val episode = deterrenceEpisode(draft, transition, identity, input.positionHistory)
        draft.proof
          .certify(rootLine, transition, episode, context.lines, context.evidenceGraph)
          .flatMap(certified =>
            certified.certifiedComparisonMetrics.map(metrics => draft.copy(proof = certified) -> metrics)
          )
          .toList
      }
      .sortBy { case (draft, (rootImprovement, comparisonContrast)) =>
        (-comparisonContrast, -rootImprovement, draft.materialGain.plyOffset)
      }
      .map(_._1)

  private def resourceLines(
      branch: NormalizedThreatBranch
  ): List[(String, CanonicalNormalizedCandidateLine)] =
    Option
      .when(branch.objective == ProbeObjective.CounterResource)(branch.opponentResourceMove)
      .flatten
      .toList
      .flatMap(resourceMove =>
      branch.lines.flatMap(normalized =>
        normalized.rootMove
          .map(EvidenceRef.normalizeMove)
          .filter(EvidenceRef.sameMove(_, resourceMove))
          .map(move => move -> normalized)
      )
    )
      .distinct

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

  private def deterrenceEpisode(
      draft: ResourceDeterrenceDraft,
      transition: StructuralTransitionBinding,
      identity: lila.chessjudgment.model.PlanEventIdentity,
      positionHistory: CanonicalPositionHistory
  ): PlanCausalEpisode =
    val root = PlanCausalEventNode(
      identity = identity,
      step = LineReplayStep(
        ply = transition.to.ply,
        moveUci = transition.moveUci,
        fenBefore = transition.from.fen,
        fenAfter = transition.to.fen
      ),
      perspective = transition.perspective,
      structuralConsequences = List(draft.consequence)
    )
    PlanCausalEpisodeBuilder.fromContinuation(
      plan = draft.plan,
      rootLine = draft.rootLine,
      role = transition.role,
      root = root,
      continuation = draft.continuation,
      positionHistory = positionHistory,
      observedResultMove = Some(draft.materialGain.moveUci),
      admittedReplay = Some(draft.replay)
    )

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
      replyBranches(input, transition, expectedEvent.requiredHorizonPlyOffset)
        .flatMap(branch =>
          branch.certifiedHorizonPlyOffset.toList.flatMap { certifiedHorizonPlyOffset =>
            branch.lines.flatMap { normalized =>
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
                      certifiedHorizonPlyOffset = certifiedHorizonPlyOffset,
                      positionHistory = input.positionHistory,
                      admittedReplay = Some(combinedReplay)
                    )
                  }
                }
            }
          }
        )
        .distinct
    )

  private def replyBranches(
      input: CanonicalNormalizedMoveReviewInput,
      transition: StructuralTransitionBinding,
      requiredHorizonPlyOffset: Int
  ): List[NormalizedThreatBranch] =
    input.threatBranches
      .filter { branch =>
        branch.objective == ProbeObjective.BranchReplyMultiPv &&
        EvidenceRef.sameMove(branch.probedMoveUci, transition.moveUci) &&
        PrincipalVariationEvidence.sameBoardState(branch.branchFen, transition.to.fen) &&
        branch.certifiedHorizonPlyOffset.contains(requiredHorizonPlyOffset)
      }
      .distinct
      .sortBy(branch => (
        branch.sourceProbeId,
        branch.certifiedHorizonPlyOffset.getOrElse(0),
        branch.lines.map(line => (line.rank, line.rootMove.getOrElse(""))).mkString(":"),
        PrincipalVariationEvidence.normalizeFen(branch.branchFen)
      ))

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
    val rootObservation = CausalStepObservation(
      rootStep,
      transition,
      structural.consequences,
      structural.certifiedRootStep.map(_.move.piece.role)
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
                  Some(rootObservation),
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
          positionHistory = positionHistory,
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
        branch.objective == ProbeObjective.CausalContinuation &&
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
  private val CandidateKinds = PlanKind.values.toList

  def structuralConsequenceEstablishesPlanGoal(
      plan: Plan,
      structural: StructuralDeltaEvidence,
      consequence: TransitionConsequence
  ): Boolean =
    consequence.kind != TransitionConsequenceKind.OpponentMobilityRestriction &&
    structural.consequences.exists(observed =>
      observed.kind == consequence.kind &&
        observed.polarity == consequence.polarity &&
        consequence.subjectFacts.toSet.subsetOf(observed.subjectFacts.toSet)
    ) &&
    consequence.establishesState &&
    consequence.strength > 0 &&
    goalConsequenceForPlan(plan, consequence, structural.transition).nonEmpty

  def causalTrace(
      rootLine: LineNodeRef,
      structural: StructuralDeltaEvidence,
      line: LineFactEvidence,
      replay: Option[CanonicalLineReplay] = None
  ): CausalLineTrace =
    val rootObservation = line.lineReplaySteps.headOption
      .filter(step => EvidenceRef.sameMove(step.moveUci, rootLine.rootMove))
      .map(step =>
        CausalStepObservation(
          step,
          structural.transition,
          structural.consequences,
          structural.certifiedRootStep.map(_.move.piece.role)
        )
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
    CandidateKinds.filter(kind =>
      observation.consequences.exists(consequence =>
        consequence.kind != TransitionConsequenceKind.OpponentMobilityRestriction &&
          PlanCausalGoalProof.proves(kind.theme, Some(kind), observation.transition, consequence)
      )
    )

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

  def goalEstablishedByContinuation(
      initial: PlanEventIdentity,
      rootConsequences: List[TransitionConsequence],
      episode: PlanCausalEpisode
  ): PlanEventIdentity =
    if rootConsequences.nonEmpty then initial
    else if initial.goalTheme == PlanTheme.PieceRedeployment &&
      episode.dependencies.exists(dependency =>
        dependency.from == episode.root &&
          dependency.kind == PlanCausalDependencyKind.LineAccessPrecondition &&
          dependency.proofPieceRoles.exists(_.name.equalsIgnoreCase(_root_.chess.Rook.toString))
      )
    then initial.copy(kind = PlanKind.RookFileTransfer)
    else initial

  def decisiveGoalProof(
      event: PlanCausalEventEvidence
  ): Boolean =
    import TransitionConsequenceKind.*
    val opponentProofReady =
      event.opponentResourceDeterrence.forall(_ => event.opponentResourceDeterrenceProofReady)
    val futureRoutes = event.observedGoalResultRoutes
    val consequences = (
      event.structuralConsequences ++
        futureRoutes.map(_.consequence)
    ).distinct
    opponentProofReady &&
    (event.identity.goalTheme match
      case PlanTheme.OpeningPrinciples =>
        false
      case _ =>
        event.goalDependencyProofReady ||
        event.directGoalConsequences.nonEmpty ||
        futureRoutes.nonEmpty)

  def goalConsequences(
      plan: Plan,
      transition: StructuralTransitionBinding,
      consequences: List[TransitionConsequence]
  ): List[TransitionConsequence] =
    consequences.flatMap(goalConsequenceForPlan(plan, _, transition))

  def goalConsequenceForPlan(
      plan: Plan,
      consequence: TransitionConsequence,
      transition: StructuralTransitionBinding
  ): Option[TransitionConsequence] =
    Option.when(PlanCausalGoalProof.proves(plan.theme, Some(plan.kind), transition, consequence))(consequence)

  def candidateConsequenceForPlan(
      plan: Plan,
      consequence: TransitionConsequence,
      transition: StructuralTransitionBinding
  ): Option[TransitionConsequence] =
    goalConsequenceForPlan(plan, consequence, transition)

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
        TransitionConsequenceKind.FileOccupationEstablished,
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
      certifiedHorizonPlyOffset: Int,
      positionHistory: CanonicalPositionHistory,
      admittedReplay: Option[CanonicalLineReplay] = None
  ): PlanCausalBranchWitness =
    val proofThroughPlyOffset = linePayload.lineReplayCount
      .min(certifiedHorizonPlyOffset)
    val continuation = linePayload.lineReplaySteps.take(proofThroughPlyOffset)
    val observedCandidate = PlanCausalEpisodeBuilder.fromContinuation(
      plan = plan,
      rootLine = rootLine,
      role = rootTransition.role,
      root = expectedEpisode.root,
      continuation = continuation,
      positionHistory = positionHistory,
      admittedReplay = admittedReplay
    )
    val observed = Option.when(observedCandidate.rootEnablesContinuation)(observedCandidate)
    val terminalOutcome =
      Option
        .when(proofThroughPlyOffset == linePayload.lineReplayCount)(continuation.lastOption)
        .flatten
        .flatMap(step => linePayload.certifiedReplay.flatMap(_.legalStep(step)))
        .flatMap(step => terminalOutcomeAt(step.after, expectedEpisode.root.perspective))
    val terminalStep = Option.when(terminalOutcome.nonEmpty)(continuation.lastOption).flatten
    PlanCausalBranchWitness(
      sourceProbeId = sourceProbeId,
      line = line,
      observedEpisode = observed,
      certifiedHorizonPlyOffset = certifiedHorizonPlyOffset,
      observedThroughPlyOffset = proofThroughPlyOffset,
      terminalOutcome = terminalOutcome,
      terminalPlyOffset = Option.when(terminalOutcome.nonEmpty)(continuation.size),
      terminalStep = terminalStep,
      canonicalReplay = linePayload.certifiedReplay
    )

  private[assembly] def terminalOutcomeAt(
      position: chess.Position,
      perspective: chess.Color
  ): Option[PlanCausalTerminalOutcome] =
    position.status.flatMap {
      case chess.Status.Mate =>
        Some(
          if !position.color == perspective then PlanCausalTerminalOutcome.Victory
          else PlanCausalTerminalOutcome.Defeat
        )
      case chess.Status.Stalemate | chess.Status.Draw | chess.Status.InsufficientMaterialClaim =>
        Some(PlanCausalTerminalOutcome.Draw)
      case _ => None
    }

private[assembly] object PlanEventIdentityBuilder:
  private val EventCategories = Set(
    TransitionConsequenceCategory.PawnStructure,
    TransitionConsequenceCategory.PawnStructureDelta,
    TransitionConsequenceCategory.PieceActivity
  )

  def from(
      rootMove: String,
      actorRole: Option[String],
      plan: Plan,
      consequences: List[TransitionConsequence]
  ): PlanEventIdentity =
    val consequenceTargets = consequences
      .filterNot(consequence => PlanCausalEpisode.meansOnlyResultKind(consequence.kind))
      .flatMap(PlanCausalEpisode.goalTargetSubjects)
    val squareTargets =
      consequences
        .filterNot(consequence => PlanCausalEpisode.meansOnlyResultKind(consequence.kind))
        .flatMap(PlanCausalEpisode.consequenceTargetSquares)
        .map(square => s"square:${square.key.toLowerCase}")
    val targets =
      consequenceTargets ++ squareTargets
    val results =
      consequences.flatMap(consequence =>
        s"kind:${consequence.kind.toString.toLowerCase}" ::
          EventCategories.toList.collect {
            case category if StructuralDeltaEvidence.hasConsequenceCategory(consequence.kind, category) =>
              s"category:${category.toString.toLowerCase}"
          }
      )
    PlanEventIdentity.from(
      rootMove = rootMove,
      kind = plan.kind,
      actorRole = actorRole.map(_.trim.toLowerCase).filter(_.nonEmpty),
      targets = targets,
      results = results
    )
