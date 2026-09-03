package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.{ PassedPawnResultActorOccurrence, PassedPawnResultEventIdentity, PassedPawnResultEventOccurrence }
import lila.chessjudgment.model.PassedPawnResultKind
import lila.chessjudgment.model.judgment.*

object PassedPawnResultEventAssembler:

  final private case class PassedPawnResultEventDraft(
      suffix: String,
      position: PositionNodeRef,
      line: LineNodeRef,
      scope: EvidenceScope,
      payload: PassedPawnResultEventEvidence,
      parents: List[EvidenceRef]
  )

  /** Exact structural changed-dependency dispatch for the passed-pawn family.
    * A root is retained when its admitted main replay owns a
    * PassedPawnProgress consequence together with its direct transition
    * proof. No event, episode, or other L1 family is materialized by this
    * predispatch.
    */
  private[assembly] final case class PassedPawnResultDemand(
      subject: CertifiedRootOccurrence,
      trace: CausalLineTrace,
      resultSeeds: List[PassedPawnResultTransitionSeed]
  ):
    require(
      subject.isObserved &&
        trace.replay.headOption.contains(subject.rootStep) &&
        subject.replay.replaySteps.startsWith(trace.replay),
      "the passed-pawn consumer may retain only a contiguous prefix of the requested occurrence"
    )
    require(resultSeeds.nonEmpty, "a passed-pawn result demand needs a graph-owned changed occurrence")
    require(
      resultSeeds.map(_.stableKey).distinct.size == resultSeeds.size &&
        resultSeeds.forall(_.belongsTo(trace)),
      "a passed-pawn result demand cannot duplicate or detach a changed occurrence"
    )

    val rootLine: LineNodeRef = subject.line

  private[assembly] def fromDemand(
      allocator: JudgmentProvenanceAllocator,
      demand: PassedPawnResultDemand
  ): List[EvidenceRecord] =
    val subject = demand.subject
    val rootLine = subject.line
    val transition = subject.transition
    val lineRecord = subject.lineOwner
    val drafts = for
      rootReplayStep <- demand.trace.replay.headOption.toList
      rootObservation <- demand.trace.observation(rootReplayStep).toList
      if rootObservation.lineOccurrenceOwner == lineRecord.ref &&
        rootObservation.transitionOccurrenceOwner == transition.evidence
      structural = rootObservation.structural
      rootMovement = rootObservation.movement
      rootKind <- PassedPawnResultEventProof.eventCandidateKinds(demand.trace, demand.resultSeeds)
      rootResultSeeds = demand.resultSeeds.filter(seed =>
        seed.step == rootReplayStep && seed.resultKind == rootKind
      )
      _ = require(
        rootResultSeeds.forall(seed =>
          seed.observation.structuralOccurrenceRecord == rootObservation.structuralOccurrenceRecord &&
            structural.consequences.count(_ == seed.consequence) == 1
        ),
        "a root result seed must retain the exact graph-owned structural occurrence and consequence"
      )
      rootResultProofs = rootResultSeeds.map(_.resultProof)
      _ = require(
        rootResultProofs.map(_.stableKey).distinct.size == rootResultProofs.size,
        "one exact root result transition proof may be consumed only once"
      )
      rootIdentity = PassedPawnResultEventIdentityBuilder.from(
        rootMove = rootLine.rootMove,
        actor = rootMovement,
        kind = rootKind
      )
      episode <- PassedPawnResultEpisodeBuilder.fromLine(
        rootIdentity = rootIdentity,
        rootConsequences = rootResultProofs.map(_.consequence),
        trace = demand.trace,
        resultSeeds = demand.resultSeeds.filter(_.resultKind == rootKind)
      ).toList
      if episode.causalEpisodeProven && episode.resultRoutes.nonEmpty
      payload = PassedPawnResultEventEvidence(
        rootTransition = structural.transition,
        subjectOccurrence = subject.publicOccurrence,
        causalEpisode = episode,
        directResultProofs = rootResultProofs,
        canonicalRootTransitionProof = structural.canonicalTransitionProof
      )
    yield
      val occurrenceKey = allocator.key(episodeOccurrenceKey(payload))
      PassedPawnResultEventDraft(
        suffix =
          s"passed-pawn-result-event:subject:${allocator.key(payload.subjectOccurrence.stableKey)}:${rootLine.rootMove}:${allocator.key(payload.passedPawnResultKind.id)}:causal:$occurrenceKey",
        position = transition.from,
        line = rootLine,
        scope = transition.role.scope,
        payload = payload,
        parents = payload.lowerOccurrenceOwners
      )
    uniqueDraftsById(
      drafts
        .filter(_.payload.onlyLegalReplyRealized)
        .map(draft =>
          EvidenceRecord(
            ref = allocator.evidenceRef(
              suffix = draft.suffix,
              producer = EvidenceProducer.PassedPawnResultEventProducer,
              layer = EvidenceLayer.PassedPawnResultEvent,
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

  private[assembly] def changedDependencyDemand(
      context: JudgmentAssemblyContext,
      demand: OccurrenceExplanationDemand
  ): Option[PassedPawnResultDemand] =
    observedRootDemand(context, demand.subject)

  private def observedRootDemand(
      context: JudgmentAssemblyContext,
      rootOccurrence: CertifiedRootOccurrence
  ): Option[PassedPawnResultDemand] =
    for
      trace <- graphOwnedTrace(context, rootOccurrence)
      rootObservation <- trace.observation(rootOccurrence.rootStep)
      structural <- rootObservation.structuralOccurrenceRecord.payload match
        case payload: StructuralDeltaEvidence => Some(payload)
        case _                                => None
      legalReply <- structural.certifiedRootResponseMoves
        .map(_.map(EvidenceRef.normalizeMove))
        .collect { case reply :: Nil => reply }
      analyzedReply <- trace.replay.lift(1)
      if EvidenceRef.sameMove(analyzedReply.moveUci, legalReply)
      resultSeeds = trace.replay.flatMap(step =>
        trace.observation(step).toList.flatMap(observation =>
          observation.consequences
            .filter(_.kind == TransitionConsequenceKind.PassedPawnProgress)
            .flatMap(PassedPawnResultTransitionSeed.direct(observation, _))
        )
      ).sortBy(_.stableKey)
      if resultSeeds.nonEmpty
    yield PassedPawnResultDemand(rootOccurrence, trace, resultSeeds)

  private def graphOwnedTrace(
      context: JudgmentAssemblyContext,
      rootOccurrence: CertifiedRootOccurrence
  ): Option[CausalLineTrace] =
    context.evidenceGraph
      .lineTransitionOccurrencePrefix(
        rootOccurrence.line,
        rootOccurrence.transition.evidence.id
      )
      .flatMap(occurrences =>
        rootOccurrence.replay
          .subset(occurrences.map(_.step))
          .flatMap(CausalLineTrace.from(occurrences, _))
      )

  private[assembly] def episodeOccurrenceKey(
      payload: PassedPawnResultEventEvidence
  ): String =
    def exact(values: Iterable[String]): String =
      values.iterator.map(value => s"${value.length}:$value").mkString
    def eventKey(event: PassedPawnResultEventNode): String =
      exact(List(
        PassedPawnResultEventOccurrence.from(
          event = event.identity,
          moveUci = event.moveUci,
          ply = event.step.ply,
          fenBefore = event.step.fenBefore,
          fenAfter = event.step.fenAfter
        ).stableKey,
        event.lineOccurrenceOwner.id,
        event.transitionOccurrenceOwner.id,
        event.structuralOccurrenceOwner.id,
        event.structuralOccurrence.occurrenceId,
        exact(event.structuralConsequences.map(_.stableKey).sorted)
      ))
    def episodeKey(episode: PassedPawnResultEpisode): String =
      val responses = episode.responses.map(response =>
        exact(List(
          eventKey(response.trigger),
          BoundedCausalIdentity.stepKey(response.step),
          response.plyOffset.toString,
          response.capturesRootActor.toString,
          response.answersCheck.toString,
          exact(response.structuralConsequences.map(_.stableKey).sorted)
        ))
      ).sorted
      exact(List(
        exact(episode.resultSteps.map(eventKey)),
        exact(episode.dependencies.map(_.stableKey).sorted),
        exact(responses),
        exact(episode.resultRoutes.map(_.stableKey).sorted)
      ))
    exact(List(
      "passed-pawn-result-event-occurrence:v5",
      payload.subjectOccurrence.stableKey,
      exact(payload.directResultProofs.map(_.stableKey).sorted),
      episodeKey(payload.causalEpisode)
    ))

  private def uniqueDraftsById(records: List[EvidenceRecord]): List[EvidenceRecord] =
    val duplicateIds = records.groupBy(_.ref.id).collect { case (id, owners) if owners.sizeIs > 1 => id }
    require(
      duplicateIds.isEmpty,
      s"passed-pawn-result evidence ids need unique owners: ${duplicateIds.toList.sorted.mkString(",")}"
    )
    records

private[assembly] object PassedPawnResultEventProof:
  def eventCandidateKinds(
      trace: CausalLineTrace,
      resultSeeds: List[PassedPawnResultTransitionSeed]
  ): List[PassedPawnResultKind] =
    require(
      resultSeeds.map(_.stableKey).distinct.size == resultSeeds.size,
      "one exact passed-pawn result seed may activate dispatch only once"
    )
    require(
      resultSeeds.forall(_.belongsTo(trace)),
      "passed-pawn candidate dispatch needs exact trace-owned result seeds"
    )
    val replay = trace.replay
    val rootStep = replay.headOption
    val observed = replay.flatMap(trace.observation)
    val demandedFutureSteps = resultSeeds
      .map(_.step)
      .filterNot(rootStep.contains)
      .toSet
    val reachable =
      rootStep
        .filter(_ => demandedFutureSteps.nonEmpty)
        .map(step => reachableSteps(step, observed.map(_.step), demandedFutureSteps, trace))
        .getOrElse(Set.empty)
    resultSeeds
      .filter(seed => rootStep.contains(seed.step) || reachable(seed.step))
      .map(_.resultKind)
      .toSet
      .toList
      .sortBy(_.id)

  private def reachableSteps(
      root: LineReplayStep,
      observed: List[LineReplayStep],
      demanded: Set[LineReplayStep],
      trace: CausalLineTrace
  ): Set[LineReplayStep] =
    val afterRoot = observed.dropWhile(_ != root).drop(1)
    val demandedPrefix = afterRoot.lastIndexWhere(demanded) match
      case -1        => Nil
      case lastIndex => afterRoot.take(lastIndex + 1)
    demandedPrefix.foldLeft(Set(root)) { (reached, to) =>
      val enabled = (root :: demandedPrefix.takeWhile(_ != to)).exists(from =>
        reached(from) && trace.relation(from, to).exists(_.exactlyEnables)
      )
      if enabled then reached + to else reached
    } - root

private[assembly] object PassedPawnResultEventIdentityBuilder:
  def from(
      rootMove: String,
      actor: CanonicalRootLegalMove,
      kind: PassedPawnResultKind
  ): PassedPawnResultEventIdentity =
    require(
      EvidenceRef.sameMove(rootMove, actor.moveUci),
      "a passed-pawn-result identity must consume the same canonical legal move"
    )
    PassedPawnResultEventIdentity.fromCanonical(
      rootMove = rootMove,
      kind = kind,
      actor = PassedPawnResultActorOccurrence.certified(
        side = actor.side,
        beforeRole = actor.beforeRole.name,
        afterRole = actor.afterRole.name,
        from = actor.from.key,
        to = actor.to.key,
        legalMoveSemanticId = actor.fact.semanticId
      )
    )
