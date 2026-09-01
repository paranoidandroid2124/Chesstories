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

  private[assembly] final case class PassedPawnChangedOccurrence(
      line: LineNodeRef,
      seed: PassedPawnResultTransitionSeed,
      replayOccurrenceKey: String
  ):
    require(
      seed.observation.lineOccurrenceRecord.ref.line.contains(line),
      "a passed-pawn changed occurrence needs its exact line owner"
    )
    require(replayOccurrenceKey.nonEmpty, "a passed-pawn changed occurrence needs its replay owner")

    def lineOwner: EvidenceRecord = seed.observation.lineOccurrenceRecord
    def step: LineReplayStep = seed.step
    def resultKind: PassedPawnResultKind = seed.resultKind

    def stableKey: String =
      List(
        BoundedCausalIdentity.lineKey(line),
        BoundedCausalIdentity.evidenceRecordKey(lineOwner),
        seed.stableKey,
        replayOccurrenceKey
      ).mkString("|")

  private[assembly] final case class PassedPawnRootDemand(
      rootLine: LineNodeRef,
      rootTransition: MoveTransitionEdge,
      rootLineOwner: EvidenceRecord,
      rootLinePayload: LineFactEvidence,
      mainReplay: CanonicalLineReplay,
      structuralOwner: EvidenceRecord,
      structural: StructuralDeltaEvidence,
      changedOccurrences: List[PassedPawnChangedOccurrence]
  ):
    require(rootLineOwner.ref.line.contains(rootLine) && rootLinePayload.line == rootLine)
    require(structuralOwner.ref.line.contains(rootLine) && structural.line.contains(rootLine))
    require(rootTransition.matches(structuralOwner))
    require(changedOccurrences.nonEmpty, "a passed-pawn root demand needs an exact changed occurrence")
    require(
      changedOccurrences.map(_.stableKey).distinct.size == changedOccurrences.size,
      "a passed-pawn root demand cannot duplicate a changed occurrence"
    )

    def stableKey: String =
      List(
        BoundedCausalIdentity.lineKey(rootLine),
        BoundedCausalIdentity.evidenceRecordKey(rootLineOwner),
        BoundedCausalIdentity.evidenceRecordKey(structuralOwner),
        rootTransition.evidence.id,
        mainReplay.replaySteps.map(BoundedCausalIdentity.stepKey).mkString("[", ",", "]"),
        changedOccurrences.map(_.stableKey).sorted.mkString("[", ",", "]")
      ).mkString("|")

  /** Exact structural changed-dependency dispatch for the passed-pawn family.
    * A root is retained when its admitted main replay owns a
    * PassedPawnProgress consequence together with its direct transition
    * proof. No event, episode, or other L1 family is materialized by this
    * predispatch.
    */
  private[assembly] final case class PassedPawnResultDemand(
      input: ExactPlayedVsBestCausalInput,
      roots: List[PassedPawnRootDemand]
  ):
    require(roots.nonEmpty, "a passed-pawn result demand needs a changed root occurrence")
    val rootLines: Set[LineNodeRef] = roots.map(_.rootLine).toSet
    require(
      roots.map(_.rootLine) == List(input.comparison.candidateLine),
      "the current passed-pawn consumer may retain only the exact played endpoint"
    )
    require(rootLines.size == roots.size, "a passed-pawn result demand cannot duplicate a root")

    val stableKey: String =
      BoundedCausalIdentity.digest(
        List(
          "passed-pawn-result-demand:v3",
          BoundedCausalIdentity.evidenceRecordKey(input.demandSource),
          CandidateComparisonSemanticKey.from(input.comparison).stableKey
        ) ++ roots.map(_.stableKey)
      )

  private[assembly] def fromDemand(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: PassedPawnResultDemand
  ): List[EvidenceRecord] =
    val exactDemand = demand.input
    val demandingComparison = exactDemand.demandSource
    val demandIdentity = demand.stableKey
    demand.roots.flatMap { rootDemand =>
        val rootLine = rootDemand.rootLine
        val transition = rootDemand.rootTransition
        val lineRecord = rootDemand.rootLineOwner
        val linePayload = rootDemand.rootLinePayload
        val structuralRecord = rootDemand.structuralOwner
        val structural = rootDemand.structural
        val drafts = for
          lineTrace <- PassedPawnResultEventProof.causalTrace(
            lineRecord,
            linePayload,
            Some(rootDemand.mainReplay)
          ).toList
          rootReplayStep <- linePayload.lineReplaySteps.headOption.toList
          rootStructuralOccurrence <- structural.replayStructuralOccurrence.toList
          if lineTrace.observation(rootReplayStep).exists(observation =>
            observation.lineOccurrenceOwner == lineRecord.ref &&
              observation.structuralOccurrence == rootStructuralOccurrence
          )
          rootCanonicalTransition <- lineTrace.transition(rootReplayStep).toList
          rootMovement = rootCanonicalTransition.relationDelta.rootMove
          mainChangedOccurrences = changedOccurrencesOwnedBy(
            rootDemand.changedOccurrences,
            line = rootLine,
            lineOwner = lineRecord,
            replay = rootDemand.mainReplay,
            admittedSteps = rootDemand.mainReplay.replaySteps.toSet,
            trace = lineTrace
          )
          rootKindCandidates = PassedPawnResultEventProof.eventCandidateKinds(
            lineTrace,
            mainChangedOccurrences.map(_.seed)
          )
          rootKind <- rootKindCandidates
          directFunctionDurable = linePayload.rootActorSurvivesLine.contains(true)
          rootResultSeeds = mainChangedOccurrences
            .filter(occurrence =>
              occurrence.step == rootReplayStep &&
                occurrence.resultKind == rootKind
            )
            .map(_.seed)
          _ = require(
            rootResultSeeds.forall(seed =>
              seed.observation.structuralOccurrence == rootStructuralOccurrence &&
                structural.consequences.count(_ == seed.consequence) == 1
            ),
            "a root result seed must retain the exact structural-delta occurrence and consequence"
          )
          rootResultProofs = rootResultSeeds
            .filter(seed =>
              val durabilityProven = directFunctionDurable || !TransitionConsequenceKind
                .requiresRootActorSurvival(seed.consequence.kind)
              durabilityProven
            )
            .map(seed =>
              PassedPawnResultTransitionProof
                .certify(
                  seed.resultKind,
                  lineRecord.ref,
                  rootStructuralOccurrence,
                  structural.transition,
                  seed.consequence,
                  rootMovement
                )
                .getOrElse(
                  throw new IllegalArgumentException(
                    "a root result seed must bind the graph-owned structural transition"
                  )
                )
            )
          _ = require(
            rootResultProofs.map(_.stableKey).distinct.size == rootResultProofs.size,
            "one exact root result transition proof may be consumed only once"
          )
          establishedConsequences = rootResultProofs.map(_.consequence)
          rootIdentity = PassedPawnResultEventIdentityBuilder.from(
            rootMove = rootLine.rootMove,
            actor = rootMovement,
            kind = rootKind
          )
          mainLineEpisode = PassedPawnResultEpisodeBuilder.fromLine(
            rootTransition = structural.transition,
            rootLineRecord = lineRecord,
            rootStructuralOccurrence = rootStructuralOccurrence,
            rootIdentity = rootIdentity,
            rootConsequences = establishedConsequences,
            line = linePayload,
            trace = lineTrace,
            resultSeeds = mainChangedOccurrences
              .filter(_.resultKind == rootKind)
              .map(_.seed)
          )
          episode = mainLineEpisode
          if episode.causalEpisodeProven && episode.resultRoutes.nonEmpty
          payload = PassedPawnResultEventEvidence(
            rootTransition = structural.transition,
            causalEpisode = episode,
            directResultProofs = rootResultProofs,
            canonicalRootTransitionProof = structural.canonicalTransitionProof
          )
        yield
          val occurrenceKey = allocator.key(episodeOccurrenceKey(payload))
          val parents = List(
            structuralRecord.ref,
            transition.evidence,
            demandingComparison.ref
          ) ++ payload.lineOccurrenceOwners
          require(
            parents.map(_.id).distinct.size == parents.size,
            "a passed-pawn result event cannot hide duplicate parent owners"
          )
          PassedPawnResultEventDraft(
            suffix =
              s"passed-pawn-result-event:${allocator.key(rootLine.role)}:${rootLine.rootMove}:${allocator.key(payload.passedPawnResultKind.id)}:demand:$demandIdentity:causal:$occurrenceKey",
            position = transition.from,
            line = rootLine,
            scope = transition.role.scope,
            payload = payload,
            parents = parents
          )
        uniqueDraftsById(
          drafts
          .filter(draft =>
            PassedPawnResultEventProof.decisiveResultProof(draft.payload)
          )
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
      }

  private[assembly] def changedDependencyDemand(
      context: JudgmentAssemblyContext,
      exact: ExactPlayedVsBestCausalInput
  ): Option[PassedPawnResultDemand] =
    playedRootDemand(context, exact).map(root => PassedPawnResultDemand(exact, List(root)))

  private def playedRootDemand(
      context: JudgmentAssemblyContext,
      exact: ExactPlayedVsBestCausalInput
  ): Option[PassedPawnRootDemand] =
    val rootLine = exact.comparison.candidateLine
    for
      rootTransition <- uniqueRootTransition(context, rootLine)
      linePayload <- exact.playedSource.payload match
        case payload: LineFactEvidence if payload.line == rootLine => Some(payload)
        case _                                                    => None
      structuralOwnerAndPayload <- uniqueStructuralRecord(context.evidenceGraph, rootLine)
      (structuralOwner, structural) = structuralOwnerAndPayload
      if rootTransition.matches(structuralOwner)
      legalReply <- structural.certifiedRootResponseMoves
        .map(_.map(EvidenceRef.normalizeMove))
        .collect { case reply :: Nil => reply }
      playedReply <- exact.playedReplay.replaySteps.lift(1)
      if EvidenceRef.sameMove(playedReply.moveUci, legalReply)
      changes = changedOccurrencesFor(
        rootLine,
        exact.playedSource,
        linePayload,
        exact.playedReplay,
        exact.playedReplay.replaySteps.toSet
      ).sortBy(_.stableKey)
      if changes.nonEmpty
    yield PassedPawnRootDemand(
      rootLine,
      rootTransition,
      exact.playedSource,
      linePayload,
      exact.playedReplay,
      structuralOwner,
      structural,
      changes
    )

  private def changedOccurrencesFor(
      line: LineNodeRef,
      lineOwner: EvidenceRecord,
      linePayload: LineFactEvidence,
      replay: CanonicalLineReplay,
      admittedSteps: Set[LineReplayStep]
  ): List[PassedPawnChangedOccurrence] =
    val replayOccurrenceKey = passedPawnReplayOccurrenceKey(replay)
    replay
      .structuralConsequenceOccurrences(TransitionConsequenceKind.PassedPawnProgress)
      .flatMap {
        case (step, consequences) if admittedSteps(step) =>
          require(
            consequences.distinct.size == consequences.size,
            "one replay occurrence cannot duplicate an exact passed-pawn consequence"
          )
          PassedPawnResultEventProof
            .exactStructuralObservation(lineOwner, linePayload, step, replay)
            .toList
            .flatMap(observation =>
              consequences.flatMap(consequence =>
                PassedPawnResultTransitionSeed
                  .direct(observation, consequence)
                  .map(seed =>
                    PassedPawnChangedOccurrence(
                      line,
                      seed,
                      replayOccurrenceKey
                    )
                  )
              )
            )
        case _ => Nil
      }

  private def passedPawnReplayOccurrenceKey(replay: CanonicalLineReplay): String =
    BoundedCausalIdentity.digest(
      "passed-pawn-replay-occurrence:v1" ::
        replay.replaySteps.map(BoundedCausalIdentity.stepKey)
    )

  private def changedOccurrencesOwnedBy(
      occurrences: List[PassedPawnChangedOccurrence],
      line: LineNodeRef,
      lineOwner: EvidenceRecord,
      replay: CanonicalLineReplay,
      admittedSteps: Set[LineReplayStep],
      trace: CausalLineTrace
  ): List[PassedPawnChangedOccurrence] =
    val replayOccurrenceKey = passedPawnReplayOccurrenceKey(replay)
    val owned = occurrences.filter(occurrence =>
      occurrence.line == line &&
        occurrence.lineOwner == lineOwner &&
        occurrence.replayOccurrenceKey == replayOccurrenceKey
    )
    require(
      owned.forall(occurrence =>
        admittedSteps(occurrence.step) && occurrence.seed.belongsTo(trace)
      ),
      "a passed-pawn changed occurrence must retain its exact replay path and line owner"
    )
    owned

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
      "passed-pawn-result-event-occurrence:v4",
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

  private def uniqueStructuralRecord(
      graph: TypedEvidenceGraph,
      line: LineNodeRef
  ): Option[(EvidenceRecord, StructuralDeltaEvidence)] =
    graph.recordsFor(line).collect {
      case record @ EvidenceRecord(ref, payload: StructuralDeltaEvidence, _)
          if ref.producer == EvidenceProducer.StructuralDeltaProducer &&
            ref.layer == EvidenceLayer.StructuralDelta &&
            ref.confidence == EvidenceConfidence.BoardDerived &&
            payload.line.contains(line) && EvidenceRef.sameMove(payload.moveUci, line.rootMove) &&
            payload.transitionIsCertified && payload.canonicalOutputShapeCertified &&
            graph.proofEligible(record) =>
        record -> payload
    } match
      case record :: Nil => Some(record)
      case _             => None

private[assembly] object PassedPawnResultEventProof:
  def causalTrace(
      lineOwner: EvidenceRecord,
      line: LineFactEvidence,
      replay: Option[CanonicalLineReplay] = None
  ): Option[CausalLineTrace] =
    replay
      .filter(_.matches(line.lineReplaySteps))
      .map(causalTrace(_, List(lineOwner -> line)))

  private[assembly] def causalTrace(
      replay: CanonicalLineReplay,
      lineOwners: List[(EvidenceRecord, LineFactEvidence)]
  ): CausalLineTrace =
    causalTrace(
      replay,
      lineOwners,
      lineOwners.map { case (record, line) => record.ref.id -> line.lineReplaySteps.toSet }.toMap
    )

  private[assembly] def causalTrace(
      replay: CanonicalLineReplay,
      lineOwners: List[(EvidenceRecord, LineFactEvidence)],
      exactOwnerSteps: Map[String, Set[LineReplayStep]]
  ): CausalLineTrace =
    val replayStepSet = replay.replaySteps.toSet
    val ownerIds = lineOwners.map(_._1.ref.id)
    require(
      ownerIds.distinct.size == ownerIds.size,
      "one causal line owner id cannot have multiple producers"
    )
    val ownerLinesById = lineOwners.map { case (record, line) => record.ref.id -> line }.toMap
    val assignments = exactOwnerSteps.toList.flatMap { case (ownerId, steps) =>
      steps.toList.map(step => step -> ownerId)
    }
    val assignmentOwnersByStep = assignments.groupMap(_._1)(_._2)
    require(
      assignmentOwnersByStep.values.forall(_.size == 1),
      "one causal replay step cannot have multiple observation producers"
    )
    val manifestCertified =
      lineOwners.nonEmpty &&
        exactOwnerSteps.keySet == ownerIds.toSet && exactOwnerSteps.values.forall(_.nonEmpty) &&
        assignments.forall { case (step, ownerId) =>
          replayStepSet(step) && ownerLinesById.get(ownerId).exists(_.lineReplaySteps.contains(step))
        } &&
        replay.replaySteps.forall(assignmentOwnersByStep.contains)
    CausalLineTrace.from(
      replay.replaySteps,
      Option.when(manifestCertified)(
        replay.replaySteps.flatMap(step =>
          lineOwners.flatMap { case (record, line) =>
            Option
              .when(exactOwnerSteps.get(record.ref.id).exists(_(step)))(())
              .flatMap(_ => exactStructuralObservation(record, line, step, replay))
          }
        )
      ).getOrElse(Nil),
      Some(replay)
    )

  private[assembly] def exactStructuralObservation(
      lineOwner: EvidenceRecord,
      line: LineFactEvidence,
      step: LineReplayStep,
    replay: CanonicalLineReplay
  ): Option[CausalStepObservation] =
    for
      canonical <- replay.transition(step)
      ownedReplay <- line.certifiedReplay
      if lineOwner.payload == line
      if lineOwner.ref.producer == EvidenceProducer.LegalLineProducer
      if lineOwner.ref.layer == EvidenceLayer.Line
      if lineOwner.ref.confidence == EvidenceConfidence.LegalReplayVerified
      if lineOwner.ref.line.contains(line.line) && lineOwner.ref.scope == line.line.role.scope
      if ownedReplay.replaySteps.contains(step)
      ownedOccurrence <- ownedReplay.structuralOccurrence(step)
      occurrence <- replay.structuralOccurrence(step)
      if ownedOccurrence == occurrence
      role <- transitionRole(line.line.role)
      movement = canonical.relationDelta.rootMove
      transition = StructuralTransitionBinding(
        moveUci = step.moveUci,
        role = role,
        from = PositionNodeRef(step.fenBefore, step.ply - 1, Some(movement.side)),
        to = PositionNodeRef(step.fenAfter, step.ply, Some(!movement.side)),
        line = Some(line.line),
        perspective = movement.side,
        actorRole = Some(movement.beforeRole)
      )
    yield CausalStepObservation(
      step,
      transition,
      occurrence.consequences.filter(_.establishesState),
      movement,
      lineOwner,
      occurrence
    )

  private def transitionRole(role: LineNodeRole): Option[TransitionEdgeRole] =
    role match
      case LineNodeRole.Played        => Some(TransitionEdgeRole.Played)
      case LineNodeRole.BestReference => Some(TransitionEdgeRole.Reference)
      case LineNodeRole.Alternative   => Some(TransitionEdgeRole.Alternative)

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

  private def rootActor(
      rootLine: LineNodeRef,
      structural: StructuralDeltaEvidence
  ): Option[(_root_.chess.Square, _root_.chess.Piece)] =
    structural.certifiedRootStep
      .filter(step => EvidenceRef.sameMove(step.uci, rootLine.rootMove))
      .map(step => step.move.orig -> step.move.piece)

  def decisiveResultProof(
      event: PassedPawnResultEventEvidence
  ): Boolean =
    event.onlyLegalReplyRealized

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
