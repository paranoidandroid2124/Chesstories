package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.line.{
  AutomaticTerminal,
  CandidateLineEvaluation,
  PrincipalVariationEvidence
}
import lila.chessjudgment.model.{ PassedPawnResultActorOccurrence, PassedPawnResultEventIdentity, PassedPawnResultEventOccurrence }
import lila.chessjudgment.model.PassedPawnResultKind
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.judgment.{
  AdmittedReviewLine,
  AdmittedMoveReviewInput
}

object PassedPawnResultEventAssembler:

  final private case class ObservedBranchEpisode(
      line: LineNodeRef,
      continuation: List[LineReplayStep],
      episode: PassedPawnResultEpisode,
      trace: CausalLineTrace
  )

  final private case class ObservedBranchLine(
      line: LineNodeRef,
      continuation: List[LineReplayStep],
      trace: CausalLineTrace
  )

  final private case class PassedPawnResultEventDraft(
      suffix: String,
      position: PositionNodeRef,
      line: LineNodeRef,
      scope: EvidenceScope,
      payload: PassedPawnResultEventEvidence,
      parents: List[EvidenceRef]
  )

  def fromAssembly(
      input: AdmittedMoveReviewInput,
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demandingComparison: EvidenceRecord
  ): List[EvidenceRecord] =
    val demandedLines = demandedRootLines(context, demandingComparison)
    val demandIdentity = exactPlayedVsBestDemand(context, demandingComparison)
      .map { case (record, fact) =>
        BoundedCausalIdentity.digest(List(
          "passed-pawn-result-event-demand:v1",
          BoundedCausalIdentity.evidenceRecordKey(record),
          CandidateComparisonSemanticKey.from(fact).stableKey
        ))
      }
      .getOrElse("")
    val graph = context.evidenceGraph
    context.lines
      .filter(line => demandedLines(line.ref))
      .flatMap { candidateLine =>
        val rootLine = candidateLine.ref
        val drafts = for
          transition <- uniqueRootTransition(context, rootLine).toList
          lineRecordAndPayload <- graph.uniqueProofEligibleLineFactRecordFor(rootLine).toList
          structuralRecordAndPayload <- uniqueStructuralRecord(graph, rootLine).toList
          (lineRecord, linePayload) = lineRecordAndPayload
          (structuralRecord, structural) = structuralRecordAndPayload
          lineTrace <- PassedPawnResultEventProof.causalTrace(
            lineRecord,
            linePayload,
            linePayload.certifiedReplay
          ).toList
          rootReplayStep <- linePayload.lineReplaySteps.headOption.toList
          rootStructuralOccurrence <- structural.replayStructuralOccurrence.toList
          if lineTrace.observation(rootReplayStep).exists(observation =>
            observation.lineOccurrenceOwner == lineRecord.ref &&
              observation.structuralOccurrence == rootStructuralOccurrence
          )
          rootCanonicalTransition <- lineTrace.transition(rootReplayStep).toList
          rootMovement = rootCanonicalTransition.relationDelta.rootMove
          observedBranchLines = observedBranchLinesFor(
            input,
            context,
            rootLine,
            lineRecord,
            linePayload,
            structural
          )
          observedBranchKinds = observedBranchLines.flatMap(branch =>
            PassedPawnResultEventProof.eventCandidateKinds(branch.trace)
          )
          rootKindCandidates = (
            PassedPawnResultEventProof.eventCandidateKinds(lineTrace) ++ observedBranchKinds
          ).distinct
          rootKind <- rootKindCandidates
          directFunctionDurable = linePayload.rootActorSurvivesLine.contains(true)
          structuralBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, List(structuralRecord.ref))
          supportedConsequences = structural.consequences
            .filter(consequence => consequence.establishesState && consequence.strength > 0)
              .flatMap(
                PassedPawnResultEventProof.candidateConsequenceForKind(
                  rootKind,
                  _,
                  lineRecord.ref,
                  rootStructuralOccurrence,
                  structural.transition,
                  rootMovement
                )
              )
              .filter(consequence =>
                val durabilityProven = directFunctionDurable || !TransitionConsequenceKind
                  .requiresRootActorSurvival(consequence.kind)
                durabilityProven &&
                PassedPawnResultEventProof.consequenceProvenForRootMove(
                  rootLine,
                  rootLine.rootMove,
                  consequence,
                  PassedPawnResultEventProof.structuralConsequenceEstablishesResult(
                    rootKind,
                    lineRecord.ref,
                    rootStructuralOccurrence,
                    structural,
                    consequence
                  ),
                  structuralBindings
                )
              )
          rootResultProofs = PassedPawnResultEventProof.resultProofs(
            rootKind,
            lineRecord.ref,
            rootStructuralOccurrence,
            structural.transition,
            supportedConsequences,
            rootMovement
          )
          establishedConsequences = rootResultProofs.map(_.consequence)
          rootIdentity = PassedPawnResultEventIdentityBuilder.from(
            rootMove = rootLine.rootMove,
            actor = rootMovement,
            kind = rootKind
          )
          mainLineEpisode = PassedPawnResultEpisodeBuilder.fromLine(
            rootLine = rootLine,
            rootTransition = structural.transition,
            rootLineOwner = lineRecord.ref,
            rootStructuralOccurrence = rootStructuralOccurrence,
            rootIdentity = rootIdentity,
            rootConsequences = establishedConsequences,
            line = linePayload,
            trace = lineTrace
          )
          observedBranchEpisodes = observedBranchEpisodesFor(
            rootLine = rootLine,
            transition = structural.transition,
            root = mainLineEpisode.root,
            branches = observedBranchLines
          )
          observedBranchEpisode <-
            Option.empty[ObservedBranchEpisode] :: observedBranchEpisodes.map(Some(_))
          initialEpisode = observedBranchEpisode.map(_.episode).getOrElse(mainLineEpisode)
          episode = initialEpisode
          if episode.rootEnablesContinuation && episode.resultRoutes.nonEmpty
        yield
          val provisionalPayload = PassedPawnResultEventEvidence(
            rootTransition = structural.transition,
            causalEpisode = episode,
            directResultProofs = rootResultProofs,
            branchWitnesses = Nil,
            continuationSourceLine = observedBranchEpisode.map(_.line),
            canonicalRootTransitionProof = structural.canonicalTransitionProof
          )
          val branchWitnesses = Option
            .when(
              episode.rootEnablesContinuation && provisionalPayload.observedResultRoutes.nonEmpty
            )(provisionalPayload)
            .toList
            .flatMap(event =>
              branchWitnessesFor(
                input,
                context,
                rootLine,
                lineRecord,
                linePayload,
                structural.transition,
                event
              )
            )
          val payload = provisionalPayload.copy(branchWitnesses = branchWitnesses)
          val branchOwnerBindings = branchWitnesses.flatMap(witness =>
            graph.uniqueProofEligibleLineFactRecordFor(witness.line).map(record => witness.line -> record._1.ref.id)
          )
          val occurrenceKey = allocator.key(
            episodeOccurrenceKey(
              payload,
              observedBranchEpisode.map(_.line),
              branchOwnerBindings
            )
          )
          PassedPawnResultEventDraft(
            suffix =
              s"passed-pawn-result-event:${allocator.key(rootLine.role)}:${rootLine.rootMove}:${allocator.key(payload.passedPawnResultKind.id)}:demand:$demandIdentity:causal:$occurrenceKey",
            position = transition.from,
            line = rootLine,
            scope = transition.role.scope,
            payload = payload,
            parents = (
              List(
                structuralRecord.ref,
                lineRecord.ref,
                transition.evidence,
                demandingComparison.ref
              ) ++
                observedBranchEpisode.toList.flatMap(observed =>
                  graph.uniqueProofEligibleLineFactRecordFor(observed.line).map(_._1.ref)
                ) ++
                branchWitnesses.flatMap(witness =>
                  graph.uniqueProofEligibleLineFactRecordFor(witness.line).map(_._1.ref)
                )
            ).distinctBy(_.id)
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

  private[assembly] def demandedRootLines(
      context: JudgmentAssemblyContext,
      demandingComparison: EvidenceRecord
  ): Set[LineNodeRef] =
    exactPlayedVsBestDemand(context, demandingComparison)
      .map { case (_, fact) => Set(fact.referenceLine, fact.candidateLine) }
      .getOrElse(Set.empty)

  private def exactPlayedVsBestDemand(
      context: JudgmentAssemblyContext,
      demandingComparison: EvidenceRecord
  ): Option[(EvidenceRecord, CandidateComparisonFact)] =
    demandingComparison match
      case record @ EvidenceRecord(ref, CandidateComparisonEvidence(fact), _)
          if fact.kind == CandidateComparisonKind.PlayedVsBest &&
            fact.referenceLine.role == LineNodeRole.BestReference &&
            fact.candidateLine.role == LineNodeRole.Played &&
            fact.hasDistinctRootMoves &&
            ref.producer == EvidenceProducer.RelativeMoveProducer &&
            ref.layer == EvidenceLayer.CandidateComparison &&
            ref.line.contains(fact.candidateLine) &&
            ref.scope == EvidenceScope.Counterfactual &&
            context.root.contains(ref.position) &&
            context.lines.exists(_.ref == fact.referenceLine) &&
            context.lines.exists(_.ref == fact.candidateLine) &&
            context.evidenceGraph.record(ref).contains(record) &&
            context.evidenceGraph.proofEligible(record) =>
        Some(record -> fact)
      case _ => None

  private[assembly] def episodeOccurrenceKey(
      payload: PassedPawnResultEventEvidence,
      continuationSource: Option[LineNodeRef],
      branchOwnerBindings: List[(LineNodeRef, String)]
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
    val witnesses = payload.branchWitnesses.map { witness =>
      val exactOwners = branchOwnerBindings.collect {
        case (line, ownerId) if line == witness.line => ownerId
      }.distinct
      require(exactOwners.size == 1, "each passed-pawn result branch witness needs one exact line owner in its occurrence id")
      exact(List(
        witness.sourceProbeId,
        BoundedCausalIdentity.lineKey(witness.line),
        exactOwners.head,
        witness.certifiedHorizonPlyOffset.toString,
        witness.observedThroughPlyOffset.toString,
        witness.terminalOutcome.map(_.toString.toLowerCase).getOrElse("none"),
        witness.terminalPlyOffset.map(_.toString).getOrElse("none"),
        witness.terminalStep.map(BoundedCausalIdentity.stepKey).getOrElse("none"),
        witness.canonicalReplay
          .map(replay => exact(replay.replaySteps.map(BoundedCausalIdentity.stepKey)))
          .getOrElse("none"),
        witness.observedEpisode.map(episodeKey).getOrElse("none")
      ))
    }.sorted
    exact(List(
      "passed-pawn-result-event-occurrence:v2",
      continuationSource.map(_.id).getOrElse("main-line"),
      payload.totalLegalReplyCount.toString,
      exact(payload.directResultProofs.map(_.stableKey).sorted),
      episodeKey(payload.causalEpisode),
      exact(witnesses)
    ))

  private def uniqueDraftsById(records: List[EvidenceRecord]): List[EvidenceRecord] =
    val duplicateIds = records.groupBy(_.ref.id).collect { case (id, owners) if owners.sizeIs > 1 => id }
    require(
      duplicateIds.isEmpty,
      s"passed-pawn-result evidence ids need unique owners: ${duplicateIds.toList.sorted.mkString(",")}"
    )
    records


  private[assembly] def exactBranchReplyLineFor(
      context: JudgmentAssemblyContext,
      branch: AdmittedReviewBranchReply,
      admittedLine: AdmittedReviewLine
  ): Option[CandidateLineNode] =
    context.lines.filter(line =>
      line.ref.role == LineNodeRole.BranchReply &&
        line.ref.rank == admittedLine.rank &&
        admittedLine.rootMove.exists(EvidenceRef.sameMove(_, line.ref.rootMove)) &&
        PrincipalVariationEvidence.sameBoardState(line.evidence.position.fen, branch.branchFen)
    ) match
      case line :: Nil => Some(line)
      case _           => None


  private def branchWitnessesFor(
      input: AdmittedMoveReviewInput,
      context: JudgmentAssemblyContext,
      rootLine: LineNodeRef,
      rootLineRecord: EvidenceRecord,
      rootLinePayload: LineFactEvidence,
      transition: StructuralTransitionBinding,
      expectedEvent: PassedPawnResultEventEvidence
  ): List[PassedPawnReplyBranchWitness] =
    expectedEvent.episode.toList.flatMap(expectedEpisode =>
      val requiredHorizonPlyOffset = expectedEvent.requiredHorizonPlyOffset
      replyBranchLines(input, transition, requiredHorizonPlyOffset)
        .flatMap { case (branch, admittedLine) =>
          exactBranchReplyLineFor(context, branch, admittedLine)
            .flatMap(line => context.evidenceGraph.uniqueProofEligibleLineFactRecordFor(line.ref).map { case (record, payload) =>
              (line, record, payload)
            })
            .flatMap { case (line, lineRecord, payload) =>
              context.continuationReplay(transition, line.ref).map { combinedReplay =>
                val combinedSteps = combinedReplay.replaySteps
                val trace = PassedPawnResultEventProof.causalTrace(
                  combinedReplay,
                  List(rootLineRecord -> rootLinePayload, lineRecord -> payload),
                  Map(
                    rootLineRecord.ref.id -> combinedSteps.headOption.toSet,
                    lineRecord.ref.id -> combinedSteps.drop(1).toSet
                  )
                )
                PassedPawnResultEventProof.branchWitness(
                  sourceProbeId = branch.sourceProbeId,
                  line = line.ref,
                  linePayload = payload,
                  rootLine = rootLine,
                  rootTransition = transition,
                  expectedEpisode = expectedEpisode,
                  requiredHorizonPlyOffset = requiredHorizonPlyOffset,
                  evaluation = admittedLine.evaluation,
                  trace = trace
                )
              }
            }
        }
    )

  private[assembly] def replyBranchLines(
      input: AdmittedMoveReviewInput,
      transition: StructuralTransitionBinding,
      requiredHorizonPlyOffset: Int
  ): List[(AdmittedReviewBranchReply, AdmittedReviewLine)] =
    val covering = input.branchReplies
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
      input: AdmittedMoveReviewInput,
      context: JudgmentAssemblyContext,
      rootLine: LineNodeRef,
      rootLineRecord: EvidenceRecord,
      rootLinePayload: LineFactEvidence,
      structural: StructuralDeltaEvidence
  ): List[ObservedBranchLine] =
    val transition = structural.transition
    val rootStep = LineReplayStep(
      ply = transition.to.ply,
      moveUci = transition.moveUci,
      fenBefore = transition.from.fen,
      fenAfter = transition.to.fen
    )
    passedPawnResultObservationBranches(input, transition)
      .flatMap(branch =>
        branch.lines.flatMap { admittedLine =>
          exactBranchReplyLineFor(context, branch, admittedLine)
            .flatMap(line => context.evidenceGraph.uniqueProofEligibleLineFactRecordFor(line.ref).map { case (record, payload) =>
              (line, record, payload)
            })
            .flatMap { case (line, lineRecord, payload) =>
              for
                combinedReplay <- context.continuationReplay(transition, line.ref)
                combinedSteps = combinedReplay.replaySteps
                if combinedSteps.headOption.contains(rootStep)
                continuation = combinedSteps.drop(1)
                if continuation.map(_.moveUci) == payload.lineReplaySteps.map(_.moveUci)
                trace = PassedPawnResultEventProof.causalTrace(
                  combinedReplay,
                  List(rootLineRecord -> rootLinePayload, lineRecord -> payload),
                  Map(
                    rootLineRecord.ref.id -> Set(rootStep),
                    lineRecord.ref.id -> continuation.toSet
                  )
                )
                if trace.observation(rootStep).exists(_.lineOccurrenceOwner == rootLineRecord.ref)
              yield ObservedBranchLine(line.ref, continuation, trace)
            }
        }
      )
      .distinct

  private def observedBranchEpisodesFor(
      rootLine: LineNodeRef,
      transition: StructuralTransitionBinding,
      root: PassedPawnResultEventNode,
      branches: List[ObservedBranchLine]
  ): List[ObservedBranchEpisode] =
    branches
      .map { branch =>
        val episode = PassedPawnResultEpisodeBuilder.fromContinuation(
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

  private def passedPawnResultObservationBranches(
      input: AdmittedMoveReviewInput,
      transition: StructuralTransitionBinding
  ): List[AdmittedReviewBranchReply] =
    val branches = input.branchReplies.filter(branch =>
        EvidenceRef.sameMove(branch.probedMoveUci, transition.moveUci) &&
        PrincipalVariationEvidence.sameBoardState(branch.branchFen, transition.to.fen)
    )
    require(
      branches.distinct.size == branches.size,
      "one exact passed-pawn-result observation branch may be produced only once"
    )
    branches

  private def branchDependencySpecificity(episode: PassedPawnResultEpisode): Int =
    episode.dependencies
      .filter(dependency => dependency.from == episode.root && dependency.enablesContinuation)
      .map {
        case PassedPawnResultDependency(
              _,
              _,
              PassedPawnResultDependencyKind.LineAccessPrecondition,
              PassedPawnResultDependencyProof.LineAccess(trajectory),
              _
            ) if trajectory.enabledTo == trajectory.vacatedSquare =>
          0
        case dependency if dependency.kind == PassedPawnResultDependencyKind.LineAccessPrecondition => 1
        case _ => 2
      }
      .minOption
      .getOrElse(Int.MaxValue)

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
            payload.transitionIsCertified && payload.exactOutputInventoryCertified &&
            graph.proofEligible(record) =>
        record -> payload
    } match
      case record :: Nil => Some(record)
      case _             => None

private[assembly] object PassedPawnResultEventProof:
  def structuralConsequenceEstablishesResult(
      resultKind: PassedPawnResultKind,
      lineOwner: EvidenceRef,
      structuralOccurrence: ReplayStructuralOccurrence,
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
      .flatMap(
        resultConsequenceForKind(
          resultKind,
          consequence,
          lineOwner,
          structuralOccurrence,
          structural.transition,
          _
        )
      )
      .nonEmpty

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
      canonicalPerspective <- replay.legalSteps.headOption.map(_.move.piece.color)
      role <- transitionRole(line.line.role)
      movement = canonical.relationDelta.rootMove
      transition = StructuralTransitionBinding(
        moveUci = step.moveUci,
        role = role,
        from = PositionNodeRef(step.fenBefore, step.ply - 1, Some(canonical.legal.move.piece.color)),
        to = PositionNodeRef(step.fenAfter, step.ply, Some(canonical.legal.after.color)),
        line = Some(line.line),
        perspective = canonicalPerspective,
        actorRole = Some(EvidencePieceRole(canonical.legal.move.piece.role.name))
      )
    yield CausalStepObservation(
      step,
      transition,
      occurrence.consequences.filter(consequence => consequence.establishesState && consequence.strength > 0),
      movement,
      lineOwner.ref,
      occurrence
    )

  private def transitionRole(role: LineNodeRole): Option[TransitionEdgeRole] =
    role match
      case LineNodeRole.Played        => Some(TransitionEdgeRole.Played)
      case LineNodeRole.BestReference => Some(TransitionEdgeRole.Reference)
      case LineNodeRole.Alternative   => Some(TransitionEdgeRole.Alternative)
      case LineNodeRole.BranchReply   => Some(TransitionEdgeRole.BranchReplyContinuation)

  def eventCandidateKinds(trace: CausalLineTrace): List[PassedPawnResultKind] =
    candidateHypothesisKinds(trace)

  private def candidateHypothesisKinds(trace: CausalLineTrace): List[PassedPawnResultKind] =
    val replay = trace.replay
    val rootStep = replay.headOption
    val observed = replay.flatMap(trace.observation)
    val candidateKinds = observed.map(event => event.step -> candidatePassedPawnResultKindsAt(event))
    val demandedFutureSteps = candidateKinds.collect {
      case (step, kinds) if !rootStep.contains(step) && kinds.nonEmpty => step
    }.toSet
    val reachable =
      rootStep
        .filter(_ => demandedFutureSteps.nonEmpty)
        .map(step => reachableSteps(step, observed.map(_.step), demandedFutureSteps, trace))
        .getOrElse(Set.empty)
    candidateKinds
      .filter { case (step, _) => rootStep.contains(step) || reachable(step) }
      .flatMap(_._2)
      .distinct

  /** Candidate prior only. Public passed-pawn-result authority is granted later by the
    * causal dependency and branch contracts; matching a result vocabulary
    * here is never proof of intent or durability.
    */
  private[assembly] def candidatePassedPawnResultKindsAt(observation: CausalStepObservation): List[PassedPawnResultKind] =
    observation.consequences
      .flatMap(consequence =>
        PassedPawnResultTransitionProof.directCandidates(
          observation.lineOccurrenceOwner,
          observation.structuralOccurrence,
          observation.transition,
          consequence,
          observation.movement
        ).map(_.resultKind)
      )
      .distinct

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

  def rootActorIsPawn(rootLine: LineNodeRef, structural: StructuralDeltaEvidence): Boolean =
    rootActor(rootLine, structural).exists(_._2.role == _root_.chess.Pawn)

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
    event.observedResultRoutes.nonEmpty

  def resultProofs(
      resultKind: PassedPawnResultKind,
      lineOccurrenceOwner: EvidenceRef,
      structuralOccurrence: ReplayStructuralOccurrence,
      transition: StructuralTransitionBinding,
      consequences: List[TransitionConsequence],
      movement: CanonicalRootLegalMove
  ): List[PassedPawnResultTransitionProof] =
    val proofs = consequences.flatMap(consequence =>
      PassedPawnResultTransitionProof.certify(
        resultKind,
        lineOccurrenceOwner,
        structuralOccurrence,
        transition,
        consequence,
        movement
      )
    )
    require(
      proofs.distinct.size == proofs.size,
      "one exact passed-pawn-result transition proof may be produced only once"
    )
    proofs

  def resultConsequenceForKind(
      resultKind: PassedPawnResultKind,
      consequence: TransitionConsequence,
      lineOccurrenceOwner: EvidenceRef,
      structuralOccurrence: ReplayStructuralOccurrence,
      transition: StructuralTransitionBinding,
      movement: CanonicalRootLegalMove
  ): Option[TransitionConsequence] =
    Option.when(
      PassedPawnResultTransitionProof.proves(
        resultKind,
        lineOccurrenceOwner,
        structuralOccurrence,
        transition,
        consequence,
        movement
      )
    )(consequence)

  def candidateConsequenceForKind(
      resultKind: PassedPawnResultKind,
      consequence: TransitionConsequence,
      lineOccurrenceOwner: EvidenceRef,
      structuralOccurrence: ReplayStructuralOccurrence,
      transition: StructuralTransitionBinding,
      movement: CanonicalRootLegalMove
  ): Option[TransitionConsequence] =
    resultConsequenceForKind(
      resultKind,
      consequence,
      lineOccurrenceOwner,
      structuralOccurrence,
      transition,
      movement
    )

  def consequenceProvenForRootMove(
      rootLine: LineNodeRef,
      rootMove: String,
      consequence: TransitionConsequence,
      structuralConsequenceEstablishesResult: Boolean,
      structuralBindings: List[EvidenceObjectBinding]
  ): Boolean =
    structuralConsequenceEstablishesResult &&
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
      expectedEpisode: PassedPawnResultEpisode,
      requiredHorizonPlyOffset: Int,
      evaluation: CandidateLineEvaluation,
      trace: CausalLineTrace
  ): PassedPawnReplyBranchWitness =
    val proofThroughPlyOffset = linePayload.lineReplayCount
      .min(requiredHorizonPlyOffset)
    val continuation = linePayload.lineReplaySteps.take(proofThroughPlyOffset)
    val canonicalPrefix = linePayload.certifiedReplay.flatMap(_.subset(continuation))
    val observedCandidate = PassedPawnResultEpisodeBuilder.fromContinuation(
      rootLine = rootLine,
      role = rootTransition.role,
      root = expectedEpisode.root,
      continuation = continuation,
      trace = Some(trace)
    )
    val observed = Option.when(observedCandidate.rootEnablesContinuation)(observedCandidate)
    val exactTerminalOutcome = evaluation match
      case CandidateLineEvaluation.ExactAutomaticTerminal(_, AutomaticTerminal.Checkmate(winner)) =>
        Some(
          if winner == expectedEpisode.root.perspective then PassedPawnResultTerminalOutcome.Victory
          else PassedPawnResultTerminalOutcome.Defeat
        )
      case CandidateLineEvaluation.ExactAutomaticTerminal(
            _,
            AutomaticTerminal.Stalemate |
            AutomaticTerminal.InsufficientMaterial |
            AutomaticTerminal.FivefoldRepetition |
            AutomaticTerminal.SeventyFiveMoveRule
          ) =>
        Some(PassedPawnResultTerminalOutcome.Draw)
      case CandidateLineEvaluation.EngineSearch(_) => None
    val terminalOutcome =
      Option
        .when(proofThroughPlyOffset == linePayload.lineReplayCount)(continuation.lastOption)
        .flatten
        .flatMap(step => canonicalPrefix.flatMap(_.legalStep(step)))
        .flatMap(_ => exactTerminalOutcome)
    val terminalStep = Option.when(terminalOutcome.nonEmpty)(continuation.lastOption).flatten
    PassedPawnReplyBranchWitness(
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
