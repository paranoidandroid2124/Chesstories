package lila.chessjudgment.analysis.assembly

import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PrincipalVariationEvidence }
import lila.chessjudgment.model.{ Plan, PlanEventIdentity, PlanMatch, PlanSupport }
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.judgment.{
  NormalizedCandidateLine as CanonicalNormalizedCandidateLine,
  NormalizedMoveReviewInput as CanonicalNormalizedMoveReviewInput
}

object PlanCausalEventAssembler:

  private final case class ObservedBranchEpisode(
      line: LineNodeRef,
      continuation: List[LineReplayStep],
      episode: PlanCausalEpisode
  )

  private final case class ResourceDeterrenceDraft(
      rootLine: LineNodeRef,
      proof: OpponentResourceDeterrenceProof,
      continuation: List[LineReplayStep],
      perspective: chess.Color,
      resourceMove: String,
      materialGain: LineMaterialCapture
  ):
    def plan: PlanMatch =
      PlanMatch(
        plan = Plan.Prophylaxis(perspective),
        score = 1.0,
        evidence = Nil,
        support = List(
          PlanSupport.Theme(PlanTheme.RestrictionProphylaxis),
          PlanSupport.Subplan(PlanKind.ProphylaxisRestraint)
        )
      )

    def consequence: TransitionConsequence =
      OpponentResourceDeterrenceProof.consequence(resourceMove)

    def comparisonContrastWinPercent(lines: List[CandidateLineNode]): Double =
      proof
        .bestComparisonContrastWinPercent(perspective, lines)
        .getOrElse(Double.NegativeInfinity)

  def fromAssembly(
      input: CanonicalNormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val graph = context.evidenceGraph
    val history = lineReplaySteps(input.positionHistory)
    graph.records.flatMap {
      case pressureRecord @ EvidenceRecord(pressureRef, pressure: PlanPressureEvidence, _)
          if pressureRef.line.exists(_.role != LineNodeRole.Threat) =>
        val drafts = for
          rootLine <- pressureRef.line.toList
          transition <- context.transitions.filter(edge =>
            edge.role.lineRole == rootLine.role && EvidenceRef.sameMove(edge.moveUci, rootLine.rootMove)
          )
          lineRecordAndPayload <- graph.records.collectFirst {
            case record @ EvidenceRecord(ref, payload: LineFactEvidence, _) if ref.line.contains(rootLine) =>
              record -> payload
          }.toList
          structuralRecordAndPayload <- pressureRecord.parents.flatMap(parent => graph.byId.get(parent.id)).collectFirst {
            case record @ EvidenceRecord(_, payload: StructuralDeltaEvidence, _)
                if payload.line.contains(rootLine) && EvidenceRef.sameMove(payload.moveUci, rootLine.rootMove) =>
              record -> payload
          }.toList
          (structuralRecord, structural) = structuralRecordAndPayload
          (_, linePayload) = lineRecordAndPayload
          deterrenceDraft = resourceDeterrenceDraft(input, context, rootLine, structural.transition)
          rootOwnedPlans = PlanCausalEventProof.rootOwnedPlans(pressure, rootLine, structural, Some(linePayload))
          rootPlan <- (
            PlanCausalEventProof.eventCandidatePlans(pressure, rootLine, structural, linePayload) ++
              deterrenceDraft.map(_.plan)
          ).distinctBy(_.plan.id)
          ownedDeterrence = deterrenceDraft.filter(_.plan.plan.id == rootPlan.plan.id)
          rootAlreadyOwned = rootOwnedPlans.exists(_.plan.id == rootPlan.plan.id) || ownedDeterrence.nonEmpty
          directFunctionDurable = linePayload.rootActorSurvivesLine.contains(true)
          directBlockadeProven = PlanCausalEventProof.directPawnBlockadeFunctionProven(
            pressure,
            rootPlan,
            rootLine,
            structural,
            linePayload
          )
          rootMoveIsPromotion = rootLine.rootMove.length == 5
          planMotifRefs = rootPlanMotifRefs(graph, pressureRecord, rootLine, rootPlan)
          planBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, planMotifRefs)
          structuralBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, List(structuralRecord.ref))
          supportedConsequences = structural.consequences.flatMap { observedConsequence =>
            PlanCausalEventProof
              .rootProofConsequence(structural, Some(linePayload), observedConsequence)
              .filter(consequence => consequence.positive && consequence.strength > 0)
              .flatMap(PlanCausalEventProof.rootCandidateConsequenceForPlan(rootPlan, _, structural.transition))
              .filter(consequence =>
                val durabilityProven =
                  if PlanCausalEventProof.directPawnBlockade(consequence) then directBlockadeProven
                  else directFunctionDurable || !TransitionConsequenceKind.requiresRootActorSurvival(consequence.kind)
                !rootMoveIsPromotion &&
                  durabilityProven &&
                  PlanCausalEventProof.consequenceProvenForRootMove(
                    rootLine,
                    rootLine.rootMove,
                    consequence,
                    PlanCausalEventProof.structuralConsequenceEstablishesPlanGoal(
                      rootPlan,
                      structural,
                      consequence,
                      Some(linePayload)
                    ),
                    planBindings,
                    structuralBindings
                  )
              )
          }
          rootPositiveConsequences = PlanCausalEventProof.goalConsequences(
            rootPlan,
            structural.transition,
            supportedConsequences
          )
          directPiecePressureConsequences =
            Option
              .when(
                rootAlreadyOwned &&
                  !rootMoveIsPromotion &&
                  directFunctionDurable &&
                  !PlanCausalEventProof.rootActorIsPawn(rootLine, structural)
              )(
                for
                  theme <- rootPlan.support.collectFirst { case PlanSupport.Theme(value) => value }.toList
                  kind = rootPlan.support.collectFirst { case PlanSupport.Subplan(value) => value }
                  observed <- structural.consequences
                  consequence <- PlanCausalEventProof.rootProofConsequence(structural, Some(linePayload), observed).toList
                  if PlanCausalGoalProof.ownsDirectPiecePressureAlongsideRoute(
                    theme,
                    kind,
                    structural.transition,
                    rootPositiveConsequences,
                    consequence
                  )
                  if PlanCausalEventProof.consequenceProvenForRootMove(
                    rootLine,
                    rootLine.rootMove,
                    consequence,
                    false,
                    planBindings,
                    structuralBindings
                  )
                yield consequence
              )
              .getOrElse(Nil)
          directExchangeLineConsequences =
            Option
              .when(rootAlreadyOwned && !rootMoveIsPromotion)(
                for
                  theme <- PlanCausalEventProof.planTheme(rootPlan).toList
                  observed <- structural.consequences
                  consequence <- PlanCausalEventProof.rootProofConsequence(structural, Some(linePayload), observed).toList
                  if PlanCausalGoalProof.lineAccessAdvancesExchangeGoal(
                    theme,
                    rootPositiveConsequences,
                    consequence
                  )
                  if PlanCausalEventProof.rootLineAccessRealized(
                    rootLine,
                    linePayload,
                    consequence
                  )
                  if PlanCausalEventProof.consequenceBoundToRootMove(
                    rootLine,
                    rootLine.rootMove,
                    consequence,
                    structuralBindings
                  )
                yield consequence
              )
              .getOrElse(Nil)
          rootDevelopmentChoices = structural.developmentChoices.filter(choice =>
            !rootMoveIsPromotion &&
              linePayload.rootActorSurvivesReply.contains(true) &&
              PlanCausalEventProof.developmentSupportsPlan(rootPlan) &&
              EvidenceRef.sameMove(s"${choice.from}${choice.to}", rootLine.rootMove)
          )
          positiveConsequences = Option
            .when(rootAlreadyOwned)(
              (
                rootPositiveConsequences ++
                  directPiecePressureConsequences ++
                  directExchangeLineConsequences ++
                  ownedDeterrence.map(_.consequence)
              ).distinct
            )
            .getOrElse(Nil)
          developmentChoices = Option.when(rootAlreadyOwned)(rootDevelopmentChoices).getOrElse(Nil)
          rootIdentity = PlanEventIdentityBuilder.from(
            rootMove = rootLine.rootMove,
            beforeFen = structural.transition.from.fen,
            plan = rootPlan,
            consequences = positiveConsequences,
            developmentChoices = developmentChoices
          )
          mainLineEpisode = ownedDeterrence
            .map(draft => deterrenceEpisode(draft, structural.transition, rootIdentity, input.positionHistory))
            .getOrElse(
              PlanCausalEpisodeBuilder.fromLine(
                plan = rootPlan,
                rootLine = rootLine,
                rootTransition = structural.transition,
                rootIdentity = rootIdentity,
                rootConsequences = positiveConsequences,
                rootDevelopmentChoices = developmentChoices,
                line = linePayload,
                positionHistory = input.positionHistory
              )
            )
          observedBranchEpisodes = observedBranchEpisodesFor(
            input = input,
            context = context,
            rootLine = rootLine,
            transition = structural.transition,
            plan = rootPlan,
            root = mainLineEpisode.root
          )
          observedBranchEpisode = preferredObservedBranchEpisode(
            input = input,
            context = context,
            rootLine = rootLine,
            transition = structural.transition,
            plan = rootPlan,
            mainLineEpisode = mainLineEpisode,
            observed = observedBranchEpisodes
          )
          initialEpisode = observedBranchEpisode.map(_.episode).getOrElse(mainLineEpisode)
          resolvedIdentity = PlanCausalEventProof.goalEstablishedByContinuation(
            rootIdentity,
            structural.transition,
            positiveConsequences,
            initialEpisode
          )
            resolvedEpisode =
              if resolvedIdentity == rootIdentity then initialEpisode
              else
                (ownedDeterrence, observedBranchEpisode) match
                  case (Some(deterrence), _) =>
                    deterrenceEpisode(deterrence, structural.transition, resolvedIdentity, input.positionHistory)
                  case (None, Some(observed)) =>
                    PlanCausalEpisodeBuilder.fromContinuation(
                      plan = rootPlan,
                      rootLine = rootLine,
                      role = structural.transition.role,
                      root = observed.episode.root.copy(identity = resolvedIdentity),
                      continuation = observed.continuation,
                      positionHistory = input.positionHistory,
                      observedReplyBranch = true
                    )
                  case (None, None) =>
                    PlanCausalEpisodeBuilder.fromLine(
                      plan = rootPlan,
                      rootLine = rootLine,
                      rootTransition = structural.transition,
                      rootIdentity = resolvedIdentity,
                      rootConsequences = positiveConsequences,
                      rootDevelopmentChoices = developmentChoices,
                    line = linePayload,
                    positionHistory = input.positionHistory
                    )
          episode = PlanCausalEpisodeBuilder.withHistory(
            plan = rootPlan,
            rootLine = rootLine,
            role = structural.transition.role,
            episode = resolvedEpisode,
            history = history
          )
          observedMaterialCosts =
            Option
              .when(
                episode.causalEpisodeProven && observedBranchEpisode.isEmpty && ownedDeterrence.isEmpty
              )(
                linePayload
                  .principalSacrificeCostSequenceForRootMove(rootLine.rootMove)
                  .flatMap(capture =>
                    linePayload.lineReplaySteps.lift(capture.plyOffset).map(step =>
                      ObservedPlanCost(
                        capture,
                        step,
                        linePayload
                          .rootMaterialCapture(rootLine.rootMove)
                          .filter(offer => LineMaterialSummary.materialSacrificePair(offer, capture))
                      )
                    )
                  )
                  .filter(_.proven(episode.root))
              )
              .getOrElse(Nil)
          deterrenceProof = ownedDeterrence
            .map(_.proof)
            .filter(_.proven(rootLine, structural.transition, episode, context.lines, context.evidenceGraph))
          if
            (ownedDeterrence.nonEmpty && deterrenceProof.nonEmpty) ||
              (ownedDeterrence.isEmpty && (
                (rootAlreadyOwned &&
                  (positiveConsequences.nonEmpty || developmentChoices.nonEmpty || episode.causalEpisodeProven)) ||
                  (!rootAlreadyOwned && (episode.rootEnablesContinuation || episode.responseResultProven))
              ))
        yield
          val provisionalPayload = PlanCausalEventEvidence(
            rootTransition = structural.transition,
            causalEpisode = episode.withRootIdentity(resolvedIdentity),
            branchWitnesses = Nil,
            observedMaterialCosts = observedMaterialCosts,
            opponentResourceDeterrence = deterrenceProof,
            continuationSourceLine = observedBranchEpisode.map(_.line)
          )
          val branchWitnesses = Option
            .when(
              episode.rootEnablesContinuation && provisionalPayload.publicTailExpectedResultAssessment.nonEmpty
            )(provisionalPayload)
            .toList
            .flatMap(event =>
              branchWitnessesFor(input, context, rootLine, structural.transition, rootPlan, event)
            )
          val payload = provisionalPayload.copy(branchWitnesses = branchWitnesses)
          val futureKey = payload.futureMove.getOrElse("direct")
          EvidenceRecord(
            ref = allocator.evidenceRef(
              suffix = s"plan-causal-event:${allocator.key(rootLine.role)}:${rootLine.rootMove}:${allocator.key(payload.planId.id)}:$futureKey",
              producer = EvidenceProducer.PlanCausalEventProducer,
              layer = EvidenceLayer.PlanCausalEvent,
              position = transition.from,
              line = Some(rootLine),
              scope = transition.role.scope,
              confidence = EvidenceConfidence.Heuristic
            ),
            payload = payload,
            parents = (
              List(pressureRecord.ref, structuralRecord.ref, lineRecordAndPayload._1.ref, transition.evidence) ++
                planMotifRefs ++
                observedBranchEpisode.toList.flatMap(observed =>
                  graph.records.find(_.ref.line.contains(observed.line)).map(_.ref)
                ) ++
                branchWitnesses.flatMap(witness => graph.records.find(_.ref.line.contains(witness.line)).map(_.ref)) ++
                deterrenceProof.toList.flatMap(proof =>
                  (proof.resourceLine :: proof.comparisons.map(_.resourceLine)).flatMap(line =>
                    graph.records.find(_.ref.line.contains(line)).map(_.ref)
                  )
                )
            ).distinctBy(_.id)
          )
        val authorityCandidates =
          drafts.filter(record =>
              (
                record.parents.exists(parent =>
                  parent.layer == EvidenceLayer.MoveMotif && parent.confidence != EvidenceConfidence.Heuristic
                ) ||
                  (record.payload match
                    case event: PlanCausalEventEvidence =>
                        event.structuralConsequences.exists(PlanCausalEventProof.isDirectPlanResult) ||
                        PlanCausalEventProof.rootMoveDirectlyRestrictsOpponent(event) ||
                        event.ownedConditionalResponseProofReady(context.lines, context.evidenceGraph) ||
                        PlanCausalEventProof.rootMovePreparesPawnAdvance(event) ||
                        event.episode.exists(_.causalEpisodeProven)
                    case _ => false)
              ) &&
                (record.payload match
                  case event: PlanCausalEventEvidence =>
                      (
                        event.structuralConsequences.nonEmpty ||
                          event.developmentChoices.nonEmpty ||
                          event.planVerifiedResponseGoalResults.nonEmpty ||
                          event.episodePublicProofReady ||
                          event.episode.exists(_.causalEpisodeProven)
                      ) &&
                        PlanCausalEventProof.decisiveGoalProof(event, context.lines, context.evidenceGraph)
                  case _ => false)
            )
        val authoritativeIds = authorityCandidates.map(_.ref.id).toSet
        drafts.map(record =>
          if authoritativeIds(record.ref.id) then
            record.copy(ref = record.ref.copy(confidence = EvidenceConfidence.Mixed))
          else record
        )
      case _ =>
        Nil
    }.distinctBy(_.ref.id)

  private def resourceDeterrenceDraft(
      input: CanonicalNormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      rootLine: LineNodeRef,
      transition: StructuralTransitionBinding
  ): Option[ResourceDeterrenceDraft] =
    val rootCandidate = context.lines.find(_.ref == rootLine)
    val rootBranches = input.threatBranches.filter(branch =>
      branch.opponentResourceMove.nonEmpty && EvidenceRef.sameMove(branch.probedMoveUci, rootLine.rootMove)
    )
    val drafts = for
      candidate <- rootCandidate.toList
      branch <- rootBranches
      resourceMove <- branch.opponentResourceMove.toList
      normalizedLine <- branch.lines.headOption.toList
      resourceLine <- resourceLineFor(context, branch, normalizedLine).toList
      payload <- linePayload(context, resourceLine.ref).toList
      continuation = payload.lineReplaySteps.zipWithIndex.map { case (step, index) =>
        step.copy(ply = transition.from.ply + index + 1)
      }
      resourceStep <- continuation.headOption
        .filter(step => EvidenceRef.sameMove(step.moveUci, resourceMove))
        .toList
      responseStep <- continuation.lift(1)
        .filter(step => Fen.read(Standard, Fen.Full(step.fenBefore)).exists(_.color == transition.perspective))
        .toList
      materialGain <- payload.opponentResourcePunishmentCapturesFor(transition.perspective).sortBy(_.plyOffset)
      materialResultStep <- continuation.lift(materialGain.plyOffset)
        .filter(step => EvidenceRef.sameMove(step.moveUci, materialGain.moveUci))
        .toList
      comparisons = input.threatBranches.filter(comparison =>
        comparison.opponentResourceMove.exists(EvidenceRef.sameMove(_, resourceMove)) &&
          !EvidenceRef.sameMove(comparison.probedMoveUci, rootLine.rootMove)
      ).flatMap(comparison =>
        for
          normalized <- comparison.lines.headOption
          line <- resourceLineFor(context, comparison, normalized)
        yield OpponentResourceComparison(
          rootMove = comparison.probedMoveUci,
          sourceProbeId = comparison.sourceProbeId,
          resourceLine = line.ref
        )
      ).distinctBy(comparison => (EvidenceRef.normalizeMove(comparison.rootMove), comparison.sourceProbeId))
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
        perspective = transition.perspective,
        resourceMove = resourceMove,
        materialGain = materialGain
      )
    yield draft
    drafts.filter { draft =>
      val identity = PlanEventIdentityBuilder.from(
        rootMove = rootLine.rootMove,
        beforeFen = transition.from.fen,
        plan = draft.plan,
        consequences = List(draft.consequence),
        developmentChoices = Nil
      )
      val episode = deterrenceEpisode(draft, transition, identity, input.positionHistory)
      draft.proof.proven(rootLine, transition, episode, context.lines, context.evidenceGraph)
    }.sortBy(draft =>
      (
        -draft.comparisonContrastWinPercent(context.lines),
        -draft.proof
          .rootResourceImprovementWinPercent(draft.perspective, draft.rootLine, context.lines)
          .getOrElse(Double.NegativeInfinity),
        draft.materialGain.plyOffset
      )
    ).headOption

  private def resourceLineFor(
      context: JudgmentAssemblyContext,
      branch: NormalizedThreatBranch,
      normalized: CanonicalNormalizedCandidateLine
  ): Option[CandidateLineNode] =
    context.lines.find(line =>
      line.ref.role == LineNodeRole.Threat &&
        line.ref.rank == normalized.rank &&
        normalized.rootMove.exists(EvidenceRef.sameMove(_, line.ref.rootMove)) &&
        PrincipalVariationEvidence.sameBoardState(line.evidence.position.fen, branch.branchFen)
    )

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
      structuralConsequences = List(draft.consequence),
      developmentChoices = Nil
    )
    PlanCausalEpisodeBuilder.fromContinuation(
      plan = draft.plan,
      rootLine = draft.rootLine,
      role = transition.role,
      root = root,
      continuation = draft.continuation,
      positionHistory = positionHistory,
      observedResultMove = Some(draft.materialGain.moveUci)
    )

  private[assembly] def lineReplaySteps(positionHistory: CanonicalPositionHistory): List[LineReplayStep] =
    positionHistory.segmentReplaySteps.map(step =>
      LineReplayStep(step.ply, step.uci, step.beforeFen, step.afterFen)
    )

  private def rootPlanMotifRefs(
      graph: TypedEvidenceGraph,
      pressureRecord: EvidenceRecord,
      rootLine: LineNodeRef,
      plan: PlanMatch
  ): List[EvidenceRef] =
    pressureRecord.parents.flatMap(parent => graph.byId.get(parent.id)).collect {
      case EvidenceRecord(ref, payload: MoveMotifEvidence, _)
          if ref.line.contains(rootLine) &&
            payload.isRootEvent &&
            EvidenceRef.sameMove(payload.rootMove, rootLine.rootMove) &&
            plan.evidence.exists(_.motif == payload.motif) =>
        ref
    }.distinctBy(_.id)

  private def branchWitnessesFor(
      input: CanonicalNormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      rootLine: LineNodeRef,
      transition: StructuralTransitionBinding,
      plan: PlanMatch,
      expectedEvent: PlanCausalEventEvidence
  ): List[PlanCausalBranchWitness] =
    expectedEvent.episode.toList.flatMap(expectedEpisode => preferredReplyBranch(input, transition)
      .flatMap(branch =>
        branch.certifiedHorizonPlyOffset.toList.flatMap { certifiedHorizonPlyOffset =>
          branch.lines.flatMap { normalized =>
            context.lines
              .find(line => line.role == LineNodeRole.Threat && line.ref.rank == normalized.rank)
              .flatMap(line => linePayload(context, line.ref).map(line -> _))
              .map { case (line, payload) =>
                PlanCausalEventProof.branchWitness(
                  sourceProbeId = branch.sourceProbeId,
                  line = line.ref,
                  linePayload = payload,
                  rootLine = rootLine,
                  rootTransition = transition,
                  plan = plan,
                  expectedEpisode = expectedEpisode,
                  expectedResult = expectedEvent.publicTailExpectedResult,
                  certifiedHorizonPlyOffset = certifiedHorizonPlyOffset,
                  positionHistory = input.positionHistory
                )
              }
            }
          }
      )
      .distinctBy(_.line)
    )

  private def preferredReplyBranch(
      input: CanonicalNormalizedMoveReviewInput,
      transition: StructuralTransitionBinding
  ): List[NormalizedThreatBranch] =
    input.threatBranches
      .zipWithIndex
      .filter { case (branch, _) =>
        branch.opponentResourceMove.isEmpty &&
          branch.certifiedHorizonPlyOffset.nonEmpty &&
          EvidenceRef.sameMove(branch.probedMoveUci, transition.moveUci) &&
          PrincipalVariationEvidence.sameBoardState(branch.branchFen, transition.to.fen)
      }
      .maxByOption { case (branch, index) => (branch.certifiedHorizonPlyOffset, index) }
      .map(_._1)
      .toList

  private def observedBranchEpisodesFor(
      input: CanonicalNormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      rootLine: LineNodeRef,
      transition: StructuralTransitionBinding,
      plan: PlanMatch,
      root: PlanCausalEventNode
  ): List[ObservedBranchEpisode] =
    planObservationBranches(input, transition)
      .flatMap(branch =>
        branch.lines.flatMap { normalized =>
          context.lines
            .find(line =>
              line.role == LineNodeRole.Threat &&
                line.ref.rank == normalized.rank &&
                PrincipalVariationEvidence.sameBoardState(line.evidence.position.fen, branch.branchFen)
            )
            .flatMap(line => linePayload(context, line.ref).map(line -> _))
            .flatMap { case (line, payload) =>
              Option
                .when(
                  payload.lineReplaySteps.headOption.exists(step =>
                    PrincipalVariationEvidence.sameBoardState(step.fenBefore, transition.to.fen)
                  )
                ) {
                  val continuation = payload.lineReplaySteps
                  val episode = PlanCausalEpisodeBuilder.fromContinuation(
                    plan = plan,
                    rootLine = rootLine,
                    role = transition.role,
                    root = root,
                    continuation = continuation,
                    positionHistory = input.positionHistory,
                    observedReplyBranch = true
                  )
                  ObservedBranchEpisode(line.ref, continuation, episode)
                }
            }
        }
      )
      .filter(observed =>
          observed.episode.rootEnablesContinuation &&
          observed.episode.representativeResult.nonEmpty &&
          branchDependencyOwnsPlan(transition, observed.episode)
      )
      .sortBy(observed => (
        branchDependencySpecificity(observed.episode),
        observed.episode.requiredPlyOffset,
        observed.line.rank,
        observed.line.id
      ))

  private def planObservationBranches(
      input: CanonicalNormalizedMoveReviewInput,
      transition: StructuralTransitionBinding
  ): List[NormalizedThreatBranch] =
    preferredReplyBranch(input, transition) match
      case preferred @ (_ :: _) => preferred
      case Nil =>
        input.threatBranches.filter(branch =>
          branch.opponentResourceMove.nonEmpty &&
            EvidenceRef.sameMove(branch.probedMoveUci, transition.moveUci) &&
            PrincipalVariationEvidence.sameBoardState(branch.branchFen, transition.to.fen)
        )

  private def preferredObservedBranchEpisode(
      input: CanonicalNormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      rootLine: LineNodeRef,
      transition: StructuralTransitionBinding,
      plan: PlanMatch,
      mainLineEpisode: PlanCausalEpisode,
      observed: List[ObservedBranchEpisode]
  ): Option[ObservedBranchEpisode] =
    if !mainLineEpisode.rootEnablesContinuation then observed.headOption
    else
      val incumbent = testedEpisodeEvidence(
        input,
        context,
        rootLine,
        transition,
        plan,
        mainLineEpisode
      ).flatMap(event =>
        observedBranchPreference(event, context.lines, context.evidenceGraph).map(event -> _)
      )
      observed
        .flatMap(candidate =>
          testedEpisodeEvidence(
            input,
            context,
            rootLine,
            transition,
            plan,
            candidate.episode
          ).flatMap(tested =>
            for
              assessment <- tested.representativeGoalResultAssessment
              if assessment.realizedObservations.size >= 2
              if !tested.observedVacatedSquareRoute || tested.observedVacatedSquareRouteProofReady
              preference <- observedBranchPreference(tested, context.lines, context.evidenceGraph)
            yield (candidate, tested, preference)
          )
        )
        .filter { case (_, candidateEvent, preference) =>
          incumbent.forall { case (currentEvent, currentPreference) =>
            preservesPositiveGoalResults(currentEvent, candidateEvent) &&
              summon[Ordering[(Int, Int, Int, Int, Int)]].gt(preference, currentPreference)
          }
        }
        .maxByOption(_._3)
        .map(_._1)

  private def observedBranchPreference(
      event: PlanCausalEventEvidence,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[(Int, Int, Int, Int, Int)] =
    for
      _ <- event.representativeGoalResultAssessment
      (specificity, strength, depth, salience) <- event.principalExplanationSortKey(lines, graph)
    yield
      val exactOccupationBoost = event.observedVacatedSquareRouteProofReady
      (if exactOccupationBoost then 1 else 0, specificity, strength, depth.max(event.requiredHorizonPlyOffset), salience)

  private def preservesPositiveGoalResults(
      current: PlanCausalEventEvidence,
      candidate: PlanCausalEventEvidence
  ): Boolean =
    (current.episode, candidate.episode) match
      case (Some(currentEpisode), Some(candidateEpisode)) =>
        current.positiveGoalResultAssessments.forall(currentAssessment =>
          candidate.positiveGoalResultAssessments.exists(candidateAssessment =>
            EvidenceObjectBinding
              .goalTargetObjects(currentAssessment.consequence)
              .subsetOf(EvidenceObjectBinding.goalTargetObjects(candidateAssessment.consequence)) &&
              PlanCausalFunctionalMatch.causallyEquivalent(
                currentEpisode,
                currentAssessment.sourceEvent,
                List(currentAssessment.consequence),
                candidateEpisode,
                candidateAssessment.sourceEvent,
                List(candidateAssessment.consequence)
              )
          )
        )
      case _ => false

  private def testedEpisodeEvidence(
      input: CanonicalNormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      rootLine: LineNodeRef,
      transition: StructuralTransitionBinding,
      plan: PlanMatch,
      episode: PlanCausalEpisode
  ): Option[PlanCausalEventEvidence] =
    val identity = PlanCausalEventProof.goalEstablishedByContinuation(
      episode.root.identity,
      transition,
      episode.root.structuralConsequences,
      episode
    )
    val provisional = PlanCausalEventEvidence(
      rootTransition = transition,
      causalEpisode = episode.withRootIdentity(identity),
      branchWitnesses = Nil,
    )
    val tested = provisional.copy(
      branchWitnesses = branchWitnessesFor(
        input,
        context,
        rootLine,
        transition,
        plan,
        provisional
      )
    )
    Option.when(
      tested.branchSetComplete && tested.representativeGoalResultAssessment.exists(_.positiveProofReady)
    )(tested)

  private def branchDependencySpecificity(episode: PlanCausalEpisode): Int =
    episode.dependencies.filter(dependency => dependency.from == episode.root && dependency.enablesContinuation).map {
      case PlanCausalEventDependency(
            _,
            _,
            PlanCausalDependencyKind.LineAccessPrecondition,
            PlanCausalDependencyProof.LineAccess(trajectory),
            _
          ) if trajectory.enabledTo == trajectory.vacatedSquare => 0
      case dependency if dependency.kind == PlanCausalDependencyKind.PawnAdvanceSupport => 0
      case dependency if dependency.kind == PlanCausalDependencyKind.LineAccessPrecondition => 1
      case _ => 2
    }.minOption.getOrElse(Int.MaxValue)

  private def branchDependencyOwnsPlan(
      transition: StructuralTransitionBinding,
      episode: PlanCausalEpisode
  ): Boolean =
    val identity = PlanCausalEventProof
      .goalEstablishedByContinuation(
        episode.root.identity,
        transition,
        episode.root.structuralConsequences,
        episode
      )
    val rootDependencies = episode.dependencies
      .filter(dependency => dependency.from == episode.root && dependency.enablesContinuation)
    val ownedInducedResponse = episode.responseResults.exists { case (response, consequence) =>
      val triggerOwnedByRoot =
        rootDependencies.exists(dependency => episode.enablingDependenciesTo(response.trigger).contains(dependency))
      val triggerTransition = transition.copy(
        moveUci = response.trigger.moveUci,
        from = PositionNodeRef(response.trigger.step.fenBefore, response.trigger.step.ply - 1, Some(response.trigger.perspective)),
        to = PositionNodeRef(response.trigger.step.fenAfter, response.trigger.step.ply, Some(!response.trigger.perspective)),
        perspective = response.trigger.perspective
      )
      triggerOwnedByRoot &&
        PlanCausalGoalProof.provesOwnedInducedResponse(
          identity.goalTheme,
          triggerTransition,
          response,
          consequence
        )
    }
    rootDependencies.exists {
        case dependency if dependency.kind == PlanCausalDependencyKind.PawnAdvanceSupport =>
          identity.goalTheme == PlanTheme.PawnBreakPreparation
        case dependency if dependency.kind == PlanCausalDependencyKind.LineAccessPrecondition =>
          identity.goalTheme == PlanTheme.PieceRedeployment
        case dependency if dependency.kind == PlanCausalDependencyKind.ObjectStatePrecondition =>
          identity.goalTheme == PlanTheme.PieceRedeployment ||
            identity.goalTheme == PlanTheme.AdvantageTransformation &&
              episode.representativeResult.exists { case (sourceEvent, consequence) =>
                episode.enablingDependenciesTo(sourceEvent).contains(dependency) &&
                  PlanCausalGoalProof.proves(identity, transition, consequence)
              }
        case _ =>
          false
      } || ownedInducedResponse

  private def linePayload(context: JudgmentAssemblyContext, line: LineNodeRef): Option[LineFactEvidence] =
    context.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, payload: LineFactEvidence, _) if ref.line.contains(line) => payload
    }

private[assembly] object PlanCausalEventProof:
  def isDirectPlanResult(consequence: TransitionConsequence): Boolean =
    consequence.positive &&
      consequence.strength > 0 &&
      consequence.kind == TransitionConsequenceKind.PieceExchangeAvailable

  def rootOwnedPlans(
      pressure: PlanPressureEvidence,
      rootLine: LineNodeRef,
      structural: StructuralDeltaEvidence,
      line: Option[LineFactEvidence]
  ): List[PlanMatch] =
    val motifBacked = pressure.rootBackedPlans(Some(rootLine.rootMove))
    val directResultBacked = pressure.activePlans.allPlans.filter(plan =>
      planTheme(plan).contains(PlanTheme.FavorableExchange) &&
      structural.consequences.exists(consequence =>
        isDirectPlanResult(consequence) &&
          goalConsequenceForPlan(plan, consequence, structural.transition).nonEmpty
      )
    )
    val structuralGoalBacked = Option
      .when(line.flatMap(_.rootActorSurvivesReply).contains(true))(
        pressure.activePlans.allPlans.filter(structuralEvidenceEstablishesPlanGoal(_, structural, line))
      )
      .getOrElse(Nil)
    val establishedPlans = motifBacked ++ directResultBacked ++ structuralGoalBacked
    val inferredRestriction =
      if establishedPlans.isEmpty && directRestrictionSurvivesReply(rootLine, structural, line) then
        directRestrictionPlans(structural, line)
      else Nil
    val developmentBacked = pressure.activePlans.allPlans.filter(plan =>
      line.flatMap(_.rootActorSurvivesReply).contains(true) &&
        developmentSupportsPlan(plan) &&
        structural.developmentChoices.exists(choice =>
          EvidenceRef.sameMove(s"${choice.from}${choice.to}", rootLine.rootMove)
        )
    )
    (establishedPlans ++ inferredRestriction ++ developmentBacked).distinctBy(_.plan.id)

  private def directRestrictionSurvivesReply(
      rootLine: LineNodeRef,
      structural: StructuralDeltaEvidence,
      line: Option[LineFactEvidence]
  ): Boolean =
    DirectOpponentRestrictionProof.directRestrictionSurvivesReply(rootLine, structural, line)

  def directPawnBlockadeFunctionProven(
      pressure: PlanPressureEvidence,
      plan: PlanMatch,
      rootLine: LineNodeRef,
      structural: StructuralDeltaEvidence,
      line: LineFactEvidence
  ): Boolean =
    val explicitlyOwnedRestriction =
      planTheme(plan).contains(PlanTheme.RestrictionProphylaxis) &&
        rootActorIsPawn(rootLine, structural) &&
        pressure.rootBackedPlans(Some(rootLine.rootMove)).exists(_.plan.id == plan.plan.id)
    !DirectOpponentRestrictionProof.rootActorIsDevelopingMinor(rootLine, structural) &&
      (line.rootActorSurvivesReply.contains(true) || explicitlyOwnedRestriction)

  def directPawnBlockade(consequence: TransitionConsequence): Boolean =
    consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction &&
      consequence.subjects.exists(StructuralDeltaEvidence.directlyBlockedPawnAdvance)

  private def directRestrictionPlans(
      structural: StructuralDeltaEvidence,
      line: Option[LineFactEvidence]
  ): List[PlanMatch] =
    Option
      .when(isPrincipalRestrictionGoal(structural, line))(
        PlanMatch(
          plan = Plan.Prophylaxis(structural.perspective),
          score = 1.0,
          evidence = Nil,
          support = List(
            PlanSupport.Theme(PlanTheme.RestrictionProphylaxis),
            PlanSupport.Subplan(PlanKind.ProphylaxisRestraint)
          )
        )
      )
      .toList

  private def isPrincipalRestrictionGoal(
      structural: StructuralDeltaEvidence,
      line: Option[LineFactEvidence]
  ): Boolean =
    import TransitionConsequenceKind.*
    val restrictionSubjects = structural
      .consequencesOf(OpponentMobilityRestriction)
      .flatMap(_.subjects)
      .filter(restrictionSubjectProven(structural, line, _))
    val restrictedEntries = restrictionSubjects.flatMap(StructuralDeltaEvidence.restrictedOpponentEntry)
    val strongerDirectFunction = structural.developmentChoices.nonEmpty || structural.consequences.exists(consequence =>
      Set(
        DevelopmentPieceActivated,
        DevelopmentMobilityGain,
        DevelopmentCenterControlGain,
        DevelopmentSafePlacement,
        KingSafetyPressure,
        KingRingPressureGain,
        SpaceGain,
        PawnTensionGain,
        WeakPawnTargetCreated,
        WeakSquareTargetCreated,
        PassedPawnProgress,
        PromotionPressureGain,
        PieceExchangeAvailable
      )(consequence.kind)
    )
    principalRestriction(restrictedEntries, restrictionSubjects) && !strongerDirectFunction

  def rootProofConsequence(
      structural: StructuralDeltaEvidence,
      line: Option[LineFactEvidence],
      consequence: TransitionConsequence
  ): Option[TransitionConsequence] =
    if consequence.kind != TransitionConsequenceKind.OpponentMobilityRestriction then Some(consequence)
    else
      val subjects = consequence.subjects.filter(restrictionSubjectProven(structural, line, _))
      Option.when(subjects.nonEmpty)(
        consequence.copy(
          strength = consequence.strength.min(subjects.size).max(1),
          subjects = subjects
        )
      )

  private def restrictionSubjectProven(
      structural: StructuralDeltaEvidence,
      line: Option[LineFactEvidence],
      subject: String
  ): Boolean =
    !StructuralDeltaEvidence.restrictedOpponentRouteNeedsLine(subject) ||
      (for
        route <- StructuralDeltaEvidence.restrictedOpponentRoute(subject)
        start <- route.headOption
        via <- route.drop(1).headOption
        payload <- line
      yield payload.lineReplaySteps.exists { step =>
        EvidenceRef.sameMove(step.moveUci, s"$start$via") &&
          Fen.read(Standard, Fen.Full(step.fenBefore)).exists(position =>
            _root_.chess.Square.fromKey(start).flatMap(position.board.pieceAt).exists(piece =>
              piece.color == !structural.perspective && piece.role == _root_.chess.Knight
            )
          )
      }).contains(true)

  private def principalRestriction(
      restrictedEntries: List[(String, String, String)],
      restrictionSubjects: List[String]
  ): Boolean =
    val centralPawnBreak = restrictedEntries.exists { case (role, from, to) =>
      role == "pawn" && from.headOption == to.headOption && to.matches("[c-f][45]")
    }
    val directPawnBlockade = restrictionSubjects.exists(StructuralDeltaEvidence.directlyBlockedPawnAdvance)
    val majorPieceInvasion = restrictedEntries.exists { case (role, _, to) =>
      Set("rook", "queen")(role) && to.lastOption.exists(rank => rank == '2' || rank == '7')
    }
    val latentKnightRoute = restrictionSubjects.exists(subject =>
      StructuralDeltaEvidence.restrictedOpponentRoute(subject).exists(_.size >= 3)
    )
    centralPawnBreak || directPawnBlockade || majorPieceInvasion || latentKnightRoute

  def structuralEvidenceEstablishesPlanGoal(
      plan: PlanMatch,
      structural: StructuralDeltaEvidence,
      line: Option[LineFactEvidence]
  ): Boolean =
    structural.consequences
      .flatMap(rootProofConsequence(structural, line, _))
      .exists(structuralConsequenceEstablishesPlanGoal(plan, structural, _, line))

  def structuralConsequenceEstablishesPlanGoal(
      plan: PlanMatch,
      structural: StructuralDeltaEvidence,
      consequence: TransitionConsequence,
      line: Option[LineFactEvidence]
  ): Boolean =
    val directStructuralGoal = planTheme(plan).exists {
      case PlanTheme.PawnBreakPreparation => rootMoveUnlocksPawnAdvance(consequence)
      case PlanTheme.RestrictionProphylaxis =>
        isPrincipalRestrictionGoal(structural, line) &&
          consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction &&
          consequence.subjects.exists(subject =>
            StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject(subject) &&
              restrictionSubjectProven(structural, line, subject)
          )
      case _ => false
    }
    structural.consequences.exists(observed =>
      observed.kind == consequence.kind &&
        observed.polarity == consequence.polarity &&
        consequence.subjects.toSet.subsetOf(observed.subjects.toSet)
    ) &&
      consequence.positive &&
      consequence.strength > 0 &&
      directStructuralGoal &&
      goalConsequenceForPlan(plan, consequence, structural.transition).nonEmpty

  def rootMoveDirectlyRestrictsOpponent(event: PlanCausalEventEvidence): Boolean =
    DirectOpponentRestrictionProof.rootMoveDirectlyRestrictsOpponent(event)

  def rootMovePreparesPawnAdvance(event: PlanCausalEventEvidence): Boolean =
    event.identity.goalTheme == PlanTheme.PawnBreakPreparation &&
      event.directGoalConsequences.exists(rootMoveUnlocksPawnAdvance)

  private def rootMoveUnlocksPawnAdvance(consequence: TransitionConsequence): Boolean =
    consequence.kind == TransitionConsequenceKind.LineUnlockGain

  def eventCandidatePlans(
      pressure: PlanPressureEvidence,
      rootLine: LineNodeRef,
      structural: StructuralDeltaEvidence,
      line: LineFactEvidence
  ): List[PlanMatch] =
    (
        rootOwnedPlans(pressure, rootLine, structural, Some(line)) ++
        pressure.activePlans.allPlans
    ).distinctBy(_.plan.id)

  def rootActorIsPawn(rootLine: LineNodeRef, structural: StructuralDeltaEvidence): Boolean =
    rootActor(rootLine, structural).exists(_._2.role == _root_.chess.Pawn)

  private def rootActor(
      rootLine: LineNodeRef,
      structural: StructuralDeltaEvidence
  ): Option[(_root_.chess.Square, _root_.chess.Piece)] =
    (for
      position <- Fen.read(Standard, Fen.Full(structural.transition.from.fen))
      from <- _root_.chess.Square.fromKey(EvidenceRef.normalizeMove(rootLine.rootMove).take(2))
      piece <- position.board.pieceAt(from)
    yield from -> piece)

  def rootCandidateConsequenceForPlan(
      plan: PlanMatch,
      consequence: TransitionConsequence,
      transition: StructuralTransitionBinding
  ): Option[TransitionConsequence] =
    if
      consequence.kind == TransitionConsequenceKind.PieceExchangeAvailable &&
        !planTheme(plan).contains(PlanTheme.FavorableExchange)
    then None
    else candidateConsequenceForPlan(plan, consequence, transition)

  def developmentSupportsPlan(plan: PlanMatch): Boolean =
    planTheme(plan).exists(PlanCausalGoalProof.developmentProves)

  def goalEstablishedByContinuation(
      initial: PlanEventIdentity,
      transition: StructuralTransitionBinding,
      rootConsequences: List[TransitionConsequence],
      episode: PlanCausalEpisode
  ): PlanEventIdentity =
    if rootConsequences.nonEmpty || episode.root.developmentChoices.nonEmpty then initial
    else
      val observedPawnBreakKinds =
        episode.dependencies
          .filter(dependency =>
            dependency.from == episode.root &&
              dependency.enablesContinuation
          )
          .flatMap(_.preparedPawnAdvanceFile)
          .map(_.trim.toLowerCase)
          .flatMap {
            case "c" | "d" | "e" | "f" => Some(PlanKind.CentralBreakTiming)
            case "a" | "b" | "g" | "h" => Some(PlanKind.WingBreakTiming)
            case _                         => None
          }
          .distinct
      val observedPawnBreakKind =
        observedPawnBreakKinds match
          case kind :: Nil => Some(kind)
          case _           => None
      val representative = episode.representativeResult.map(_._2)
      val declaredGoalProven = representative.exists(consequence =>
        PlanCausalGoalProof.proves(initial.goalTheme, initial.goalKind, transition, consequence)
      )
      val futureRouteRestriction = episode.continuationsEnabledByRoot
        .flatMap(sourceEvent => PlanCausalEpisode.resultConsequences(sourceEvent).map(sourceEvent -> _))
        .sortBy(_._1.step.ply)
        .find { case (sourceEvent, consequence) =>
          val route = episode.enablingDependenciesTo(sourceEvent)
          initial.goalTheme == PlanTheme.PieceRedeployment &&
            consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction &&
            PlanCausalGoalProof.movedPieceCreatesRouteResult(sourceEvent, consequence) &&
            route.size >= 2 &&
            route.forall {
              case PlanCausalEventDependency(
                    _,
                    _,
                    PlanCausalDependencyKind.ObjectStatePrecondition,
                    PlanCausalDependencyProof.ObjectState(trajectory),
                    _
                  ) =>
                initial.actorRole.exists(_.equalsIgnoreCase(trajectory.pieceRole.name))
              case _ => false
            }
        }
      futureRouteRestriction match
        case Some((_, consequence)) =>
          val resultTargets = PlanCausalEpisode.goalTargetSubjects(consequence)
          val squareTargets = resultTargets
            .flatMap(value => "[a-h][1-8]".r.findAllIn(value.toLowerCase).map(square => s"square:$square"))
          initial.copy(
            kind = PlanKind.ProphylaxisRestraint,
            targets = (resultTargets ++ squareTargets).distinct.sorted,
            results = List(s"kind:${consequence.kind.toString.toLowerCase}")
          )
        case None
            if initial.goalTheme == PlanTheme.PieceRedeployment &&
              episode.dependencies.exists(dependency =>
                dependency.from == episode.root &&
                  dependency.kind == PlanCausalDependencyKind.LineAccessPrecondition &&
                  dependency.proofPieceRoles.exists(_.name.equalsIgnoreCase(_root_.chess.Rook.toString))
              ) =>
          initial.copy(kind = PlanKind.RookFileTransfer)
        case None if declaredGoalProven =>
          initial
        case None =>
          observedPawnBreakKind.fold(initial)(kind => initial.copy(kind = kind))

  def decisiveGoalProof(
      event: PlanCausalEventEvidence,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Boolean =
    import TransitionConsequenceKind.*
    val opponentProofReady =
      event.opponentResourceDeterrence.forall(_ =>
        event.opponentResourceDeterrenceProofReady(lines, graph)
      )
    val futureResults = event.representativeResult.toList
    val consequences = (
      event.structuralConsequences ++
        event.planVerifiedResponseGoalResults.map(_._2) ++
        futureResults.map(_._2)
    ).distinct
    val developmentChoices = (
      event.developmentChoices ++
        event.positiveCausalResultAssessments.flatMap(_.sourceEvent.developmentChoices)
    ).distinct
    opponentProofReady &&
      (event.identity.goalTheme match
        case PlanTheme.OpeningPrinciples =>
          event.positiveCausalResultAssessments.nonEmpty &&
            (
              developmentChoices.nonEmpty || consequences.exists(consequence =>
                Set(DevelopmentPieceActivated, DevelopmentMobilityGain, DevelopmentCenterControlGain, DevelopmentSafePlacement)(
                  consequence.kind
                )
              )
            )
        case PlanTheme.Unknown =>
          false
        case _ =>
          event.goalDependencyProofReady ||
            event.directGoalConsequences.nonEmpty ||
            event.planVerifiedResponseGoalResults.nonEmpty ||
            futureResults.exists((sourceEvent, consequence) => event.resultAdvancesGoal(sourceEvent, consequence)))

  def goalConsequences(
      plan: PlanMatch,
      transition: StructuralTransitionBinding,
      consequences: List[TransitionConsequence]
  ): List[TransitionConsequence] =
    val direct = consequences.flatMap(goalConsequenceForPlan(plan, _, transition))
    if direct.nonEmpty then strongestLineUnlockWhenItIsTheOnlyGoalProof(planKind(plan), direct)
    else consequences

  private def strongestLineUnlockWhenItIsTheOnlyGoalProof(
      kind: Option[PlanKind],
      consequences: List[TransitionConsequence]
  ): List[TransitionConsequence] =
    if !kind.contains(PlanKind.WorstPieceImprovement) then consequences
    else
      val otherProof = consequences.filterNot(_.kind == TransitionConsequenceKind.LineUnlockGain)
      val strongestLineUnlock = consequences
        .filter(_.kind == TransitionConsequenceKind.LineUnlockGain)
        .sortBy(consequence => -consequence.strength)
        .headOption
      if otherProof.nonEmpty then otherProof
      else strongestLineUnlock.toList

  def goalConsequenceForPlan(
      plan: PlanMatch,
      consequence: TransitionConsequence,
      transition: StructuralTransitionBinding
  ): Option[TransitionConsequence] =
    for
      theme <- planTheme(plan)
      matching <- consequenceForPlan(plan, consequence, transition)
      if PlanCausalGoalProof.proves(theme, planKind(plan), transition, matching)
    yield matching

  def candidateConsequenceForPlan(
      plan: PlanMatch,
      consequence: TransitionConsequence,
      transition: StructuralTransitionBinding
  ): Option[TransitionConsequence] =
    for
      theme <- planTheme(plan)
      matching <- consequenceForPlan(plan, consequence, transition)
      if
        PlanCausalGoalProof.proves(theme, planKind(plan), transition, matching) ||
          mayAdvanceUnprovenEpisode(theme, matching)
    yield matching

  private def mayAdvanceUnprovenEpisode(
      theme: PlanTheme,
      consequence: TransitionConsequence
  ): Boolean =
    import TransitionConsequenceKind.*
    theme match
      case PlanTheme.PieceRedeployment =>
        Set(
          MobilityGain,
          LineUnlockGain,
          FileOccupationGain,
          OutpostGain,
          RookLiftActivation,
          BatteryPressureGain,
          PieceExchangeAvailable,
          DevelopmentPieceActivated,
          DevelopmentMobilityGain,
          DevelopmentCenterControlGain,
          DevelopmentSafePlacement
        )(consequence.kind)
      case PlanTheme.SpaceClamp =>
        Set(SpaceGain, PawnTensionGain)(consequence.kind)
      case PlanTheme.WeaknessFixation =>
        consequence.kind == PawnTensionGain
      case PlanTheme.PawnBreakPreparation =>
        Set(OpenFileGain, SemiOpenFileGain, CenterControlGain)(consequence.kind)
      case PlanTheme.WingPlay =>
        Set(
          SpaceGain,
          PawnTensionGain,
          TargetPressureGain,
          KingRingPressureGain,
          RookLiftActivation,
          LineUnlockGain,
          BatteryPressureGain
        )(consequence.kind)
      case PlanTheme.AdvantageTransformation =>
        Set(OpenFileGain, SemiOpenFileGain, TargetPressureGain)(consequence.kind)
      case _ =>
        false

  def consequenceForPlan(
      plan: PlanMatch,
      consequence: TransitionConsequence,
      transition: StructuralTransitionBinding
  ): Option[TransitionConsequence] =
    PlanCausalGoalProof.consequenceOnNamedAttackWing(plan.plan.id, consequence).flatMap { matching =>
      planTheme(plan) match
        case Some(PlanTheme.WeaknessFixation) if matching.kind == TransitionConsequenceKind.TargetPressureGain =>
          val matchingTargets = matching.targetSubjects.map(_.trim.toLowerCase).distinct
          Option.when(matchingTargets.nonEmpty)(
            matching.copy(
              strength = matching.strength.min(matchingTargets.size).max(1),
              subjects = matchingTargets,
              targetSubjects = matchingTargets
            )
          )
        case _ =>
          Some(matching)
    }

  def responseConsequenceForPlan(
      plan: PlanMatch,
      transition: StructuralTransitionBinding,
      response: PlanCausalResponse,
      consequence: TransitionConsequence
  ): Option[TransitionConsequence] =
    PlanCausalGoalProof.consequenceOnNamedAttackWing(plan.plan.id, consequence).filter(matching =>
      planTheme(plan).exists(theme =>
        PlanCausalGoalProof.provesInducedResponse(
          theme,
          transition,
          response,
          matching
        )
      )
    ).flatMap { matching =>
      matching.kind match
        case TransitionConsequenceKind.WeakPawnTargetCreated | TransitionConsequenceKind.WeakSquareTargetCreated =>
          val responseDestination = EvidenceRef.normalizeMove(response.step.moveUci).slice(2, 4)
          val matchingSubjects = matching.subjectsAt(responseDestination)
          Option.when(matchingSubjects.nonEmpty)(matching.copy(subjects = matchingSubjects))
        case _ =>
          Some(matching)
    }

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
    (planMatchesRootMove || structuralConsequenceEstablishesPlanGoal || isDirectPlanResult(consequence)) &&
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
    val subjectSquares = consequence.subjects.flatMap(subject => "[a-h][1-8]".r.findAllIn(subject.toLowerCase)).toSet
    val bindingCoversSubjects =
      consequence.kind != TransitionConsequenceKind.TargetPressureGain ||
        subjectSquares.isEmpty ||
        consequenceBindings.exists(binding =>
          subjectSquares.subsetOf(
            binding.target.collect {
              case obj if obj.kind == EvidenceObjectKind.Square => obj.key.trim.toLowerCase
            }.toSet
          )
        )
    val concreteTargetRequired =
      Set(
        TransitionConsequenceKind.OpenFileGain,
        TransitionConsequenceKind.SemiOpenFileGain,
        TransitionConsequenceKind.FileOccupationGain,
        TransitionConsequenceKind.WeakPawnTargetCreated,
        TransitionConsequenceKind.WeakSquareTargetCreated,
        TransitionConsequenceKind.PawnTensionGain,
        TransitionConsequenceKind.SpaceGain,
        TransitionConsequenceKind.TargetPressureGain,
        TransitionConsequenceKind.CenterControlGain,
        TransitionConsequenceKind.PassedPawnProgress,
        TransitionConsequenceKind.PromotionPressureGain,
        TransitionConsequenceKind.OutpostGain,
        TransitionConsequenceKind.RookLiftActivation,
        TransitionConsequenceKind.BatteryPressureGain,
        TransitionConsequenceKind.PieceExchangeAvailable,
        TransitionConsequenceKind.OpponentMobilityRestriction,
        TransitionConsequenceKind.KingRingPressureGain,
        TransitionConsequenceKind.KingSafetyPressure
      )(consequence.kind)
    consequenceBindings.nonEmpty && bindingCoversSubjects &&
      (!concreteTargetRequired ||
        consequence.subjects.nonEmpty && consequenceBindings.exists(
          _.target.exists(EvidenceObjectBinding.specificSurfaceTargetObject)
        ))

  def rootLineAccessRealized(
      rootLine: LineNodeRef,
      line: LineFactEvidence,
      consequence: TransitionConsequence
  ): Boolean =
    val steps = line.lineReplaySteps
    val rootIndex = steps.indexWhere(step => EvidenceRef.sameMove(step.moveUci, rootLine.rootMove))
    steps.lift(rootIndex).exists { rootStep =>
      steps.zipWithIndex.drop(rootIndex + 1).exists { case (futureStep, futureIndex) =>
        LineAccessTrajectory
          .find(rootStep, futureStep, steps.slice(rootIndex + 1, futureIndex))
          .exists(trajectory =>
            trajectory.enabledTo == trajectory.vacatedSquare &&
              consequence
                .subjectsForPieceAt(trajectory.enabledPieceRole.name, trajectory.enabledFrom.key)
                .nonEmpty
          )
      }
    }

  def planTheme(plan: PlanMatch): Option[PlanTheme] =
    plan.support.collectFirst { case PlanSupport.Theme(theme) => theme }

  def planKind(plan: PlanMatch): Option[PlanKind] =
    plan.support.collectFirst { case PlanSupport.Subplan(kind) => kind }

  def branchWitness(
      sourceProbeId: String,
      line: LineNodeRef,
      linePayload: LineFactEvidence,
      rootLine: LineNodeRef,
      rootTransition: StructuralTransitionBinding,
      plan: PlanMatch,
      expectedEpisode: PlanCausalEpisode,
      expectedResult: Option[(PlanCausalEventNode, TransitionConsequence)],
      certifiedHorizonPlyOffset: Int,
      positionHistory: CanonicalPositionHistory
  ): PlanCausalBranchWitness =
    val requiredPlyOffset = expectedResult
      .map((sourceEvent, _) => sourceEvent.step.ply - expectedEpisode.root.step.ply)
      .getOrElse(expectedEpisode.requiredPlyOffset)
      .max(1)
    val expectedFutureEvent = expectedResult.map(_._1).orElse(expectedEpisode.futureEvent)
    val expectedPlyOffset = expectedFutureEvent
      .map(_.step.ply - expectedEpisode.root.step.ply)
      .getOrElse(requiredPlyOffset)
    val proofThroughPlyOffset = linePayload.lineReplayCount
      .min(certifiedHorizonPlyOffset)
    val continuation = linePayload.lineReplaySteps.take(proofThroughPlyOffset)
    val expectedConsequences = expectedResult.map(_._2).toList
    val observedReplyBranch = expectedResult.exists { case (sourceEvent, consequence) =>
      consequence.kind == TransitionConsequenceKind.MobilityGain &&
        expectedEpisode.vacatedSquareLineAccessTo(sourceEvent).exists(trajectory =>
          !trajectory.enabledPieceRole.name.equalsIgnoreCase(_root_.chess.Rook.toString)
        )
    }
    val observedCandidate = PlanCausalEpisodeBuilder.fromContinuation(
      plan = plan,
      rootLine = rootLine,
      role = rootTransition.role,
      root = expectedEpisode.root,
      continuation = continuation,
      positionHistory = positionHistory,
      observedReplyBranch = observedReplyBranch
    )
    val observed = Option.when(observedCandidate.rootEnablesContinuation)(observedCandidate)
    val observedConsequences = observed.toList.flatMap(
      _.continuationsEnabledByRoot.flatMap(PlanCausalEpisode.resultConsequences)
    ).distinct
    val matchedEvent = for
      expectedEvent <- expectedFutureEvent.toList
      expectedMove = expectedEvent.moveUci
      observedEpisode <- observed.toList
      candidate <- observedEpisode.continuationsEnabledByRoot
      offset = candidate.step.ply - observedEpisode.root.step.ply
      candidateConsequences = PlanCausalEpisode.resultConsequences(candidate)
      typedGoalMatched = PlanCausalFunctionalMatch.causallyEquivalent(
        expectedEpisode,
        expectedEvent,
        expectedConsequences,
        observedEpisode,
        candidate,
        candidateConsequences
      )
      realization <-
        if EvidenceRef.sameMove(expectedMove, candidate.moveUci) && offset == expectedPlyOffset && typedGoalMatched then
          List(PlanCausalRealizationMatch.ExactMove)
        else
          Option
            .when(
              offset <= proofThroughPlyOffset && typedGoalMatched
            )(PlanCausalRealizationMatch.EquivalentFunction)
            .toList
    yield (candidate, realization)
    val selectedMatch = matchedEvent.sortBy { case (event, realization) =>
      (if realization == PlanCausalRealizationMatch.ExactMove then 0 else 1, event.step.ply)
    }.headOption
    val realizationMatch = selectedMatch.map(_._2)
    val terminalOutcome =
      Option
        .when(proofThroughPlyOffset == linePayload.lineReplayCount)(continuation.lastOption)
        .flatten
        .flatMap(step => terminalOutcomeAt(step.fenAfter, expectedEpisode.root.perspective))
    val terminalStep = Option.when(terminalOutcome.nonEmpty)(continuation.lastOption).flatten
    val outcome = observed match
      case Some(_) if realizationMatch.nonEmpty =>
        PlanCausalBranchOutcome.Realized
      case _ if terminalOutcome.contains(PlanCausalTerminalOutcome.Defeat) =>
        PlanCausalBranchOutcome.Refuted
      case _ if terminalOutcome.nonEmpty =>
        PlanCausalBranchOutcome.Diverted
      case _ if proofThroughPlyOffset < requiredPlyOffset =>
        PlanCausalBranchOutcome.Deferred
      case Some(_) =>
        PlanCausalBranchOutcome.Diverted
      case None =>
        PlanCausalBranchOutcome.Refuted
    PlanCausalBranchWitness(
      sourceProbeId = sourceProbeId,
      line = line,
      outcome = outcome,
      observedEpisode = observed,
      observedConsequences = observedConsequences,
      realizationMatch = realizationMatch,
      realizationMove = selectedMatch.map(_._1.moveUci),
      requiredPlyOffset = requiredPlyOffset,
      certifiedHorizonPlyOffset = certifiedHorizonPlyOffset,
      observedPlyOffset = selectedMatch
        .map(_._1.step.ply - expectedEpisode.root.step.ply)
        .orElse(terminalOutcome.map(_ => proofThroughPlyOffset))
        .getOrElse(proofThroughPlyOffset),
      observedThroughPlyOffset = proofThroughPlyOffset,
      terminalOutcome = terminalOutcome,
      terminalPlyOffset = Option.when(terminalOutcome.nonEmpty)(continuation.size),
      terminalStep = terminalStep
    )

  private[assembly] def terminalOutcomeAt(fen: String, perspective: chess.Color): Option[PlanCausalTerminalOutcome] =
    Fen.read(Standard, Fen.Full(fen)).flatMap { position =>
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
    }

private[assembly] object PlanEventIdentityBuilder:
  private val EventCategories = Set(
    TransitionConsequenceCategory.PawnStructure,
    TransitionConsequenceCategory.PawnStructureDelta,
    TransitionConsequenceCategory.Development,
    TransitionConsequenceCategory.PieceActivity,
    TransitionConsequenceCategory.TargetPressure,
    TransitionConsequenceCategory.CenterControl,
    TransitionConsequenceCategory.OpeningCenterControl,
    TransitionConsequenceCategory.OpeningDevelopment
  )

  def from(
      rootMove: String,
      beforeFen: String,
      plan: PlanMatch,
      consequences: List[TransitionConsequence],
      developmentChoices: List[StructuralDevelopmentChoice]
  ): PlanEventIdentity =
    val normalizedRootMove = Option(rootMove).getOrElse("").trim.toLowerCase
    val consequenceTargets = consequences
      .filterNot(consequence => PlanCausalEpisode.meansOnlyResultKind(consequence.kind))
      .flatMap(PlanCausalEpisode.goalTargetSubjects)
    val squareTargets =
      (consequenceTargets.map(_.toLowerCase.replace(normalizedRootMove, "")) ++ developmentChoices.map(_.to))
        .flatMap(value => "[a-h][1-8]".r.findAllIn(value.toLowerCase).map(square => s"square:$square"))
    val targets =
      consequenceTargets ++ squareTargets
    val results =
      consequences.flatMap(consequence =>
        s"kind:${consequence.kind.toString.toLowerCase}" ::
          EventCategories.toList.collect {
            case category if StructuralDeltaEvidence.hasConsequenceCategory(consequence.kind, category) =>
              s"category:${category.toString.toLowerCase}"
          }
      ) ++ Option.when(developmentChoices.nonEmpty)(
        s"kind:${TransitionConsequenceKind.DevelopmentPieceActivated.toString.toLowerCase}"
      )
    PlanEventIdentity.from(
      rootMove = rootMove,
      support = plan.support,
      actorRole = actorRole(beforeFen, rootMove),
      targets = targets,
      results = results
    )

  private def actorRole(fen: String, moveUci: String): Option[String] =
    val origin = Option(moveUci).getOrElse("").trim.toLowerCase.take(2)
    Fen.read(Standard, Fen.Full(fen)).flatMap(position =>
      chess.Square.fromKey(origin).flatMap(position.board.roleAt).map(_.name.toLowerCase)
    )
