package lila.chessjudgment.analysis.assembly

import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.structure.WeaknessTargetProfile
import lila.chessjudgment.model.{ PlanEventIdentity, PlanMatch, PlanSupport }
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanTheme
import lila.chessjudgment.model.judgment.*

object PlanCausalEventAssembler:

  def fromAssembly(
      input: NormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val graph = context.evidenceGraph
    graph.records.flatMap {
      case pressureRecord @ EvidenceRecord(pressureRef, pressure: PlanPressureEvidence, _)
          if pressureRef.line.exists(_.role != LineNodeRole.Threat) =>
        val drafts = for
          rootLine <- pressureRef.line.toList
          principalPlan <- pressure.rootBackedPlans(Some(rootLine.rootMove)).distinctBy(_.plan.id)
          planId = principalPlan.plan.id
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
          planMotifRefs = rootPlanMotifRefs(graph, pressureRecord, rootLine, principalPlan)
          planBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, planMotifRefs)
          structuralBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, List(structuralRecord.ref))
          positiveConsequences = structural.consequences.flatMap { consequence =>
            Option
              .when(
                consequence.positive &&
                  consequence.strength > 0 &&
                  PlanCausalEventProof.consequenceSupportsPlan(principalPlan, consequence)
              )(consequence)
              .flatMap(PlanCausalEventProof.ownedConsequence(principalPlan, _, structural.transition))
              .filter(consequence =>
                PlanCausalEventProof.consequenceOwnedByRoot(
                  rootLine,
                  rootLine.rootMove,
                  consequence,
                  planBindings,
                  structuralBindings
                )
              )
          }
          developmentChoices = structural.developmentChoices.filter(choice =>
            PlanCausalEventProof.developmentSupportsPlan(principalPlan) &&
              EvidenceRef.sameMove(s"${choice.from}${choice.to}", rootLine.rootMove)
          )
          (_, linePayload) = lineRecordAndPayload
          rootIdentity = PlanEventIdentityBuilder.from(
            rootMove = rootLine.rootMove,
            beforeFen = structural.transition.from.fen,
            plan = principalPlan,
            consequences = positiveConsequences,
            developmentChoices = developmentChoices
          )
          episode = PlanCausalEpisodeBuilder
            .fromLine(
              plan = principalPlan,
              rootLine = rootLine,
              rootTransition = structural.transition,
              rootIdentity = rootIdentity,
              rootConsequences = positiveConsequences,
              rootDevelopmentChoices = developmentChoices,
              line = linePayload
            )
          if positiveConsequences.nonEmpty || developmentChoices.nonEmpty || episode.dependencyProven
        yield
          val branchWitnesses = Option.when(episode.dependencyProven)(episode).toList.flatMap(principalEpisode =>
            branchWitnessesFor(
              input,
              context,
              rootLine,
              structural.transition,
              principalPlan,
              principalEpisode
            )
          )
          val payload = PlanCausalEventEvidence(
            planId = planId,
            identity = rootIdentity,
            rootLine = rootLine,
            rootTransition = structural.transition,
            structuralConsequences = positiveConsequences,
            developmentChoices = developmentChoices,
            branchWitnesses = branchWitnesses,
            episode = Option.when(episode.dependencyProven)(episode)
          )
          val futureKey = payload.futureMove.getOrElse("direct")
          EvidenceRecord(
            ref = allocator.evidenceRef(
              suffix = s"plan-causal-event:${allocator.key(rootLine.role)}:${rootLine.rootMove}:${allocator.key(planId)}:$futureKey",
              producer = EvidenceProducer.PlanCausalEventProducer,
              layer = EvidenceLayer.PlanCausalEvent,
              position = transition.from,
              line = Some(rootLine),
              scope = transition.role.scope,
              confidence = EvidenceConfidence.Heuristic
            ),
            payload = payload,
            parents = (
              List(structuralRecord.ref, lineRecordAndPayload._1.ref, transition.evidence) ++
                planMotifRefs ++
                branchWitnesses.flatMap(witness => graph.records.find(_.ref.line.contains(witness.line)).map(_.ref))
            ).distinctBy(_.id)
          )
        val authorityCandidates =
          drafts.filter(record =>
              record.parents.exists(parent =>
                parent.layer == EvidenceLayer.MoveMotif && parent.confidence != EvidenceConfidence.Heuristic
              ) &&
                (record.payload match
                  case event: PlanCausalEventEvidence =>
                      event.structuralConsequences.nonEmpty ||
                      event.developmentChoices.nonEmpty ||
                      event.episodePublicProofReady ||
                      event.episode.exists(_.dependencyProven)
                  case _ => false)
            )
        val decisiveCandidates = authorityCandidates.filter(record =>
          record.payload match
            case event: PlanCausalEventEvidence => PlanCausalEventProof.decisiveGoalProof(event)
            case _                              => false
        )
        val ambiguousRootPlans =
          pressure.rootBackedPlans(pressureRef.line.map(_.rootMove)).distinctBy(_.plan.id).size > 1
        val authorityPool =
          if authorityCandidates.size == 1 && !ambiguousRootPlans then authorityCandidates
          else decisiveCandidates
        val authoritativeIds = authorityPool.map(_.ref.id).toSet
        drafts.map(record =>
          if authoritativeIds(record.ref.id) then
            record.copy(ref = record.ref.copy(confidence = EvidenceConfidence.Mixed))
          else record
        )
      case _ =>
        Nil
    }.distinctBy(_.ref.id)

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
      input: NormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      rootLine: LineNodeRef,
      transition: StructuralTransitionBinding,
      plan: PlanMatch,
      principal: PlanCausalEpisode
  ): List[PlanCausalBranchWitness] =
    input.threatBranches
      .filter(branch =>
        EvidenceRef.sameMove(branch.probedMoveUci, transition.moveUci) && branch.branchFen == transition.to.fen
      )
      .flatMap(branch =>
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
                principal = principal
              )
            }
        }
      )
      .distinctBy(_.line)

  private def linePayload(context: JudgmentAssemblyContext, line: LineNodeRef): Option[LineFactEvidence] =
    context.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, payload: LineFactEvidence, _) if ref.line.contains(line) => payload
    }

private[assembly] object PlanCausalEventProof:
  private val FlankInfrastructureConsequences = Set(
    TransitionConsequenceKind.PawnTensionGain,
    TransitionConsequenceKind.TargetPressureGain,
    TransitionConsequenceKind.KingRingPressureGain,
    TransitionConsequenceKind.RookLiftActivation,
    TransitionConsequenceKind.LineUnlockGain,
    TransitionConsequenceKind.BatteryPressureGain
  )

  def developmentSupportsPlan(plan: PlanMatch): Boolean =
    planTheme(plan).exists(theme =>
      theme == PlanTheme.OpeningPrinciples || theme == PlanTheme.PieceRedeployment
    )

  def decisiveGoalProof(event: PlanCausalEventEvidence): Boolean =
    import TransitionConsequenceKind.*
    val consequences = (
      event.structuralConsequences ++
        event.episode.toList.flatMap(_.futureEvents.flatMap(_.structuralConsequences))
    ).distinct
    val developmentChoices = (
      event.developmentChoices ++
        event.episode.toList.flatMap(_.futureEvents.flatMap(_.developmentChoices))
    ).distinct
    event.identity.goalTheme match
      case PlanTheme.OpeningPrinciples =>
        developmentChoices.nonEmpty || consequences.exists(consequence =>
          Set(DevelopmentPieceActivated, DevelopmentMobilityGain, DevelopmentCenterControlGain, DevelopmentSafePlacement)(
            consequence.kind
          )
        )
      case PlanTheme.RestrictionProphylaxis =>
        consequences.exists(_.kind == OpponentMobilityRestriction)
      case PlanTheme.PieceRedeployment =>
        consequences.exists(consequence =>
          Set(FileOccupationGain, OutpostGain, RookLiftActivation, BatteryPressureGain)(consequence.kind)
        )
      case PlanTheme.WeaknessFixation =>
        consequences.exists(consequence =>
          Set(WeakPawnTargetCreated, WeakSquareTargetCreated)(consequence.kind) ||
            consequence.kind == TargetPressureGain && weakTargetOwned(event.rootTransition, consequence.subjects)
        )
      case PlanTheme.PawnBreakPreparation =>
        consequences.exists(_.kind == PawnTensionGain)
      case PlanTheme.SpaceClamp =>
        consequences.exists(consequence =>
          Set(CenterControlGain, OpponentMobilityRestriction)(consequence.kind)
        )
      case PlanTheme.FlankInfrastructure =>
        consequences.exists(consequence => FlankInfrastructureConsequences(consequence.kind))
      case PlanTheme.AdvantageTransformation =>
        consequences.exists(consequence =>
          Set(PassedPawnProgress, PromotionPressureGain, FileOccupationGain)(consequence.kind)
        )
      case PlanTheme.FavorableExchange | PlanTheme.Unknown =>
        false

  def planOwnsConsequenceTarget(
      plan: PlanMatch,
      consequence: TransitionConsequence,
      transition: StructuralTransitionBinding
  ): Boolean =
    ownedConsequence(plan, consequence, transition).contains(consequence)

  def ownedConsequence(
      plan: PlanMatch,
      consequence: TransitionConsequence,
      transition: StructuralTransitionBinding
  ): Option[TransitionConsequence] =
    planTheme(plan) match
      case Some(PlanTheme.WeaknessFixation) if consequence.kind == TransitionConsequenceKind.TargetPressureGain =>
        val ownedSubjects = weakTargetSubjects(transition, consequence.subjects)
        Option.when(ownedSubjects.nonEmpty)(
          consequence.copy(
            strength = consequence.strength.min(ownedSubjects.size).max(1),
            subjects = ownedSubjects
          )
        )
      case _ =>
        Some(consequence)

  private def weakTargetOwned(
      transition: StructuralTransitionBinding,
      subjects: List[String]
  ): Boolean =
    weakTargetSubjects(transition, subjects).nonEmpty

  private def weakTargetSubjects(
      transition: StructuralTransitionBinding,
      subjects: List[String]
  ): List[String] =
    Fen.read(Standard, Fen.Full(transition.from.fen)).toList.flatMap { position =>
      val weakTargets =
        WeaknessTargetProfile.targetsForPressure(position.board, transition.perspective).map(_.targetSquare).toSet
      subjects.map(_.trim.toLowerCase).filter(weakTargets).distinct
    }

  def consequenceSupportsPlan(plan: PlanMatch, consequence: TransitionConsequence): Boolean =
    import TransitionConsequenceKind.*

    planTheme(plan).exists {
      case PlanTheme.OpeningPrinciples =>
        Set(
          CenterControlGain,
          DevelopmentLagReduced,
          DevelopmentPieceActivated,
          DevelopmentMobilityGain,
          DevelopmentCenterControlGain,
          DevelopmentSafePlacement
        )(consequence.kind)
      case PlanTheme.RestrictionProphylaxis =>
        consequence.kind == OpponentMobilityRestriction
      case PlanTheme.PieceRedeployment =>
        Set(
          MobilityGain,
          LineUnlockGain,
          FileOccupationGain,
          OutpostGain,
          RookLiftActivation,
          BatteryPressureGain,
          DevelopmentPieceActivated,
          DevelopmentMobilityGain,
          DevelopmentCenterControlGain,
          DevelopmentSafePlacement
        )(consequence.kind)
      case PlanTheme.SpaceClamp =>
        Set(CenterControlGain, OpponentMobilityRestriction, PawnTensionGain)(consequence.kind)
      case PlanTheme.WeaknessFixation =>
        Set(TargetPressureGain, WeakPawnTargetCreated, WeakSquareTargetCreated, PawnTensionGain)(consequence.kind)
      case PlanTheme.PawnBreakPreparation =>
        Set(OpenFileGain, SemiOpenFileGain, PawnTensionGain, CenterControlGain)(consequence.kind)
      case PlanTheme.FlankInfrastructure =>
        FlankInfrastructureConsequences(consequence.kind)
      case PlanTheme.AdvantageTransformation =>
        Set(
          PassedPawnProgress,
          PromotionPressureGain,
          FileOccupationGain,
          OpenFileGain,
          SemiOpenFileGain,
          TargetPressureGain
        ).contains(consequence.kind)
      case PlanTheme.FavorableExchange | PlanTheme.Unknown =>
        false
    }

  def consequenceOwnedByRoot(
      rootLine: LineNodeRef,
      rootMove: String,
      consequence: TransitionConsequence,
      planBindings: List[EvidenceObjectBinding],
      structuralBindings: List[EvidenceObjectBinding]
  ): Boolean =
    val normalizedKind = consequence.kind.toString.trim.toLowerCase
    def ownsRoot(binding: EvidenceObjectBinding): Boolean =
      binding.line.contains(rootLine) &&
        (binding.actor ++ binding.witness).exists(obj =>
          obj.kind == EvidenceObjectKind.Move && EvidenceRef.sameMove(obj.key, rootMove)
        )
    val planOwnsRoot = planBindings.exists(ownsRoot)
    val consequenceBindings = structuralBindings.filter(binding =>
      ownsRoot(binding) &&
        binding.mechanism.exists(obj =>
          obj.kind == EvidenceObjectKind.Mechanism && obj.key.trim.toLowerCase == normalizedKind
        )
    )
    val subjectSquares = consequence.subjects.flatMap(subject => "[a-h][1-8]".r.findAllIn(subject.toLowerCase)).toSet
    val bindingOwnsSubjects =
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
        TransitionConsequenceKind.TargetPressureGain,
        TransitionConsequenceKind.CenterControlGain,
        TransitionConsequenceKind.PassedPawnProgress,
        TransitionConsequenceKind.PromotionPressureGain,
        TransitionConsequenceKind.OutpostGain,
        TransitionConsequenceKind.RookLiftActivation,
        TransitionConsequenceKind.BatteryPressureGain,
        TransitionConsequenceKind.OpponentMobilityRestriction,
        TransitionConsequenceKind.KingRingPressureGain
      )(consequence.kind)
    planOwnsRoot && consequenceBindings.nonEmpty && bindingOwnsSubjects &&
      (!concreteTargetRequired ||
        consequence.subjects.nonEmpty && consequenceBindings.exists(
          _.target.exists(EvidenceObjectBinding.specificSurfaceTargetObject)
        ))

  private def planTheme(plan: PlanMatch): Option[PlanTheme] =
    plan.support.collectFirst { case PlanSupport.Theme(theme) => theme }

  def branchWitness(
      sourceProbeId: String,
      line: LineNodeRef,
      linePayload: LineFactEvidence,
      rootLine: LineNodeRef,
      rootTransition: StructuralTransitionBinding,
      plan: PlanMatch,
      principal: PlanCausalEpisode
  ): PlanCausalBranchWitness =
    val observedCandidate = PlanCausalEpisodeBuilder.fromContinuation(
      plan = plan,
      rootLine = rootLine,
      role = rootTransition.role,
      root = principal.root,
      continuation = linePayload.lineReplaySteps
    )
    val observed = Option.when(observedCandidate.dependencyProven)(observedCandidate)
    val expectedConsequences = principal.futureEvent.toList.flatMap(_.structuralConsequences)
    val observedConsequences = observed.toList.flatMap(_.futureEvent.toList.flatMap(_.structuralConsequences)).distinct
    val exactMoves = observed.exists(episode =>
      episode.events.map(_.moveUci) == principal.events.map(_.moveUci) &&
        (
          PlanCausalFunctionalMatch.functionallyEquivalent(expectedConsequences, observedConsequences) ||
            principal.responses.map(_.target).toSet.intersect(episode.responses.map(_.target).toSet).nonEmpty
        )
    )
    val realizationMatch =
      Option.when(exactMoves)(PlanCausalRealizationMatch.ExactMove).orElse(
        for
          expectedMove <- principal.futureMove
          observedEpisode <- observed
          observedMove <- observedEpisode.futureMove
          result <- PlanCausalFunctionalMatch.classify(
            expectedMove = expectedMove,
            expectedConsequences = expectedConsequences,
            observedMove = observedMove,
            observedConsequences = observedConsequences
          )
        yield result
      )
    val outcome = observed match
      case Some(_) if realizationMatch.nonEmpty =>
        PlanCausalBranchOutcome.Realized
      case Some(_) =>
        PlanCausalBranchOutcome.Diverted
      case None if linePayload.lineReplayCount < principal.futureEvent.fold(principal.spanPlies - 1)(_.step.ply - principal.root.step.ply) =>
        PlanCausalBranchOutcome.Deferred
      case None =>
        PlanCausalBranchOutcome.Refuted
    PlanCausalBranchWitness(
      sourceProbeId = sourceProbeId,
      line = line,
      outcome = outcome,
      observedEpisode = observed,
      observedConsequences = observedConsequences,
      realizationMatch = realizationMatch
    )

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
    val squareTargets =
      (consequences.flatMap(_.subjects).map(_.toLowerCase.replace(normalizedRootMove, "")) ++ developmentChoices.map(_.to))
        .flatMap(value => "[a-h][1-8]".r.findAllIn(value.toLowerCase).map(square => s"square:$square"))
    val targets =
      consequences.flatMap(_.subjects) ++ squareTargets
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
