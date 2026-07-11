package lila.chessjudgment.analysis.assembly

import chess.Color
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.structure.{ StructuralDeltaAnalyzer, StructuralDeltaContracts }
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
        for
          rootLine <- pressureRef.line.toList
          principalPlan <- pressure.rootBackedPlans(Some(rootLine.rootMove)).headOption.toList
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
          positiveConsequences = structural.consequences.filter(consequence =>
              consequence.positive &&
              consequence.strength > 0 &&
              PlanCausalEventProof.consequenceSupportsPlan(principalPlan, consequence) &&
              consequenceOwnedByPlan(rootLine.rootMove, consequence, planBindings, structuralBindings)
          )
          developmentChoices = structural.developmentChoices.filter(choice =>
            PlanCausalEventProof.developmentSupportsPlan(principalPlan) &&
              EvidenceRef.sameMove(s"${choice.from}${choice.to}", rootLine.rootMove)
          )
          (_, linePayload) = lineRecordAndPayload
          futureRealization = linePayload
            .futureRootObjectMove(linePayload.lineReplayCount.max(1))
            .map(trajectory =>
              PlanCausalFutureRealization(
                trajectory = trajectory,
                dependencyKind = PlanCausalDependencyKind.ObjectStatePrecondition,
                consequences = PlanCausalEventProof
                  .positiveConsequences(trajectory.futureStep, trajectory.color)
                  .filter(PlanCausalEventProof.consequenceSupportsPlan(principalPlan, _))
              )
            )
            .filter(realization => realization.dependencyProven && realization.consequences.nonEmpty)
          if positiveConsequences.nonEmpty || developmentChoices.nonEmpty || futureRealization.nonEmpty
        yield
          val branchWitnesses = futureRealization.toList.flatMap(realization =>
            branchWitnessesFor(
              input,
              context,
              structural.transition,
              realization.trajectory,
              realization.consequences
            )
          )
          val payload = PlanCausalEventEvidence(
            planId = planId,
            identity = PlanEventIdentityBuilder.from(
              rootMove = rootLine.rootMove,
              beforeFen = structural.transition.from.fen,
              plan = principalPlan,
              consequences = positiveConsequences,
              developmentChoices = developmentChoices
            ),
            rootLine = rootLine,
            rootTransition = structural.transition,
            structuralConsequences = positiveConsequences,
            developmentChoices = developmentChoices,
            futureRealization = futureRealization,
            branchWitnesses = branchWitnesses
          )
          val futureKey = payload.futureMove.getOrElse("direct")
          val authoritativePlan =
            pressure.uniqueRootBackedPlan(Some(rootLine.rootMove)).contains(principalPlan) &&
              planMotifRefs.nonEmpty &&
              planMotifRefs.forall(ref => ref.confidence != EvidenceConfidence.Heuristic)
          EvidenceRecord(
            ref = allocator.evidenceRef(
              suffix = s"plan-causal-event:${allocator.key(rootLine.role)}:${rootLine.rootMove}:${allocator.key(planId)}:$futureKey",
              producer = EvidenceProducer.PlanCausalEventProducer,
              layer = EvidenceLayer.PlanCausalEvent,
              position = transition.from,
              line = Some(rootLine),
              scope = transition.role.scope,
              confidence = if authoritativePlan then EvidenceConfidence.Mixed else EvidenceConfidence.Heuristic
            ),
            payload = payload,
            parents = (
              List(pressureRecord.ref, structuralRecord.ref, lineRecordAndPayload._1.ref, transition.evidence) ++
                planMotifRefs ++
                branchWitnesses.flatMap(witness => graph.records.find(_.ref.line.contains(witness.line)).map(_.ref))
            ).distinctBy(_.id)
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

  private def consequenceOwnedByPlan(
      rootMove: String,
      consequence: TransitionConsequence,
      planBindings: List[EvidenceObjectBinding],
      structuralBindings: List[EvidenceObjectBinding]
  ): Boolean =
    val normalizedKind = consequence.kind.toString.trim.toLowerCase
    val consequenceBindings = structuralBindings.filter(binding =>
      binding.mechanism.exists(obj =>
        obj.kind == EvidenceObjectKind.Mechanism && obj.key.trim.toLowerCase == normalizedKind
      )
    )
    val planObjects = planBindings.flatMap(binding => binding.actor ++ binding.target ++ binding.witness)
    val rootDestination = EvidenceRef.normalizeMove(rootMove).slice(2, 4)
    val planSquares =
      planObjects.collect { case ConcreteChessObject(EvidenceObjectKind.Square, key) => key.trim.toLowerCase }.toSet ++
        Option(rootDestination).filter(_.matches("[a-h][1-8]")).toSet
    val consequenceSquares = consequenceBindings
      .flatMap(_.target)
      .collect { case ConcreteChessObject(EvidenceObjectKind.Square, key) => key.trim.toLowerCase }
      .toSet
    val planFiles =
      planObjects.collect { case ConcreteChessObject(EvidenceObjectKind.File, key) => key.trim.toLowerCase }.toSet ++
        Option(rootDestination.take(1)).filter(_.matches("[a-h]")).toSet
    val consequenceFiles = consequenceBindings
      .flatMap(_.target)
      .collect { case ConcreteChessObject(EvidenceObjectKind.File, key) => key.trim.toLowerCase }
      .toSet
    consequenceSquares.intersect(planSquares).nonEmpty ||
      (consequenceSquares.isEmpty && consequenceFiles.intersect(planFiles).nonEmpty)

  private def branchWitnessesFor(
      input: NormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      transition: StructuralTransitionBinding,
      principal: LineObjectTrajectory,
      principalConsequences: List[TransitionConsequence]
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
                rootTransition = transition,
                principal = principal,
                principalConsequences = principalConsequences
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
  def developmentSupportsPlan(plan: PlanMatch): Boolean =
    planTheme(plan).exists(theme =>
      theme == PlanTheme.OpeningPrinciples || theme == PlanTheme.PieceRedeployment
    )

  def consequenceSupportsPlan(plan: PlanMatch, consequence: TransitionConsequence): Boolean =
    import TransitionConsequenceCategory.*
    import TransitionConsequenceKind.*

    def category(value: TransitionConsequenceCategory): Boolean =
      StructuralDeltaEvidence.hasConsequenceCategory(consequence.kind, value)

    planTheme(plan).exists {
      case PlanTheme.OpeningPrinciples =>
        category(OpeningDevelopment) || category(OpeningCenterControl)
      case PlanTheme.RestrictionProphylaxis =>
        consequence.kind == OpponentMobilityRestriction || category(TargetPressure) || category(CenterControl)
      case PlanTheme.PieceRedeployment =>
        category(PieceActivity) || category(Development) || consequence.kind == OutpostGain
      case PlanTheme.SpaceClamp =>
        category(CenterControl) || category(TargetPressure) ||
          consequence.kind == OpponentMobilityRestriction || consequence.kind == PawnTensionGain
      case PlanTheme.WeaknessFixation =>
        category(TargetPressure) ||
          Set(WeakPawnTargetCreated, WeakSquareTargetCreated, PawnTensionGain).contains(consequence.kind)
      case PlanTheme.PawnBreakPreparation =>
        category(PawnStructure) || category(PawnStructureDelta) || category(CenterControl)
      case PlanTheme.FlankInfrastructure =>
        category(PawnStructureDelta) || category(TargetPressure) || category(PieceActivity) ||
          Set(KingRingPressureGain, RookLiftActivation, LineUnlockGain).contains(consequence.kind)
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

  private def planTheme(plan: PlanMatch): Option[PlanTheme] =
    plan.support.collectFirst { case PlanSupport.Theme(theme) => theme }

  def branchWitness(
      sourceProbeId: String,
      line: LineNodeRef,
      linePayload: LineFactEvidence,
      rootTransition: StructuralTransitionBinding,
      principal: LineObjectTrajectory,
      principalConsequences: List[TransitionConsequence]
  ): PlanCausalBranchWitness =
    val rootStep = LineReplayStep(
      ply = rootTransition.to.ply,
      moveUci = rootTransition.moveUci,
      fenBefore = rootTransition.from.fen,
      fenAfter = rootTransition.to.fen
    )
    val observed = LineObjectTrajectory
      .find(rootStep, linePayload.lineReplaySteps, linePayload.lineReplayCount.max(1))
      .filter(trajectory => trajectory.pieceRole == principal.pieceRole && trajectory.color == principal.color)
    val observedConsequences = observed.toList.flatMap(trajectory =>
      positiveConsequences(trajectory.futureStep, trajectory.color)
    )
    val realizationMatch = observed.flatMap(trajectory =>
      PlanCausalFunctionalMatch.classify(
        expectedMove = principal.futureStep.moveUci,
        expectedConsequences = principalConsequences,
        observedMove = trajectory.futureStep.moveUci,
        observedConsequences = observedConsequences
      )
    )
    val outcome = observed match
      case Some(_) if realizationMatch.nonEmpty =>
        PlanCausalBranchOutcome.Realized
      case Some(_) =>
        PlanCausalBranchOutcome.Diverted
      case None if LineObjectTrajectory.remainsAtRootDestination(principal, linePayload.lineReplaySteps) =>
        PlanCausalBranchOutcome.Deferred
      case None =>
        PlanCausalBranchOutcome.Refuted
    PlanCausalBranchWitness(
      sourceProbeId = sourceProbeId,
      line = line,
      outcome = outcome,
      observedTrajectory = observed,
      observedConsequences = observedConsequences,
      realizationMatch = realizationMatch
    )

  def positiveConsequences(step: LineReplayStep, side: Color): List[TransitionConsequence] =
    for
      before <- Fen.read(Standard, Fen.Full(step.fenBefore)).toList
      after <- Fen.read(Standard, Fen.Full(step.fenAfter)).toList
      delta <- StructuralDeltaAnalyzer.delta(
        beforeFen = step.fenBefore,
        beforeBoard = before.board,
        afterFen = step.fenAfter,
        afterBoard = after.board,
        side = side,
        files = ('a' to 'h').toList,
        targets = Nil,
        createdTensionFrom = Option(EvidenceRef.normalizeMove(step.moveUci).slice(2, 4)).filter(_.matches("[a-h][1-8]")),
        moveUci = Some(step.moveUci)
      ).toList
      consequence <- StructuralDeltaContracts.consequences(delta)
      if consequence.positive && consequence.strength > 0
    yield consequence

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
