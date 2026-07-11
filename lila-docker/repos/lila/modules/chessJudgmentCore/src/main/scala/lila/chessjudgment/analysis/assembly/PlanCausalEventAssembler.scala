package lila.chessjudgment.analysis.assembly

import chess.Color
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.structure.{ StructuralDeltaAnalyzer, StructuralDeltaContracts }
import lila.chessjudgment.model.{ PlanEventIdentity, PlanMatch }
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
          positiveConsequences = structural.consequences.filter(consequence => consequence.positive && consequence.strength > 0)
          if positiveConsequences.nonEmpty || structural.developmentChoices.nonEmpty
        yield
          val (_, linePayload) = lineRecordAndPayload
          val futureRealization = linePayload
            .futureRootObjectMove(linePayload.lineReplayCount.max(1))
            .map(trajectory =>
              PlanCausalFutureRealization(
                trajectory = trajectory,
                dependencyKind = PlanCausalDependencyKind.ObjectStatePrecondition,
                consequences = PlanCausalEventProof.positiveConsequences(trajectory.futureStep, trajectory.color)
              )
            )
            .filter(_.dependencyProven)
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
              developmentChoices = structural.developmentChoices
            ),
            rootLine = rootLine,
            rootTransition = structural.transition,
            structuralConsequences = positiveConsequences,
            developmentChoices = structural.developmentChoices,
            futureRealization = futureRealization,
            branchWitnesses = branchWitnesses
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
              confidence =
                if payload.branchCoverageComplete then EvidenceConfidence.EngineBacked
                else EvidenceConfidence.LegalReplayVerified
            ),
            payload = payload,
            parents = (
              List(pressureRecord.ref, structuralRecord.ref, lineRecordAndPayload._1.ref, transition.evidence) ++
                branchWitnesses.flatMap(witness => graph.records.find(_.ref.line.contains(witness.line)).map(_.ref))
            ).distinctBy(_.id)
          )
      case _ =>
        Nil
    }.distinctBy(_.ref.id)

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
