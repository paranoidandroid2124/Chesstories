package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.PrincipalVariationEvidence

/** The passed-pawn-result family's sole construction authority for the bounded causal core.
  * Every method validates an already certified lower authority; it performs no
  * move generation, relation extraction, or fallback board analysis.
  */
private[chessjudgment] object PassedPawnResultCausalAuthority:
  private final case class PropositionDescriptor(
      event: PassedPawnResultEventEvidence,
      assessment: PassedPawnResultReplyAssessment
  ) extends CausalSemanticDescriptor:
    require(
      event.exactRobustPublicResultAssessments.contains(assessment) &&
        assessment.consequence.kind == TransitionConsequenceKind.PassedPawnProgress,
      "a passed-pawn causal proposition needs an exact robust passed-pawn result"
    )
    val contractKind = BoundedCausalContractKind.PassedPawnResultUnderClosedReplies
    val rootFen: String = event.causalEpisode.root.step.fenBefore
    val semanticParts: List[String] = List(
      event.causalEpisode.root.identity.stableKey,
      assessment.sourceEvent.identity.stableKey,
      assessment.consequence.stableKey,
      assessment.resultProof.functionIdentity(event.causalEpisode.root).stableKey
    )

  private final case class DependencyAuthority(
      event: PassedPawnResultEventEvidence,
      assessment: PassedPawnResultReplyAssessment,
      dependencyIndex: Int
  ) extends CausalCertifiedDependencyAuthority:
    private val path = assessment.causalPath
    require(
      event.exactRobustPublicResultAssessments.contains(assessment) &&
        path.nonEmpty && path.head.from == event.causalEpisode.root &&
        path.last.to == assessment.sourceEvent &&
        path.zip(path.drop(1)).forall { case (left, right) => left.to == right.from },
      "a passed-pawn dependency authority needs one exact event-owned path"
    )
    require(path.indices.contains(dependencyIndex), "a passed-pawn dependency authority needs its exact path index")
    private val dependency = path(dependencyIndex)
    require(
      event.causalEpisode.dependencies.contains(dependency) && dependency.enablesContinuation,
      "a passed-pawn dependency authority needs an exact certified event dependency"
    )
    val before: LineReplayStep = dependency.from.step
    val after: LineReplayStep = dependency.to.step
    val lowerProofKey: String = dependency.stableKey

  def proposition(
      event: PassedPawnResultEventEvidence,
      assessment: PassedPawnResultReplyAssessment
  ): CausalPropositionIdentity =
    CausalPropositionIdentity.from(PropositionDescriptor(event, assessment))

  def expectedRoute(
      event: PassedPawnResultEventEvidence,
      assessment: PassedPawnResultReplyAssessment
  ): CausalBranchOccurrence =
    require(
      event.exactRobustPublicResultAssessments.contains(assessment),
      "a bounded passed-pawn result route needs an exact robust result owned by its event"
    )
    val dependencies = assessment.causalPath
    require(
      dependencies.nonEmpty && dependencies.head.from == event.causalEpisode.root &&
        dependencies.last.to == assessment.sourceEvent &&
        dependencies.zip(dependencies.drop(1)).forall { case (left, right) => left.to == right.from },
      "a bounded passed-pawn result route needs one connected root-owned dependency path"
    )
    val nodes = event.causalEpisode.root :: dependencies.map(_.to)
    require(
      nodes.forall(_.certifiedLegalStep.nonEmpty),
      "every retained passed-pawn result occurrence needs legal certification"
    )
    val continuationLine = event.continuationSourceLine.getOrElse(event.rootLine)
    val rootProvenance = CausalBranchOccurrence.rootProvenanceFor(event.rootLine)
    CausalBranchOccurrence.fromCertifiedOccurrences(
      PassedPawnResultBranchRole.ExpectedResultRoute,
      rootProvenance,
      event.rootLine,
      nodes.zipWithIndex.map { case (node, index) =>
        new CausalStepOccurrence(
          index,
          node.step,
          if index == 0 then event.rootLine else continuationLine,
          if index == 0 && rootProvenance == CausalRootProvenance.ObservedGameRoot then
            CausalStepProvenance.ObservedGameMove
          else CausalStepProvenance.CertifiedAnalysisMove,
          Option.when(index > 0)(dependencyLink(event, assessment, index - 1))
        )
      }
    )

  def legalReply(
      event: PassedPawnResultEventEvidence,
      witness: PassedPawnReplyBranchWitness
  ): CausalBranchOccurrence =
    require(event.branchWitnesses.contains(witness), "a bounded reply must belong to the exact passed-pawn result event")
    val replay = witness.canonicalReplay.getOrElse(
      throw IllegalArgumentException("a bounded reply needs its admitted canonical replay")
    )
    val replySteps = replay.replaySteps
    require(
      replySteps.nonEmpty && replySteps.size == witness.observedThroughPlyOffset &&
        replySteps.forall(replay.legalStep(_).nonEmpty) &&
        EvidenceRef.sameMove(replySteps.head.moveUci, witness.line.rootMove),
      "a bounded reply must retain its complete certified observation horizon"
    )
    val allSteps = event.causalEpisode.root.step :: replySteps
    val rootProvenance = CausalBranchOccurrence.rootProvenanceFor(event.rootLine)
    CausalBranchOccurrence.fromCertifiedOccurrences(
      PassedPawnResultBranchRole.LegalReply(witness.line.rootMove, witness.sourceProbeId),
      rootProvenance,
      event.rootLine,
      allSteps.zipWithIndex.map { case (step, index) =>
        new CausalStepOccurrence(
          index,
          step,
          if index == 0 then event.rootLine else witness.line,
          if index == 0 && rootProvenance == CausalRootProvenance.ObservedGameRoot then
            CausalStepProvenance.ObservedGameMove
          else CausalStepProvenance.CertifiedAnalysisMove,
          Option.when(index > 0)(CausalOccurrenceLink.adjacent(allSteps(index - 1), allSteps(index)))
        )
      }
    )

  private[judgment] def ownsExpectedRoute(
      event: PassedPawnResultEventEvidence,
      assessment: PassedPawnResultReplyAssessment,
      branch: CausalBranchOccurrence
  ): Boolean =
    val dependencies = assessment.causalPath
    val pathOwned =
      event.exactRobustPublicResultAssessments.contains(assessment) &&
        dependencies.nonEmpty && dependencies.head.from == event.causalEpisode.root &&
        dependencies.last.to == assessment.sourceEvent &&
        dependencies.zip(dependencies.drop(1)).forall { case (left, right) => left.to == right.from }
    if !pathOwned then false
    else
      val nodes = event.causalEpisode.root :: dependencies.map(_.to)
      val continuationLine = event.continuationSourceLine.getOrElse(event.rootLine)
      val rootProvenance = CausalBranchOccurrence.rootProvenanceFor(event.rootLine)
      nodes.forall(_.certifiedLegalStep.nonEmpty) &&
      branch.role == PassedPawnResultBranchRole.ExpectedResultRoute &&
      branch.rootProvenance == rootProvenance && branch.line == event.rootLine &&
      branch.rootPositionIdentity == SemanticPositionIdentity.fromFen(event.causalEpisode.root.step.fenBefore) &&
      branch.steps.size == nodes.size &&
      branch.steps.zip(nodes).zipWithIndex.forall { case ((occurrence, node), index) =>
        val expectedLine = if index == 0 then event.rootLine else continuationLine
        val expectedProvenance =
          if index == 0 && rootProvenance == CausalRootProvenance.ObservedGameRoot then
            CausalStepProvenance.ObservedGameMove
          else CausalStepProvenance.CertifiedAnalysisMove
        occurrence.index == index && occurrence.step == node.step && occurrence.line == expectedLine &&
        occurrence.provenance == expectedProvenance &&
        (if index == 0 then occurrence.linkFromPrevious.isEmpty
         else occurrence.linkFromPrevious.exists(link =>
           link.kind == CausalOccurrenceLinkKind.CertifiedCausalDependency &&
             link.lowerProofKey == dependencies(index - 1).stableKey &&
             link.links(nodes(index - 1).step, node.step)
         ))
      }

  private[judgment] def ownsLegalReply(
      event: PassedPawnResultEventEvidence,
      witness: PassedPawnReplyBranchWitness,
      branch: CausalBranchOccurrence
  ): Boolean =
    event.branchWitnesses.contains(witness) && witness.canonicalReplay.exists { replay =>
      val replySteps = replay.replaySteps
      val allSteps = event.causalEpisode.root.step :: replySteps
      val rootProvenance = CausalBranchOccurrence.rootProvenanceFor(event.rootLine)
      replySteps.nonEmpty && replySteps.size == witness.observedThroughPlyOffset &&
      replySteps.forall(replay.legalStep(_).nonEmpty) &&
      EvidenceRef.sameMove(replySteps.head.moveUci, witness.line.rootMove) &&
      branch.role == PassedPawnResultBranchRole.LegalReply(witness.line.rootMove, witness.sourceProbeId) &&
      branch.rootProvenance == rootProvenance && branch.line == event.rootLine &&
      branch.rootPositionIdentity == SemanticPositionIdentity.fromFen(event.causalEpisode.root.step.fenBefore) &&
      branch.steps.size == allSteps.size &&
      branch.steps.zip(allSteps).zipWithIndex.forall { case ((occurrence, step), index) =>
        val expectedLine = if index == 0 then event.rootLine else witness.line
        val expectedProvenance =
          if index == 0 && rootProvenance == CausalRootProvenance.ObservedGameRoot then
            CausalStepProvenance.ObservedGameMove
          else CausalStepProvenance.CertifiedAnalysisMove
        occurrence.index == index && occurrence.step == step && occurrence.line == expectedLine &&
        occurrence.provenance == expectedProvenance &&
        (if index == 0 then occurrence.linkFromPrevious.isEmpty
         else occurrence.linkFromPrevious.exists(link =>
           link.kind == CausalOccurrenceLinkKind.AdjacentLegalReplay && link.links(allSteps(index - 1), step)
         ))
      }
    }

  private def dependencyLink(
      event: PassedPawnResultEventEvidence,
      assessment: PassedPawnResultReplyAssessment,
      dependencyIndex: Int
  ): CausalOccurrenceLink =
    CausalOccurrenceLink.fromCertifiedDependency(DependencyAuthority(event, assessment, dependencyIndex))

private[chessjudgment] final case class PassedPawnResultClosedReplyInventoryBinding private (
    issuerEvidenceId: String,
    coverageEvidenceId: String,
    rootAfter: PositionNodeRef,
    scope: EvidenceScope,
    legalReplyMoves: List[String],
    branchByReply: List[(String, String)],
    certifiedHorizonPlyOffset: Int
) extends CausalSupplementalClosureBinding:
  require(issuerEvidenceId.nonEmpty, "a closed reply inventory needs its exact issuer")
  require(coverageEvidenceId.nonEmpty, "a closed reply inventory needs its exact branch-coverage issuer")
  require(legalReplyMoves.nonEmpty, "a closed reply inventory needs at least one legal reply")
  require(
    legalReplyMoves == legalReplyMoves.sorted && legalReplyMoves.distinct.size == legalReplyMoves.size,
    "closed legal replies must be unique and canonical"
  )
  require(
    branchByReply.map(_._1) == legalReplyMoves && branchByReply.map(_._2).distinct.size == branchByReply.size,
    "a closed reply inventory needs one occurrence branch for every legal reply"
  )
  require(certifiedHorizonPlyOffset > 0, "a closed reply inventory needs an exact positive horizon")

  val branchIds: Set[String] = branchByReply.map(_._2).toSet

  def stableKey: String =
    List(
      issuerEvidenceId,
      coverageEvidenceId,
      PrincipalVariationEvidence.normalizeFen(rootAfter.fen),
      rootAfter.ply.toString,
      scope.toString.toLowerCase,
      branchByReply.map { case (move, branch) => s"$move@$branch" }.mkString("[", ",", "]"),
      certifiedHorizonPlyOffset.toString
    ).mkString("|")

private[chessjudgment] object PassedPawnResultClosedReplyInventoryBinding:
  def from(
      inventoryRecord: EvidenceRecord,
      sourceRecord: EvidenceRecord,
      event: PassedPawnResultEventEvidence,
      replyBranches: List[(PassedPawnReplyBranchWitness, CausalBranchOccurrence)]
  ): PassedPawnResultClosedReplyInventoryBinding =
    val inventory = inventoryRecord.payload match
      case exact: StructuralDeltaEvidence => exact
      case _ => throw IllegalArgumentException("a closed reply inventory needs a StructuralDelta authority")
    require(
      sourceRecord.payload == event && event.branchSetComplete &&
        sourceRecord.parents.contains(inventoryRecord.ref),
      "a reply closure must retain its exact lower inventory and branch-coverage authorities"
    )
    require(
      inventoryRecord.ref.producer == EvidenceProducer.StructuralDeltaProducer &&
        inventoryRecord.ref.layer == EvidenceLayer.StructuralDelta &&
        inventoryRecord.ref.confidence == EvidenceConfidence.BoardDerived &&
        inventoryRecord.ref.position == event.rootTransition.from &&
        inventoryRecord.ref.line.contains(event.rootLine) &&
        inventoryRecord.ref.scope == event.rootTransition.role.scope &&
        inventory.transition == event.rootTransition &&
        inventory.transitionIsCertified && inventory.canonicalOutputShapeCertified &&
        inventory.canonicalTransitionProof == event.canonicalRootTransitionProof,
      "a reply closure must consume the exact graph-owned root transition inventory"
    )
    val legalReplies = inventory.certifiedRootResponseMoves
      .map(_.map(EvidenceRef.normalizeMove).sorted)
      .getOrElse(throw IllegalArgumentException("a closed passed-pawn result needs the root legal-reply inventory"))
    val byMove = replyBranches.map { case (witness, branch) =>
      require(event.branchWitnesses.contains(witness), "a closed reply branch must belong to its event")
      require(
        PassedPawnResultCausalAuthority.ownsLegalReply(event, witness, branch),
        "a closed reply inventory may bind only the exact family-owned reply occurrence"
      )
      EvidenceRef.normalizeMove(witness.line.rootMove) -> branch.branchId
    }.sortBy(_._1)
    require(
      byMove.map(_._1) == legalReplies,
      "a closed passed-pawn result must preserve every and only root legal reply"
    )
    val horizons = event.branchWitnesses.map(_.certifiedHorizonPlyOffset).distinct
    PassedPawnResultClosedReplyInventoryBinding(
      inventoryRecord.ref.id,
      sourceRecord.ref.id,
      event.rootTransition.to,
      inventoryRecord.ref.scope,
      legalReplies,
      byMove,
      horizons match
        case exact :: Nil => exact
        case _ => throw IllegalArgumentException("a closed passed-pawn result needs one exact branch horizon")
    )

/** One exact passed-pawn lower-premise occurrence use. Its concrete name
  * prevents other L2 families from treating passed-pawn route evidence as a
  * generic causal authority.
  */
private[chessjudgment] final case class PassedPawnResultPremiseUse private (
    role: PassedPawnResultPremiseRole,
    lowerKind: String,
    lowerSemanticKey: String,
    sourcePremiseIds: List[String],
    branchId: String,
    branchRole: CausalBranchRole,
    relatedBranchIds: List[String],
    fromStepIndex: Int,
    toStepIndex: Int,
    private[chessjudgment] val dependencyWitness: Option[PassedPawnResultDependencyPremiseWitness] = None
) extends CausalSupplementalPremiseUse:
  require(lowerKind.nonEmpty && lowerSemanticKey.nonEmpty, "a typed causal premise needs exact lower meaning")
  require(sourcePremiseIds.nonEmpty, "a typed causal premise needs exact lower evidence owners")
  require(
    sourcePremiseIds == sourcePremiseIds.sorted && sourcePremiseIds.distinct.size == sourcePremiseIds.size,
    "typed causal premise owners must be unique and canonical"
  )
  require(fromStepIndex >= 0 && toStepIndex >= fromStepIndex, "a typed premise needs ordered occurrences")
  require(
    relatedBranchIds == relatedBranchIds.sorted && relatedBranchIds.distinct.size == relatedBranchIds.size &&
      !relatedBranchIds.contains(branchId),
    "related causal branches must be distinct and canonical"
  )
  require(
    if Set("passed_pawn_result_dependency", "observed_passed_pawn_result_dependency")(lowerKind) then
      dependencyWitness.exists(witness =>
        witness.dependencyOccurrenceKey == lowerSemanticKey &&
          witness.relationOwnerIds.forall(sourcePremiseIds.contains)
      )
    else dependencyWitness.isEmpty,
    "only exact passed-pawn dependency premises may retain a dependency witness"
  )

  val branchIds: Set[String] = relatedBranchIds.toSet + branchId

  def stableKey: String =
    List(
      role.stableKey,
      lowerKind,
      lowerSemanticKey,
      sourcePremiseIds.mkString("[", ",", "]"),
      branchId,
      branchRole.stableKey,
      relatedBranchIds.mkString("[", ",", "]"),
      fromStepIndex.toString,
      toStepIndex.toString,
      dependencyWitness.map(_.dependencyOccurrenceKey).getOrElse("none")
    ).mkString("|")

private[chessjudgment] object PassedPawnResultPremiseUse:
  private def lineOccurrenceOwnerStepBinding(
      ownerId: String,
      occurrenceId: String,
      step: LineReplayStep
  ): String =
    BoundedCausalIdentity.digest(List(
      "passed-pawn-result-line-occurrence-owner-step:v1",
      ownerId,
      BoundedCausalIdentity.stepKey(step),
      occurrenceId
    ))

  private def structuralPremiseIds(event: PassedPawnResultEventNode): List[String] =
    (
      List(
        event.lineOccurrenceOwner.id,
        event.structuralOccurrence.occurrenceId,
        lineOccurrenceOwnerStepBinding(
          event.lineOccurrenceOwner.id,
          event.structuralOccurrence.occurrenceId,
          event.step
        )
      ) ++
        event.structuralOccurrence.sourcePremiseKeys
    ).distinct.sorted

  private def resultPremiseIds(proof: PassedPawnResultTransitionProof): List[String] =
    (
      List(
        proof.sourceLineOccurrenceOwner.id,
        proof.sourceOccurrenceId,
        lineOccurrenceOwnerStepBinding(
          proof.sourceLineOccurrenceOwner.id,
          proof.sourceOccurrenceId,
          LineReplayStep(
            proof.sourceTransition.to.ply,
            proof.sourceTransition.moveUci,
            proof.sourceTransition.from.fen,
            proof.sourceTransition.to.fen
          )
        )
      ) ++ proof.sourcePremiseKeys
    ).distinct.sorted

  def dependency(
      dependency: PassedPawnResultDependency,
      assessment: PassedPawnResultReplyAssessment,
      sourceRecord: EvidenceRecord,
      branch: CausalBranchOccurrence,
      fromStepIndex: Int,
      toStepIndex: Int
  ): PassedPawnResultPremiseUse =
    val event = sourceRecord.payload match
      case exact: PassedPawnResultEventEvidence => exact
      case _ => throw IllegalArgumentException("a passed-pawn result dependency premise needs its owning event record")
    val dependencyIndex = assessment.causalPath.zipWithIndex.collect {
      case (owned, index) if owned == dependency => index
    } match
      case exact :: Nil => exact
      case _ => throw IllegalArgumentException("an expected dependency needs one exact result-route index")
    require(
      event.causalEpisode.dependencies.contains(dependency) && dependency.enablesContinuation &&
        PassedPawnResultCausalAuthority.ownsExpectedRoute(event, assessment, branch),
      "a typed passed-pawn result premise must belong to its event's certified dependency inventory"
    )
    require(
      fromStepIndex == dependencyIndex && toStepIndex == dependencyIndex + 1 &&
      branch.stepAt(fromStepIndex).exists(_.step == dependency.from.step) &&
        branch.stepAt(toStepIndex).exists(_.step == dependency.to.step),
      "a typed passed-pawn result premise must bind its exact ordered occurrence endpoints"
    )
    val lineOccurrenceOwners =
      List(dependency.from.lineOccurrenceOwner.id, dependency.to.lineOccurrenceOwner.id).distinct
    require(
      lineOccurrenceOwners.forall(id => sourceRecord.parents.exists(_.id == id)),
      "a typed passed-pawn result dependency must retain both exact line occurrence owners"
    )
    val dependencyWitness = PassedPawnResultDependencyPremiseWitness.from(dependency)
    PassedPawnResultPremiseUse(
      PassedPawnResultPremiseRole.ExpectedDependency(dependencyIndex),
      "passed_pawn_result_dependency",
      dependency.stableKey,
      (
        sourceRecord.ref.id ::
          (
            structuralPremiseIds(dependency.from) ++
              structuralPremiseIds(dependency.to) ++
              dependencyWitness.relationOwnerIds
          )
      ).distinct.sorted,
      branch.branchId,
      branch.role,
      Nil,
      fromStepIndex,
      toStepIndex,
      Some(dependencyWitness)
    )

  def result(
      assessment: PassedPawnResultReplyAssessment,
      sourceRecord: EvidenceRecord,
      branch: CausalBranchOccurrence,
      stepIndex: Int
  ): PassedPawnResultPremiseUse =
    val event = sourceRecord.payload match
      case exact: PassedPawnResultEventEvidence => exact
      case _ => throw IllegalArgumentException("a passed-pawn-result premise needs its owning event record")
    require(
      event.exactRobustPublicResultAssessments.contains(assessment) &&
        assessment.resultProof.binds(assessment.sourceEvent, assessment.consequence, assessment.causalPath) &&
        PassedPawnResultCausalAuthority.ownsExpectedRoute(event, assessment, branch),
      "a typed passed-pawn-result premise needs an exact robust result route"
    )
    require(
      branch.stepAt(stepIndex).exists(_.step == assessment.sourceEvent.step),
      "a passed-pawn-result premise must bind its exact realizing occurrence"
    )
    require(
      sourceRecord.parents.exists(_.id == assessment.resultProof.sourceLineOccurrenceOwner.id),
      "a passed-pawn-result premise must retain its exact line occurrence owner"
    )
    PassedPawnResultPremiseUse(
      PassedPawnResultPremiseRole.ExpectedResult,
      "passed_pawn_result",
      assessment.resultProof.stableKey,
      (sourceRecord.ref.id :: resultPremiseIds(assessment.resultProof)).distinct.sorted,
      branch.branchId,
      branch.role,
      Nil,
      stepIndex,
      stepIndex
    )

  def observedDependency(
      dependency: PassedPawnResultDependency,
      realization: PassedPawnResultRealization,
      sourceRecord: EvidenceRecord,
      witness: PassedPawnReplyBranchWitness,
      branch: CausalBranchOccurrence,
      fromStepIndex: Int,
      toStepIndex: Int
  ): PassedPawnResultPremiseUse =
    val event = sourceRecord.payload match
      case exact: PassedPawnResultEventEvidence => exact
      case _ => throw IllegalArgumentException("an observed passed-pawn result dependency needs its owning event record")
    val observedEpisode = witness.observedEpisode.getOrElse(
      throw IllegalArgumentException("an observed passed-pawn result dependency needs its exact reply episode")
    )
    val dependencyIndex = realization.resultRoute.causalPath.zipWithIndex.collect {
      case (owned, index) if owned == dependency => index
    } match
      case exact :: Nil => exact
      case _ => throw IllegalArgumentException("an observed dependency needs one exact realizing-route index")
    require(
      event.branchWitnesses.contains(witness) && observedEpisode.resultRoutes.contains(realization.resultRoute) &&
        observedEpisode.dependencies.contains(dependency) &&
        dependency.enablesContinuation && PassedPawnResultCausalAuthority.ownsLegalReply(event, witness, branch),
      "an observed passed-pawn result premise must belong to its exact branch episode"
    )
    require(
      fromStepIndex < toStepIndex &&
      branch.stepAt(fromStepIndex).exists(_.step == dependency.from.step) &&
        branch.stepAt(toStepIndex).exists(_.step == dependency.to.step),
      "an observed passed-pawn result premise must bind its exact ordered reply occurrences"
    )
    val branchLineOwners = sourceRecord.parents
      .filter(_.line.contains(witness.line))
      .map(_.id)
    require(branchLineOwners.nonEmpty, "an observed passed-pawn result premise needs its exact branch-line owner")
    val lineOccurrenceOwners =
      List(dependency.from.lineOccurrenceOwner.id, dependency.to.lineOccurrenceOwner.id).distinct
    require(
      lineOccurrenceOwners.forall(id => sourceRecord.parents.exists(_.id == id)),
      "an observed passed-pawn result dependency must retain both exact line occurrence owners"
    )
    val dependencyWitness = PassedPawnResultDependencyPremiseWitness.from(dependency)
    PassedPawnResultPremiseUse(
      PassedPawnResultPremiseRole.ObservedDependency(witness.line.rootMove, dependencyIndex),
      "observed_passed_pawn_result_dependency",
      dependency.stableKey,
      (
        sourceRecord.ref.id ::
          (
            branchLineOwners ++
              structuralPremiseIds(dependency.from) ++
              structuralPremiseIds(dependency.to) ++
              dependencyWitness.relationOwnerIds
          )
      ).distinct.sorted,
      branch.branchId,
      branch.role,
      Nil,
      fromStepIndex,
      toStepIndex,
      Some(dependencyWitness)
    )

  def observedResult(
      realization: PassedPawnResultRealization,
      sourceRecord: EvidenceRecord,
      witness: PassedPawnReplyBranchWitness,
      branch: CausalBranchOccurrence,
      stepIndex: Int
  ): PassedPawnResultPremiseUse =
    val event = sourceRecord.payload match
      case exact: PassedPawnResultEventEvidence => exact
      case _ => throw IllegalArgumentException("an observed passed-pawn result needs its owning event record")
    val observedEpisode = witness.observedEpisode.getOrElse(
      throw IllegalArgumentException("an observed passed-pawn result needs its exact reply episode")
    )
    require(
      event.branchWitnesses.contains(witness) && realization.observedRoot == observedEpisode.root &&
        observedEpisode.resultRoutes.contains(realization.resultRoute) &&
        PassedPawnResultCausalAuthority.ownsLegalReply(event, witness, branch) &&
        realization.resultRoute.resultProof.binds(
          realization.resultRoute.sourceEvent,
          realization.resultRoute.consequence,
          realization.resultRoute.causalPath
        ),
      "an observed passed-pawn result must be an exact certified result route in its branch"
    )
    require(
      branch.stepAt(stepIndex).exists(_.step == realization.event.step),
      "an observed passed-pawn result must bind its exact realizing occurrence"
    )
    val branchLineOwners = sourceRecord.parents
      .filter(_.line.contains(witness.line))
      .map(_.id)
    require(branchLineOwners.nonEmpty, "an observed passed-pawn result needs its exact branch-line owner")
    require(
      sourceRecord.parents.exists(_.id == realization.resultRoute.resultProof.sourceLineOccurrenceOwner.id),
      "an observed passed-pawn result must retain its exact line occurrence owner"
    )
    PassedPawnResultPremiseUse(
      PassedPawnResultPremiseRole.ObservedResult(witness.line.rootMove),
      "observed_passed_pawn_result",
      realization.resultRoute.resultProof.stableKey,
      (
        sourceRecord.ref.id ::
          (branchLineOwners ++ resultPremiseIds(realization.resultRoute.resultProof))
      ).distinct.sorted,
      branch.branchId,
      branch.role,
      Nil,
      stepIndex,
      stepIndex
    )

  def functionalMatch(
      expected: PassedPawnResultReplyAssessment,
      realization: PassedPawnResultRealization,
      sourceRecord: EvidenceRecord,
      witness: PassedPawnReplyBranchWitness,
      expectedBranch: CausalBranchOccurrence,
      observedBranch: CausalBranchOccurrence,
      expectedStepIndex: Int,
      observedStepIndex: Int
  ): PassedPawnResultPremiseUse =
    val event = sourceRecord.payload match
      case exact: PassedPawnResultEventEvidence => exact
      case _ => throw IllegalArgumentException("a passed-pawn result functional match needs its owning event record")
    require(
      event.exactRobustPublicResultAssessments.contains(expected) && event.branchWitnesses.contains(witness) &&
        PassedPawnResultCausalAuthority.ownsExpectedRoute(event, expected, expectedBranch) &&
        PassedPawnResultCausalAuthority.ownsLegalReply(event, witness, observedBranch) &&
        PassedPawnResultFunctionalMatch.causallyEquivalent(
          event.causalEpisode.root,
          expected.resultRoute,
          realization.observedRoot,
          realization.resultRoute
        ),
      "a passed-pawn result functional-match premise needs two exact causally equivalent routes"
    )
    require(
      expectedBranch.stepAt(expectedStepIndex).exists(_.step == expected.sourceEvent.step) &&
        observedBranch.stepAt(observedStepIndex).exists(_.step == realization.event.step),
      "a passed-pawn result functional match must bind both exact result occurrences"
    )
    val lineOccurrenceOwners =
      List(
        expected.resultProof.sourceLineOccurrenceOwner.id,
        realization.resultRoute.resultProof.sourceLineOccurrenceOwner.id
      ).distinct
    require(
      lineOccurrenceOwners.forall(id => sourceRecord.parents.exists(_.id == id)),
      "a passed-pawn result functional match must retain both exact line occurrence owners"
    )
    PassedPawnResultPremiseUse(
      PassedPawnResultPremiseRole.FunctionalMatch(witness.line.rootMove),
      "passed_pawn_result_functional_match",
      BoundedCausalIdentity.digest(
        List(
          expected.resultRoute.stableKey,
          realization.resultRoute.stableKey,
          realization.matchKind.toString.toLowerCase
        )
      ),
      (
        sourceRecord.ref.id ::
          (resultPremiseIds(expected.resultProof) ++ resultPremiseIds(realization.resultRoute.resultProof))
      ).distinct.sorted,
      observedBranch.branchId,
      observedBranch.role,
      List(expectedBranch.branchId).sorted,
      observedStepIndex,
      observedStepIndex
    )

  def comparisonDemand(
      comparisonRecord: EvidenceRecord,
      branch: CausalBranchOccurrence,
      event: PassedPawnResultEventEvidence,
      assessment: PassedPawnResultReplyAssessment
  ): PassedPawnResultPremiseUse =
    val comparison = comparisonRecord.payload match
      case CandidateComparisonEvidence(exact) => exact
      case _ => throw IllegalArgumentException("a causal demand premise needs a typed comparison record")
    require(
      comparison.kind == CandidateComparisonKind.PlayedVsBest &&
        Set(comparison.referenceLine, comparison.candidateLine)(branch.line) &&
        PassedPawnResultCausalAuthority.ownsExpectedRoute(event, assessment, branch),
      "a passed-pawn result demand must be the exact PlayedVsBest endpoint comparison"
    )
    PassedPawnResultPremiseUse(
      PassedPawnResultPremiseRole.ComparisonDemand,
      "played_vs_best_demand",
      CandidateComparisonSemanticKey.from(comparison).stableKey,
      (comparisonRecord.ref.id :: comparisonRecord.parents.map(_.id)).distinct.sorted,
      branch.branchId,
      branch.role,
      Nil,
      0,
      0
    )
