package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.PrincipalVariationEvidence

/** Sole construction authority for the passed-pawn-progress-realized-after-only-legal-reply
  * bounded causal core. It consumes only graph-owned certified analysis continuations and the
  * StructuralDelta root-response closure; it performs no board analysis.
  */
private[chessjudgment] object PassedPawnProgressRealizedAfterOnlyLegalReplyAuthority:
  private final case class PropositionDescriptor(
      event: PassedPawnResultEventEvidence,
      routes: List[PassedPawnResultRoute]
  ) extends CausalSemanticDescriptor:
    val semanticIdentity = exactSemanticIdentity(event, routes)
    val contractKind = BoundedCausalContractKind.PassedPawnProgressRealizedAfterOnlyLegalReply
    val rootFen: String = event.causalEpisode.root.step.fenBefore
    val semanticParts: List[String] = List(semanticIdentity.stableKey)

  def proposition(
      event: PassedPawnResultEventEvidence,
      routes: List[PassedPawnResultRoute]
  ): CausalPropositionIdentity =
    CausalPropositionIdentity.from(PropositionDescriptor(event, routes))

  def analysisContinuationRoute(
      event: PassedPawnResultEventEvidence,
      routes: List[PassedPawnResultRoute]
  ): CausalBranchOccurrence =
    val steps = exactAnalysisContinuationSteps(event, routes)
    val replyMove = steps.lift(1).map(_.moveUci).getOrElse(
      throw IllegalArgumentException("a passed-pawn analysis continuation needs the certified root reply")
    )
    require(
      event.onlyLegalReplyMove.exists(EvidenceRef.sameMove(_, replyMove)),
      "a passed-pawn analysis continuation must begin with the StructuralDelta-certified only reply"
    )
    val rootProvenance = event.subjectOccurrence.rootProvenance
    require(
      rootProvenance == CausalRootProvenance.ObservedGameRoot &&
        event.subjectOccurrence.rootProvenance == rootProvenance,
      "a passed-pawn occurrence explanation needs its certified history root"
    )
    CausalBranchOccurrence.fromCertifiedOccurrences(
      PassedPawnProgressBranchRole.ObservedRootWithAnalyzedContinuation(
        EvidenceRef.normalizeMove(replyMove),
        event.causalEpisode.root.structuralOccurrence.occurrenceId
      ),
      rootProvenance,
      event.rootLine,
      event.subjectOccurrence.lineOwnerEvidenceId,
      event.subjectOccurrence.transitionEvidenceId,
      steps.zipWithIndex.map { case (step, index) =>
        new CausalStepOccurrence(
          index,
          step,
          event.rootLine,
          if index == 0 && rootProvenance == CausalRootProvenance.ObservedGameRoot then
            CausalStepProvenance.ObservedGameMove
          else CausalStepProvenance.CertifiedAnalysisMove
        )
      }
    )

  private[judgment] def ownsAnalysisContinuationRoute(
      event: PassedPawnResultEventEvidence,
      routes: List[PassedPawnResultRoute],
      branch: CausalBranchOccurrence
  ): Boolean =
    scala.util.Try {
      val steps = exactAnalysisContinuationSteps(event, routes)
      val replyMove = steps(1).moveUci
      val rootProvenance = event.subjectOccurrence.rootProvenance
      branch.role == PassedPawnProgressBranchRole.ObservedRootWithAnalyzedContinuation(
        EvidenceRef.normalizeMove(replyMove),
        event.causalEpisode.root.structuralOccurrence.occurrenceId
      ) && branch.rootProvenance == rootProvenance && branch.line == event.rootLine &&
        branch.lineOwnerEvidenceId == event.subjectOccurrence.lineOwnerEvidenceId &&
        branch.rootTransitionEvidenceId == event.subjectOccurrence.transitionEvidenceId &&
        branch.rootPositionIdentity == SemanticPositionIdentity.fromFen(event.causalEpisode.root.step.fenBefore) &&
        branch.steps.size == steps.size && branch.steps.zip(steps).zipWithIndex.forall {
          case ((occurrence, step), index) =>
            val expectedProvenance =
              if index == 0 && rootProvenance == CausalRootProvenance.ObservedGameRoot then
                CausalStepProvenance.ObservedGameMove
              else CausalStepProvenance.CertifiedAnalysisMove
            occurrence.index == index && occurrence.step == step && occurrence.line == event.rootLine &&
              occurrence.provenance == expectedProvenance
        }
    }.getOrElse(false)

  private[judgment] def exactSemanticIdentity(
      event: PassedPawnResultEventEvidence,
      routes: List[PassedPawnResultRoute]
  ): PassedPawnProgressSemanticIdentity =
    require(
      routes.nonEmpty && routes == routes.sortBy(_.stableKey) && routes.distinct.size == routes.size &&
        routes.forall(event.exactOnlyReplyResultRoutes.contains),
      "a passed-pawn proof needs canonical graph-owned analysis result routes"
    )
    routes.map(PassedPawnProgressSemanticIdentity.from(event, _)).distinct match
      case exact :: Nil => exact
      case _ =>
        throw IllegalArgumentException(
          "independent passed-pawn proof paths must share one exact result proposition"
        )

  private def exactAnalysisContinuationSteps(
      event: PassedPawnResultEventEvidence,
      routes: List[PassedPawnResultRoute]
  ): List[LineReplayStep] =
    exactSemanticIdentity(event, routes)
    val paths = routes.map(route =>
      event.causalEpisode.root.step :: route.causalPath.flatMap(dependency =>
        dependency.interveningSteps :+ dependency.to.step
      )
    )
    require(
      paths.forall(path => path.size > 1 && path.map(_.ply) == (path.head.ply to path.last.ply).toList),
      "a passed-pawn analysis continuation must retain every ordered replay occurrence"
    )
    paths.distinct match
      case exact :: Nil => exact
      case _ =>
        throw IllegalArgumentException(
          "independent dependency proofs for one result must retain the same certified analysis occurrence"
        )

private[chessjudgment] final case class PassedPawnProgressClosedReplyInventoryBinding private (
    issuerEvidenceId: String,
    eventEvidenceId: String,
    rootAfter: PositionNodeRef,
    scope: EvidenceScope,
    legalReplyMove: String,
    analysisContinuationBranchId: String
) extends CausalSupplementalClosureBinding:
  require(issuerEvidenceId.nonEmpty, "a closed reply inventory needs its exact issuer")
  require(eventEvidenceId.nonEmpty, "a closed reply inventory needs its exact progress-event owner")
  require(EvidenceRef.normalizeMove(legalReplyMove).nonEmpty, "a closed reply inventory needs its only legal reply")
  require(
    analysisContinuationBranchId.nonEmpty,
    "a closed reply inventory needs its analysis-continuation occurrence"
  )

  val branchIds: Set[String] = Set(analysisContinuationBranchId)

  def stableKey: String =
    List(
      issuerEvidenceId,
      eventEvidenceId,
      PrincipalVariationEvidence.normalizeFen(rootAfter.fen),
      rootAfter.ply.toString,
      scope.toString.toLowerCase,
      EvidenceRef.normalizeMove(legalReplyMove),
      analysisContinuationBranchId
    ).mkString("|")

private[chessjudgment] object PassedPawnProgressClosedReplyInventoryBinding:
  def from(
      inventoryRecord: EvidenceRecord,
      sourceRecord: EvidenceRecord,
      event: PassedPawnResultEventEvidence,
      routes: List[PassedPawnResultRoute],
      analysisContinuation: CausalBranchOccurrence
  ): PassedPawnProgressClosedReplyInventoryBinding =
    val inventory = inventoryRecord.payload match
      case exact: StructuralDeltaEvidence => exact
      case _ => throw IllegalArgumentException("a closed reply inventory needs a StructuralDelta authority")
    require(
      sourceRecord.payload == event && event.onlyLegalReplyClosed && sourceRecord.parents.contains(inventoryRecord.ref),
      "a reply closure must retain its exact lower inventory and progress-event authorities"
    )
    require(
      inventoryRecord.ref.producer == EvidenceProducer.StructuralDeltaProducer &&
        inventoryRecord.ref.layer == EvidenceLayer.StructuralDelta &&
        inventoryRecord.ref.confidence == EvidenceConfidence.BoardDerived &&
        inventoryRecord.ref.position == event.rootTransition.from &&
        inventoryRecord.ref.line.contains(event.rootLine) &&
        inventoryRecord.ref.scope == event.rootTransition.role.scope &&
        inventory.transition == event.rootTransition && inventory.transitionIsCertified &&
        inventory.canonicalOutputShapeCertified &&
        inventory.canonicalTransitionProof == event.canonicalRootTransitionProof,
      "a reply closure must consume the exact graph-owned root transition inventory"
    )
    val legalReply = inventory.certifiedRootResponseMoves
      .map(_.map(EvidenceRef.normalizeMove))
      .collect { case reply :: Nil => reply }
      .getOrElse(
        throw IllegalArgumentException(
          "a closed passed-pawn-progress-realized-after-only-legal-reply proof needs exactly one legal root reply"
        )
      )
    require(
      event.onlyLegalReplyMove.exists(EvidenceRef.sameMove(_, legalReply)) &&
        PassedPawnProgressRealizedAfterOnlyLegalReplyAuthority
          .ownsAnalysisContinuationRoute(event, routes, analysisContinuation) &&
        analysisContinuation.steps.lift(1).exists(step => EvidenceRef.sameMove(step.step.moveUci, legalReply)),
      "a closed reply inventory may bind only the same analysis continuation certified by its StructuralDelta owner"
    )
    PassedPawnProgressClosedReplyInventoryBinding(
      inventoryRecord.ref.id,
      sourceRecord.ref.id,
      event.rootTransition.to,
      inventoryRecord.ref.scope,
      legalReply,
      analysisContinuation.branchId
    )

/** One exact passed-pawn lower-premise occurrence use. */
private[chessjudgment] final case class PassedPawnProgressPremiseUse private (
    role: PassedPawnProgressPremiseRole,
    lowerKind: String,
    lowerSemanticKey: String,
    sourcePremiseIds: List[String],
    branchId: String,
    branchRole: CausalBranchRole,
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
    if lowerKind == "passed_pawn_progress_dependency" then
      dependencyWitness.exists(witness =>
        witness.dependencyOccurrenceKey == lowerSemanticKey &&
          witness.requiredSourcePremiseIds.forall(sourcePremiseIds.contains)
      )
    else dependencyWitness.isEmpty,
    "only exact passed-pawn dependency premises may retain a dependency witness"
  )

  val branchIds: Set[String] = Set(branchId)

  def stableKey: String =
    List(
      role.stableKey,
      lowerKind,
      lowerSemanticKey,
      sourcePremiseIds.mkString("[", ",", "]"),
      branchId,
      branchRole.stableKey,
      fromStepIndex.toString,
      toStepIndex.toString,
      dependencyWitness.map(_.dependencyOccurrenceKey).getOrElse("none")
    ).mkString("|")

private[chessjudgment] object PassedPawnProgressPremiseUse:
  private def lineOccurrenceOwnerStepBinding(
      ownerId: String,
      occurrenceId: String,
      step: LineReplayStep
  ): String =
    BoundedCausalIdentity.digest(List(
      "passed-pawn-progress-realized-after-only-legal-reply-line-owner-step:v1",
      ownerId,
      BoundedCausalIdentity.stepKey(step),
      occurrenceId
    ))

  private def structuralPremiseIds(event: PassedPawnResultEventNode): List[String] =
    (List(
      event.lineOccurrenceOwner.id,
      event.structuralOccurrence.occurrenceId,
      lineOccurrenceOwnerStepBinding(
        event.lineOccurrenceOwner.id,
        event.structuralOccurrence.occurrenceId,
        event.step
      )
    ) ++ event.structuralOccurrence.sourcePremiseKeys).distinct.sorted

  private def resultPremiseIds(proof: PassedPawnResultTransitionProof): List[String] =
    (List(
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
    ) ++ proof.sourcePremiseKeys).distinct.sorted

  def dependency(
      dependency: PassedPawnResultDependency,
      route: PassedPawnResultRoute,
      routes: List[PassedPawnResultRoute],
      sourceRecord: EvidenceRecord,
      branch: CausalBranchOccurrence,
      fromStepIndex: Int,
      toStepIndex: Int,
      stateBindings: List[CausalClosedStateBinding]
  ): PassedPawnProgressPremiseUse =
    val event = exactEvent(sourceRecord)
    val dependencyIndex = route.causalPath.zipWithIndex.collect {
      case (owned, index) if owned == dependency => index
    } match
      case exact :: Nil => exact
      case _ =>
        throw IllegalArgumentException("an analysis-continuation dependency needs one exact result-route index")
    require(
      routes.contains(route) && event.causalEpisode.dependencies.contains(dependency) &&
        dependency.enablesContinuation &&
        PassedPawnProgressRealizedAfterOnlyLegalReplyAuthority.ownsAnalysisContinuationRoute(event, routes, branch),
      "a typed passed-pawn premise must belong to its event's certified analysis dependency inventory"
    )
    require(
      fromStepIndex < toStepIndex && branch.stepAt(fromStepIndex).exists(_.step == dependency.from.step) &&
        branch.stepAt(toStepIndex).exists(_.step == dependency.to.step),
      "a typed passed-pawn dependency must bind its exact ordered occurrence endpoints"
    )
    val lineOccurrenceOwners = List(dependency.from.lineOccurrenceOwner.id, dependency.to.lineOccurrenceOwner.id).distinct
    require(
      lineOccurrenceOwners.forall(id => sourceRecord.parents.exists(_.id == id)) &&
        dependency.lowerIssuerRecords.forall(owner => sourceRecord.parents.contains(owner.ref)),
      "a typed passed-pawn dependency must retain every exact lower occurrence owner"
    )
    val witness = PassedPawnResultDependencyPremiseWitness.from(dependency, stateBindings)
    PassedPawnProgressPremiseUse(
      PassedPawnProgressPremiseRole.Dependency(dependencyIndex),
      "passed_pawn_progress_dependency",
      dependency.stableKey,
      (sourceRecord.ref.id ::
        (structuralPremiseIds(dependency.from) ++ structuralPremiseIds(dependency.to) ++
          witness.requiredSourcePremiseIds)).distinct.sorted,
      branch.branchId,
      branch.role,
      fromStepIndex,
      toStepIndex,
      Some(witness)
    )

  def result(
      route: PassedPawnResultRoute,
      routes: List[PassedPawnResultRoute],
      sourceRecord: EvidenceRecord,
      branch: CausalBranchOccurrence,
      stepIndex: Int
  ): PassedPawnProgressPremiseUse =
    val event = exactEvent(sourceRecord)
    require(
      routes.contains(route) && event.exactOnlyReplyResultRoutes.contains(route) &&
        route.resultProof.binds(route.sourceEvent, route.consequence, route.causalPath) &&
        PassedPawnProgressRealizedAfterOnlyLegalReplyAuthority.ownsAnalysisContinuationRoute(event, routes, branch),
      "a typed passed-pawn result premise needs an exact analysis-continuation route"
    )
    require(
      branch.stepAt(stepIndex).exists(_.step == route.sourceEvent.step) &&
        sourceRecord.parents.exists(_.id == route.resultProof.sourceLineOccurrenceOwner.id),
      "a passed-pawn result premise must bind its realizing occurrence and line owner"
    )
    PassedPawnProgressPremiseUse(
      PassedPawnProgressPremiseRole.Result,
      "passed_pawn_progress",
      route.resultProof.stableKey,
      (sourceRecord.ref.id :: resultPremiseIds(route.resultProof)).distinct.sorted,
      branch.branchId,
      branch.role,
      stepIndex,
      stepIndex
    )

  private def exactEvent(sourceRecord: EvidenceRecord): PassedPawnResultEventEvidence =
    sourceRecord.payload match
      case exact: PassedPawnResultEventEvidence => exact
      case _ =>
        throw IllegalArgumentException(
          "a passed-pawn-progress-realized-after-only-legal-reply premise needs its owning event record"
        )
