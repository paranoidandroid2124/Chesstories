package lila.chessjudgment.model.judgment

private[chessjudgment] enum PassedPawnProgressBranchRole extends CausalBranchRole:
  case ObservedRootWithAnalyzedContinuation(replyMove: String, sourceOccurrenceId: String)

  def stableKey: String = this match
    case ObservedRootWithAnalyzedContinuation(replyMove, sourceOccurrenceId) =>
      s"observed_root_with_analyzed_continuation:${EvidenceRef.normalizeMove(replyMove)}:$sourceOccurrenceId"

private[chessjudgment] enum PassedPawnProgressPremiseRole extends CausalPremiseRole:
  case Dependency(index: Int)
  case Result

  def stableKey: String = this match
    case Dependency(index) => s"dependency:$index"
    case Result            => "result"

private[chessjudgment] enum PassedPawnProgressStateRole extends CausalStateRole:
  case Dependency(dependencyIndex: Int, stateIndex: Int)

  def stableKey: String = this match
    case Dependency(dependencyIndex, stateIndex) =>
      s"dependency:$dependencyIndex:state:$stateIndex"

private[chessjudgment] final case class PassedPawnProgressPathManifest(
    resultRoute: PassedPawnResultRoute,
    dependencies: List[PassedPawnProgressPremiseUse],
    result: PassedPawnProgressPremiseUse,
    states: List[CausalClosedStateBinding],
    replyInventory: PassedPawnProgressClosedReplyInventoryBinding
) extends BoundedCausalContractManifest:
  val contractKind: BoundedCausalContractKind =
    BoundedCausalContractKind.PassedPawnProgressRealizedAfterOnlyLegalReply
  val premiseUses: List[CausalVerticalRelationPremiseUse] = Nil
  val absenceBindings: List[CausalClosedAbsenceBinding] = Nil
  override val stateBindings: List[CausalClosedStateBinding] = states
  override val supplementalPremiseUses: List[CausalSupplementalPremiseUse] =
    dependencies :+ result
  override val supplementalClosureBindings: List[CausalSupplementalClosureBinding] =
    List(replyInventory)

  val analysisContinuationBranchId: String = result.branchId

  def stableKey: String =
    List(
      contractKind.semanticNamespace,
      resultRoute.stableKey,
      supplementalPremiseUses.map(_.stableKey).mkString("[", ",", "]"),
      stateBindings.map(_.stableKey).mkString("[", ",", "]"),
      replyInventory.stableKey
    ).mkString("|")

private[chessjudgment] final case class PassedPawnProgressDependencyManifest(
    subjectOccurrence: ExplanationSubjectOccurrence,
    sourceRecord: EvidenceRecord,
    replyInventoryRecord: EvidenceRecord,
    resultRoutes: List[PassedPawnResultRoute],
    semanticIdentity: PassedPawnProgressSemanticIdentity,
    proofSet: BoundedCausalProofSet,
    replyInventory: PassedPawnProgressClosedReplyInventoryBinding
) extends BoundedCausalDependencyManifest:
  val contractKind: BoundedCausalContractKind =
    BoundedCausalContractKind.PassedPawnProgressRealizedAfterOnlyLegalReply

  def stableKey: String =
    List(
      subjectOccurrence.stableKey,
      BoundedCausalIdentity.evidenceRecordKey(sourceRecord),
      BoundedCausalIdentity.evidenceRecordKey(replyInventoryRecord),
      resultRoutes.map(_.stableKey).mkString("[", ",", "]"),
      semanticIdentity.stableKey,
      proofSet.proposition.semanticId,
      proofSet.occurrence.occurrenceId,
      proofSet.paths.map(_.pathOccurrenceId).mkString("[", ",", "]"),
      replyInventory.stableKey
    ).mkString("|")

/** Opaque authority for one certified analysis-continuation passed-pawn result
  * occurrence and all of its independent graph-owned dependency routes.
  */
private[chessjudgment] final class PassedPawnProgressOccurrenceProof private[chessjudgment] (
    val subject: CertifiedRootOccurrence,
    val event: PassedPawnResultEventEvidence,
    val resultRoutes: List[PassedPawnResultRoute],
    val semanticIdentity: PassedPawnProgressSemanticIdentity,
    val proofSet: BoundedCausalProofSet,
    val replyInventory: PassedPawnProgressClosedReplyInventoryBinding,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: PassedPawnProgressDependencyManifest,
    private val sourceRecord: EvidenceRecord,
    private val replyInventoryRecord: EvidenceRecord
):
  private val exactParentSources =
    List(sourceRecord.ref, replyInventoryRecord.ref) ++ subject.parentSources
  require(
    exactParentSources.map(_.id).distinct.size == exactParentSources.size,
    "a passed-pawn proof needs distinct event, reply-inventory, and subject owners"
  )
  require(
    subject.isObserved && subject.remainsCertified && event.subjectOccurrence == subject.publicOccurrence &&
      sourceRecord.payload == event && event.retainsLineOccurrenceOwners(sourceRecord) &&
      resultRoutes.forall(event.exactOnlyReplyResultRoutes.contains) &&
      semanticIdentity == PassedPawnProgressRealizedAfterOnlyLegalReplyAuthority.exactSemanticIdentity(event, resultRoutes) &&
      dependencyManifest.subjectOccurrence == subject.publicOccurrence &&
      dependencyManifest.sourceRecord == sourceRecord &&
      dependencyManifest.replyInventoryRecord == replyInventoryRecord &&
      dependencyManifest.resultRoutes == resultRoutes &&
      dependencyManifest.semanticIdentity == semanticIdentity &&
      dependencyManifest.proofSet == proofSet && dependencyManifest.replyInventory == replyInventory &&
      sourceRecord.parents.contains(replyInventoryRecord.ref) &&
      replyInventory.issuerEvidenceId == replyInventoryRecord.ref.id &&
      replyInventory.eventEvidenceId == sourceRecord.ref.id &&
      BoundedCausalDependencyFingerprint.from(dependencyManifest) == dependency,
    "a passed-pawn authority must retain its exact lower records and complete route manifest"
  )

  def parentSources: List[EvidenceRef] = exactParentSources.sortBy(_.id)

  private[chessjudgment] def lowerIssuerRecords: List[EvidenceRecord] =
    List(sourceRecord, replyInventoryRecord, subject.lineOwner, subject.transitionOwner)

  def consumesExactDependencies(
      requestedSubject: CertifiedRootOccurrence,
      source: EvidenceRecord,
      inventory: EvidenceRecord,
      routes: List[PassedPawnResultRoute]
  ): Boolean =
    subject == requestedSubject && sourceRecord == source && replyInventoryRecord == inventory && resultRoutes == routes

  def proves(record: EvidenceRecord, payload: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence): Boolean =
    record.ref.producer == EvidenceProducer.CausalProofProducer &&
      record.ref.layer == EvidenceLayer.CausalProof &&
      record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
      record.ref.position == subject.transition.from && record.ref.line.contains(subject.line) &&
      record.ref.scope == subject.transition.role.scope && record.parents == parentSources &&
      sourceRecord.ref.producer == EvidenceProducer.PassedPawnResultEventProducer &&
      sourceRecord.ref.layer == EvidenceLayer.PassedPawnResultEvent &&
      sourceRecord.ref.position == event.rootTransition.from && sourceRecord.ref.line.contains(event.rootLine) &&
      sourceRecord.parents.contains(replyInventoryRecord.ref) &&
      event.subjectOccurrence == subject.publicOccurrence && event.retainsLineOccurrenceOwners(sourceRecord) &&
      event.rootTransitionIsCertified &&
      event.onlyLegalReplyRealized && resultRoutes.forall(event.exactOnlyReplyResultRoutes.contains) &&
      payload.eventSource == sourceRecord.ref && payload.event == event &&
      payload.subjectOccurrence == subject.publicOccurrence &&
      payload.resultRoutes == resultRoutes && payload.semanticIdentity == semanticIdentity &&
      payload.proofSet == proofSet && payload.closedReplyInventory == replyInventory &&
      payload.dependencyFingerprint == dependency.value && payload.lowerPremiseIds == lowerPremiseIds &&
      payload.occurrenceProof == this && payload.hasCompleteProofPaths

  private val lowerPremiseIds: List[String] =
    (exactParentSources.map(_.id) ++ sourceRecord.parents.map(_.id)).distinct.sorted

private[chessjudgment] object PassedPawnProgressRealizedAfterOnlyLegalReplyProofDerivation:
  def derive(
      subject: CertifiedRootOccurrence,
      sourceRecord: EvidenceRecord,
      replyInventoryRecord: EvidenceRecord,
      routes: List[PassedPawnResultRoute]
  ): Option[PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence] =
    for
      event <- sourceRecord.payload match
        case exact: PassedPawnResultEventEvidence => Some(exact)
        case _                                    => None
      if subject.isObserved && subject.remainsCertified && event.subjectOccurrence == subject.publicOccurrence
      canonicalRoutes = routes.sortBy(_.stableKey)
      if canonicalRoutes == routes && canonicalRoutes.nonEmpty && canonicalRoutes.forall(event.exactOnlyReplyResultRoutes.contains)
      semanticIdentity = PassedPawnProgressRealizedAfterOnlyLegalReplyAuthority.exactSemanticIdentity(event, canonicalRoutes)
      analysisContinuation =
        PassedPawnProgressRealizedAfterOnlyLegalReplyAuthority.analysisContinuationRoute(event, canonicalRoutes)
      replyInventory = PassedPawnProgressClosedReplyInventoryBinding.from(
        replyInventoryRecord,
        sourceRecord,
        event,
        canonicalRoutes,
        analysisContinuation
      )
      manifests <- exactPathManifests(
        sourceRecord,
        event,
        canonicalRoutes,
        analysisContinuation,
        replyInventory
      )
      proposition = PassedPawnProgressRealizedAfterOnlyLegalReplyAuthority.proposition(event, canonicalRoutes)
      occurrence = CausalOccurrenceIdentity.from(proposition, List(analysisContinuation))
      paths = manifests.map(CausalProofPathOccurrence.from(proposition, _))
      proofSet = BoundedCausalProofSet.from(proposition, occurrence, paths)
      dependencyManifest = PassedPawnProgressDependencyManifest(
        subject.publicOccurrence,
        sourceRecord,
        replyInventoryRecord,
        canonicalRoutes,
        semanticIdentity,
        proofSet,
        replyInventory
      )
      dependency = BoundedCausalDependencyFingerprint.from(dependencyManifest)
      authority = new PassedPawnProgressOccurrenceProof(
        subject,
        event,
        canonicalRoutes,
        semanticIdentity,
        proofSet,
        replyInventory,
        dependency,
        dependencyManifest,
        sourceRecord,
        replyInventoryRecord
      )
      result = PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence(
        sourceRecord.ref,
        event,
        canonicalRoutes,
        semanticIdentity,
        proofSet,
        replyInventory,
        dependency.value,
        ((List(sourceRecord.ref, replyInventoryRecord.ref) ++ subject.parentSources).map(_.id) ++
          sourceRecord.parents.map(_.id)).distinct.sorted,
        authority
      )
      if result.hasCompleteProofPaths
    yield result

  private def exactPathManifests(
      sourceRecord: EvidenceRecord,
      event: PassedPawnResultEventEvidence,
      routes: List[PassedPawnResultRoute],
      analysisContinuation: CausalBranchOccurrence,
      replyInventory: PassedPawnProgressClosedReplyInventoryBinding
  ): Option[List[PassedPawnProgressPathManifest]] =
    val manifests = routes.flatMap(route =>
      manifestForRoute(sourceRecord, event, routes, analysisContinuation, route, replyInventory)
    )
    Option.when(
      manifests.size == routes.size && manifests.map(_.resultRoute.stableKey) == routes.map(_.stableKey) &&
        manifests.map(_.stableKey).distinct.size == manifests.size &&
        manifests.forall(_.analysisContinuationBranchId == analysisContinuation.branchId)
    )(manifests)

  private def manifestForRoute(
      sourceRecord: EvidenceRecord,
      event: PassedPawnResultEventEvidence,
      routes: List[PassedPawnResultRoute],
      analysisContinuation: CausalBranchOccurrence,
      route: PassedPawnResultRoute,
      replyInventory: PassedPawnProgressClosedReplyInventoryBinding
  ): Option[PassedPawnProgressPathManifest] =
    for
      resultIndex <- exactStepIndex(analysisContinuation, route.sourceEvent.step)
      dependencyIndices <- exactDependencyIndices(analysisContinuation, route.causalPath)
      stateGroups <- exactDependencyStateBindings(analysisContinuation, route.causalPath)
      dependencies = route.causalPath.zip(dependencyIndices).zip(stateGroups).map {
        case ((dependency, (fromIndex, toIndex)), stateBindings) =>
          PassedPawnProgressPremiseUse.dependency(
            dependency,
            route,
            routes,
            sourceRecord,
            analysisContinuation,
            fromIndex,
            toIndex,
            stateBindings
          )
      }
      result = PassedPawnProgressPremiseUse.result(route, routes, sourceRecord, analysisContinuation, resultIndex)
    yield PassedPawnProgressPathManifest(
      route,
      dependencies,
      result,
      stateGroups.flatten,
      replyInventory
    )

  private def exactDependencyStateBindings(
      branch: CausalBranchOccurrence,
      dependencies: List[PassedPawnResultDependency]
  ): Option[List[List[CausalClosedStateBinding]]] =
    val groups = dependencies.zipWithIndex.map { case (dependency, dependencyIndex) =>
      val bindings = dependency.positionStateAuthorities.zipWithIndex.flatMap { case (authority, stateIndex) =>
        exactStepIndex(branch, authority.step).map(stepIndex =>
          CausalClosedStateBinding.afterStep(
            PassedPawnProgressStateRole.Dependency(dependencyIndex, stateIndex),
            authority,
            branch,
            stepIndex
          )
        )
      }
      Option.when(bindings.size == dependency.positionStateAuthorities.size)(bindings)
    }
    Option.when(groups.forall(_.nonEmpty))(groups.flatten)

  private def exactDependencyIndices(
      branch: CausalBranchOccurrence,
      dependencies: List[PassedPawnResultDependency]
  ): Option[List[(Int, Int)]] =
    val indices = dependencies.map(dependency =>
      for
        from <- exactStepIndex(branch, dependency.from.step)
        to <- exactStepIndex(branch, dependency.to.step)
        if from < to
      yield from -> to
    )
    Option.when(indices.forall(_.nonEmpty))(indices.flatten)

  private def exactStepIndex(branch: CausalBranchOccurrence, step: LineReplayStep): Option[Int] =
    branch.steps.filter(_.step == step).map(_.index) match
      case exact :: Nil => Some(exact)
      case _            => None
