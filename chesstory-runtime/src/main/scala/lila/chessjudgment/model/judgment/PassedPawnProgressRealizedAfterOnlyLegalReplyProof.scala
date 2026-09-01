package lila.chessjudgment.model.judgment

private[chessjudgment] enum PassedPawnProgressBranchRole extends CausalBranchRole:
  case ActualResultRoute(replyMove: String, sourceOccurrenceId: String)

  def stableKey: String = this match
    case ActualResultRoute(replyMove, sourceOccurrenceId) =>
      s"actual_result_route:${EvidenceRef.normalizeMove(replyMove)}:$sourceOccurrenceId"

private[chessjudgment] enum PassedPawnProgressPremiseRole extends CausalPremiseRole:
  case ComparisonDemand
  case Dependency(index: Int)
  case Result

  def stableKey: String = this match
    case ComparisonDemand => "comparison_demand"
    case Dependency(index) => s"dependency:$index"
    case Result            => "result"

private[chessjudgment] enum PassedPawnProgressStateRole extends CausalStateRole:
  case Dependency(dependencyIndex: Int, stateIndex: Int)

  def stableKey: String = this match
    case Dependency(dependencyIndex, stateIndex) =>
      s"dependency:$dependencyIndex:state:$stateIndex"

private[chessjudgment] final case class PassedPawnProgressPathManifest(
    resultRoute: PassedPawnResultRoute,
    comparisonDemand: PassedPawnProgressPremiseUse,
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
    comparisonDemand :: (dependencies :+ result)
  override val supplementalClosureBindings: List[CausalSupplementalClosureBinding] =
    List(replyInventory)

  val actualBranchId: String = result.branchId

  def stableKey: String =
    List(
      contractKind.toString.toLowerCase,
      resultRoute.stableKey,
      supplementalPremiseUses.map(_.stableKey).mkString("[", ",", "]"),
      stateBindings.map(_.stableKey).mkString("[", ",", "]"),
      replyInventory.stableKey
    ).mkString("|")

private[chessjudgment] final case class PassedPawnProgressDependencyManifest(
    sourceRecord: EvidenceRecord,
    comparisonRecord: EvidenceRecord,
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
      BoundedCausalIdentity.evidenceRecordKey(sourceRecord),
      BoundedCausalIdentity.evidenceRecordKey(comparisonRecord),
      BoundedCausalIdentity.evidenceRecordKey(replyInventoryRecord),
      resultRoutes.map(_.stableKey).mkString("[", ",", "]"),
      semanticIdentity.stableKey,
      proofSet.proposition.semanticId,
      proofSet.occurrence.occurrenceId,
      proofSet.paths.map(_.pathOccurrenceId).mkString("[", ",", "]"),
      replyInventory.stableKey
    ).mkString("|")

/** Opaque authority for one actual passed-pawn result occurrence and all of
  * its independent graph-owned dependency routes.
  */
private[chessjudgment] final class PassedPawnProgressOccurrenceProof private[chessjudgment] (
    val event: PassedPawnResultEventEvidence,
    val resultRoutes: List[PassedPawnResultRoute],
    val semanticIdentity: PassedPawnProgressSemanticIdentity,
    val proofSet: BoundedCausalProofSet,
    val replyInventory: PassedPawnProgressClosedReplyInventoryBinding,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: PassedPawnProgressDependencyManifest,
    private val sourceRecord: EvidenceRecord,
    private val comparisonRecord: EvidenceRecord,
    private val replyInventoryRecord: EvidenceRecord
):
  private val exactParentSources = List(sourceRecord.ref, comparisonRecord.ref, replyInventoryRecord.ref)
  require(
    exactParentSources.map(_.id).distinct.size == exactParentSources.size,
    "a passed-pawn proof needs distinct event, comparison, and reply-inventory owners"
  )
  require(
    sourceRecord.payload == event && event.retainsLineOccurrenceOwners(sourceRecord) &&
      resultRoutes.forall(event.exactOnlyReplyResultRoutes.contains) &&
      semanticIdentity == PassedPawnProgressRealizedAfterOnlyLegalReplyAuthority.exactSemanticIdentity(event, resultRoutes) &&
      dependencyManifest.sourceRecord == sourceRecord &&
      dependencyManifest.comparisonRecord == comparisonRecord &&
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
    List(sourceRecord, comparisonRecord, replyInventoryRecord)

  def consumesExactDependencies(
      source: EvidenceRecord,
      comparison: EvidenceRecord,
      inventory: EvidenceRecord,
      routes: List[PassedPawnResultRoute]
  ): Boolean =
    sourceRecord == source && comparisonRecord == comparison && replyInventoryRecord == inventory &&
      resultRoutes == routes

  def proves(record: EvidenceRecord, payload: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence): Boolean =
    val comparisonIsExact = comparisonRecord.payload match
      case CandidateComparisonEvidence(comparison) =>
        comparison.kind == CandidateComparisonKind.PlayedVsBest && comparison.candidateLine == event.rootLine
      case _ => false
    record.ref.producer == EvidenceProducer.CausalProofProducer &&
      record.ref.layer == EvidenceLayer.CausalProof &&
      record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
      record.ref.position == event.rootTransition.from && record.ref.line.contains(event.rootLine) &&
      record.ref.scope == event.rootTransition.role.scope && record.parents == parentSources &&
      sourceRecord.ref.producer == EvidenceProducer.PassedPawnResultEventProducer &&
      sourceRecord.ref.layer == EvidenceLayer.PassedPawnResultEvent &&
      sourceRecord.ref.position == event.rootTransition.from && sourceRecord.ref.line.contains(event.rootLine) &&
      sourceRecord.parents.contains(comparisonRecord.ref) && sourceRecord.parents.contains(replyInventoryRecord.ref) &&
      event.retainsLineOccurrenceOwners(sourceRecord) && event.rootTransitionIsCertified && comparisonIsExact &&
      event.onlyLegalReplyRealized && resultRoutes.forall(event.exactOnlyReplyResultRoutes.contains) &&
      payload.eventSource == sourceRecord.ref && payload.comparisonDemand == comparisonRecord.ref &&
      payload.event == event && payload.resultRoutes == resultRoutes && payload.semanticIdentity == semanticIdentity &&
      payload.proofSet == proofSet && payload.closedReplyInventory == replyInventory &&
      payload.dependencyFingerprint == dependency.value && payload.lowerPremiseIds == lowerPremiseIds &&
      payload.occurrenceProof.contains(this) && payload.hasCompleteProofPaths

  private val lowerPremiseIds: List[String] =
    (List(sourceRecord.ref.id, comparisonRecord.ref.id, replyInventoryRecord.ref.id) ++
      sourceRecord.parents.map(_.id) ++ comparisonRecord.parents.map(_.id)).distinct.sorted

private[chessjudgment] object PassedPawnProgressRealizedAfterOnlyLegalReplyProofDerivation:
  def derive(
      sourceRecord: EvidenceRecord,
      comparisonRecord: EvidenceRecord,
      replyInventoryRecord: EvidenceRecord,
      routes: List[PassedPawnResultRoute]
  ): Option[PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence] =
    for
      event <- sourceRecord.payload match
        case exact: PassedPawnResultEventEvidence => Some(exact)
        case _                                    => None
      comparison <- comparisonRecord.payload match
        case CandidateComparisonEvidence(exact) => Some(exact)
        case _                                  => None
      if comparison.kind == CandidateComparisonKind.PlayedVsBest && comparison.candidateLine == event.rootLine
      if sourceRecord.parents.contains(comparisonRecord.ref)
      canonicalRoutes = routes.sortBy(_.stableKey)
      if canonicalRoutes == routes && canonicalRoutes.nonEmpty && canonicalRoutes.forall(event.exactOnlyReplyResultRoutes.contains)
      semanticIdentity = PassedPawnProgressRealizedAfterOnlyLegalReplyAuthority.exactSemanticIdentity(event, canonicalRoutes)
      actualBranch = PassedPawnProgressRealizedAfterOnlyLegalReplyAuthority.actualRoute(event, canonicalRoutes)
      replyInventory = PassedPawnProgressClosedReplyInventoryBinding.from(
        replyInventoryRecord,
        sourceRecord,
        event,
        canonicalRoutes,
        actualBranch
      )
      manifests <- exactPathManifests(
        sourceRecord,
        comparisonRecord,
        event,
        canonicalRoutes,
        actualBranch,
        replyInventory
      )
      proposition = PassedPawnProgressRealizedAfterOnlyLegalReplyAuthority.proposition(event, canonicalRoutes)
      occurrence = CausalOccurrenceIdentity.from(proposition, List(actualBranch))
      paths = manifests.map(CausalProofPathOccurrence.from(proposition, _))
      proofSet = BoundedCausalProofSet.from(proposition, occurrence, paths)
      dependencyManifest = PassedPawnProgressDependencyManifest(
        sourceRecord,
        comparisonRecord,
        replyInventoryRecord,
        canonicalRoutes,
        semanticIdentity,
        proofSet,
        replyInventory
      )
      dependency = BoundedCausalDependencyFingerprint.from(dependencyManifest)
      authority = new PassedPawnProgressOccurrenceProof(
        event,
        canonicalRoutes,
        semanticIdentity,
        proofSet,
        replyInventory,
        dependency,
        dependencyManifest,
        sourceRecord,
        comparisonRecord,
        replyInventoryRecord
      )
      result = PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence(
        sourceRecord.ref,
        comparisonRecord.ref,
        event,
        canonicalRoutes,
        semanticIdentity,
        proofSet,
        replyInventory,
        dependency.value,
        (List(sourceRecord.ref.id, comparisonRecord.ref.id, replyInventoryRecord.ref.id) ++
          sourceRecord.parents.map(_.id) ++ comparisonRecord.parents.map(_.id)).distinct.sorted,
        Some(authority)
      )
      if result.hasCompleteProofPaths
    yield result

  private def exactPathManifests(
      sourceRecord: EvidenceRecord,
      comparisonRecord: EvidenceRecord,
      event: PassedPawnResultEventEvidence,
      routes: List[PassedPawnResultRoute],
      actualBranch: CausalBranchOccurrence,
      replyInventory: PassedPawnProgressClosedReplyInventoryBinding
  ): Option[List[PassedPawnProgressPathManifest]] =
    val manifests = routes.flatMap(route =>
      manifestForRoute(sourceRecord, comparisonRecord, event, routes, actualBranch, route, replyInventory)
    )
    Option.when(
      manifests.size == routes.size && manifests.map(_.resultRoute.stableKey) == routes.map(_.stableKey) &&
        manifests.map(_.stableKey).distinct.size == manifests.size &&
        manifests.forall(_.actualBranchId == actualBranch.branchId)
    )(manifests)

  private def manifestForRoute(
      sourceRecord: EvidenceRecord,
      comparisonRecord: EvidenceRecord,
      event: PassedPawnResultEventEvidence,
      routes: List[PassedPawnResultRoute],
      actualBranch: CausalBranchOccurrence,
      route: PassedPawnResultRoute,
      replyInventory: PassedPawnProgressClosedReplyInventoryBinding
  ): Option[PassedPawnProgressPathManifest] =
    for
      resultIndex <- exactStepIndex(actualBranch, route.sourceEvent.step)
      dependencyIndices <- exactDependencyIndices(actualBranch, route.causalPath)
      stateGroups <- exactDependencyStateBindings(actualBranch, route.causalPath)
      dependencies = route.causalPath.zip(dependencyIndices).zip(stateGroups).map {
        case ((dependency, (fromIndex, toIndex)), stateBindings) =>
          PassedPawnProgressPremiseUse.dependency(
            dependency,
            route,
            routes,
            sourceRecord,
            actualBranch,
            fromIndex,
            toIndex,
            stateBindings
          )
      }
      result = PassedPawnProgressPremiseUse.result(route, routes, sourceRecord, actualBranch, resultIndex)
      demand = PassedPawnProgressPremiseUse.comparisonDemand(comparisonRecord, actualBranch, event, routes)
    yield PassedPawnProgressPathManifest(
      route,
      demand,
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
