package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum PassedPawnResultBranchRole extends CausalBranchRole:
  case ExpectedResultRoute
  case LegalReply(replyMove: String, sourceProbeId: String)

  def stableKey: String =
    this match
      case ExpectedResultRoute => "expected_result_route"
      case LegalReply(replyMove, sourceProbeId) =>
        s"legal_reply:${EvidenceRef.normalizeMove(replyMove)}:$sourceProbeId"

private[chessjudgment] enum PassedPawnResultPremiseRole extends CausalPremiseRole:
  case ComparisonDemand
  case ExpectedDependency(index: Int)
  case ExpectedResult
  case ObservedDependency(replyMove: String, index: Int)
  case ObservedResult(replyMove: String)
  case FunctionalMatch(replyMove: String)

  def stableKey: String =
    this match
      case ComparisonDemand => "comparison_demand"
      case ExpectedDependency(index) => s"expected_dependency:$index"
      case ExpectedResult => "expected_result"
      case ObservedDependency(replyMove, index) =>
        s"observed_dependency:${EvidenceRef.normalizeMove(replyMove)}:$index"
      case ObservedResult(replyMove) =>
        s"observed_result:${EvidenceRef.normalizeMove(replyMove)}"
      case FunctionalMatch(replyMove) =>
        s"functional_match:${EvidenceRef.normalizeMove(replyMove)}"

private[chessjudgment] final case class PassedPawnResultPathManifest(
    replyWitness: PassedPawnReplyBranchWitness,
    realization: PassedPawnResultRealization,
    comparisonDemand: PassedPawnResultPremiseUse,
    expectedDependencies: List[PassedPawnResultPremiseUse],
    expectedResult: PassedPawnResultPremiseUse,
    observedDependencies: List[PassedPawnResultPremiseUse],
    observedResult: PassedPawnResultPremiseUse,
    functionalMatch: PassedPawnResultPremiseUse,
    replyInventory: PassedPawnResultClosedReplyInventoryBinding
) extends BoundedCausalContractManifest:
  val contractKind: BoundedCausalContractKind =
    BoundedCausalContractKind.PassedPawnResultUnderClosedReplies
  val premiseUses: List[CausalVerticalRelationPremiseUse] = Nil
  val absenceBindings: List[CausalClosedAbsenceBinding] = Nil
  override val supplementalPremiseUses: List[CausalSupplementalPremiseUse] =
    comparisonDemand ::
      (expectedDependencies ++ List(expectedResult) ++ observedDependencies ++ List(observedResult, functionalMatch))
  override val supplementalClosureBindings: List[CausalSupplementalClosureBinding] =
    List(replyInventory)

  val replyBranchId: String = observedResult.branchId

  def stableKey: String =
    List(
      contractKind.toString.toLowerCase,
      EvidenceRef.normalizeMove(replyWitness.line.rootMove),
      replyWitness.sourceProbeId,
      realization.matchKind.toString.toLowerCase,
      realization.resultRoute.stableKey,
      supplementalPremiseUses.map(_.stableKey).mkString("[", ",", "]"),
      replyInventory.stableKey
    ).mkString("|")

private[chessjudgment] final case class PassedPawnResultDependencyManifest(
    sourceRecord: EvidenceRecord,
    comparisonRecord: EvidenceRecord,
    replyInventoryRecord: EvidenceRecord,
    semanticIdentity: PassedPawnResultSemanticIdentity,
    proofSet: BoundedCausalProofSet,
    replyInventory: PassedPawnResultClosedReplyInventoryBinding
) extends BoundedCausalDependencyManifest:
  val contractKind: BoundedCausalContractKind =
    BoundedCausalContractKind.PassedPawnResultUnderClosedReplies

  def stableKey: String =
    List(
      BoundedCausalIdentity.evidenceRecordKey(sourceRecord),
      BoundedCausalIdentity.evidenceRecordKey(comparisonRecord),
      BoundedCausalIdentity.evidenceRecordKey(replyInventoryRecord),
      semanticIdentity.stableKey,
      proofSet.proposition.semanticId,
      proofSet.occurrence.occurrenceId,
      proofSet.paths.map(_.pathOccurrenceId).mkString("[", ",", "]"),
      replyInventory.stableKey
    ).mkString("|")

/** Opaque authority for one passed-pawn-result occurrence. The public payload may
  * project this proof, but it cannot certify itself without the exact
  * lower records and the complete dependency manifest retained here.
  */
private[chessjudgment] final class PassedPawnResultOccurrenceProof private[chessjudgment] (
    val event: PassedPawnResultEventEvidence,
    val assessment: PassedPawnResultReplyAssessment,
    val semanticIdentity: PassedPawnResultSemanticIdentity,
    val proofSet: BoundedCausalProofSet,
    val replyInventory: PassedPawnResultClosedReplyInventoryBinding,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: PassedPawnResultDependencyManifest,
    private val sourceRecord: EvidenceRecord,
    private val comparisonRecord: EvidenceRecord,
    private val replyInventoryRecord: EvidenceRecord
):
  private val exactParentSources = List(sourceRecord.ref, comparisonRecord.ref, replyInventoryRecord.ref)
  require(
    exactParentSources.map(_.id).distinct.size == exactParentSources.size,
    "a passed-pawn-result proof needs distinct event, comparison, and reply-inventory owners"
  )
  require(
    sourceRecord.payload == event &&
      event.retainsLineOccurrenceOwners(sourceRecord) &&
      event.exactRobustPublicResultAssessments.contains(assessment) &&
      semanticIdentity == PassedPawnResultSemanticIdentity.from(event, assessment) &&
      dependencyManifest.sourceRecord == sourceRecord &&
      dependencyManifest.comparisonRecord == comparisonRecord &&
      dependencyManifest.replyInventoryRecord == replyInventoryRecord &&
      dependencyManifest.semanticIdentity == semanticIdentity &&
      dependencyManifest.proofSet == proofSet &&
      dependencyManifest.replyInventory == replyInventory &&
      sourceRecord.parents.contains(replyInventoryRecord.ref) &&
      replyInventory.issuerEvidenceId == replyInventoryRecord.ref.id &&
      replyInventory.coverageEvidenceId == sourceRecord.ref.id &&
      BoundedCausalDependencyFingerprint.from(dependencyManifest) == dependency,
    "a passed-pawn-result authority must retain its exact lower records and complete manifest"
  )

  def parentSources: List[EvidenceRef] =
    exactParentSources.sortBy(_.id)

  def consumesExactDependencies(
      source: EvidenceRecord,
      comparison: EvidenceRecord,
      inventory: EvidenceRecord,
      resultAssessment: PassedPawnResultReplyAssessment
  ): Boolean =
    sourceRecord == source && comparisonRecord == comparison && replyInventoryRecord == inventory &&
      assessment == resultAssessment

  def proves(record: EvidenceRecord, payload: PassedPawnResultProofEvidence): Boolean =
    val comparisonIsExact = comparisonRecord.payload match
      case CandidateComparisonEvidence(comparison) =>
        comparison.kind == CandidateComparisonKind.PlayedVsBest &&
          Set(comparison.referenceLine, comparison.candidateLine)(event.rootLine)
      case _ => false
    record.ref.producer == EvidenceProducer.CausalProofProducer &&
      record.ref.layer == EvidenceLayer.CausalProof &&
      record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
      record.ref.position == event.rootTransition.from &&
      record.ref.line.contains(event.rootLine) &&
      record.ref.scope == event.rootTransition.role.scope &&
      record.parents == parentSources &&
      sourceRecord.ref.producer == EvidenceProducer.PassedPawnResultEventProducer &&
      sourceRecord.ref.layer == EvidenceLayer.PassedPawnResultEvent &&
      sourceRecord.ref.position == event.rootTransition.from &&
      sourceRecord.ref.line.contains(event.rootLine) &&
      sourceRecord.parents.contains(comparisonRecord.ref) &&
      sourceRecord.parents.contains(replyInventoryRecord.ref) &&
      event.retainsLineOccurrenceOwners(sourceRecord) &&
      event.rootTransitionIsCertified && comparisonIsExact &&
      event.branchSetComplete && event.branchCoverageComplete &&
      event.exactRobustPublicResultAssessments.contains(assessment) &&
      assessment.consequence.kind == TransitionConsequenceKind.PassedPawnProgress &&
      payload.eventSource == sourceRecord.ref &&
      payload.comparisonDemand == comparisonRecord.ref &&
      payload.event == event && payload.assessment == assessment &&
      payload.semanticIdentity == semanticIdentity && payload.proofSet == proofSet &&
      payload.closedReplyInventory == replyInventory &&
      payload.dependencyFingerprint == dependency.value &&
      payload.lowerPremiseIds == lowerPremiseIds &&
      payload.occurrenceProof.contains(this) && payload.hasCompleteProofPaths

  private val lowerPremiseIds: List[String] =
    (List(sourceRecord.ref.id, comparisonRecord.ref.id, replyInventoryRecord.ref.id) ++
      sourceRecord.parents.map(_.id) ++ comparisonRecord.parents.map(_.id)).distinct.sorted

private[chessjudgment] object PassedPawnResultProofDerivation:
  def derive(
      sourceRecord: EvidenceRecord,
      comparisonRecord: EvidenceRecord,
      replyInventoryRecord: EvidenceRecord,
      assessment: PassedPawnResultReplyAssessment
  ): Option[PassedPawnResultProofEvidence] =
    for
      event <- sourceRecord.payload match
        case exact: PassedPawnResultEventEvidence => Some(exact)
        case _                              => None
      comparison <- comparisonRecord.payload match
        case CandidateComparisonEvidence(exact) => Some(exact)
        case _                                  => None
      if comparison.kind == CandidateComparisonKind.PlayedVsBest
      if Set(comparison.referenceLine, comparison.candidateLine)(event.rootLine)
      if sourceRecord.parents.contains(comparisonRecord.ref)
      if event.exactRobustPublicResultAssessments.contains(assessment)
      if assessment.consequence.kind == TransitionConsequenceKind.PassedPawnProgress
      if event.branchSetComplete && event.branchCoverageComplete
      sourceBranch = PassedPawnResultCausalAuthority.expectedRoute(event, assessment)
      replyPairs <- exactReplyBranches(event)
      replyInventory = PassedPawnResultClosedReplyInventoryBinding.from(
        replyInventoryRecord,
        sourceRecord,
        event,
        replyPairs
      )
      manifests <- exactPathManifests(
        sourceRecord,
        comparisonRecord,
        event,
        assessment,
        sourceBranch,
        replyPairs,
        replyInventory
      )
      if manifests.nonEmpty
      proposition = PassedPawnResultCausalAuthority.proposition(event, assessment)
      occurrence = CausalOccurrenceIdentity.from(
        proposition,
        sourceBranch :: replyPairs.map(_._2)
      )
      paths = manifests.map(CausalProofPathOccurrence.from(proposition, _))
      proofSet = BoundedCausalProofSet.from(proposition, occurrence, paths)
      semanticIdentity = PassedPawnResultSemanticIdentity.from(event, assessment)
      dependencyManifest = PassedPawnResultDependencyManifest(
        sourceRecord,
        comparisonRecord,
        replyInventoryRecord,
        semanticIdentity,
        proofSet,
        replyInventory
      )
      dependency = BoundedCausalDependencyFingerprint.from(dependencyManifest)
      authority = new PassedPawnResultOccurrenceProof(
        event,
        assessment,
        semanticIdentity,
        proofSet,
        replyInventory,
        dependency,
        dependencyManifest,
        sourceRecord,
        comparisonRecord,
        replyInventoryRecord
      )
      result = PassedPawnResultProofEvidence(
        sourceRecord.ref,
        comparisonRecord.ref,
        event,
        assessment,
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

  private def exactReplyBranches(
      event: PassedPawnResultEventEvidence
  ): Option[List[(PassedPawnReplyBranchWitness, CausalBranchOccurrence)]] =
    val pairs = event.branchWitnesses.map { witness =>
      witness -> PassedPawnResultCausalAuthority.legalReply(event, witness)
    }.sortBy { case (witness, _) => EvidenceRef.normalizeMove(witness.line.rootMove) }
    Option.when(
      pairs.nonEmpty && pairs.map(_._1.line.rootMove).map(EvidenceRef.normalizeMove).distinct.size == pairs.size
    )(pairs)

  private def exactPathManifests(
      sourceRecord: EvidenceRecord,
      comparisonRecord: EvidenceRecord,
      event: PassedPawnResultEventEvidence,
      assessment: PassedPawnResultReplyAssessment,
      sourceBranch: CausalBranchOccurrence,
      replyPairs: List[(PassedPawnReplyBranchWitness, CausalBranchOccurrence)],
      replyInventory: PassedPawnResultClosedReplyInventoryBinding
  ): Option[List[PassedPawnResultPathManifest]] =
    val observationsByLine = assessment.observations.groupBy(_.line)
    val manifests = replyPairs.flatMap { case (witness, replyBranch) =>
      observationsByLine.get(witness.line) match
        case Some(observation :: Nil)
            if EvidenceRef.sameMove(observation.replyMove, witness.line.rootMove) &&
              observation.outcome == PassedPawnResultBranchOutcome.Realized && observation.realizations.nonEmpty =>
          observation.realizations.flatMap(realization =>
            manifestForRealization(
              sourceRecord,
              comparisonRecord,
              event,
              assessment,
              sourceBranch,
              witness,
              replyBranch,
              realization,
              replyInventory
            )
          )
        case _ => Nil
    }
    val coveredReplies = manifests.map(_.replyBranchId).toSet
    Option.when(
      manifests.nonEmpty && coveredReplies == replyPairs.map(_._2.branchId).toSet &&
        manifests.map(_.stableKey).distinct.size == manifests.size
    )(manifests.sortBy(_.stableKey))

  private def manifestForRealization(
      sourceRecord: EvidenceRecord,
      comparisonRecord: EvidenceRecord,
      event: PassedPawnResultEventEvidence,
      assessment: PassedPawnResultReplyAssessment,
      sourceBranch: CausalBranchOccurrence,
      witness: PassedPawnReplyBranchWitness,
      replyBranch: CausalBranchOccurrence,
      realization: PassedPawnResultRealization,
      replyInventory: PassedPawnResultClosedReplyInventoryBinding
  ): Option[PassedPawnResultPathManifest] =
    val observedDependencies = realization.resultRoute.causalPath
    for
      observedResultIndex <- exactStepIndex(replyBranch, realization.event.step)
      observedDependencyIndices <- exactDependencyIndices(replyBranch, observedDependencies)
      if assessment.causalPath.size + 1 == sourceBranch.steps.size
      expectedDependencies = assessment.causalPath.zipWithIndex.map { case (dependency, index) =>
        PassedPawnResultPremiseUse.dependency(
          dependency,
          assessment,
          sourceRecord,
          sourceBranch,
          index,
          index + 1
        )
      }
      expectedResult = PassedPawnResultPremiseUse.result(
        assessment,
        sourceRecord,
        sourceBranch,
        sourceBranch.steps.size - 1
      )
      observedPremises = observedDependencies.zip(observedDependencyIndices).zipWithIndex.map {
        case ((dependency, (fromIndex, toIndex)), index) =>
          PassedPawnResultPremiseUse.observedDependency(
            dependency,
            realization,
            sourceRecord,
            witness,
            replyBranch,
            fromIndex,
            toIndex
          )
      }
      observedResult = PassedPawnResultPremiseUse.observedResult(
        realization,
        sourceRecord,
        witness,
        replyBranch,
        observedResultIndex
      )
      functionalMatch = PassedPawnResultPremiseUse.functionalMatch(
        assessment,
        realization,
        sourceRecord,
        witness,
        sourceBranch,
        replyBranch,
        sourceBranch.steps.size - 1,
        observedResultIndex
      )
      demand = PassedPawnResultPremiseUse.comparisonDemand(
        comparisonRecord,
        sourceBranch,
        event,
        assessment
      )
    yield PassedPawnResultPathManifest(
      witness,
      realization,
      demand,
      expectedDependencies,
      expectedResult,
      observedPremises,
      observedResult,
      functionalMatch,
      replyInventory
    )

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

  private def exactStepIndex(
      branch: CausalBranchOccurrence,
      step: LineReplayStep
  ): Option[Int] =
    branch.steps.filter(_.step == step).map(_.index) match
      case exact :: Nil => Some(exact)
      case _            => None
