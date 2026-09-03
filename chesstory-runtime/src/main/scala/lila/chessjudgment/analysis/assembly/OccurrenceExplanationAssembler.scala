package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Produces the currently supported exact local relation proofs after an
  * explicit request resolves to a graph-owned subject occurrence.
  */
private[chessjudgment] object OccurrenceExplanationAssembler:

  def enrichProofs(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): JudgmentAssemblyContext =
    val relationRecords = RelationCausalProofAssembler.fromDemand(allocator, demand)
    val directContext = context.withEvidence(
      captureExclusionRecords(allocator, demand) ++
        relocationEnablesRecaptureRecords(allocator, demand) ++ relationRecords
    )
    val passedPawnDemand = PassedPawnResultEventAssembler.changedDependencyDemand(
      directContext,
      demand
    )
    val eventContext = directContext.withEvidence(
      passedPawnDemand.toList.flatMap(
        PassedPawnResultEventAssembler.fromDemand(allocator, _)
      )
    )
    eventContext.withEvidence(
      passedPawnDemand.toList.flatMap(
        PassedPawnProgressRealizedAfterOnlyLegalReplyProofAssembler.fromDemand(
          eventContext,
          allocator,
          _
        )
      )
    )

  private def captureExclusionRecords(
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): List[EvidenceRecord] =
    val certified = CaptureExclusionProofDemand.from(demand).flatMap { exactDemand =>
      CaptureExclusionMoveOrderProof.certifyDemanded(
        exactDemand.subject,
        exactDemand.vacatingBranch,
        exactDemand.immediateBranch,
        exactDemand.occurrences
      )
    }
    occurrenceProofRecords(
      family = BoundedCausalContractKind.CaptureExclusionMoveOrder,
      allocator = allocator,
      subject = demand.subject
    )(certified)(
      semanticId = _.semantic.semanticId,
      occurrenceId = _.occurrence.occurrenceId,
      dependencyId = _.dependency.value,
      proofPathIds = _.occurrence.proofPaths.map(_.pathOccurrenceId),
      parentSources = _.parentSources,
      payload = exact =>
        CaptureExclusionMoveOrderEvidence(
          exact.semantic,
          exact.occurrence,
          demand.subject.publicOccurrence,
          exact.dependency.value,
          exact
        )
    )

  private def relocationEnablesRecaptureRecords(
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): List[EvidenceRecord] =
    val certified = RelocationEnablesRecaptureProofDemand.from(demand).flatMap { exactDemand =>
      RelocationEnablesRecaptureProof.certifyDemanded(
        exactDemand.subject,
        exactDemand.relocatedBranch,
        exactDemand.retainedBranch,
        exactDemand.occurrences
      )
    }
    occurrenceProofRecords(
      family = BoundedCausalContractKind.RelocationEnablesRecapture,
      allocator = allocator,
      subject = demand.subject
    )(certified)(
      semanticId = _.semantic.semanticId,
      occurrenceId = _.occurrence.occurrenceId,
      dependencyId = _.dependency.value,
      proofPathIds = _.occurrence.proofPaths.map(_.pathOccurrenceId),
      parentSources = _.parentSources,
      payload = exact =>
        RelocationEnablesRecaptureEvidence(
          exact.semantic,
          exact.occurrence,
          demand.subject.publicOccurrence,
          exact.dependency.value,
          exact
        )
    )

  private[assembly] def occurrenceProofRecords[A](
      family: BoundedCausalContractKind,
      allocator: JudgmentProvenanceAllocator,
      subject: CertifiedRootOccurrence
  )(
      certified: => List[A]
  )(
      semanticId: A => String,
      occurrenceId: A => String,
      dependencyId: A => String,
      proofPathIds: A => List[String],
      parentSources: A => List[EvidenceRef],
      payload: A => EvidencePayload
  ): List[EvidenceRecord] =
    val familyNamespace = family.semanticNamespace
    val exact = certified
    require(
      exact.map(occurrenceId).distinct.size == exact.size,
      s"one exact $familyNamespace occurrence may be produced only once"
    )
    require(
      exact.map(dependencyId).distinct.size == exact.size,
      s"one exact $familyNamespace dependency may be produced only once"
    )
    exact.map { proof =>
      val paths = proofPathIds(proof).sorted
      val parents = parentSources(proof)
      require(
        paths.nonEmpty && paths.distinct.size == paths.size,
        s"a $familyNamespace record must retain every independent proof path"
      )
      require(
        parents.nonEmpty && parents == parents.sortBy(_.id) &&
          parents.map(_.id).distinct.size == parents.size,
        s"a $familyNamespace record needs canonical distinct lower owners"
      )
      val publicSubject = subject.publicOccurrence
      EvidenceRecord(
        EvidenceRef(
          allocator.evidenceId(
            s"causal-proof:$familyNamespace:${semanticId(proof)}:${occurrenceId(proof)}:${dependencyId(proof)}:${paths.mkString(":")}:subject:${publicSubject.occurrenceId}"
          ),
          EvidenceProducer.CausalProofProducer,
          EvidenceLayer.CausalProof,
          publicSubject.start,
          Some(publicSubject.line),
          subject.transitionOwner.ref.scope,
          EvidenceConfidence.LegalReplayVerified
        ),
        payload(proof),
        parents
      )
    }.sortBy(_.ref.id)
