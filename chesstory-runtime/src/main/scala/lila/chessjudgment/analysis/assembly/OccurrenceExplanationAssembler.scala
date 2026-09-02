package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Sole producer for occurrence-demanded typed explanations and their Cause
  * records. It is called only after an explicit request resolves to the
  * graph-owned subject occurrence.
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

  def enrichCauses(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): JudgmentAssemblyContext =
    val subject = demand.subject.publicOccurrence
    val proofRecords = context.evidenceGraph.records.filter(record =>
      context.evidenceGraph.proofEligible(record) &&
        DirectCauseChannel.fromTypedProofRecord(record).exists(
          OccurrenceExplanationCause.proofOwnsSubject(_, subject)
        )
    )
    val causes = proofRecords.map { proofRecord =>
      val channel = DirectCauseChannel.fromTypedProofRecord(proofRecord).getOrElse(
        throw IllegalStateException("a demanded proof record lost its typed proof channel")
      )
      val cause = OccurrenceExplanationCause(subject, proofRecord.ref, channel.typedProofIdentity)
      val causeOccurrenceKey = allocator.exactKey(List(
        subject.occurrenceId,
        channel.typedProofIdentity.family.semanticNamespace,
        channel.typedProofIdentity.semanticId,
        channel.typedProofIdentity.occurrenceId,
        channel.typedProofIdentity.dependencyFingerprint,
        channel.typedProofIdentity.proofPathOccurrenceIds.mkString("[", ",", "]")
      ))
      val transitionParent = demand.subject.transitionOwner.ref
      require(
        transitionParent.id == subject.transitionEvidenceId && transitionParent.id != proofRecord.ref.id,
        "an occurrence Cause needs distinct subject-transition and proof parents"
      )
      EvidenceRecord(
        EvidenceRef(
          id = allocator.evidenceId(
            s"occurrence-explanation:$causeOccurrenceKey"
          ),
          producer = EvidenceProducer.OccurrenceExplanationProducer,
          layer = EvidenceLayer.OccurrenceExplanation,
          position = subject.start,
          line = Some(subject.line),
          scope = transitionParent.scope,
          confidence = EvidenceConfidence.LegalReplayVerified
        ),
        OccurrenceExplanationCauseEvidence(cause),
        List(transitionParent, proofRecord.ref)
      )
    }
    require(
      causes.map(_.ref.id).distinct.size == causes.size,
      "one demanded typed proof may own only one occurrence Cause"
    )
    context.withEvidence(causes)

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
