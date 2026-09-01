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
      captureExclusionRecords(allocator, demand) ++ relationRecords
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
    val familyDemands = CaptureExclusionProofDemand.from(demand)
    val derived = familyDemands.flatMap { exactDemand =>
      CaptureExclusionMoveOrderProof
        .certifyDemanded(
          exactDemand.subject,
          exactDemand.vacatingBranch,
          exactDemand.immediateBranch,
          exactDemand.occurrences
        )
        .map(exact => exact.dependency.value -> exact)
    }
    require(
      derived.map(_._1).distinct.size == derived.size,
      "one capture-exclusion dependency may be produced only once"
    )

    derived
      .map { case (_, exact) =>
        val subject = exact.subject.publicOccurrence
        val proofPathIds = exact.occurrence.proofPaths.map(_.pathOccurrenceId).sorted
        require(
          proofPathIds.nonEmpty && proofPathIds.distinct.size == proofPathIds.size,
          "a capture-exclusion record must retain every independent proof path"
        )
        EvidenceRecord(
          EvidenceRef(
            allocator.evidenceId(
              s"causal-proof:${BoundedCausalContractKind.CaptureExclusionMoveOrder.semanticNamespace}:${exact.semantic.semanticId}:${exact.occurrence.occurrenceId}:${exact.dependency.value}:${proofPathIds.mkString(":")}:subject:${subject.occurrenceId}"
            ),
            EvidenceProducer.CausalProofProducer,
            EvidenceLayer.CausalProof,
            subject.start,
            Some(subject.line),
            exact.subject.transitionOwner.ref.scope,
            EvidenceConfidence.LegalReplayVerified
          ),
          CaptureExclusionMoveOrderEvidence(
            exact.semantic,
            exact.occurrence,
            subject,
            exact.dependency.value,
            exact
          ),
          exact.parentSources
        )
      }
      .sortBy(_.ref.id)
