package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Sole graph producer for the four relation-backed causal families. Each
  * family orients exact roots from its own lower facts; only deterministic
  * graph ownership is shared here.
  */
private[chessjudgment] object RelationCausalProofAssembler:

  private[assembly] def fromDemand(
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): List[EvidenceRecord] =
    val records =
      uniqueCheckReply(allocator, demand) ++
        soleRecapturerRemoval(allocator, demand) ++
        vacatedGateCapture(allocator, demand) ++
        squareReleaseRoute(allocator, demand)
    require(
      records.map(_.ref.id).distinct.size == records.size,
      "one relation-causal evidence id may have only one graph owner"
    )
    records.sortBy(_.ref.id)

  private def uniqueCheckReply(
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): List[EvidenceRecord] =
    UniqueCheckReplyProofDemand.from(demand).flatMap { exactDemand =>
      familyRecords(
        family = BoundedCausalContractKind.UniqueCheckReplyDefenderDisplacementBeforeCapture,
        allocator = allocator,
        subject = exactDemand.subject
      )(
        UniqueCheckReplyDefenderDisplacementBeforeCaptureProof.certifyDemanded(
          exactDemand.subject,
          exactDemand.displacementBranch,
          exactDemand.immediateCaptureBranch,
          exactDemand.lower
        )
      )(
        semanticId = _.semantic.semanticId,
        occurrenceId = _.occurrence.occurrenceId,
        dependencyId = _.dependency.value,
        proofPathIds = _.occurrence.proofPaths.map(_.pathOccurrenceId),
        parentSources = _.proof.parentSources,
        payload = exact =>
          UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence(
            exact.semantic,
            exact.occurrence,
            exactDemand.subject.publicOccurrence,
            exact.dependency.value,
            exact.proof
          )
      )
    }

  private def soleRecapturerRemoval(
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): List[EvidenceRecord] =
    SoleRecapturerRemovalProofDemand.from(demand).flatMap { exactDemand =>
      familyRecords(
        family = BoundedCausalContractKind.SoleRecapturerRemovalBeforeTargetCapture,
        allocator = allocator,
        subject = exactDemand.subject
      )(
        SoleRecapturerRemovalBeforeTargetCaptureProof.certifyDemanded(
          exactDemand.subject,
          exactDemand.removalBranch,
          exactDemand.immediateCaptureBranch,
          exactDemand.lower
        )
      )(
        semanticId = _.semantic.semanticId,
        occurrenceId = _.occurrence.occurrenceId,
        dependencyId = _.dependency.value,
        proofPathIds = _.occurrence.proofPaths.map(_.pathOccurrenceId),
        parentSources = _.parentSources,
        payload = exact =>
          SoleRecapturerRemovalBeforeTargetCaptureEvidence(
            exact.semantic,
            exact.occurrence,
            exactDemand.subject.publicOccurrence,
            exact.dependency.value,
            exact
          )
      )
    }

  private def vacatedGateCapture(
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): List[EvidenceRecord] =
    VacatedGateCaptureProofDemand.from(demand).flatMap { exactDemand =>
      familyRecords(
        family = BoundedCausalContractKind.VacatedGateEnablesUnrecapturableSliderCapture,
        allocator = allocator,
        subject = exactDemand.subject
      )(
        VacatedGateEnablesUnrecapturableSliderCaptureProof.certifyDemanded(
          exactDemand.subject,
          exactDemand.vacatedGateBranch,
          exactDemand.retainedGateBranch,
          exactDemand.lowers
        )
      )(
        semanticId = _.semantic.semanticId,
        occurrenceId = _.occurrence.occurrenceId,
        dependencyId = _.dependency.value,
        proofPathIds = _.occurrence.proofPaths.map(_.pathOccurrenceId),
        parentSources = _.parentSources,
        payload = exact =>
          VacatedGateEnablesUnrecapturableSliderCaptureEvidence(
            exact.semantic,
            exact.occurrence,
            exactDemand.subject.publicOccurrence,
            exact.dependency.value,
            exact
          )
      )
    }

  private def squareReleaseRoute(
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): List[EvidenceRecord] =
    SquareReleaseRouteProofDemand.from(demand).flatMap { exactDemand =>
      familyRecords(
        family = BoundedCausalContractKind.SquareReleaseRoute,
        allocator = allocator,
        subject = exactDemand.subject
      )(
        SquareReleaseRouteProof.certifyDemanded(
          exactDemand.subject,
          exactDemand.releasedSquareBranch,
          exactDemand.retainedSquareBranch,
          exactDemand.lowers
        )
      )(
        semanticId = _.semantic.semanticId,
        occurrenceId = _.occurrence.occurrenceId,
        dependencyId = _.dependency.value,
        proofPathIds = _.occurrence.proofPaths.map(_.pathOccurrenceId),
        parentSources = _.parentSources,
        payload = exact =>
          SquareReleaseRouteEvidence(
            exact.semantic,
            exact.occurrence,
            exactDemand.subject.publicOccurrence,
            exact.dependency.value,
            exact
          )
      )
    }

  private def familyRecords[A](
      family: BoundedCausalContractKind,
      allocator: JudgmentProvenanceAllocator,
      subject: CertifiedRootOccurrence
  )(
      derive: => List[A]
  )(
      semanticId: A => String,
      occurrenceId: A => String,
      dependencyId: A => String,
      proofPathIds: A => List[String],
      parentSources: A => List[EvidenceRef],
      payload: A => EvidencePayload
  ): List[EvidenceRecord] =
    val familyNamespace = family.semanticNamespace
    val certified = derive
    require(
      certified.map(occurrenceId).distinct.size == certified.size,
      s"one exact $familyNamespace occurrence may be produced only once"
    )
    val derived = certified.map(exact => dependencyId(exact) -> exact)
    require(
      derived.map(_._1).distinct.size == derived.size,
      s"one exact $familyNamespace dependency may be produced only once"
    )
    derived.map { case (_, exact) =>
      val paths = proofPathIds(exact).sorted
      val parents = parentSources(exact)
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
            s"causal-proof:$familyNamespace:${semanticId(exact)}:${occurrenceId(exact)}:${dependencyId(exact)}:${paths.mkString(":")}:subject:${publicSubject.occurrenceId}"
          ),
          EvidenceProducer.CausalProofProducer,
          EvidenceLayer.CausalProof,
          publicSubject.start,
          Some(publicSubject.line),
          subject.transitionOwner.ref.scope,
          EvidenceConfidence.LegalReplayVerified
        ),
        payload(exact),
        parents
      )
    }.sortBy(_.ref.id)
