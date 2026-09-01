package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Test access to the production demand dispatcher. Tests outside the
  * assembly package must not require a second production entry point.
  */
private[chessjudgment] object RelationCausalProofTestDispatch:

  def forced(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    exactDemand(context).flatMap(demand =>
      RelationCausalProofAssembler
        .fromDemand(allocator, demand)
        .collect {
          case record @ EvidenceRecord(
                _,
                _: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence,
                _
              ) => record
        }
    )

  def defense(
      context: JudgmentAssemblyContext
  ): List[CertifiedSoleRecapturerRemovalBeforeTargetCapture] =
    exactDemand(context).flatMap(demand =>
      RelationCausalProofAssembler
        .fromDemand(
          JudgmentProvenanceAllocator.forInput(context.input),
          demand
        )
        .collect {
          case EvidenceRecord(_, exact: SoleRecapturerRemovalBeforeTargetCaptureEvidence, _) =>
            exact.occurrenceProof
        }
    )

  private def exactDemand(context: JudgmentAssemblyContext): List[OccurrenceExplanationDemand] =
    OccurrenceExplanationDemand
      .resolve(
        context,
        ExplanationRequest.forObservedMove(context.input)
      )
      .toList
