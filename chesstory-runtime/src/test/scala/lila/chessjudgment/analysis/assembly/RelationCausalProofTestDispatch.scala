package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Test access to the production demand dispatcher. Tests outside the
  * assembly package must not require a second production entry point.
  */
private[chessjudgment] object RelationCausalProofTestDispatch:

  def forced(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demandSource: EvidenceRecord
  ): List[EvidenceRecord] =
    exactDemand(context, demandSource).flatMap(demand =>
      RelationCausalProofAssembler
        .fromDemand(context, allocator, demand)
        .collect {
          case record @ EvidenceRecord(
                _,
                _: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence,
                _
              ) => record
        }
    )

  def defense(
      context: JudgmentAssemblyContext,
      demandSource: EvidenceRecord
  ): List[CertifiedSoleRecapturerRemovalBeforeTargetCapture] =
    exactDemand(context, demandSource).flatMap(demand =>
      RelationCausalProofAssembler
        .fromDemand(
          context,
          JudgmentProvenanceAllocator.forInput(context.input),
          demand
        )
        .flatMap(_.payload match
          case exact: SoleRecapturerRemovalBeforeTargetCaptureEvidence => exact.occurrenceProof
          case _                                      => None
        )
    )

  private def exactDemand(
      context: JudgmentAssemblyContext,
      demandSource: EvidenceRecord
  ): List[RelationCausalProofDemand] =
    ExactPlayedVsBestCausalInput
      .from(context, demandSource)
      .map(RelationCausalProofDemand.from)
      .toList
