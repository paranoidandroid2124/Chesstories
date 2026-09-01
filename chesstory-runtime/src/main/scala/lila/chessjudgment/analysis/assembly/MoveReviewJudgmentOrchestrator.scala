package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

object MoveReviewJudgmentOrchestrator:

  final case class ReviewExecution(
      packet: EvidenceBackedJudgmentPacket,
      preparedInput: AdmittedMoveReviewInput
  )

  /** Development-only direct v6 entrypoint; player jobs enter through executePreparedReview. */
  def execute(raw: RawMoveReviewInput): Option[EvidenceBackedJudgmentPacket] =
    MoveReviewInputAdmission.admit(raw).flatMap(executePreparedReview).map(_.packet)

  /** Sole F -> C -> Jp -> Ja -> R authority for an admitted root review. */
  def executePreparedReview(
      preparedInput: AdmittedMoveReviewInput
  ): Option[ReviewExecution] =
    for
      f <- NodeLineTransitionAssembler
        .assemble(preparedInput)
        .map(EvidenceFactAssembler.enrich)
        .map(RelativeAssessmentAssembler.enrichFacts)
      _ <- Option.when(evidenceClosed(f) && factStagePayloadsClosed(f))((): Unit)
      c = RelativeAssessmentAssembler.enrichCauses(f)
      jp = JudgmentClaimAssembler.propose(c)
      ja = ClaimCandidateGraphAssembler.fromClaims(jp, c)
      r <- ClaimArbitrator.rankDetailed(ja, c.relativeAssessments)
      rankedContext = r.rankedClaims.foldLeft(c.copy(claims = Nil))((context, claim) =>
        context.withClaim(claim)
      )
      packet <- EvidenceBackedJudgmentPacket.fromAssembly(
        rankedContext,
        r.primary,
        r.playerFacingClaimDecisions,
        r.causeExposureResolution,
        r.causeDispositionLedger
      )
    yield ReviewExecution(packet, preparedInput)

  private def factStagePayloadsClosed(context: JudgmentAssemblyContext): Boolean =
    context.evidenceGraph.records.forall(record => !causeOwnedPayload(record.payload))

  private def causeOwnedPayload(payload: EvidencePayload): Boolean =
    payload match
      case _: RelativeCauseFactEvidence   => true
      case _: RelativeAssessmentEvidence => true
      case _                              => false

  private def evidenceClosed(context: JudgmentAssemblyContext): Boolean =
    val records = context.evidenceGraph.records
    val recordIds = records.map(_.ref.id)
    val recordsById = records.map(record => record.ref.id -> record).toMap
    val positionRefs = context.positions.map(_.ref).toSet
    val lineRefs = context.lines.map(_.ref).toSet
    def registered(ref: EvidenceRef): Boolean =
      recordsById.get(ref.id).exists(_.ref == ref)
    context.root.exists(root => root.fen == context.input.beforeFen && root.ply == context.input.beforePly) &&
    records.nonEmpty &&
    recordIds.distinct.size == recordIds.size &&
    records.forall(record =>
      positionRefs(record.ref.position) &&
        record.ref.line.forall(lineRefs) &&
        record.payloadLineRefs.forall(lineRefs) &&
        record.parents.forall(registered)
    ) &&
    context.lines.forall(line => registered(line.evidence)) &&
    context.transitions.forall(transition =>
      positionRefs(transition.from) &&
        positionRefs(transition.to) &&
        registered(transition.evidence)
    )
