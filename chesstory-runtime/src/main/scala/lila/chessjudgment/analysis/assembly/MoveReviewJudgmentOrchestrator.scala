package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.{
  BranchReplyProbeBinding,
  ProbeKind,
  ProbeRequest,
  ProbeResult,
  ProbeVariant
}
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.judgment.*

object MoveReviewJudgmentOrchestrator:

  /** Packet construction together with the exact client reports admitted into it. */
  final case class ReviewExecution(
      packet: EvidenceBackedJudgmentPacket,
      admittedProbeResultIds: List[String],
      preparedInput: AdmittedMoveReviewInput
  )

  final private case class ProbeAdmission(
      admittedBranches: List[MoveReviewInputAdmission.AdmittedProbeBranch]
  )

  /** Development-only direct v6 entrypoint; player jobs enter through executePreparedReview. */
  def execute(raw: RawMoveReviewInput): Option[EvidenceBackedJudgmentPacket] =
    MoveReviewInputAdmission
      .admit(raw.copy(probeResults = Nil))
      .flatMap { preparedInput =>
        executeCommon(preparedInput, admitIssuedProbeResults(preparedInput, raw.probeResults))
      }
      .map(_.packet)

  /** Sole F -> C -> Jp -> Ja -> R authority for an admitted root review. */
  def executePreparedReview(
      preparedInput: AdmittedMoveReviewInput
  ): Option[ReviewExecution] =
    executeCommon(preparedInput, ProbeAdmission(Nil))

  /** Development/test handoff for raw probe results. Player jobs admit probe
    * histories before invoking the one-argument entrypoint above.
    */
  def executePreparedReview(
      preparedInput: AdmittedMoveReviewInput,
      fulfilledProbeResults: List[(ProbeRequest, ProbeResult)]
  ): Option[ReviewExecution] =
    executeCommon(preparedInput, admitPreparedProbeResults(preparedInput, fulfilledProbeResults))

  private def executeCommon(
      preparedInput: AdmittedMoveReviewInput,
      admission: ProbeAdmission
  ): Option[ReviewExecution] =
    val admittedInput = MoveReviewInputAdmission.withAdmittedProbeBranches(
      preparedInput,
      admission.admittedBranches
    )
    for
      f <- assembledFactContext(admittedInput, Nil).map(
        RelativeAssessmentAssembler.enrichFacts
      )
      pendingRequests = pendingProbeRequests(
        f,
        admittedInput.branchReplies.map(_.sourceProbeId).toSet
      )
      _ <- Option.when(
        evidenceClosed(f) && factStagePayloadsClosed(f)
      )(())
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
        pendingRequests,
        r.playerFacingClaimDecisions,
        r.causeExposureResolution,
        r.causeDispositionLedger
      )
    yield ReviewExecution(
      packet,
      admittedInput.branchReplies.map(_.sourceProbeId),
      admittedInput
    )

  private def factStagePayloadsClosed(context: JudgmentAssemblyContext): Boolean =
    context.evidenceGraph.records.forall(record => !causeOwnedPayload(record.payload))

  private def causeOwnedPayload(payload: EvidencePayload): Boolean =
    payload match
      case _: RelativeCauseFactEvidence => true
      case _: RelativeAssessmentEvidence => true
      case _ => false

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

  private def assembledFactContext(
      preparedInput: AdmittedMoveReviewInput,
      admittedBranches: List[MoveReviewInputAdmission.AdmittedProbeBranch]
  ): Option[JudgmentAssemblyContext] =
    NodeLineTransitionAssembler
      .assemble(MoveReviewInputAdmission.withAdmittedProbeBranches(preparedInput, admittedBranches))
      .map(EvidenceFactAssembler.enrich)

  private def pendingProbeRequests(
      context: JudgmentAssemblyContext,
      fulfilledProbeIds: Set[String]
  ): List[ProbeRequest] =
    BranchReplyProbePlanner
      .fromAssembly(context)
      .filterNot(request => fulfilledProbeIds(request.id))

  private def pendingProbeRequestsForPreparedReview(
      preparedInput: AdmittedMoveReviewInput,
      admittedBranches: List[MoveReviewInputAdmission.AdmittedProbeBranch]
  ): List[ProbeRequest] =
    assembledFactContext(preparedInput, admittedBranches).map(
      RelativeAssessmentAssembler.enrichFacts
    ).toList.flatMap(context =>
      pendingProbeRequests(
        context,
        (
          preparedInput.branchReplies.map(_.sourceProbeId) ++
            admittedBranches.map(_.branch.sourceProbeId)
        ).toSet
      )
    )

  private final case class IssuedProbeReplay(
      issued: List[ProbeRequest],
      admitted: List[MoveReviewInputAdmission.AdmittedProbeBranch]
  )

  /** Reconstructs reachability while admitting every submitted result at most once. */
  private def replayIssuedProbeResults(
      preparedInput: AdmittedMoveReviewInput,
      submittedProbeResults: List[ProbeResult]
  ): IssuedProbeReplay =
    @annotation.tailrec
    def loop(
        issued: List[ProbeRequest],
        admitted: List[MoveReviewInputAdmission.AdmittedProbeBranch],
        attemptedIds: Set[String],
        remainingRounds: Int
    ): IssuedProbeReplay =
      if remainingRounds <= 0 then IssuedProbeReplay(issued, admitted)
      else
        val roundRequests = pendingProbeRequestsForPreparedReview(preparedInput, admitted)
        val nextIssued = (issued ++ roundRequests).distinctBy(_.id)
        val newlyReachable = submittedProbeResults.flatMap(result =>
          if attemptedIds(result.id) then None
          else nextIssued.find(_.id == result.id).map(_ -> result)
        )
        val nextAdmitted = newlyReachable.foldLeft(admitted) {
          case (accepted, (request, result)) =>
            MoveReviewInputAdmission.admitIssuedProbeResult(
              preparedInput,
              request,
              result,
              MoveReviewInputAdmission.nextBranchReplyRank(preparedInput, accepted)
            ) match
              case Some(admission) => accepted :+ admission
              case None            => accepted
        }
        val nextAttempted = attemptedIds ++ newlyReachable.map(_._2.id)
        if nextIssued == issued && nextAdmitted == admitted && nextAttempted == attemptedIds then
          IssuedProbeReplay(nextIssued, nextAdmitted)
        else
          loop(
            nextIssued,
            nextAdmitted,
            nextAttempted,
            remainingRounds - 1
          )

    loop(Nil, Nil, Set.empty, submittedProbeResults.size + 1)

  private def admitIssuedProbeResults(
      preparedInput: AdmittedMoveReviewInput,
      submittedProbeResults: List[ProbeResult]
  ): ProbeAdmission =
    if submittedProbeResults.isEmpty then ProbeAdmission(Nil)
    else
      val duplicateIds = submittedProbeResults
        .groupMapReduce(_.id)(_ => 1)(_ + _)
        .collect {
          case (id, count) if count > 1 => id
        }
        .toSet
      val eligible = submittedProbeResults.filterNot(result => duplicateIds(result.id))
      val replay = replayIssuedProbeResults(preparedInput, eligible)
      ProbeAdmission(replay.admitted)

  private def admitPreparedProbeResults(
      preparedInput: AdmittedMoveReviewInput,
      fulfilledProbeResults: List[(ProbeRequest, ProbeResult)]
  ): ProbeAdmission =
    require(
      fulfilledProbeResults.map(_._1.id).distinct.size == fulfilledProbeResults.size &&
        fulfilledProbeResults.map(_._2.id).distinct.size == fulfilledProbeResults.size,
      "trusted player probe handoff has duplicate request or result ids"
    )
    val admitted = fulfilledProbeResults.foldLeft(List.empty[MoveReviewInputAdmission.AdmittedProbeBranch]) {
      case (accepted, (request, result)) =>
        MoveReviewInputAdmission.admitIssuedProbeResult(
          preparedInput,
          request,
          result,
          MoveReviewInputAdmission.nextBranchReplyRank(preparedInput, accepted)
        ) match
          case Some(admission) => accepted :+ admission
          case None =>
            throw IllegalArgumentException(
              s"trusted player probe handoff mismatch for ${request.id}"
            )
    }
    ProbeAdmission(admitted)

private[assembly] object BranchReplyProbePlanner:
  def fromAssembly(ctx: JudgmentAssemblyContext): List[ProbeRequest] =
    ctx.root.toList
      .flatMap { root =>
        val selectedLines = selectedRootLines(ctx.lines)
        val requiredHorizonByRootMove =
          ctx.evidenceGraph.records
            .flatMap {
              case record @ EvidenceRecord(_, event: PassedPawnResultEventEvidence, _)
                  if ctx.evidenceGraph.proofEligible(record) &&
                    event.observedRootEnablesContinuation &&
                    event.causalResultAssessments.nonEmpty &&
                    !event.branchCoverageComplete =>
                List(EvidenceRef.normalizeMove(event.rootMove) -> event.requiredHorizonPlyOffset)
              case _ => Nil
            }
            .groupMapReduce(_._1)(_._2)(math.max)
        selectedLines
          .flatMap { line =>
            val rootMove = EvidenceRef.normalizeMove(line.ref.rootMove)
            requiredHorizonByRootMove.get(rootMove).toList.flatMap { requiredHorizon =>
                line.evaluation.engineLine.toList.flatMap { engineLine =>
                  ctx.lineReplay(line.ref).toList.flatMap { replay =>
                    val covered = coveredReplies(ctx, line.ref.rootMove, requiredHorizon)
                    for
                      rootStep <- replay.replaySteps.headOption.toList
                      legalReplies <- ctx.evidenceGraph.certifiedRootResponseMovesFor(line.ref).toList
                      replyMove <- legalReplies.filterNot(covered)
                      bindingHash = BranchReplyProbeBinding.variationHash(
                        rootFen = root.fen,
                        role = line.ref.role,
                        rootMove = line.ref.rootMove,
                        whitePovEvalCp = engineLine.scoreCp,
                        mate = engineLine.mate,
                        depth = engineLine.depth,
                        moves = engineLine.moves,
                        replyMove = replyMove,
                        certifiedHorizonPlyOffset = requiredHorizon
                      )
                    yield ProbeRequest(
                      id = ProbeRequest.transportId(
                        ProbeKind.BranchReply,
                        bindingHash
                      ),
                      fen = rootStep.fenAfter,
                      depth = BranchReplyProbeBinding.Depth,
                      candidateMove = line.ref.rootMove,
                      depthFloor = BranchReplyProbeBinding.DepthFloor,
                      variationHash = bindingHash,
                      variant = ProbeVariant.BranchReply(replyMove, requiredHorizon)
                    )
                  }
                }
            }
          }
      }

  private def coveredReplies(
      ctx: JudgmentAssemblyContext,
      rootMove: String,
      requiredHorizonPlyOffset: Int
  ): Set[String] =
    ctx.input.branchReplies
      .filter(branch =>
        EvidenceRef.sameMove(branch.probedMoveUci, rootMove) &&
          branch.certifiedHorizonPlyOffset >= requiredHorizonPlyOffset
      )
      .flatMap(_.firstRankedLinePerRootMove.flatMap(_.rootMove))
      .map(EvidenceRef.normalizeMove)
      .toSet

  private[assembly] def selectedRootLines(lines: List[CandidateLineNode]): List[CandidateLineNode] =
    val rootLines = lines.filterNot(_.role == LineNodeRole.BranchReply)
    val primary =
      List(LineNodeRole.Played, LineNodeRole.BestReference).flatMap(role => rootLines.find(_.role == role))
    val alternatives =
      rootLines.filter(_.role == LineNodeRole.Alternative).sortBy(_.ref.rank)
    (primary ++ alternatives)
      .filter(line => line.evaluation.engineLine.exists(_.moves.nonEmpty))
      .distinctBy(_.ref.rootMove)
