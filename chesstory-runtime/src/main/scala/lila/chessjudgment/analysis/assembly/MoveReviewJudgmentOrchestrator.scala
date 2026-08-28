package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.{
  BranchReplyProbeBinding,
  ProbeAdmissionDiagnostic,
  ProbeAdmissionStatus,
  ProbeKind,
  ProbeRequest,
  ProbeResolution,
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
      preparedInput: NormalizedMoveReviewInput
  )

  final private case class ProbeAdmission(
      admittedBranches: List[MoveReviewInputNormalizer.AdmittedProbeBranch],
      diagnostics: List[ProbeAdmissionDiagnostic]
  )

  /** Development-only direct v6 entrypoint; player jobs enter through executePreparedReview. */
  def execute(raw: RawMoveReviewInput): Option[EvidenceBackedJudgmentPacket] =
    MoveReviewInputNormalizer
      .normalize(raw.copy(probeResults = Nil))
      .flatMap { preparedInput =>
        executeCommon(preparedInput, admitIssuedProbeResults(preparedInput, raw.probeResults))
      }
      .map(_.packet)

  /** Sole F -> C -> Jp -> Ja -> R authority for a normalizer-owned root review. */
  def executePreparedReview(
      preparedInput: NormalizedMoveReviewInput,
      fulfilledProbeResults: List[(ProbeRequest, ProbeResult)]
  ): Option[ReviewExecution] =
    executeCommon(preparedInput, admitPreparedProbeResults(preparedInput, fulfilledProbeResults))

  private def executeCommon(
      preparedInput: NormalizedMoveReviewInput,
      admission: ProbeAdmission
  ): Option[ReviewExecution] =
    val admittedInput = MoveReviewInputNormalizer.withAdmittedProbeBranches(
      preparedInput,
      admission.admittedBranches
    )
    for
      f <- assembledFactContext(admittedInput, Nil).map(
        RelativeAssessmentAssembler.enrichFacts
      )
      pendingRequests = pendingProbeRequests(
        f,
        admittedInput.threatBranches.map(_.sourceProbeId).toSet
      )
      _ <- Option.when(
        evidenceClosed(f) &&
          factStagePayloadsClosed(f) &&
          (
            pendingRequests.nonEmpty ||
              EvidenceFactAssembler.exactStrategicMechanisms(f).nonEmpty &&
                RelativeAssessmentAssembler.exactStrategicContrasts(f).nonEmpty
          )
      )(())
      c = RelativeAssessmentAssembler.enrichCauses(f)
      jp = JudgmentClaimAssembler.propose(c)
      ja = ClaimCandidateGraphAssembler.fromClaims(jp, c)
      r <- ClaimArbitrator.rankDetailed(ja, c.relativeAssessments)
      rankedContext = r.rankedClaims.foldLeft(c.copy(claims = Nil))((context, claim) =>
        context.withClaim(claim)
      )
      context = rankedContext.copy(
        probeDiagnostics = (rankedContext.probeDiagnostics ++ admission.diagnostics)
          .distinctBy(diagnostic => diagnostic.probeId -> diagnostic.reasonCodes)
      )
      packet <- EvidenceBackedJudgmentPacket.fromAssembly(
        context,
        r.primary,
        pendingRequests,
        r.playerFacingClaimDecisions,
        r.onlyMoveConstraintResolutions,
        r.causeExposureResolution,
        r.causeDispositionLedger
      )
    yield ReviewExecution(
      packet,
      admittedInput.threatBranches.map(_.sourceProbeId),
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
      preparedInput: NormalizedMoveReviewInput,
      admittedBranches: List[MoveReviewInputNormalizer.AdmittedProbeBranch]
  ): Option[JudgmentAssemblyContext] =
    NodeLineTransitionAssembler
      .assemble(MoveReviewInputNormalizer.withAdmittedProbeBranches(preparedInput, admittedBranches))
      .map(EvidenceFactAssembler.enrich)

  private def pendingProbeRequests(
      context: JudgmentAssemblyContext,
      fulfilledProbeIds: Set[String]
  ): List[ProbeRequest] =
    BranchReplyProbePlanner
      .fromAssembly(context)
      .filterNot(request => fulfilledProbeIds(request.id))

  private def pendingProbeRequestsForPreparedReview(
      preparedInput: NormalizedMoveReviewInput,
      admittedBranches: List[MoveReviewInputNormalizer.AdmittedProbeBranch]
  ): List[ProbeRequest] =
    assembledFactContext(preparedInput, admittedBranches).toList.flatMap(context =>
      pendingProbeRequests(
        context,
        (
          preparedInput.threatBranches.map(_.sourceProbeId) ++
            admittedBranches.map(_.request.id)
        ).toSet
      )
    )

  private final case class IssuedProbeReplay(
      issued: List[ProbeRequest],
      admitted: List[MoveReviewInputNormalizer.AdmittedProbeBranch],
      rejected: List[ProbeAdmissionDiagnostic]
  )

  /** Reconstructs reachability while admitting every submitted result at most once. */
  private def replayIssuedProbeResults(
      preparedInput: NormalizedMoveReviewInput,
      submittedProbeResults: List[ProbeResult]
  ): IssuedProbeReplay =
    @annotation.tailrec
    def loop(
        issued: List[ProbeRequest],
        admitted: List[MoveReviewInputNormalizer.AdmittedProbeBranch],
        attemptedIds: Set[String],
        rejected: List[ProbeAdmissionDiagnostic],
        remainingRounds: Int
    ): IssuedProbeReplay =
      if remainingRounds <= 0 then IssuedProbeReplay(issued, admitted, rejected)
      else
        val roundRequests = pendingProbeRequestsForPreparedReview(preparedInput, admitted)
        val nextIssued = (issued ++ roundRequests).distinctBy(_.id)
        val newlyReachable = submittedProbeResults.flatMap(result =>
          if attemptedIds(result.id) then None
          else nextIssued.find(_.id == result.id).map(_ -> result)
        )
        val (nextAdmitted, newRejections) = newlyReachable.foldLeft(admitted -> List.empty[ProbeAdmissionDiagnostic]) {
          case ((accepted, failures), (request, result)) =>
            MoveReviewInputNormalizer.admitIssuedProbeResult(
              preparedInput,
              request,
              result,
              MoveReviewInputNormalizer.nextThreatRank(preparedInput, accepted)
            ) match
              case Right(admission) => (accepted :+ admission) -> failures
              case Left(diagnostic) => accepted -> (failures :+ diagnostic)
        }
        val nextAttempted = attemptedIds ++ newlyReachable.map(_._2.id)
        val nextRejected = rejected ++ newRejections
        if nextIssued == issued && nextAdmitted == admitted && nextAttempted == attemptedIds then
          IssuedProbeReplay(nextIssued, nextAdmitted, nextRejected)
        else
          loop(
            nextIssued,
            nextAdmitted,
            nextAttempted,
            nextRejected,
            remainingRounds - 1
          )

    loop(Nil, Nil, Set.empty, Nil, submittedProbeResults.size + 1)

  private def admitIssuedProbeResults(
      preparedInput: NormalizedMoveReviewInput,
      submittedProbeResults: List[ProbeResult]
  ): ProbeAdmission =
    if submittedProbeResults.isEmpty then ProbeAdmission(Nil, Nil)
    else
      val duplicateIds = submittedProbeResults
        .groupMapReduce(_.id)(_ => 1)(_ + _)
        .collect {
          case (id, count) if count > 1 => id
        }
        .toSet
      val eligible = submittedProbeResults.filterNot(result => duplicateIds(result.id))
      val replay = replayIssuedProbeResults(preparedInput, eligible)
      val admittedIds = replay.admitted.map(_.request.id).toSet
      val rejectedIds = replay.rejected.map(_.probeId).toSet
      val duplicateDiagnostics = submittedProbeResults
        .filter(result => duplicateIds(result.id))
        .map(result => rejectedDiagnostic(result, None, List("DUPLICATE_PROBE_RESULT_ID")))
      val unissuedDiagnostics = eligible
        .filterNot(result => admittedIds(result.id) || rejectedIds(result.id))
        .map(result => rejectedDiagnostic(result, None, List("PROBE_REQUEST_UNISSUED")))
      ProbeAdmission(
        replay.admitted,
        (replay.admitted.map(_.diagnostic) ++ replay.rejected ++ duplicateDiagnostics ++ unissuedDiagnostics)
          .distinctBy(diagnostic => diagnostic.probeId -> diagnostic.reasonCodes)
      )

  private def admitPreparedProbeResults(
      preparedInput: NormalizedMoveReviewInput,
      fulfilledProbeResults: List[(ProbeRequest, ProbeResult)]
  ): ProbeAdmission =
    require(
      fulfilledProbeResults.map(_._1.id).distinct.size == fulfilledProbeResults.size &&
        fulfilledProbeResults.map(_._2.id).distinct.size == fulfilledProbeResults.size,
      "trusted player probe handoff has duplicate request or result ids"
    )
    val admitted = fulfilledProbeResults.foldLeft(List.empty[MoveReviewInputNormalizer.AdmittedProbeBranch]) {
      case (accepted, (request, result)) =>
        MoveReviewInputNormalizer.admitIssuedProbeResult(
          preparedInput,
          request,
          result,
          MoveReviewInputNormalizer.nextThreatRank(preparedInput, accepted)
        ) match
          case Right(admission) => accepted :+ admission
          case Left(diagnostic) =>
            throw IllegalArgumentException(
              s"trusted player probe handoff mismatch for ${request.id}: ${diagnostic.reasonCodes.mkString(",")}"
            )
    }
    ProbeAdmission(admitted, admitted.map(_.diagnostic))

  private def rejectedDiagnostic(
      result: ProbeResult,
      request: Option[ProbeRequest],
      reasons: List[String]
  ): ProbeAdmissionDiagnostic =
    ProbeAdmissionDiagnostic(
      probeId = Option(result.id).map(_.trim).filter(_.nonEmpty).getOrElse("probe-result"),
      status = ProbeAdmissionStatus.Rejected,
      reasonCodes = reasons.distinct,
      purpose = request.map(_.kind),
      candidateMove = request.map(_.candidateMove),
      fen = request.map(_.fen),
      admittedLineCount = 0,
      legalLineCount = 0,
      scoredLineCount = (result.resolution match
        case ProbeResolution.EngineSearch(evaluations, _) =>
          evaluations.count(_.moves.nonEmpty)
        case ProbeResolution.ExactAutomaticTerminal(_) => 0
      ),
      depthFloor = request.map(_.depthFloor),
      horizon = request.flatMap(_.horizon),
      variationHash = request.map(_.variationHash)
    )

private[assembly] object BranchReplyProbePlanner:
  def fromAssembly(ctx: JudgmentAssemblyContext): List[ProbeRequest] =
    ctx.root.toList
      .flatMap { root =>
        val selectedLines = selectedRootLines(ctx.lines)
        val requiredCausalResultsByHorizon =
          ctx.evidenceGraph.records
            .flatMap {
              case record @ EvidenceRecord(_, event: PlanCausalEventEvidence, _)
                  if ctx.evidenceGraph.proofEligible(record) &&
                    event.observedRootEnablesContinuation &&
                    event.causalResultAssessments.nonEmpty &&
                    !event.branchCoverageComplete =>
                event.causalResultAssessments.map(assessment =>
                  (EvidenceRef.normalizeMove(event.rootMove) -> assessment.sourcePlyOffset) -> assessment
                )
              case _ => Nil
            }
            .groupMap(_._1)(_._2)
        selectedLines
          .flatMap { line =>
            val rootMove = EvidenceRef.normalizeMove(line.ref.rootMove)
            val requiredHorizons = requiredCausalResultsByHorizon.keysIterator.collect {
              case (`rootMove`, horizon) => horizon
            }.toList.sorted
            requiredHorizons.flatMap {
              requiredHorizon =>
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
    ctx.input.threatBranches
      .filter(branch =>
        EvidenceRef.sameMove(branch.probedMoveUci, rootMove) &&
          branch.certifiedHorizonPlyOffset >= requiredHorizonPlyOffset
      )
      .flatMap(_.rankedUniqueLines.flatMap(_.rootMove))
      .map(EvidenceRef.normalizeMove)
      .toSet

  private[assembly] def selectedRootLines(lines: List[CandidateLineNode]): List[CandidateLineNode] =
    val rootLines = lines.filterNot(_.role == LineNodeRole.Threat)
    val primary =
      List(LineNodeRole.Played, LineNodeRole.BestReference).flatMap(role => rootLines.find(_.role == role))
    val alternatives =
      rootLines.filter(_.role == LineNodeRole.Alternative).sortBy(_.ref.rank)
    (primary ++ alternatives)
      .filter(line => line.evaluation.engineLine.exists(_.moves.nonEmpty))
      .distinctBy(_.ref.rootMove)
