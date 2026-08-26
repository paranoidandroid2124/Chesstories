package lila.chessjudgment.analysis.assembly

import chess.{ Move, Pawn, Queen, Rook }
import chess.format.Fen

import lila.chessjudgment.analysis.position.{ PositionAnalysis, PositionAnalyzer }
import lila.chessjudgment.model.position.BoardGeometry
import lila.chessjudgment.model.{
  BranchReplyProbeBinding,
  CausalContinuationProbeBinding,
  CounterResourceProbeBinding,
  ProbeAdmissionDiagnostic,
  ProbeAdmissionStatus,
  ProbeObjective,
  ProbeRequest,
  ProbeResolution,
  ProbeResult,
  ProbeVariant
}
import lila.chessjudgment.model.line.{ LegalReplayStep, PrincipalVariationEvidence }
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
    val causalContinuationCatalog = ExactCausalContinuationProbePlanner.catalog(context)
    val branchWork = BranchReplyProbePlanner
      .fromAssembly(context, causalContinuationCatalog)
      .distinctBy(_.id)
      .filterNot(request => fulfilledProbeIds(request.id))
    if branchWork.nonEmpty then branchWork
    else
      val causalContinuationWork = ExactCausalContinuationProbePlanner
        .fromAssembly(context, causalContinuationCatalog)
        .distinctBy(_.id)
        .filterNot(request => fulfilledProbeIds(request.id))
      if causalContinuationWork.nonEmpty then causalContinuationWork
      else
        CounterResourceProbePlanner
          .fromAssembly(context)
          .distinctBy(_.id)
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
      purpose = request.map(_.purpose),
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

private[assembly] object ExactCausalContinuationProbePlanner:
  final case class Catalog(candidatesByRootMove: Map[String, List[String]]):
    def candidatesFor(rootMove: String): List[String] =
      candidatesByRootMove.getOrElse(EvidenceRef.normalizeMove(rootMove), Nil)

  def catalog(ctx: JudgmentAssemblyContext): Catalog =
    val candidates = BranchReplyProbePlanner.selectedRootLines(ctx.lines).flatMap { line =>
      ctx.lineReplay(line.ref).flatMap(rootTransitionReplay).map { rootReplay =>
        val rootMove = EvidenceRef.normalizeMove(line.ref.rootMove)
        rootMove -> latentCandidateMoves(line.ref, rootReplay)
      }
    }.toMap
    Catalog(candidates)

  def fromAssembly(ctx: JudgmentAssemblyContext): List[ProbeRequest] =
    fromAssembly(ctx, catalog(ctx))

  def fromAssembly(ctx: JudgmentAssemblyContext, catalog: Catalog): List[ProbeRequest] =
    BranchReplyProbePlanner.selectedRootLines(ctx.lines).flatMap { line =>
      ctx.lineReplay(line.ref).flatMap(rootTransitionReplay).toList.flatMap { rootReplay =>
        val rootMove = EvidenceRef.normalizeMove(line.ref.rootMove)
        val latentMoves = catalog.candidatesFor(rootMove)
        val replies = observedReplies(ctx, line)
        line.evaluation.engineLine.toList.flatMap { engineLine =>
          for
            observedReply <- replies
            afterReplyAnalysis <- observedReply.afterReplyAnalysis.toList
            candidateMove <- afterReplyAnalysis.actualLegalMoves.filter(move =>
              latentMoves.exists(EvidenceRef.sameMove(_, move.toUci.uci))
            )
            candidate = EvidenceRef.normalizeMove(candidateMove.toUci.uci)
            reply = observedReply.moveUci
            continuation = List(reply, candidate)
            if exactContinuation(line.ref, observedReply.prefix, candidateMove)
            if !continuationAlreadyCovered(ctx, line, continuation)
            branchFen <- rootReplay.replaySteps.headOption.map(_.fenAfter).toList
            bindingHash = CausalContinuationProbeBinding.variationHash(
              rootFen = rootReplay.replaySteps.head.fenBefore,
              role = line.ref.role,
              rootMove = rootMove,
              whitePovEvalCp = engineLine.scoreCp,
              mate = engineLine.mate,
              depth = engineLine.depth,
              moves = engineLine.moves,
              continuationMoves = continuation
            )
          yield ProbeRequest(
            id = ProbeRequest.transportId(ProbeObjective.CausalContinuation, bindingHash),
            fen = branchFen,
            depth = BranchReplyProbeBinding.Depth,
            multiPv = 1,
            candidateMove = rootMove,
            depthFloor = BranchReplyProbeBinding.DepthFloor,
            variationHash = bindingHash,
            variant = ProbeVariant.CausalContinuation(reply, candidate)
          )
        }
      }
    }.distinctBy(_.id)

  private[assembly] def latentCandidateMoves(
      rootLine: LineNodeRef,
      rootReplay: CanonicalLineReplay
  ): List[String] =
    (for
      rootReplayStep <- rootReplay.replaySteps.headOption.toList
      root <- rootReplay.legalStep(rootReplayStep).toList
      beforeAnalysis <- rootReplay.analysisBefore(rootReplayStep).toList
      mover = root.move.piece.color
      legalBeforeRoot = beforeAnalysis.actualLegalMoves
        .iterator
        .map(move => EvidenceRef.normalizeMove(move.toUci.uci))
        .toSet
      hypothetical = LegalIfTurnPosition.from(root.after, mover)
      activeFen = Fen.write(hypothetical.active).value
      activeAnalysis = PositionAnalyzer.analyze(hypothetical.active, activeFen, root.ply + 1)
      candidate <- activeAnalysis.actualLegalMoves
      potential <- LatentCausalPotential
        .discover(
          rootLine,
          transitionRole(rootLine.role),
          root,
          activeAnalysis,
          candidate,
          legalBeforeRoot
        )
        .toList
    yield potential.moveUci).distinct.sorted

  private[assembly] def needsReplyDiscovery(
      ctx: JudgmentAssemblyContext,
      line: CandidateLineNode,
      rootFen: String
  ): Boolean =
    needsReplyDiscovery(ctx, line, rootFen, catalog(ctx))

  private[assembly] def needsReplyDiscovery(
      ctx: JudgmentAssemblyContext,
      line: CandidateLineNode,
      rootFen: String,
      catalog: Catalog
  ): Boolean =
    val _ = rootFen
    catalog.candidatesFor(line.ref.rootMove).nonEmpty && observedReplies(ctx, line).isEmpty

  private final case class ObservedReply(moveUci: String, prefix: CanonicalLineReplay):
    def afterReply: chess.Position = prefix.legalSteps.last.after
    def afterReplyAnalysis: Option[PositionAnalysis] =
      prefix.replaySteps.lastOption.flatMap(prefix.analysisAfter)

  private def observedReplies(ctx: JudgmentAssemblyContext, line: CandidateLineNode): List[ObservedReply] =
    val rootMove = EvidenceRef.normalizeMove(line.ref.rootMove)
    val mainLine = ctx.lineReplay(line.ref).toList.flatMap { replay =>
      replay.replaySteps.take(2) match
        case root :: reply :: Nil if EvidenceRef.sameMove(root.moveUci, rootMove) =>
          replay.subset(List(root, reply)).toList.map(prefix => ObservedReply(reply.moveUci, prefix))
        case _ => Nil
    }
    val branchLines = ctx.input.threatBranches
      .filter(branch =>
        EvidenceRef.sameMove(branch.probedMoveUci, rootMove) &&
          branch.objective != ProbeObjective.CounterResource
      )
      .flatMap(_.rankedUniqueLines)
      .flatMap { continuation =>
        for
          rootPrefix <- continuation.predecessorReplay.toList.flatMap(rootTransitionReplay)
          replyStep <- continuation.replay.replaySteps.headOption.toList
          replyReplay <- continuation.replay.subset(List(replyStep)).toList
          prefix <- CanonicalLineReplay.concatenate(rootPrefix, replyReplay).toList
        yield ObservedReply(replyStep.moveUci, prefix)
      }
    (mainLine ++ branchLines).distinctBy(_.moveUci)

  private def exactContinuation(
      rootLine: LineNodeRef,
      prefix: CanonicalLineReplay,
      candidate: Move
  ): Boolean =
    (for
      root <- prefix.legalSteps.headOption
      afterReply <- prefix.legalSteps.lastOption.map(_.after)
      candidateStep = LineReplayStep(
        prefix.replaySteps.last.ply + 1,
        candidate.toUci.uci,
        Fen.write(afterReply).value,
        Fen.write(candidate.after).value
      )
      replay <- prefix.append(candidateStep, afterReply, candidate)
      rootStep <- replay.replaySteps.headOption
      admittedCandidateStep <- replay.replaySteps.lastOption
      trace = CausalLineTrace.from(
        rootLine,
        transitionRole(rootLine.role),
        root.move.piece.color,
        replay.replaySteps,
        admittedReplay = Some(replay)
      )
    yield exactCandidateGoal(trace, rootStep, admittedCandidateStep)).contains(true)

  private def exactCandidateGoal(
      trace: CausalLineTrace,
      rootStep: LineReplayStep,
      candidateStep: LineReplayStep
  ): Boolean =
    trace.positionAnalysis(candidateStep.fenAfter, candidateStep.ply).nonEmpty &&
      trace.relation(rootStep, candidateStep).exists(_.exactlyEnables) &&
        trace
          .observation(candidateStep)
          .exists(PlanCausalEventProof.latentCandidatePlanKindsAt(_).nonEmpty)

  private def rootTransitionReplay(replay: CanonicalLineReplay): Option[CanonicalLineReplay] =
    replay.replaySteps.headOption.flatMap(step => replay.subset(List(step)))

  private def transitionRole(role: LineNodeRole): TransitionEdgeRole =
    role match
      case LineNodeRole.Played        => TransitionEdgeRole.Played
      case LineNodeRole.BestReference => TransitionEdgeRole.Reference
      case LineNodeRole.Alternative   => TransitionEdgeRole.Alternative
      case LineNodeRole.Threat        => TransitionEdgeRole.Threat

  private def continuationAlreadyCovered(
      ctx: JudgmentAssemblyContext,
      line: CandidateLineNode,
      continuation: List[String]
  ): Boolean =
    val rootMove = EvidenceRef.normalizeMove(line.ref.rootMove)
    val mainLine = line.evaluation.engineLine.toList.map(_.moves.drop(1))
    val branchLines = ctx.input.threatBranches
      .filter(branch => EvidenceRef.sameMove(branch.probedMoveUci, rootMove))
      .flatMap(_.rankedUniqueLines.map(_.evaluation.moves))
    continuation match
      case reply :: candidate :: Nil =>
        (mainLine ++ branchLines).exists(moves =>
          moves.headOption.exists(EvidenceRef.sameMove(_, reply)) &&
            moves.drop(1).exists(EvidenceRef.sameMove(_, candidate))
        )
      case _ => false

private enum LatentCausalLinkKind:
  case RootActorContinuation
  case RootClearance

/** Exact counterfactual transition used only to decide whether an engine
  * continuation probe is worth issuing. It is deliberately not a line fact:
  * the opponent reply has not yet been observed, so only the later admitted
  * root-reply-candidate replay may become causal evidence.
  */
private final case class LatentCausalPotential private (
    moveUci: String,
    relations: List[LatentCausalLinkKind],
    observation: CausalStepObservation
)

/** Explicitly non-authoritative turn-adjusted position. Changing the active
  * color is permitted only here, and one shared PositionAnalysis owns its legal
  * move list. No move generated from this position can be stored as evidence;
  * an observed reply followed by a normal legal replay is required first.
  */
private final case class LegalIfTurnPosition private (active: chess.Position)

private object LegalIfTurnPosition:
  def from(position: chess.Position, side: chess.Color): LegalIfTurnPosition =
    val active = if position.color == side then position else position.withColor(side)
    LegalIfTurnPosition(active)

private object LatentCausalPotential:
  def discover(
      rootLine: LineNodeRef,
      role: TransitionEdgeRole,
      root: LegalReplayStep,
      activeAnalysis: PositionAnalysis,
      candidate: Move,
      legalBeforeRoot: Set[String]
  ): Option[LatentCausalPotential] =
    val active = activeAnalysis.position
    val mover = root.move.piece.color
    val candidateUci = EvidenceRef.normalizeMove(candidate.toUci.uci)
    for
      _ <- Option.when(active.board == root.after.board && active.color == mover)(())
      step = LineReplayStep(
        ply = root.ply + 2,
        moveUci = candidateUci,
        fenBefore = Fen.write(active).value,
        fenAfter = Fen.write(candidate.after).value
      )
      replay <- CanonicalLineReplay.fromAnalyzedLegalMove(step, activeAnalysis, candidate)
      relations = exactRelations(root, candidate, legalBeforeRoot)
      if relations.nonEmpty
      observation <- CausalLineTrace.observe(rootLine, role, mover, step, replay)
      if PlanCausalEventProof.latentCandidatePlanKindsAt(observation).nonEmpty
    yield LatentCausalPotential(candidateUci, relations, observation)

  private def exactRelations(
      root: LegalReplayStep,
      candidate: Move,
      legalBeforeRoot: Set[String]
  ): List[LatentCausalLinkKind] =
    List(
      Option.when(rootActorContinues(root, candidate))(LatentCausalLinkKind.RootActorContinuation),
      Option.when(rootClearanceEnables(root, candidate, legalBeforeRoot))(
        LatentCausalLinkKind.RootClearance
      )
    ).flatten

  private def rootActorContinues(root: LegalReplayStep, candidate: Move): Boolean =
    candidate.orig == root.move.dest &&
      candidate.dest != root.move.orig &&
      candidate.piece == root.move.piece &&
      root.after.board.pieceAt(root.move.dest).contains(root.move.piece)

  private def rootClearanceEnables(
      root: LegalReplayStep,
      candidate: Move,
      legalBeforeRoot: Set[String]
  ): Boolean =
    val candidateUci = EvidenceRef.normalizeMove(candidate.toUci.uci)
    candidate.piece.color == root.move.piece.color &&
      !legalBeforeRoot(candidateUci) &&
      root.before.board.pieceAt(candidate.orig).contains(candidate.piece) &&
      root.after.board.pieceAt(candidate.orig).contains(candidate.piece) &&
      root.after.board.pieceAt(root.move.orig).isEmpty &&
      (candidate.dest == root.move.orig ||
        BoardGeometry.liesStrictlyBetween(candidate.orig, candidate.dest, root.move.orig))

private[assembly] object BranchReplyProbePlanner:
  def fromAssembly(ctx: JudgmentAssemblyContext): List[ProbeRequest] =
    fromAssembly(ctx, ExactCausalContinuationProbePlanner.catalog(ctx))

  def fromAssembly(
      ctx: JudgmentAssemblyContext,
      causalContinuationCatalog: ExactCausalContinuationProbePlanner.Catalog
  ): List[ProbeRequest] =
    ctx.root.toList
      .flatMap { root =>
        val selectedLines = selectedRootLines(ctx.lines)
        val provenCausalHorizons =
          ctx.evidenceGraph.records
            .collect {
              case record @ EvidenceRecord(ref, event: PlanCausalEventEvidence, _)
                  if ctx.evidenceGraph.proofEligible(record) &&
                    event.counterfactualContinuationProven &&
                    event.causalResultAssessments.nonEmpty &&
                    !event.branchCoverageComplete =>
                EvidenceRef.normalizeMove(event.rootMove) -> event.requiredHorizonPlyOffset
            }
        val latentDiscoveryHorizons = selectedLines
          .filter(line =>
            ExactCausalContinuationProbePlanner.needsReplyDiscovery(
              ctx,
              line,
              root.fen,
              causalContinuationCatalog
            )
          )
          .map(line => EvidenceRef.normalizeMove(line.ref.rootMove) -> 2)
        val requestedCausalHorizons = (provenCausalHorizons ++ latentDiscoveryHorizons)
          .groupMapReduce(_._1)(_._2)(_.max(_))
        selectedLines
          .filter(line => requestedCausalHorizons.contains(EvidenceRef.normalizeMove(line.ref.rootMove)))
          .filterNot(line =>
            existingReplyCensus(
              ctx,
              line.ref.rootMove,
              requestedCausalHorizons(EvidenceRef.normalizeMove(line.ref.rootMove)).max(1)
            )
          )
          .flatMap { line =>
            line.evaluation.engineLine.toList.flatMap { engineLine =>
              ctx.lineReplay(line.ref).toList.flatMap { replay =>
                for
                  rootStep <- replay.replaySteps.headOption.toList
                  requiredReplies <- ctx.evidenceGraph
                    .certifiedRootResponseCountFor(line.ref, BranchReplyProbeBinding.ReplyMultiPv)
                    .toList
                  if requiredReplies > 0
                  requiredHorizon =
                    requestedCausalHorizons(EvidenceRef.normalizeMove(line.ref.rootMove)).max(1)
                  bindingHash = BranchReplyProbeBinding.variationHash(
                    rootFen = root.fen,
                    role = line.ref.role,
                    rootMove = line.ref.rootMove,
                    whitePovEvalCp = engineLine.scoreCp,
                    mate = engineLine.mate,
                    depth = engineLine.depth,
                    moves = engineLine.moves,
                    certifiedHorizonPlyOffset = requiredHorizon
                  )
                yield ProbeRequest(
                  id = ProbeRequest.transportId(
                    ProbeObjective.BranchReplyMultiPv,
                    bindingHash
                  ),
                  fen = rootStep.fenAfter,
                  depth = BranchReplyProbeBinding.Depth,
                  multiPv = requiredReplies,
                  candidateMove = line.ref.rootMove,
                  depthFloor = BranchReplyProbeBinding.DepthFloor,
                  variationHash = bindingHash,
                  variant = ProbeVariant.BranchReply(requiredHorizon)
                )
              }
            }
          }
      }
      .distinctBy(_.id)

  private def existingReplyCensus(
      ctx: JudgmentAssemblyContext,
      rootMove: String,
      requiredHorizonPlyOffset: Int
  ): Boolean =
    ctx.input.threatBranches.exists(branch =>
      EvidenceRef.sameMove(branch.probedMoveUci, rootMove) &&
        branch.objective == ProbeObjective.BranchReplyMultiPv &&
        branch.certifiedHorizonPlyOffset.exists(_ >= requiredHorizonPlyOffset)
    )

  private[assembly] def selectedRootLines(lines: List[CandidateLineNode]): List[CandidateLineNode] =
    val rootLines = lines.filterNot(_.role == LineNodeRole.Threat)
    val primary =
      List(LineNodeRole.Played, LineNodeRole.BestReference).flatMap(role => rootLines.find(_.role == role))
    val alternatives =
      rootLines.filter(_.role == LineNodeRole.Alternative).sortBy(_.ref.rank)
    (primary ++ alternatives)
      .filter(line => line.evaluation.engineLine.exists(_.moves.nonEmpty))
      .distinctBy(_.ref.rootMove)
      .take(BranchReplyProbeBinding.ReplyMultiPv)

private[assembly] object CounterResourceProbePlanner:
  final private case class Candidate(
      moveUci: String,
      capturesRootActor: Boolean,
      escapesNewRootContactWithCapture: Boolean,
      createsGeometricContact: Boolean
  ):
    def probeRelevant: Boolean =
      capturesRootActor || escapesNewRootContactWithCapture || createsGeometricContact

  def fromAssembly(ctx: JudgmentAssemblyContext): List[ProbeRequest] =
    ctx.root.toList
      .flatMap { root =>
        targets(ctx).flatMap { case (resourceMove, lineCandidates) =>
          lineCandidates.flatMap { case (line, _) =>
            if replyBranchCovers(ctx, line.ref.rootMove, resourceMove) then Nil
            else
              for
                engineLine <- line.evaluation.engineLine.toList
                branchFen <- ctx.lineReplay(line.ref).flatMap(_.replaySteps.headOption).map(_.fenAfter).toList
              yield
                val bindingHash = CounterResourceProbeBinding.variationHash(
                  rootFen = root.fen,
                  role = line.ref.role,
                  rootMove = line.ref.rootMove,
                  whitePovEvalCp = engineLine.scoreCp,
                  mate = engineLine.mate,
                  depth = engineLine.depth,
                  moves = engineLine.moves,
                  opponentResourceMove = resourceMove
                )
                ProbeRequest(
                  id = ProbeRequest.transportId(ProbeObjective.CounterResource, bindingHash),
                  fen = branchFen,
                  depth = BranchReplyProbeBinding.Depth,
                  multiPv = 1,
                  candidateMove = line.ref.rootMove,
                  depthFloor = BranchReplyProbeBinding.DepthFloor,
                  variationHash = bindingHash,
                  variant = ProbeVariant.CounterResource(resourceMove)
                )
          }
        }
      }
      .distinctBy(_.id)

  private def targets(
      ctx: JudgmentAssemblyContext
  ): List[(String, List[(CandidateLineNode, Candidate)])] =
    val selectedLines = BranchReplyProbePlanner
      .selectedRootLines(ctx.lines)
      .filter(line => Set(LineNodeRole.Played, LineNodeRole.BestReference)(line.role))
    val rootLines = Option
      .when(selectedLines.exists(exactContinuationDemand(ctx, _)))(selectedLines)
      .getOrElse(Nil)
    val candidatesByLine = rootLines.flatMap(line =>
      ctx.lineReplay(line.ref).map(replay => line -> legalResourceCandidatesAfter(replay))
    )
    val changedResources = candidatesByLine.flatMap { case (_, candidates) =>
      candidates.filter(_.probeRelevant).map(_.moveUci)
    }.distinct
    changedResources
      .map { move =>
        move -> candidatesByLine.flatMap { case (line, candidates) =>
          candidates.find(_.moveUci == move).map(line -> _)
        }
      }
      .toList
      .filter { case (_, lineCandidates) =>
        lineCandidates.size >= 2
      }
      .filter { case (move, lineCandidates) =>
        lineCandidates.exists { case (line, _) =>
          !replyBranchCovers(ctx, line.ref.rootMove, move)
        }
      }
      .sortBy(_._1)

  private def exactContinuationDemand(
      ctx: JudgmentAssemblyContext,
      line: CandidateLineNode
  ): Boolean =
    val rootMove = EvidenceRef.normalizeMove(line.ref.rootMove)
    val admittedBranches = ctx.input.threatBranches.filter(branch =>
      EvidenceRef.sameMove(branch.probedMoveUci, rootMove) &&
        branch.objective != ProbeObjective.CounterResource
    )
    val admittedForcedContinuation = admittedBranches.exists(_.objective == ProbeObjective.CausalContinuation)
    val admittedReplyCensus = admittedBranches.exists(_.objective == ProbeObjective.BranchReplyMultiPv)
    admittedForcedContinuation ||
      admittedReplyCensus && exactContinuationInRootLine(ctx, line)

  private def exactContinuationInRootLine(
      ctx: JudgmentAssemblyContext,
      line: CandidateLineNode
  ): Boolean =
    val structural = ctx.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(ref, payload: StructuralDeltaEvidence, _)
          if ctx.evidenceGraph.proofEligible(record) && ref.line.contains(line.ref) =>
        payload
    }
    val facts = ctx.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(ref, payload: LineFactEvidence, _)
          if ctx.evidenceGraph.proofEligible(record) && ref.line.contains(line.ref) =>
        payload
    }
    (for
      structuralEvidence <- structural
      lineFacts <- facts
    yield PlanCausalEventProof.hasLatentContinuationCandidate(
      PlanCausalEventProof.causalTrace(
        line.ref,
        structuralEvidence,
        lineFacts,
        lineFacts.certifiedReplay
      )
    )).contains(true)

  private def replyBranchCovers(
      ctx: JudgmentAssemblyContext,
      rootMove: String,
      resourceMove: String
  ): Boolean =
    ctx.input.threatBranches.exists(branch =>
      EvidenceRef.sameMove(branch.probedMoveUci, rootMove) &&
        branch.rankedUniqueLines.exists(_.rootMove.exists(EvidenceRef.sameMove(_, resourceMove)))
    )

  private[assembly] def exactResourceMovesAfter(
      replay: CanonicalLineReplay
  ): List[String] =
    legalResourceCandidatesAfter(replay)
      .filter(_.probeRelevant)
      .map(_.moveUci)

  private def legalResourceCandidatesAfter(
      replay: CanonicalLineReplay
  ): List[Candidate] =
    for
      rootReplayStep <- replay.replaySteps.headOption.toList
      root <- replay.legalStep(rootReplayStep).toList
      rootTransition <- replay.transition(rootReplayStep).toList
      positionAnalysis = rootTransition.afterAnalysis
      rootDestination = root.move.dest
      rootActor = root.move.piece
      position = root.after
      newlyContactedTargets = probeGeometricContactTargetsFrom(
        rootTransition.relationDelta,
        rootDestination,
        rootActor.color
      )
      resourceMoves = positionAnalysis.actualLegalMoves
      move <- resourceMoves
      resourceStep = LegalReplayStep.fromMove(root.ply + 1, position, move)
      resourceReplayStep = LineReplayStep(
        ply = resourceStep.ply,
        moveUci = resourceStep.uci,
        fenBefore = Fen.write(resourceStep.before).value,
        fenAfter = Fen.write(resourceStep.after).value
      )
      rootPrefix <- replay.subset(List(rootReplayStep)).toList
      resourceReplay <- rootPrefix.append(resourceReplayStep, position, move).toList
      resourceTransition <- resourceReplay.transition(resourceReplayStep).toList
      capturesRootActor = move.capture.contains(rootDestination)
      newlyGeometricallyContactedResource = newlyContactedTargets(move.orig)
      escapesNewRootContactWithCapture = newlyGeometricallyContactedResource && move.captures
      createsGeometricContact = probeGeometricContactTargetsFrom(
        resourceTransition.relationDelta,
        resourceStep.move.dest,
        resourceStep.move.piece.color
      )(rootDestination)
    yield Candidate(
      moveUci = EvidenceRef.normalizeMove(move.toUci.uci),
      capturesRootActor = capturesRootActor,
      escapesNewRootContactWithCapture = escapesNewRootContactWithCapture,
      createsGeometricContact = createsGeometricContact
    )

  private def probeGeometricContactTargetsFrom(
      delta: RelationSemanticDelta,
      actorSquare: _root_.chess.Square,
      color: _root_.chess.Color
  ): Set[_root_.chess.Square] =
    delta.geometricControlTransition.newlyEstablished
      .flatMap(_.detail match
        case RelationWitnessDetail.GeometricControl(
              side,
              attacker,
              _,
              target,
              RelationControlTarget.Enemy(_)
            )
            if side == color && attacker.key.equalsIgnoreCase(actorSquare.key) =>
          _root_.chess.Square.fromKey(target.key).toList
        case _ => Nil
      )
      .toSet
