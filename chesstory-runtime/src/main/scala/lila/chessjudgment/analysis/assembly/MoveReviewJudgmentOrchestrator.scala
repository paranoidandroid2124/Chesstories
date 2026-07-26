package lila.chessjudgment.analysis.assembly
import lila.chessjudgment.model.{
  BranchReplyProbeBinding,
  CounterResourceProbeBinding,
  ProbeAdmissionDiagnostic,
  ProbeAdmissionStatus,
  ProbeContractValidator,
  ProbePurpose,
  ProbeRequest,
  ProbeResult
}
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.judgment.*

object MoveReviewJudgmentOrchestrator:

  private final case class ProbeAuthorization(
      input: RawMoveReviewInput,
      diagnostics: List[ProbeAdmissionDiagnostic]
  )

  def assemble(raw: RawMoveReviewInput): Option[JudgmentAssemblyContext] =
    val authorization = authorizeProbeResults(raw)
    assembleUnchecked(authorization.input).map(context =>
      context.copy(probeDiagnostics = context.probeDiagnostics ++ authorization.diagnostics)
    )

  def packet(raw: RawMoveReviewInput): Option[EvidenceBackedJudgmentPacket] =
    assemble(raw).flatMap(context =>
      EvidenceBackedJudgmentPacket.fromAssembly(context, plannedProbeRequests(context))
    )

  private def assembleUnchecked(raw: RawMoveReviewInput): Option[JudgmentAssemblyContext] =
    JudgmentClaimAssembler.assemble(raw)

  private def probeRequestsUnchecked(raw: RawMoveReviewInput): List[ProbeRequest] =
    EvidenceFactAssembler.assemble(raw).toList.flatMap(plannedProbeRequests)

  private def plannedProbeRequests(context: JudgmentAssemblyContext): List[ProbeRequest] =
    (
      BranchReplyProbePlanner.fromAssembly(context) ++
        CounterResourceProbePlanner.fromAssembly(context) ++
        EndgameTablebaseProbePlanner.fromAssembly(context)
    ).distinctBy(_.id)

  /**
   * Replays the deterministic request/result exchange from an empty result set.
   * A later-round result becomes authoritative only after the preceding admitted
   * results make its exact request reachable.
   */
  private def issuedProbeRequests(raw: RawMoveReviewInput): List[ProbeRequest] =
    if raw.probeResults.isEmpty then Nil
    else
      @annotation.tailrec
      def loop(
          admitted: List[ProbeResult],
          issued: List[ProbeRequest],
          remainingRounds: Int
      ): List[ProbeRequest] =
        if remainingRounds <= 0 then issued
        else
          val roundRequests = probeRequestsUnchecked(raw.copy(probeResults = admitted))
          val nextIssued = (issued ++ roundRequests).distinctBy(_.id)
          val nextAdmitted = raw.probeResults.flatMap(result =>
            nextIssued
              .find(request => request.id == result.id && exactIssuedResult(request, result))
              .map(canonicalResult(_, result))
          )
          if nextIssued == issued && nextAdmitted == admitted then nextIssued
          else loop(nextAdmitted, nextIssued, remainingRounds - 1)

      loop(Nil, Nil, raw.probeResults.size + 1)

  private def exactIssuedResult(request: ProbeRequest, result: ProbeResult): Boolean =
    val validation = ProbeContractValidator.validateAgainstRequest(request, result)
    request.variationHash == result.variationHash &&
      request.horizon == result.horizon &&
      validation.isValid &&
      validation.softReasonCodes.isEmpty

  private def authorizeProbeResults(raw: RawMoveReviewInput): ProbeAuthorization =
    if raw.probeResults.isEmpty then ProbeAuthorization(raw, Nil)
    else
      val duplicateIds = raw.probeResults.groupMapReduce(_.id)(_ => 1)(_ + _).collect {
        case (id, count) if count > 1 => id
      }.toSet
      val eligible = raw.probeResults.filterNot(result => duplicateIds(result.id))
      val issuedRequests = issuedProbeRequests(raw.copy(probeResults = eligible))
      val admitted = List.newBuilder[ProbeResult]
      val rejected = List.newBuilder[ProbeAdmissionDiagnostic]

      raw.probeResults.foreach { result =>
        if duplicateIds(result.id) then
          rejected += rejectedDiagnostic(result, None, List("DUPLICATE_PROBE_RESULT_ID"))
        else
          issuedRequests.find(_.id == result.id) match
            case Some(request) if exactIssuedResult(request, result) =>
              admitted += canonicalResult(request, result)
            case Some(request) =>
              rejected += rejectedDiagnostic(result, Some(request), contractFailureReasons(request, result))
            case None if result.purpose.nonEmpty =>
              rejected += rejectedDiagnostic(result, None, List("PROBE_REQUEST_UNISSUED"))
            case None =>
              admitted += result
      }

      ProbeAuthorization(
        raw.copy(probeResults = admitted.result()),
        rejected.result().distinctBy(diagnostic => diagnostic.probeId -> diagnostic.reasonCodes)
      )

  private def canonicalResult(request: ProbeRequest, result: ProbeResult): ProbeResult =
    result.copy(
      id = request.id,
      fen = Some(request.fen),
      purpose = request.purpose,
      probedMove = request.candidateMove,
      objective = request.objective,
      seedId = request.seedId,
      requiredSignals = request.requiredSignals,
      horizon = request.horizon,
      candidateMove = request.candidateMove,
      opponentResourceMove = request.opponentResourceMove,
      depthFloor = request.depthFloor,
      variationHash = request.variationHash,
      engineConfigFingerprint = request.engineConfigFingerprint
    )

  private def contractFailureReasons(request: ProbeRequest, result: ProbeResult): List[String] =
    val validation = ProbeContractValidator.validateAgainstRequest(request, result)
    val explicitMismatches = List(
      Option.when(request.horizon.nonEmpty && result.horizon.isEmpty)("HORIZON_MISSING"),
      Option.when(request.horizon != result.horizon)("HORIZON_MISMATCH"),
      Option.when(request.opponentResourceMove.nonEmpty && result.opponentResourceMove.isEmpty)(
        "OPPONENT_RESOURCE_MISSING"
      ),
      Option.when(request.variationHash != result.variationHash)("VARIATION_HASH_MISMATCH")
    ).flatten
    (
      List("ISSUED_REQUEST_MISMATCH", "PROBE_CONTRACT_INVALID") ++
        validation.hardReasonCodes ++
        validation.softReasonCodes ++
        explicitMismatches
    ).distinct

  private def rejectedDiagnostic(
      result: ProbeResult,
      request: Option[ProbeRequest],
      reasons: List[String]
  ): ProbeAdmissionDiagnostic =
    ProbeAdmissionDiagnostic(
      probeId = Option(result.id).map(_.trim).filter(_.nonEmpty).getOrElse("probe-result"),
      status = ProbeAdmissionStatus.Rejected,
      reasonCodes = reasons.distinct,
      purpose = request.flatMap(_.purpose).orElse(result.purpose),
      candidateMove = request.flatMap(_.candidateMove).orElse(result.probedMove).orElse(result.candidateMove),
      fen = request.map(_.fen).orElse(result.fen),
      admittedLineCount = 0,
      legalLineCount = 0,
      scoredLineCount = result.replyLines.map(_.count(line => line.moves.nonEmpty && line.depth > 0)).getOrElse(0),
      depthFloor = request.flatMap(_.depthFloor),
      horizon = result.horizon,
      variationHash = result.variationHash
    )

private[assembly] object EndgameTablebaseProbePlanner:
  def fromAssembly(ctx: JudgmentAssemblyContext): List[ProbeRequest] =
    ctx.root.toList.flatMap { root =>
      ctx.evidenceGraph.records.flatMap {
        case EvidenceRecord(ref, lineFacts: LineFactEvidence, _) if ref.position == root && ref.line.exists(_.role != LineNodeRole.Threat) =>
          ref.line.toList.flatMap(lineRef => ctx.lines.find(_.ref == lineRef)).flatMap { line =>
            lineFacts.endgameTechniqueHorizons.flatMap(EndgameTablebaseProbeBinding.requestFor(line, lineFacts, _))
          }
        case _ =>
          Nil
      }
    }.distinctBy(_.id)

private[assembly] object BranchReplyProbePlanner:
  private val DiscoveryHorizonPly = 6
  private val DirectRootEventKinds = Set(
    LineEventKind.Capture,
    LineEventKind.Recapture,
    LineEventKind.Check,
    LineEventKind.Mate,
    LineEventKind.Castling,
    LineEventKind.Stalemate,
    LineEventKind.Promotion
  )

  def fromAssembly(ctx: JudgmentAssemblyContext): List[ProbeRequest] =
    ctx.root.toList.flatMap { root =>
      val unresolvedCausalHorizons =
        ctx.evidenceGraph.records.collect {
          case EvidenceRecord(_, event: PlanCausalEventEvidence, _)
              if event.counterfactualContinuationProven &&
                event.causalResultAssessments.nonEmpty &&
                !event.branchCoverageComplete =>
            EvidenceRef.normalizeMove(event.rootMove) -> event.requiredHorizonPlyOffset
        }.groupMapReduce(_._1)(_._2)(_.max(_))
      val discoveryHorizons = discoveryCausalHorizons(ctx)
      val requestedCausalHorizons =
        (unresolvedCausalHorizons.toList ++ discoveryHorizons.toList)
          .groupMapReduce(_._1)(_._2)(_.max(_))
      val discoveryOnlyMoves = discoveryHorizons.keySet -- unresolvedCausalHorizons.keySet
      val admittedProbeIds =
        ctx.probeDiagnostics.collect {
          case diagnostic if diagnostic.status == ProbeAdmissionStatus.Admitted => diagnostic.probeId
        }.toSet
      selectedRootLines(ctx.lines).filter(line =>
        requestedCausalHorizons.contains(EvidenceRef.normalizeMove(line.ref.rootMove))
      ).flatMap { line =>
        PrincipalVariationEvidence.legalFenAfter(root.fen, line.ref.rootMove).toList.flatMap { branchFen =>
          val requiredReplies = BranchReplyProbeBinding.requiredReplyCount(branchFen)
          val requiredHorizon = requestedCausalHorizons(EvidenceRef.normalizeMove(line.ref.rootMove)).max(1)
          Option.when(requiredReplies > 0)(
            ProbeRequest(
              id = s"${line.ref.id}:reply-multipv:ply-$requiredHorizon",
              fen = branchFen,
              moves = Nil,
              depth = BranchReplyProbeBinding.Depth,
              purpose = Some(ProbePurpose.ReplyMultipv),
              multiPv = Some(requiredReplies),
              baselineMove = Some(line.ref.rootMove),
              baselineEvalCp = Some(line.whitePovEvalCp),
              baselineMate = line.mate,
              baselineDepth = Some(line.depth).filter(_ > 0),
              objective = Some(BranchReplyProbeBinding.Objective),
              requiredSignals = BranchReplyProbeBinding.RequiredSignals,
              horizon = Some(BranchReplyProbeBinding.horizon(requiredHorizon)),
              candidateMove = Some(line.ref.rootMove),
              depthFloor = Some(BranchReplyProbeBinding.DepthFloor),
              variationHash = Some(BranchReplyProbeBinding.variationHash(root, line, requiredHorizon))
            )
          )
        }
      }.filterNot(request =>
        admittedProbeIds(request.id) &&
          request.candidateMove.exists(move => discoveryOnlyMoves(EvidenceRef.normalizeMove(move)))
      )
    }.distinctBy(_.id)

  private def discoveryCausalHorizons(ctx: JudgmentAssemblyContext): Map[String, Int] =
    ctx.lines
      .find(_.role == LineNodeRole.Played)
      .filter(needsPlanDiscovery(ctx, _))
      .map(line => EvidenceRef.normalizeMove(line.ref.rootMove) -> DiscoveryHorizonPly)
      .toMap

  private def needsPlanDiscovery(ctx: JudgmentAssemblyContext, line: CandidateLineNode): Boolean =
    val rootMove = line.ref.rootMove
    val hasPlanPressure =
      ctx.evidenceGraph.records.exists {
        case EvidenceRecord(ref, _: PlanPressureEvidence, _) => ref.line.contains(line.ref)
        case _                                               => false
      }
    val hasPublicPlanProof =
      ctx.evidenceGraph.records.exists {
        case EvidenceRecord(ref, event: PlanCausalEventEvidence, _)
            if ref.confidence != EvidenceConfidence.Heuristic &&
              EvidenceRef.sameMove(event.rootMove, rootMove) =>
          event.publicGoalProofReady(ctx.lines, ctx.evidenceGraph)
        case _ =>
          false
      }
    val hasConcreteRootEvent =
      ctx.evidenceGraph.records.exists {
        case EvidenceRecord(ref, lineFacts: LineFactEvidence, _) if ref.line.contains(line.ref) =>
          lineFacts.lineEvents.exists(event =>
            event.plyOffset == 0 &&
              EvidenceRef.sameMove(event.moveUci, rootMove) &&
              DirectRootEventKinds(event.kind)
          )
        case _ => false
      }
    hasPlanPressure && !hasPublicPlanProof && !hasConcreteRootEvent

  private[assembly] def selectedRootLines(lines: List[CandidateLineNode]): List[CandidateLineNode] =
    val rootLines = lines.filterNot(_.role == LineNodeRole.Threat)
    val primary =
      List(LineNodeRole.Played, LineNodeRole.BestReference).flatMap(role => rootLines.find(_.role == role))
    val alternatives =
      rootLines.filter(_.role == LineNodeRole.Alternative).sortBy(_.ref.rank).take(1)
    (primary ++ alternatives)
      .filter(_.line.moves.nonEmpty)
      .distinctBy(_.ref.rootMove)
      .take(BranchReplyProbeBinding.ReplyMultiPv)

private[assembly] object CounterResourceProbePlanner:
  private val MaxResources = 3

  private final case class Candidate(
      moveUci: String,
      directBreak: Boolean,
      preparesBreak: Boolean,
      touchedByRootActor: Boolean
  ):
    def relevant: Boolean = directBreak || preparesBreak || touchedByRootActor
    def priority: Int =
      if directBreak then 0
      else if preparesBreak then 1
      else 2

  def fromAssembly(ctx: JudgmentAssemblyContext): List[ProbeRequest] =
    ctx.root.toList.flatMap { root =>
      val breakFiles = counterBreakFiles(ctx, root)
      val rootLines = BranchReplyProbePlanner.selectedRootLines(ctx.lines).take(2)
      val candidatesByLine = rootLines.map(line => line -> candidatesAfter(root, line, breakFiles))
      val shared = candidatesByLine
        .flatMap { case (line, candidates) => candidates.map(candidate => candidate.moveUci -> (line -> candidate)) }
        .groupBy(_._1)
        .toList
        .map { case (move, entries) =>
          val lineCandidates = entries.map(_._2).distinctBy(_._1.ref.rootMove)
          move -> lineCandidates
        }
        .filter { case (_, lineCandidates) => lineCandidates.size >= 2 && lineCandidates.exists(_._2.relevant) }
        .sortBy { case (move, lineCandidates) =>
          (lineCandidates.map(_._2.priority).min, move)
        }
        .take(MaxResources)
      val admittedIds = ctx.probeDiagnostics.collect {
        case diagnostic if diagnostic.status == ProbeAdmissionStatus.Admitted => diagnostic.probeId
      }.toSet
      shared.flatMap { case (resourceMove, lineCandidates) =>
        lineCandidates.map { case (line, _) =>
          val branchFen = PrincipalVariationEvidence.legalFenAfter(root.fen, line.ref.rootMove).get
          val bindingHash = CounterResourceProbeBinding.variationHash(root, line, resourceMove)
          ProbeRequest(
            id = s"${line.ref.id}:opponent-resource:${resourceMove}:${bindingHash.take(16)}",
            fen = branchFen,
            moves = List(resourceMove),
            depth = BranchReplyProbeBinding.Depth,
            purpose = Some(ProbePurpose.ReplyMultipv),
            multiPv = Some(1),
            baselineMove = Some(line.ref.rootMove),
            baselineEvalCp = Some(line.whitePovEvalCp),
            baselineMate = line.mate,
            baselineDepth = Some(line.depth).filter(_ > 0),
            objective = Some(CounterResourceProbeBinding.Objective),
            requiredSignals = CounterResourceProbeBinding.RequiredSignals,
            candidateMove = Some(line.ref.rootMove),
            opponentResourceMove = Some(resourceMove),
            depthFloor = Some(BranchReplyProbeBinding.DepthFloor),
            variationHash = Some(bindingHash)
          )
        }
      }.filterNot(request => admittedIds(request.id))
    }.distinctBy(_.id)

  private def counterBreakFiles(ctx: JudgmentAssemblyContext, root: PositionNodeRef): Set[String] =
    ctx.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, payload: PawnStructureFactEvidence, _)
          if ref.position == root && ref.line.isEmpty =>
        payload.pawnPlay.toList.flatMap(_.counterBreakFiles)
    }.getOrElse(Nil).map(_.trim.toLowerCase).filter(_.matches("[a-h]")).toSet

  private def candidatesAfter(
      root: PositionNodeRef,
      line: CandidateLineNode,
      breakFiles: Set[String]
  ): List[Candidate] =
    for
      branchFen <- PrincipalVariationEvidence.legalFenAfter(root.fen, line.ref.rootMove).toList
      position <- _root_.chess.format.Fen
        .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(branchFen))
        .toList
      directTargets = position.board.byPiece(position.color, _root_.chess.Pawn).squares.flatMap { square =>
        val file = square.file.char.toString.toLowerCase
        val nextRank = square.rank.value + (if position.color.white then 1 else -1)
        Option
          .when(breakFiles(file))(_root_.chess.Square.at(square.file.value, nextRank))
          .flatten
          .filter(position.board.pieceAt(_).isEmpty)
      }.toSet
      move <- position.legalMoves.toList
      if move.piece.role == _root_.chess.Pawn && !move.captures && move.orig.file == move.dest.file
      file = move.orig.file.char.toString.toLowerCase
      direct = breakFiles(file)
      supportsBreak = directTargets.exists(target => move.dest.pawnAttacks(position.color).squares.contains(target))
      rootDestination = _root_.chess.Square.fromKey(EvidenceRef.normalizeMove(line.ref.rootMove).slice(2, 4))
      rootSide = !position.color
      touchedByRootActor = rootDestination.exists(destination =>
        position.board.attackers(move.orig, rootSide).squares.contains(destination) ||
          position.board.attackers(move.dest, rootSide).squares.contains(destination)
      )
    yield Candidate(
      moveUci = EvidenceRef.normalizeMove(move.toUci.uci),
      directBreak = direct,
      preparesBreak = supportsBreak,
      touchedByRootActor = touchedByRootActor
    )
