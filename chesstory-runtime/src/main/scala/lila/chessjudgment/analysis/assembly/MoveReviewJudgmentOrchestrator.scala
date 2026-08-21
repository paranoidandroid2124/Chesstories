package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.{
  BranchReplyProbeBinding,
  CounterResourceProbeBinding,
  ProbeAdmissionDiagnostic,
  ProbeAdmissionStatus,
  ProbeContractValidator,
  ProbePurpose,
  ProbeRequest,
  ProbeResolution,
  ProbeResult
}
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.judgment.*

object MoveReviewJudgmentOrchestrator:

  /** Packet construction together with the exact client reports admitted into it. */
  final case class ReviewExecution(
      packet: EvidenceBackedJudgmentPacket,
      admittedProbeResultIds: List[String]
  )

  private final case class ProbeAdmission(
      fulfilledProbeResults: List[(ProbeRequest, ProbeResult)],
      diagnostics: List[ProbeAdmissionDiagnostic]
  )

  /** Development-only compatibility entrypoint; player jobs enter through executePreparedReview. */
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
    executeCommon(preparedInput, admitPreparedProbeResults(fulfilledProbeResults))

  private def executeCommon(
      preparedInput: NormalizedMoveReviewInput,
      admission: ProbeAdmission
  ): Option[ReviewExecution] =
    for
      f <- assembledFactContext(preparedInput, admission.fulfilledProbeResults).map(RelativeAssessmentAssembler.enrichFacts)
      _ <- Option.when(
        evidenceClosed(f) &&
          factStagePayloadsClosed(f) &&
          EvidenceFactAssembler.exactStrategicMechanisms(f).nonEmpty &&
          RelativeAssessmentAssembler.exactStrategicContrasts(f).nonEmpty
      )(())
      c = RelativeAssessmentAssembler.enrichCauses(f)
      jp = JudgmentClaimAssembler.propose(c)
      ja = ClaimCandidateGraphAssembler.fromClaims(jp, c)
      r <- ClaimArbitrator.rankDetailed(ja, c.relativeAssessments)
      rankedContext = r.rankedClaims.foldLeft(c.copy(claims = Nil))((context, claim) => context.withClaim(claim))
      context = rankedContext.copy(
        probeDiagnostics = rankedContext.probeDiagnostics ++ admission.diagnostics
      )
      packet <- EvidenceBackedJudgmentPacket.fromAssembly(
        context,
        r.primary,
        pendingProbeRequests(context, admission.fulfilledProbeResults.map(_._1)),
        r.playerFacingClaimDecisions,
        r.onlyMoveConstraintResolutions,
        r.causeExposureResolution,
        r.causeDispositionLedger
      )
    yield ReviewExecution(packet, admission.fulfilledProbeResults.map(_._1.id))

  private def factStagePayloadsClosed(context: JudgmentAssemblyContext): Boolean =
    context.evidenceGraph.records.forall(record => !causeOwnedPayload(record.payload))

  private def causeOwnedPayload(payload: EvidencePayload): Boolean =
    payload match
      case _: RelativeCauseFactEvidence  => true
      case _: RelativeAssessmentEvidence => true
      case _                             => false

  private def evidenceClosed(context: JudgmentAssemblyContext): Boolean =
    val records = context.evidenceGraph.records
    val recordIds = records.map(_.ref.id)
    val recordsById = records.map(record => record.ref.id -> record).toMap
    val positionRefs = context.positions.map(_.ref).toSet
    val lineRefs = context.lines.map(_.ref).toSet
    def registered(ref: EvidenceRef): Boolean =
      recordsById.get(ref.id).exists(_.ref == ref)
    context.root.exists(root =>
      root.fen == context.input.beforeFen && root.ply == context.input.beforePly
    ) &&
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
      fulfilledProbeResults: List[(ProbeRequest, ProbeResult)]
  ): Option[JudgmentAssemblyContext] =
    NodeLineTransitionAssembler
      .assemble(MoveReviewInputNormalizer.withAdmittedProbeResults(preparedInput, fulfilledProbeResults))
      .map(EvidenceFactAssembler.enrich)

  private def pendingProbeRequests(
      context: JudgmentAssemblyContext,
      fulfilledRequests: List[ProbeRequest] = Nil
  ): List[ProbeRequest] =
    val fulfilled = fulfilledRequests.toSet
    (
      BranchReplyProbePlanner.fromAssembly(context) ++
        CounterResourceProbePlanner.fromAssembly(context)
    ).distinctBy(_.id).filterNot(fulfilled)

  private def pendingProbeRequestsForPreparedReview(
      preparedInput: NormalizedMoveReviewInput,
      fulfilledProbeResults: List[(ProbeRequest, ProbeResult)]
  ): List[ProbeRequest] =
    assembledFactContext(preparedInput, fulfilledProbeResults).toList.flatMap(context => pendingProbeRequests(context))

  /**
    * Replays the deterministic request/result exchange from an empty result set.
    * A later-round result becomes authoritative only after the preceding admitted
    * results make its exact request reachable.
    */
  private def replayIssuedProbeRequests(
      preparedInput: NormalizedMoveReviewInput,
      submittedProbeResults: List[ProbeResult]
  ): List[ProbeRequest] =
    if submittedProbeResults.isEmpty then Nil
    else
      @annotation.tailrec
      def loop(
          admitted: List[(ProbeRequest, ProbeResult)],
          issued: List[ProbeRequest],
          remainingRounds: Int
      ): List[ProbeRequest] =
        if remainingRounds <= 0 then issued
        else
          val roundRequests = pendingProbeRequestsForPreparedReview(preparedInput, admitted)
          val nextIssued = (issued ++ roundRequests).distinctBy(_.id)
          val nextAdmitted = submittedProbeResults.flatMap(result =>
            nextIssued
              .find(request => request.id == result.id && matchesIssuedRequest(request, result))
              .map(request => request -> canonicalizeIssuedResult(request, result))
          )
          if nextIssued == issued && nextAdmitted == admitted then nextIssued
          else loop(nextAdmitted, nextIssued, remainingRounds - 1)

      loop(Nil, Nil, submittedProbeResults.size + 1)

  private def matchesIssuedRequest(request: ProbeRequest, result: ProbeResult): Boolean =
    val validation = ProbeContractValidator.validateAgainstRequest(request, result)
    request.variationHash == result.variationHash &&
      request.horizon == result.horizon &&
      validation.isValid &&
      validation.softReasonCodes.isEmpty

  private def admitIssuedProbeResults(
      preparedInput: NormalizedMoveReviewInput,
      submittedProbeResults: List[ProbeResult]
  ): ProbeAdmission =
    if submittedProbeResults.isEmpty then ProbeAdmission(Nil, Nil)
    else
      val duplicateIds = submittedProbeResults.groupMapReduce(_.id)(_ => 1)(_ + _).collect {
        case (id, count) if count > 1 => id
      }.toSet
      val eligible = submittedProbeResults.filterNot(result => duplicateIds(result.id))
      val issuedRequests = replayIssuedProbeRequests(preparedInput, eligible)
      val admitted = List.newBuilder[(ProbeRequest, ProbeResult)]
      val rejected = List.newBuilder[ProbeAdmissionDiagnostic]

      submittedProbeResults.foreach { result =>
        if duplicateIds(result.id) then
          rejected += rejectedDiagnostic(result, None, List("DUPLICATE_PROBE_RESULT_ID"))
        else
          issuedRequests.find(_.id == result.id) match
            case Some(request) if matchesIssuedRequest(request, result) =>
              admitted += request -> canonicalizeIssuedResult(request, result)
            case Some(request) =>
              rejected += rejectedDiagnostic(result, Some(request), contractFailureReasons(request, result))
            case None =>
              rejected += rejectedDiagnostic(result, None, List("PROBE_REQUEST_UNISSUED"))
      }

      val admittedProbeResults = admitted.result()

      ProbeAdmission(
        admittedProbeResults,
        rejected.result().distinctBy(diagnostic => diagnostic.probeId -> diagnostic.reasonCodes)
      )

  private def admitPreparedProbeResults(
      fulfilledProbeResults: List[(ProbeRequest, ProbeResult)]
  ): ProbeAdmission =
    require(
      fulfilledProbeResults.map(_._1.id).distinct.size == fulfilledProbeResults.size &&
        fulfilledProbeResults.map(_._2.id).distinct.size == fulfilledProbeResults.size,
      "trusted player probe handoff has duplicate request or result ids"
    )
    fulfilledProbeResults.foreach { case (request, result) =>
      val expectedMoves = request.moves.map(PrincipalVariationEvidence.normalizeUci)
      val resolutionMatchesRequest = result.resolution match
        case ProbeResolution.EngineSearch(evaluations, depth) =>
            request.multiPv.contains(evaluations.size) &&
            evaluations.nonEmpty &&
            depth >= request.depth &&
            request.depthFloor.forall(floor => depth >= floor) &&
            evaluations.forall(evaluation =>
              evaluation.moves.nonEmpty &&
                evaluation.moves.take(expectedMoves.size) == expectedMoves
            )
        case ProbeResolution.ExactAutomaticTerminal(evaluation) =>
          evaluation.moves.isEmpty || evaluation.moves.take(expectedMoves.size) == expectedMoves
      val metadataMatches =
        request.id == result.id &&
          result.fen.contains(request.fen) &&
          request.purpose == result.purpose &&
          request.objective == result.objective &&
          request.requiredSignals == result.requiredSignals &&
          request.horizon == result.horizon &&
          request.candidateMove == result.probedMove &&
          request.candidateMove == result.candidateMove &&
          request.opponentResourceMove == result.opponentResourceMove &&
          request.variationHash == result.variationHash
      val contract = ProbeContractValidator.validateAgainstRequest(request, result)
      require(
        metadataMatches && resolutionMatchesRequest && contract.isValid && contract.softReasonCodes.isEmpty,
        s"trusted player probe handoff mismatch for ${request.id}"
      )
    }
    ProbeAdmission(fulfilledProbeResults, Nil)

  private def canonicalizeIssuedResult(request: ProbeRequest, result: ProbeResult): ProbeResult =
    result.copy(
      id = request.id,
      fen = Some(request.fen),
      purpose = request.purpose,
      probedMove = request.candidateMove,
      objective = request.objective,
      requiredSignals = request.requiredSignals,
      horizon = request.horizon,
      candidateMove = request.candidateMove,
      opponentResourceMove = request.opponentResourceMove,
      variationHash = request.variationHash
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
      scoredLineCount = (result.resolution match
        case ProbeResolution.EngineSearch(evaluations, _) =>
          evaluations.count(_.moves.nonEmpty)
        case ProbeResolution.ExactAutomaticTerminal(_) => 0),
      depthFloor = request.flatMap(_.depthFloor),
      horizon = result.horizon,
      variationHash = result.variationHash
    )

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
      selectedRootLines(ctx.lines).filter(line =>
        requestedCausalHorizons.contains(EvidenceRef.normalizeMove(line.ref.rootMove))
      ).flatMap { line =>
        line.evaluation.engineLine.toList.flatMap { engineLine =>
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
              objective = Some(BranchReplyProbeBinding.Objective),
              requiredSignals = BranchReplyProbeBinding.RequiredSignals,
              horizon = Some(BranchReplyProbeBinding.horizon(requiredHorizon)),
              candidateMove = Some(line.ref.rootMove),
              depthFloor = Some(BranchReplyProbeBinding.DepthFloor),
              variationHash = Some(
                BranchReplyProbeBinding.variationHash(
                  rootFen = root.fen,
                  role = line.ref.role,
                  rootMove = line.ref.rootMove,
                  whitePovEvalCp = engineLine.scoreCp,
                  mate = engineLine.mate,
                  depth = engineLine.depth,
                  moves = engineLine.moves,
                  certifiedHorizonPlyOffset = requiredHorizon
                )
              )
            )
          )
          }
        }
      }
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
      .filter(line => line.evaluation.engineLine.exists(_.moves.nonEmpty))
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
      shared.flatMap { case (resourceMove, lineCandidates) =>
        lineCandidates.flatMap { case (line, _) =>
          line.evaluation.engineLine.toList.map { engineLine =>
          val branchFen = PrincipalVariationEvidence.legalFenAfter(root.fen, line.ref.rootMove).get
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
            id = s"${line.ref.id}:opponent-resource:${resourceMove}:${bindingHash.take(16)}",
            fen = branchFen,
            moves = List(resourceMove),
            depth = BranchReplyProbeBinding.Depth,
            purpose = Some(ProbePurpose.ReplyMultipv),
            multiPv = Some(1),
            objective = Some(CounterResourceProbeBinding.Objective),
            requiredSignals = CounterResourceProbeBinding.RequiredSignals,
            candidateMove = Some(line.ref.rootMove),
            opponentResourceMove = Some(resourceMove),
            depthFloor = Some(BranchReplyProbeBinding.DepthFloor),
            variationHash = Some(bindingHash)
          )
          }
        }
      }
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
      if move.after.legalMoves.nonEmpty
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
