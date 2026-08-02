package lila.chessjudgment.analysis.assembly
import scala.util.Try

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
      diagnostics: List[ProbeAdmissionDiagnostic],
      fulfilledRequests: List[ProbeRequest]
  )

  def execute(
      raw: RawMoveReviewInput,
      intervention: JudgmentBoundaryIntervention
  ): Option[JudgmentBoundaryExecution] =
    runStages(raw, intervention)

  /** Sole execution authority for F -> C -> Jp -> Ja -> R.
    * Native stages retain their exception behavior; only supplied boundary callbacks
    * are isolated as fallible interventions.
    */
  private def runStages(
      raw: RawMoveReviewInput,
      intervention: JudgmentBoundaryIntervention
  ): Option[JudgmentBoundaryExecution] =
    val identity = isIdentity(intervention)
    for
      qCandidate <- intervention.q match
        case None => Some(raw)
        case Some(provider) =>
          Try(provider(JudgmentQuestionInput.fromRaw(raw))).toOption.flatMap(Option(_))
      _ <- Option.when(identity || questionPreserved(raw, qCandidate))(())
      authorization = authorizeProbeResults(qCandidate)
      q = authorization.input
      normalized <-
        if identity then Some(Option.empty[NormalizedMoveReviewInput])
        else MoveReviewInputNormalizer.normalize(q).map(Some(_))
      fCandidate <- intervention.f match
        case None => EvidenceFactAssembler.assemble(q).map(RelativeAssessmentAssembler.enrichFacts)
        case Some(provider) => Try(provider(q)).toOption.flatMap(Option(_))
      f <- Option.when(
        identity || normalized.exists(expected =>
          factStageContextClosed(expected, fCandidate) && factStagePayloadsClosed(fCandidate)
        )
      )(fCandidate)
      cCandidate <- intervention.c match
        case None => Some(RelativeAssessmentAssembler.enrichCauses(f))
        case Some(provider) => Try(provider(f)).toOption.flatMap(Option(_))
      c <- Option.when(
        identity ||
          (upstreamStagePreserved(f, cCandidate) &&
            cCandidate.claims.isEmpty &&
            evidenceClosed(cCandidate) &&
            causeStageExtensionsClosed(f, cCandidate))
      )(cCandidate)
      jpCandidate <- intervention.jp match
        case None => Some(JudgmentClaimAssembler.propose(c))
        case Some(provider) => Try(provider(c)).toOption.flatMap(Option(_))
      jp <- Option.when(
        identity || jpCandidate.map(_.id).distinct.size == jpCandidate.size
      )(jpCandidate)
      jaCandidate <- intervention.ja match
        case None => Some(ClaimCandidateGraphAssembler.fromClaims(jp, c.evidenceGraph))
        case Some(provider) => Try(provider(c, jp)).toOption.flatMap(Option(_))
      ja <- Option.when(identity || admissionClosed(c, jp, jaCandidate))(jaCandidate)
      r <- intervention.r match
        case None => Some(ClaimArbitrator.rankDetailed(ja, c.relativeAssessments))
        case Some(provider) => Try(provider(c, ja)).toOption.flatMap(Option(_))
      rankedContext = r.rankedClaims.foldLeft(c.copy(claims = Nil))((context, claim) => context.withClaim(claim))
    yield
      val context = rankedContext.copy(
        probeDiagnostics = rankedContext.probeDiagnostics ++ authorization.diagnostics
      )
      val packet =
        if identity then
          EvidenceBackedJudgmentPacket.fromAssembly(
            context,
            plannedProbeRequests(context, authorization.fulfilledRequests),
            r.playerFacingClaimDecisions,
            r.onlyMoveConstraintResolutions,
            r.causeExposureResolution,
            r.causeDispositionLedger,
            r.selectedContentClaimIds
          )
        else
          Option
            .when(boundaryClosed(f, c, jp, ja, r))(context)
            .flatMap(packetFromClosedAssembly(_, authorization.fulfilledRequests, r))
      JudgmentBoundaryExecution(q, f, c, jp, ja, r, context, packet)

  private def isIdentity(intervention: JudgmentBoundaryIntervention): Boolean =
    intervention.q.isEmpty &&
      intervention.f.isEmpty &&
      intervention.c.isEmpty &&
      intervention.jp.isEmpty &&
      intervention.ja.isEmpty &&
      intervention.r.isEmpty

  private def packetFromClosedAssembly(
      context: JudgmentAssemblyContext,
      fulfilledRequests: List[ProbeRequest],
      ranking: ClaimRankingResult
  ): Option[EvidenceBackedJudgmentPacket] =
    EvidenceBackedJudgmentPacket
      .fromAssembly(
        context,
        Nil,
        ranking.playerFacingClaimDecisions,
        ranking.onlyMoveConstraintResolutions,
        ranking.causeExposureResolution,
        ranking.causeDispositionLedger,
        ranking.selectedContentClaimIds
      )
      .flatMap(_ =>
        EvidenceBackedJudgmentPacket.fromAssembly(
          context,
          plannedProbeRequests(context, fulfilledRequests),
          ranking.playerFacingClaimDecisions,
          ranking.onlyMoveConstraintResolutions,
          ranking.causeExposureResolution,
          ranking.causeDispositionLedger,
          ranking.selectedContentClaimIds
        )
      )

  private def boundaryClosed(
      f: JudgmentAssemblyContext,
      c: JudgmentAssemblyContext,
      jp: List[JudgmentClaim],
      ja: ClaimCandidateGraph,
      r: ClaimRankingResult
  ): Boolean =
    val admittedClaims = ja.decisions.map(_.claim)
    f.claims.isEmpty &&
      upstreamStagePreserved(f, c) &&
      c.claims.isEmpty &&
      jp.map(_.id).distinct.size == jp.size &&
      admittedClaims == jp &&
      ja.evidenceGraph == c.evidenceGraph &&
      rankingClosed(ja, r)

  private def rankingClosed(
      graph: ClaimCandidateGraph,
      ranking: ClaimRankingResult
  ): Boolean =
    val certifiedIds = graph.certified.map(_.claim.id).toSet
    val rankedIds = ranking.ranked.map(_.claim.id)
    val rankedIdSet = rankedIds.toSet
    val trace = ranking.deduplicationTrace
    val traceOriginalIds = trace.map(_.originalClaimId)
    val traceByOriginal = trace.map(item => item.originalClaimId -> item.keptClaimId).toMap
    val lineageIds = certifiedIds ++ rankedIdSet
    val expectedDeduplication =
      ClaimDeduplicator.deduplicateDetailed(graph.certified, graph.evidenceGraph)
    val expectedPlayedMoves = graph.evidenceGraph.records.collect {
      case EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) =>
        JudgmentSubjectBinding.normalizeMove(assessment.played.moveUci)
    }.filter(_.nonEmpty).toSet
    val expectedExposure = PlayerFacingCauseExposurePipeline.resolve(
      expectedDeduplication.decisions.map(_.claim),
      graph.evidenceGraph,
      expectedPlayedMoves
    )
    val expectedHostRows = ClaimArbitrator.canonicalHostRows(
      expectedDeduplication.decisions.map(_.claim),
      expectedExposure,
      graph.evidenceGraph,
      expectedPlayedMoves
    )
    val expectedCauseDispositionLedger =
      CauseDispositionPolicy.resolve(graph, expectedDeduplication, expectedExposure)
    val expectedContentClaimIds =
      ClaimArbitrator.contentClaimIds(expectedDeduplication.decisions)
    val causeDominance = ranking.causeDominanceDecisions
    val causeDominanceById = causeDominance.map(decision => decision.causeEvidenceId -> decision).toMap
    val retainedCauseIds = expectedExposure.retainedCauseEvidenceIds
    val crossComparisonExposure = ranking.crossComparisonExposureDecisions
    val crossComparisonExposureById =
      crossComparisonExposure.map(decision => decision.causeEvidenceId -> decision).toMap
    val selectedCauseIds = expectedExposure.selectedCauseEvidenceIds
    val rankedFacingCauseIds = ranking.ranked.flatMap(_.playerFacingCauseEvidenceIds).toSet

    def registeredRelativeCause(id: String): Boolean =
      graph.evidenceGraph.byId.get(id).exists {
        case EvidenceRecord(_, RelativeCauseFactEvidence(_), _) => true
        case _                                                  => false
      }

    val exposureResolutionClosed =
      ranking.causeExposureResolution == expectedExposure &&
        ranking.causeDominanceDecisions == expectedExposure.dominanceDecisions &&
        ranking.crossComparisonExposureDecisions == expectedExposure.crossDecisions

    val causeDominanceClosed =
      causeDominance.map(_.causeEvidenceId).distinct.size == causeDominance.size &&
        causeDominance == expectedExposure.dominanceDecisions &&
        causeDominance.forall { decision =>
          val dominators = decision.dominatingCauseEvidenceIds
          registeredRelativeCause(decision.causeEvidenceId) &&
            dominators.distinct.size == dominators.size &&
            !dominators.contains(decision.causeEvidenceId) &&
            (
              if decision.retained then dominators.isEmpty
              else
                dominators.nonEmpty && dominators.forall(id =>
                  causeDominanceById.get(id).exists(_.retained)
                )
            )
        }

    val retainedCauses = expectedExposure.readyByClaim.values.flatten.toList
      .distinctBy(_._2.id)
      .filter { case (_, ref) => retainedCauseIds(ref.id) }
    val crossComparisonExposureClosed =
      crossComparisonExposure.map(_.causeEvidenceId).distinct.size == crossComparisonExposure.size &&
        crossComparisonExposure.map(_.causeEvidenceId).toSet == retainedCauseIds &&
        crossComparisonExposure == expectedExposure.crossDecisions &&
        crossComparisonExposure.forall { decision =>
          crossComparisonExposureById.get(decision.representativeCauseEvidenceId).exists { representative =>
            if decision.selected then
              representative.causeEvidenceId == decision.causeEvidenceId && representative.selected
            else if decision.status == CrossComparisonExposureStatus.RedundantAcrossComparison then
              representative.selected && representative.causeEvidenceId != decision.causeEvidenceId
            else representative.causeEvidenceId == decision.causeEvidenceId
          }
        } &&
        rankedFacingCauseIds == selectedCauseIds

    val selectedCauseHostsClosed = selectedCauseIds.forall { causeId =>
      val hosts = ranking.ranked.filter(_.playerFacingCauseEvidenceIds.contains(causeId))
      hosts.size == 1 &&
        expectedExposure.ownerClaimIdByCauseId.get(causeId).contains(hosts.head.claim.id)
    } && expectedExposure.ownerClaimIdByCauseId.keySet == selectedCauseIds

    val selectedCauses = retainedCauses.filter { case (_, ref) => selectedCauseIds(ref.id) }
    val expectedOnlyMoveResolutions =
      OnlyMoveConstraintPolicy.resolveAll(
        graph.evidenceGraph,
        selectedCauses,
        expectedPlayedMoves
      )
    val onlyMoveClosed =
      ranking.onlyMoveConstraintResolutions == expectedOnlyMoveResolutions &&
        ranking.ranked.forall { decision =>
          val expected = expectedOnlyMoveResolutions
            .flatMap(_.qualifiers)
            .filter(item => decision.playerFacingCauseEvidenceIds.contains(item.causeEvidence.id))
            .distinctBy(item => (item.comparisonEvidence.id, item.causeEvidence.id))
          decision.onlyMoveQualifiers == expected
        }

    val rankedExposureClosed = ranking.ranked.forall { decision =>
      val causeIds = decision.playerFacingCauseEvidenceIds
      val directEvidenceIds = decision.claim.evidence.map(_.id).toSet
      causeIds.distinct.size == causeIds.size &&
        causeIds.forall(id =>
          directEvidenceIds(id) && selectedCauseIds(id) && registeredRelativeCause(id)
        )
    }

    def resolvesToRanked(claimId: String, visited: Set[String]): Boolean =
      if rankedIdSet(claimId) then true
      else if visited(claimId) then false
      else
        traceByOriginal
          .get(claimId)
          .exists(nextId => resolvesToRanked(nextId, visited + claimId))

    rankedIds.distinct.size == rankedIds.size &&
      rankedIdSet.subsetOf(certifiedIds) &&
      exposureResolutionClosed &&
      ranking.causeDispositionLedger == expectedCauseDispositionLedger &&
      ranking.selectedContentClaimIds == expectedContentClaimIds &&
      causeDominanceClosed &&
      crossComparisonExposureClosed &&
      selectedCauseHostsClosed &&
      onlyMoveClosed &&
      rankedExposureClosed &&
      ranking.ranked == expectedHostRows &&
      trace.map(_.order) == trace.indices.toList &&
      trace == expectedDeduplication.trace &&
      traceOriginalIds.distinct.size == traceOriginalIds.size &&
      trace.forall(item =>
        item.originalClaimId != item.keptClaimId &&
          lineageIds(item.originalClaimId) &&
          lineageIds(item.keptClaimId)
      ) &&
      rankedIdSet.intersect(traceByOriginal.keySet).isEmpty &&
      certifiedIds == rankedIdSet ++ traceByOriginal.keySet &&
      certifiedIds.forall(claimId => resolvesToRanked(claimId, Set.empty))

  private def factStageContextClosed(
      expectedInput: NormalizedMoveReviewInput,
      actual: JudgmentAssemblyContext
  ): Boolean =
    actual.input == expectedInput &&
      actual.claims.isEmpty &&
      evidenceClosed(actual)

  private def questionPreserved(
      expected: RawMoveReviewInput,
      actual: RawMoveReviewInput
  ): Boolean =
    actual.fen == expected.fen &&
      actual.playedMoveUci == expected.playedMoveUci &&
      actual.ply == expected.ply &&
      actual.openingContext == expected.openingContext &&
      actual.movePrefixUci == expected.movePrefixUci

  private def upstreamStagePreserved(
      upstream: JudgmentAssemblyContext,
      downstream: JudgmentAssemblyContext
  ): Boolean =
    sameStructuralFrame(upstream, downstream) &&
      downstream.probeDiagnostics == upstream.probeDiagnostics &&
      upstream.evidenceGraph.records.forall(record =>
        downstream.evidenceGraph.byId.get(record.ref.id).contains(record)
      )

  private def factStagePayloadsClosed(context: JudgmentAssemblyContext): Boolean =
    context.evidenceGraph.records.forall(record => !causeOwnedPayload(record.payload))

  private def causeStageExtensionsClosed(
      upstream: JudgmentAssemblyContext,
      downstream: JudgmentAssemblyContext
  ): Boolean =
    val upstreamIds = upstream.evidenceGraph.byId.keySet
    downstream.evidenceGraph.records
      .filterNot(record => upstreamIds(record.ref.id))
      .forall(record => causeOwnedPayload(record.payload))

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

  private def admissionClosed(
      context: JudgmentAssemblyContext,
      proposals: List[JudgmentClaim],
      graph: ClaimCandidateGraph
  ): Boolean =
    graph.evidenceGraph == context.evidenceGraph &&
      graph.decisions.map(_.claim) == proposals

  private def sameStructuralFrame(
      expected: JudgmentAssemblyContext,
      actual: JudgmentAssemblyContext
  ): Boolean =
    actual.input == expected.input &&
      actual.root == expected.root &&
      actual.positions == expected.positions &&
      actual.lines == expected.lines &&
      actual.transitions == expected.transitions

  private def probeRequestsUnchecked(raw: RawMoveReviewInput): List[ProbeRequest] =
    EvidenceFactAssembler.assemble(raw).toList.flatMap(context => plannedProbeRequests(context))

  private def plannedProbeRequests(
      context: JudgmentAssemblyContext,
      fulfilledRequests: List[ProbeRequest] = Nil
  ): List[ProbeRequest] =
    val fulfilled = fulfilledRequests.toSet
    (
      BranchReplyProbePlanner.fromAssembly(context) ++
        CounterResourceProbePlanner.fromAssembly(context) ++
        EndgameTablebaseProbePlanner.fromAssembly(context)
    ).distinctBy(_.id).filterNot(fulfilled)

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
    if raw.probeResults.isEmpty then ProbeAuthorization(raw, Nil, Nil)
    else
      val duplicateIds = raw.probeResults.groupMapReduce(_.id)(_ => 1)(_ + _).collect {
        case (id, count) if count > 1 => id
      }.toSet
      val eligible = raw.probeResults.filterNot(result => duplicateIds(result.id))
      val issuedRequests = issuedProbeRequests(raw.copy(probeResults = eligible))
      val admitted = List.newBuilder[ProbeResult]
      val rejected = List.newBuilder[ProbeAdmissionDiagnostic]
      val fulfilled = List.newBuilder[ProbeRequest]

      raw.probeResults.foreach { result =>
        if duplicateIds(result.id) then
          rejected += rejectedDiagnostic(result, None, List("DUPLICATE_PROBE_RESULT_ID"))
        else
          issuedRequests.find(_.id == result.id) match
            case Some(request) if exactIssuedResult(request, result) =>
              admitted += canonicalResult(request, result)
              fulfilled += request
            case Some(request) =>
              rejected += rejectedDiagnostic(result, Some(request), contractFailureReasons(request, result))
            case None if result.purpose.nonEmpty =>
              rejected += rejectedDiagnostic(result, None, List("PROBE_REQUEST_UNISSUED"))
            case None =>
              admitted += result
      }

      ProbeAuthorization(
        raw.copy(probeResults = admitted.result()),
        rejected.result().distinctBy(diagnostic => diagnostic.probeId -> diagnostic.reasonCodes),
        fulfilled.result().distinct
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
