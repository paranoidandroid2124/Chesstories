package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.analysis.qc.JudgmentQualityReport
import lila.chessjudgment.model.{
  ProbeAdmissionDiagnostic,
  ProbeAdmissionStatus,
  ProbeContractValidator,
  ProbeRequest,
  ProbeResult
}
import lila.chessjudgment.model.judgment.*

final case class MoveReviewJudgmentResult(
    context: JudgmentAssemblyContext,
    packet: EvidenceBackedJudgmentPacket,
    validation: JudgmentPacketValidationResult,
    quality: JudgmentQualityReport
):
  def isValid: Boolean = validation.isValid

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
    assemble(raw).flatMap(JudgmentPacketBuilder.fromAssembly)

  def build(raw: RawMoveReviewInput): Option[MoveReviewJudgmentResult] =
    assemble(raw).flatMap { context =>
      JudgmentPacketBuilder.fromAssembly(context).map { packet =>
        val validation = JudgmentPacketValidator.validate(packet)
        MoveReviewJudgmentResult(
          context = context,
          packet = packet,
          validation = validation,
          quality = JudgmentQualityReport.fromPacket(packet, validation)
        )
      }
    }

  private def assembleUnchecked(raw: RawMoveReviewInput): Option[JudgmentAssemblyContext] =
    ClaimSeedAssembler.assemble(raw).map(_.context)

  private def probeRequestsUnchecked(raw: RawMoveReviewInput): List[ProbeRequest] =
    EvidenceFactAssembler.assemble(raw).toList.flatMap { assembly =>
      val context = assembly.context
      (
        BranchReplyProbePlanner.fromAssembly(context) ++
          CounterResourceProbePlanner.fromAssembly(context) ++
          EndgameTablebaseProbePlanner.fromAssembly(context)
      ).distinctBy(_.id)
    }

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
