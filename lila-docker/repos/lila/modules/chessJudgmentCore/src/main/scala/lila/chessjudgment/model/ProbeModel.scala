package lila.chessjudgment.model

import _root_.chess.format.Fen
import _root_.chess.variant.Standard
import lila.chessjudgment.model.strategic.VariationLine
import play.api.libs.json._

enum ProbePurpose(val key: String):
  // ReplyMultipv is the only purpose with an admitted branch-ingestion contract today.
  case ReplyMultipv extends ProbePurpose("reply_multipv")
  case DefenseReplyMultipv extends ProbePurpose("defense_reply_multipv")
  case ConvertReplyMultipv extends ProbePurpose("convert_reply_multipv")
  case RecaptureBranches extends ProbePurpose("recapture_branches")
  case KeepTensionBranches extends ProbePurpose("keep_tension_branches")
  case FreeTempoBranches extends ProbePurpose("free_tempo_branches")

object ProbePurpose:
  private val byKey = ProbePurpose.values.map(p => p.key -> p).toMap
  def fromKey(key: String): Option[ProbePurpose] =
    Option(key).map(_.trim).filter(_.nonEmpty).flatMap(byKey.get)
  def isBranchReply(purpose: ProbePurpose): Boolean =
    purpose == ProbePurpose.ReplyMultipv

  given Reads[ProbePurpose] = Reads:
    case JsString(raw) =>
      fromKey(raw).fold[JsResult[ProbePurpose]](JsError(s"Unknown probe purpose: $raw"))(JsSuccess(_))
    case _ => JsError("Probe purpose must be a string id")

  given Writes[ProbePurpose] = Writes(p => JsString(p.key))

/**
 * Request for client-side engine probing.
 * The purpose identifies whether a returned probe result can enter a certified graph branch.
 */
case class ProbeRequest(
  id: String,
  fen: String,
  moves: List[String], // UCI format moves to probe (e.g. "e2e4")
  depth: Int,          // Target depth for the WASM engine
  // Optional metadata for diagnostics and probe contracts
  purpose: Option[ProbePurpose] = None,
  multiPv: Option[Int] = None,
  planId: Option[String] = None,
  planScore: Option[Double] = None,
  // Optional baseline line context so the probe can be self-contained
  baselineMove: Option[String] = None,
  baselineEvalCp: Option[Int] = None, // External probe contract; white POV centipawns.
  baselineMate: Option[Int] = None,
  baselineDepth: Option[Int] = None,
  // Objective-driven probing contract
  objective: Option[String] = None,        // e.g. "branch_reply_multipv", "compare_branches"
  seedId: Option[String] = None,           // Latent seed identifier when relevant
  requiredSignals: List[String] = Nil,
  horizon: Option[String] = None,          // "short" | "medium" | "long"
  candidateMove: Option[String] = None,    // explicit root move when the request is move-bound
  depthFloor: Option[Int] = None,          // minimum acceptable realized depth for certification
  variationHash: Option[String] = None,    // binds the request to a specific logical variation bundle
  engineConfigFingerprint: Option[String] = None // binds the request to engine/config generation
)

object ProbeRequest:
  given Reads[ProbeRequest] = Json.reads[ProbeRequest]
  given Writes[ProbeRequest] = Json.writes[ProbeRequest]

/**
 * Raw engine evidence returned by the client.
 */
case class ProbeResult(
  id: String,
  fen: Option[String] = None, // Base FEN the probe was run from (critical when probing non-root branches)
  evalCp: Int,               // White POV centipawns (same convention as PlanInteractionContext.whitePovEvalCp)
  replyLines: Option[List[VariationLine]] = None,
  deltaVsBaseline: Int,      // Probe white POV eval - baselineEvalCp. Negative = worse than baseline.
  // Optional metadata that keeps branch probes self-describing.
  purpose: Option[ProbePurpose] = None,
  probedMove: Option[String] = None, // The probed candidate move (UCI)
  mate: Option[Int] = None,          // Mate distance if applicable
  depth: Option[Int] = None,         // Depth reached by the client engine
  // Optional contract diagnostics.
  objective: Option[String] = None,
  seedId: Option[String] = None,
  requiredSignals: List[String] = Nil,
  candidateMove: Option[String] = None,
  depthFloor: Option[Int] = None,
  variationHash: Option[String] = None,
  engineConfigFingerprint: Option[String] = None,
  generatedAtEpochMs: Option[Long] = None
)

object ProbeResult:
  given Reads[ProbeResult] = Json.reads[ProbeResult]
  given Writes[ProbeResult] = Json.writes[ProbeResult]

enum ProbeAdmissionStatus:
  case Admitted
  case Rejected
  case Ignored

object ProbeAdmissionStatus:
  given Writes[ProbeAdmissionStatus] = Writes(status => JsString(status.toString))

case class ProbeAdmissionDiagnostic(
  probeId: String,
  status: ProbeAdmissionStatus,
  reasonCodes: List[String],
  purpose: Option[ProbePurpose] = None,
  candidateMove: Option[String] = None,
  fen: Option[String] = None,
  admittedLineCount: Int = 0,
  legalLineCount: Int = 0,
  scoredLineCount: Int = 0,
  depthFloor: Option[Int] = None,
  variationHash: Option[String] = None
)

object ProbeAdmissionDiagnostic:
  given Writes[ProbeAdmissionDiagnostic] = Json.writes[ProbeAdmissionDiagnostic]

/**
 * Purpose-aware probe contract validator.
 * Fail-closed: if required signals are missing for a purpose, the probe should
 * not be used as certified evidence.
 */
object ProbeContractValidator:

  private val DefaultBranchReplyMultiPv = 3

  enum ProbeCertificateStatus:
    case Valid
    case WeaklyValid
    case Invalid
    case StaleOrMismatched

  case class ValidationResult(
      isValid: Boolean,
      missingSignals: List[String],
      reasonCodes: List[String],
      certificateStatus: ProbeCertificateStatus = ProbeCertificateStatus.Valid,
      hardReasonCodes: List[String] = Nil,
      softReasonCodes: List[String] = Nil
  )

  def validateAgainstRequest(
      request: ProbeRequest,
      result: ProbeResult
  ): ValidationResult =
    val fromRequest = request.requiredSignals.toSet
    val requestPurpose = request.purpose
    val requestPurposeSignals = requestPurpose.map(purposeRequiredSignals).getOrElse(Set.empty)
    val resultPurposeSignals =
      result.purpose.map(purposeRequiredSignals).getOrElse(Set.empty)
    val required =
      if fromRequest.nonEmpty then fromRequest
      else if requestPurpose.nonEmpty then requestPurposeSignals
      else resultPurposeSignals
    val purposeContractMissing =
      requestPurpose.exists(_ => requestPurposeSignals.isEmpty) ||
        (fromRequest.isEmpty && requestPurpose.isEmpty && resultPurposeSignals.isEmpty)
    val base = validateSignals(result, required)
    val purposeMismatch =
      request.purpose.flatMap(rp => result.purpose.map(_ != rp)).contains(true)
    val idMismatch = request.id != result.id
    val resultFen = result.fen.map(_.trim).filter(_.nonEmpty)
    val requestFen = Option(request.fen).map(_.trim).filter(_.nonEmpty)
    val fenMissing =
      requestFen.nonEmpty && resultFen.isEmpty
    val fenMismatch =
      requestFen.exists(expected => resultFen.exists(_ != expected))
    val requestFenInvalid =
      requestFen.exists(fen => Fen.read(Standard, Fen.Full(fen)).isEmpty)
    val resultFenInvalid =
      resultFen.exists(fen => Fen.read(Standard, Fen.Full(fen)).isEmpty)
    val objectiveMismatch =
      request.objective.flatMap(expected => result.objective.map(_ != expected)).contains(true)
    val seedMismatch =
      request.seedId.flatMap(expected => result.seedId.map(_ != expected)).contains(true)
    val requestMoves =
      request.moves.map(_.trim).filter(_.nonEmpty)
    val allowedMoves =
      (request.candidateMove.map(_.trim).filter(_.nonEmpty).toList ++ requestMoves).distinct
    val resultMove =
      result.probedMove
        .orElse(result.candidateMove)
        .map(_.trim)
        .filter(_.nonEmpty)
    val moveMissing =
      allowedMoves.nonEmpty && resultMove.isEmpty
    val moveMismatch =
      resultMove.exists(move => allowedMoves.nonEmpty && !allowedMoves.contains(move))
    val requestMoveInvalid =
      allowedMoves.exists(move => !validUciMove(move))
    val resultMoveInvalid =
      resultMove.exists(move => !validUciMove(move))
    val variationHashMismatch =
      request.variationHash.flatMap(expected => result.variationHash.map(_ != expected)).contains(true)
    val engineConfigMismatch =
      request.engineConfigFingerprint.flatMap(expected => result.engineConfigFingerprint.map(_ != expected)).contains(true)
    val depthFloor =
      request.depthFloor
        .orElse(if request.depth > 0 then Some(request.depth) else None)
        .filter(_ > 0)
    val depthFloorUnmet =
      depthFloor.exists(floor => result.depth.exists(_ < floor))
    val scoredReplyLineCount =
      result.replyLines.map(_.count(line => line.moves.nonEmpty && line.depth > 0)).getOrElse(0)
    val replyMultiPvIncomplete =
      request.purpose.exists(ProbePurpose.isBranchReply) &&
        request.multiPv.exists(required => scoredReplyLineCount < required)
    val hardReasonBuilder = List.newBuilder[String]
    if fenMissing then hardReasonBuilder += "FEN_UNVERIFIED"
    if requestFenInvalid then hardReasonBuilder += "REQUEST_FEN_INVALID"
    if resultFenInvalid then hardReasonBuilder += "RESULT_FEN_INVALID"
    if fenMismatch then hardReasonBuilder += "FEN_MISMATCH"
    if idMismatch then hardReasonBuilder += "ID_MISMATCH"
    if moveMissing then hardReasonBuilder += "PROBED_MOVE_UNVERIFIED"
    if requestMoveInvalid then hardReasonBuilder += "REQUEST_MOVE_INVALID"
    if moveMismatch then hardReasonBuilder += "PROBED_MOVE_MISMATCH"
    if resultMoveInvalid then hardReasonBuilder += "PROBED_MOVE_INVALID"
    if purposeContractMissing then hardReasonBuilder += "PURPOSE_CONTRACT_MISSING"
    if depthFloor.nonEmpty && result.depth.isEmpty then hardReasonBuilder += "DEPTH_FLOOR_UNVERIFIED"
    if depthFloorUnmet then hardReasonBuilder += "DEPTH_FLOOR_UNMET"
    if replyMultiPvIncomplete then hardReasonBuilder += "REPLY_MULTIPV_INCOMPLETE"
    val hardReasons = hardReasonBuilder.result()

    val softReasonBuilder = List.newBuilder[String]
    if purposeMismatch then softReasonBuilder += "PURPOSE_MISMATCH"
    if objectiveMismatch then softReasonBuilder += "OBJECTIVE_MISMATCH"
    if seedMismatch then softReasonBuilder += "SEED_MISMATCH"
    if request.variationHash.exists(_.trim.nonEmpty) && result.variationHash.forall(_.trim.isEmpty) then
      softReasonBuilder += "VARIATION_HASH_MISSING"
    if variationHashMismatch then softReasonBuilder += "VARIATION_HASH_MISMATCH"
    if request.engineConfigFingerprint.exists(_.trim.nonEmpty) &&
      result.engineConfigFingerprint.forall(_.trim.isEmpty)
    then softReasonBuilder += "ENGINE_CONFIG_FINGERPRINT_MISSING"
    if engineConfigMismatch then softReasonBuilder += "ENGINE_CONFIG_MISMATCH"
    val softReasons = softReasonBuilder.result()
    val allHardReasons = (base.hardReasonCodes ++ hardReasons).distinct
    val certificateStatus =
      if allHardReasons.nonEmpty then ProbeCertificateStatus.Invalid
      else if softReasons.nonEmpty then ProbeCertificateStatus.WeaklyValid
      else ProbeCertificateStatus.Valid
    base.copy(
      isValid = allHardReasons.isEmpty,
      reasonCodes = (base.reasonCodes ++ allHardReasons ++ softReasons).distinct,
      certificateStatus = certificateStatus,
      hardReasonCodes = allHardReasons,
      softReasonCodes = softReasons.distinct
    )

  private def validUciMove(raw: String): Boolean =
    Option(raw).map(_.trim.toLowerCase).exists(_.matches("""[a-h][1-8][a-h][1-8][nbrq]?"""))

  private def validateSignals(
      result: ProbeResult,
      requiredSignals: Set[String]
  ): ValidationResult =
    if requiredSignals.isEmpty then
      ValidationResult(
        isValid = false,
        missingSignals = Nil,
        reasonCodes = List("NO_REQUIRED_SIGNALS"),
        hardReasonCodes = List("NO_REQUIRED_SIGNALS")
      )
    else
      val missing = requiredSignals.filterNot(sig => hasSignal(sig, result)).toList.sorted
      val hardReasons =
        if missing.isEmpty then Nil else List("MISSING_REQUIRED_SIGNALS") ++ missing
      ValidationResult(
        isValid = missing.isEmpty,
        missingSignals = missing,
        reasonCodes =
          if missing.isEmpty then List("REQUIRED_SIGNALS_PRESENT")
          else List("MISSING_REQUIRED_SIGNALS"),
        hardReasonCodes = hardReasons
      )

  private def purposeRequiredSignals(purpose: ProbePurpose): Set[String] =
    if ProbePurpose.isBranchReply(purpose) then Set("replyLines") else Set.empty[String]

  private def hasSignal(signal: String, result: ProbeResult): Boolean =
    signal match
      case "replyLines" =>
        result.replyLines.exists(_.count(line => line.moves.nonEmpty && line.depth > 0) >= DefaultBranchReplyMultiPv)
      case "purpose" =>
        result.purpose.nonEmpty
      case "depth" =>
        result.depth.exists(_ > 0)
      case "variationHash" =>
        result.variationHash.exists(_.trim.nonEmpty)
      case "engineConfigFingerprint" =>
        result.engineConfigFingerprint.exists(_.trim.nonEmpty)
      case _ =>
        false
