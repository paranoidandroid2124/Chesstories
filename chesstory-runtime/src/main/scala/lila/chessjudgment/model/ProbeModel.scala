package lila.chessjudgment.model

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import _root_.chess.format.Fen
import _root_.chess.variant.Standard
import lila.chessjudgment.model.judgment.{ EvidenceRef, LineNodeRole }
import lila.chessjudgment.model.line.CandidateLineEvaluation
import play.api.libs.json._

enum ProbePurpose(val key: String):
  case ReplyMultipv extends ProbePurpose("reply_multipv")

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

object ProbeHorizon:
  private val PlyOffset = raw"ply:([1-9][0-9]*)".r

  def renderPlyOffset(plyOffset: Int): String = s"ply:${plyOffset.max(1)}"

  def plyOffset(value: String): Option[Int] =
    Option(value) match
      case Some(PlyOffset(rawOffset)) => rawOffset.toIntOption.filter(_ > 0)
      case _                          => None

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
  // Objective-driven probing contract
  objective: Option[String] = None,        // e.g. "branch_reply_multipv", "compare_branches"
  requiredSignals: List[String] = Nil,
  horizon: Option[String] = None,          // exact branch proof deadline, e.g. "ply:8"
  candidateMove: Option[String] = None,    // explicit root move when the request is move-bound
  opponentResourceMove: Option[String] = None, // exact opponent resource forced from the post-root position
  depthFloor: Option[Int] = None,          // minimum acceptable realized depth for certification
  variationHash: Option[String] = None     // binds the request to a specific logical variation bundle
)

object ProbeRequest:
  given Reads[ProbeRequest] = Json.reads[ProbeRequest]
  given Writes[ProbeRequest] = Json.writes[ProbeRequest]

enum ProbeResolution:
  case EngineSearch(evaluations: List[CandidateLineEvaluation], depth: Int)
  case ExactAutomaticTerminal(evaluation: CandidateLineEvaluation.ExactAutomaticTerminal)

/** Canonical server-bound resolution of an issued probe. */
case class ProbeResult(
  id: String,
  fen: Option[String] = None, // Base FEN the probe was run from (critical when probing non-root branches)
  resolution: ProbeResolution,
  // Optional metadata that keeps branch probes self-describing.
  purpose: Option[ProbePurpose] = None,
  probedMove: Option[String] = None, // The probed candidate move (UCI)
  // Optional contract diagnostics.
  objective: Option[String] = None,
  requiredSignals: List[String] = Nil,
  horizon: Option[String] = None,
  candidateMove: Option[String] = None,
  opponentResourceMove: Option[String] = None,
  variationHash: Option[String] = None
)

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
  horizon: Option[String] = None,
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
    val requiredReplyLineCount = request.multiPv.getOrElse(DefaultBranchReplyMultiPv)
    val base = result.resolution match
      case ProbeResolution.EngineSearch(_, _) =>
        validateSignals(result, required, requiredReplyLineCount)
      case ProbeResolution.ExactAutomaticTerminal(_) =>
        ValidationResult(true, Nil, List("EXACT_AUTOMATIC_TERMINAL"))
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
    val requestMoves =
      request.moves.map(_.trim).filter(_.nonEmpty)
    val candidateMoves = request.candidateMove.map(_.trim).filter(_.nonEmpty).toList
    val allowedMoves = (candidateMoves ++ requestMoves).distinct
    val resultMove =
      result.probedMove
        .orElse(result.candidateMove)
        .map(_.trim)
        .filter(_.nonEmpty)
    val moveMissing =
      allowedMoves.nonEmpty && resultMove.isEmpty
    val moveMismatch =
      resultMove.exists(move => allowedMoves.nonEmpty && !allowedMoves.contains(move))
    val requestedOpponentResource = request.opponentResourceMove.map(_.trim).filter(_.nonEmpty)
    val observedOpponentResource = result.opponentResourceMove.map(_.trim).filter(_.nonEmpty)
    val opponentResourceMissing = requestedOpponentResource.nonEmpty && observedOpponentResource.isEmpty
    val opponentResourceMismatch =
      requestedOpponentResource.exists(expected => observedOpponentResource.exists(_ != expected))
    val opponentResourceInvalid =
      (requestedOpponentResource.toList ++ observedOpponentResource.toList).exists(move => !validUciMove(move))
    val requestMoveInvalid =
      (candidateMoves ++ requestMoves).exists(move => !validUciMove(move))
    val resultMoveInvalid =
      resultMove.exists(move => !validUciMove(move))
    val variationHashMismatch =
      request.variationHash.flatMap(expected => result.variationHash.map(_ != expected)).contains(true)
    val requestHorizon = request.horizon.filter(_.nonEmpty)
    val resultHorizon = result.horizon.filter(_.nonEmpty)
    val horizonMissing = requestHorizon.nonEmpty && resultHorizon.isEmpty
    val horizonUnexpected = requestHorizon.isEmpty && resultHorizon.nonEmpty
    val horizonMismatch = requestHorizon.exists(expected => resultHorizon.exists(_ != expected))
    val requestHorizonInvalid = requestHorizon.exists(ProbeHorizon.plyOffset(_).isEmpty)
    val resultHorizonInvalid = resultHorizon.exists(ProbeHorizon.plyOffset(_).isEmpty)
    val depthFloor =
      request.depthFloor
        .orElse(if request.depth > 0 then Some(request.depth) else None)
        .filter(_ > 0)
    val resultDepth = result.resolution match
      case ProbeResolution.EngineSearch(_, depth) => Some(depth)
      case ProbeResolution.ExactAutomaticTerminal(_) => None
    val depthFloorUnmet =
      depthFloor.exists(floor => resultDepth.exists(_ < floor))
    val scoredReplyLineCount =
      result.resolution match
        case ProbeResolution.EngineSearch(evaluations, _) => evaluations.count(_.moves.nonEmpty)
        case ProbeResolution.ExactAutomaticTerminal(_) => 0
    val engineResolution = result.resolution match
      case ProbeResolution.EngineSearch(_, _) => true
      case ProbeResolution.ExactAutomaticTerminal(_) => false
    val replyMultiPvIncomplete =
      engineResolution &&
        request.purpose.exists(ProbePurpose.isBranchReply) &&
        scoredReplyLineCount < requiredReplyLineCount
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
    if opponentResourceMissing then hardReasonBuilder += "OPPONENT_RESOURCE_UNVERIFIED"
    if opponentResourceMismatch then hardReasonBuilder += "OPPONENT_RESOURCE_MISMATCH"
    if opponentResourceInvalid then hardReasonBuilder += "OPPONENT_RESOURCE_INVALID"
    if purposeContractMissing then hardReasonBuilder += "PURPOSE_CONTRACT_MISSING"
    if engineResolution && depthFloor.nonEmpty && resultDepth.isEmpty then
      hardReasonBuilder += "DEPTH_FLOOR_UNVERIFIED"
    if depthFloorUnmet then hardReasonBuilder += "DEPTH_FLOOR_UNMET"
    if replyMultiPvIncomplete then hardReasonBuilder += "REPLY_MULTIPV_INCOMPLETE"
    if horizonMissing then hardReasonBuilder += "HORIZON_UNVERIFIED"
    if horizonUnexpected then hardReasonBuilder += "HORIZON_UNEXPECTED"
    if horizonMismatch then hardReasonBuilder += "HORIZON_MISMATCH"
    if requestHorizonInvalid then hardReasonBuilder += "REQUEST_HORIZON_INVALID"
    if resultHorizonInvalid then hardReasonBuilder += "RESULT_HORIZON_INVALID"
    val hardReasons = hardReasonBuilder.result()

    val softReasonBuilder = List.newBuilder[String]
    if purposeMismatch then softReasonBuilder += "PURPOSE_MISMATCH"
    if objectiveMismatch then softReasonBuilder += "OBJECTIVE_MISMATCH"
    if request.variationHash.exists(_.trim.nonEmpty) && result.variationHash.forall(_.trim.isEmpty) then
      softReasonBuilder += "VARIATION_HASH_MISSING"
    if variationHashMismatch then softReasonBuilder += "VARIATION_HASH_MISMATCH"
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
      requiredSignals: Set[String],
      requiredReplyLineCount: Int
  ): ValidationResult =
    if requiredSignals.isEmpty then
      ValidationResult(
        isValid = false,
        missingSignals = Nil,
        reasonCodes = List("NO_REQUIRED_SIGNALS"),
        hardReasonCodes = List("NO_REQUIRED_SIGNALS")
      )
    else
      val missing = requiredSignals.filterNot(sig => hasSignal(sig, result, requiredReplyLineCount)).toList.sorted
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
    if ProbePurpose.isBranchReply(purpose) then Set("replyLines")
    else Set.empty[String]

  private def hasSignal(signal: String, result: ProbeResult, requiredReplyLineCount: Int): Boolean =
    signal match
      case "replyLines" =>
        result.resolution match
          case ProbeResolution.EngineSearch(evaluations, _) =>
            evaluations.count(_.moves.nonEmpty) >= requiredReplyLineCount
          case ProbeResolution.ExactAutomaticTerminal(_) => false
      case "purpose" =>
        result.purpose.nonEmpty
      case "depth" =>
        result.resolution match
          case ProbeResolution.EngineSearch(_, depth) => depth > 0
          case ProbeResolution.ExactAutomaticTerminal(_) => false
      case "variationHash" =>
        result.variationHash.exists(_.trim.nonEmpty)
      case "horizon" =>
        result.horizon.flatMap(ProbeHorizon.plyOffset).nonEmpty
      case "opponentResourceMove" =>
        result.opponentResourceMove.exists(_.trim.nonEmpty)
      case _ =>
        false

object BranchReplyProbeBinding:
  val ReplyMultiPv = 3
  val Depth = 16
  val DepthFloor = 12
  val Objective = "branch_reply_multipv"
  val RequiredSignals: List[String] = List("replyLines", "depth", "purpose", "variationHash", "horizon")

  def requiredReplyCount(branchFen: String): Int =
    _root_.chess.format.Fen
      .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(branchFen))
      .map(_.legalMoves.size.min(ReplyMultiPv))
      .getOrElse(ReplyMultiPv)

  def horizon(requiredPlyOffset: Int): String = ProbeHorizon.renderPlyOffset(requiredPlyOffset)

  def horizonPlyOffset(value: String): Option[Int] =
    ProbeHorizon.plyOffset(value)

  def variationHash(
      rootFen: String,
      role: LineNodeRole,
      rootMove: String,
      whitePovEvalCp: Int,
      mate: Option[Int],
      depth: Int,
      moves: List[String],
      certifiedHorizonPlyOffset: Int
  ): String =
    digest(variationFields(rootFen, role, rootMove, whitePovEvalCp, mate, depth, moves) :+
      horizon(certifiedHorizonPlyOffset))

  private[model] def variationBaseHash(
      rootFen: String,
      role: LineNodeRole,
      rootMove: String,
      whitePovEvalCp: Int,
      mate: Option[Int],
      depth: Int,
      moves: List[String]
  ): String =
    digest(variationFields(rootFen, role, rootMove, whitePovEvalCp, mate, depth, moves))

  private def variationFields(
      rootFen: String,
      role: LineNodeRole,
      rootMove: String,
      whitePovEvalCp: Int,
      mate: Option[Int],
      depth: Int,
      moves: List[String]
  ): List[String] =
    List(
      rootFen,
      role.toString,
      rootMove,
      whitePovEvalCp.toString,
      mate.map(_.toString).getOrElse(""),
      depth.toString,
      moves.mkString(",")
    )

  private def digest(fields: List[String]): String =
    val raw = fields.mkString("||")
    MessageDigest
      .getInstance("SHA-256")
      .digest(raw.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

object CounterResourceProbeBinding:
  val Objective = "opponent_resource_reply"
  val RequiredSignals: List[String] =
    List("replyLines", "depth", "purpose", "variationHash", "opponentResourceMove")

  def variationHash(
      rootFen: String,
      role: LineNodeRole,
      rootMove: String,
      whitePovEvalCp: Int,
      mate: Option[Int],
      depth: Int,
      moves: List[String],
      opponentResourceMove: String
  ): String =
    val raw =
      List(
        BranchReplyProbeBinding.variationBaseHash(
          rootFen,
          role,
          rootMove,
          whitePovEvalCp,
          mate,
          depth,
          moves
        ),
        EvidenceRef.normalizeMove(opponentResourceMove),
        Objective
      ).mkString("||")
    MessageDigest
      .getInstance("SHA-256")
      .digest(raw.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
