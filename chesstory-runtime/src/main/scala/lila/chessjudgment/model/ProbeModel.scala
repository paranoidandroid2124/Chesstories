package lila.chessjudgment.model

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import _root_.chess.format.Fen
import _root_.chess.variant.Standard
import lila.chessjudgment.model.judgment.{ CandidateLineNode, EvidenceRef, LineNodeRole, PositionNodeRef }
import lila.chessjudgment.model.strategic.EngineLine
import play.api.libs.json._

enum ProbePurpose(val key: String):
  case ReplyMultipv extends ProbePurpose("reply_multipv")
  case EndgameTablebase extends ProbePurpose("endgame_tablebase")

object ProbePurpose:
  private val byKey = ProbePurpose.values.map(p => p.key -> p).toMap
  def fromKey(key: String): Option[ProbePurpose] =
    Option(key).map(_.trim).filter(_.nonEmpty).flatMap(byKey.get)
  def isBranchReply(purpose: ProbePurpose): Boolean =
    purpose == ProbePurpose.ReplyMultipv
  def isEndgameTablebase(purpose: ProbePurpose): Boolean =
    purpose == ProbePurpose.EndgameTablebase

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
  horizon: Option[String] = None,          // exact branch proof deadline, e.g. "ply:8"
  candidateMove: Option[String] = None,    // explicit root move when the request is move-bound
  opponentResourceMove: Option[String] = None, // exact opponent resource forced from the post-root position
  depthFloor: Option[Int] = None,          // minimum acceptable realized depth for certification
  variationHash: Option[String] = None,    // binds the request to a specific logical variation bundle
  engineConfigFingerprint: Option[String] = None, // binds the request to engine/config generation
  comparisonFen: Option[String] = None
)

object ProbeRequest:
  given Reads[ProbeRequest] = Json.reads[ProbeRequest]
  given Writes[ProbeRequest] = Json.writes[ProbeRequest]

case class TablebasePositionEvidence(
    fen: String,
    wdl: Int,
    dtm: Option[Int] = None
)

object TablebasePositionEvidence:
  given OFormat[TablebasePositionEvidence] = Json.format[TablebasePositionEvidence]

case class TablebaseMoveEvidence(
    moveUci: String,
    resultingWdl: Int,
    resultingDtm: Option[Int] = None
)

object TablebaseMoveEvidence:
  given OFormat[TablebaseMoveEvidence] = Json.format[TablebaseMoveEvidence]

case class EndgameTablebaseEvidence(
    terminal: TablebasePositionEvidence,
    comparison: TablebasePositionEvidence,
    legalMoves: List[TablebaseMoveEvidence]
)

object EndgameTablebaseEvidence:
  given OFormat[EndgameTablebaseEvidence] = Json.format[EndgameTablebaseEvidence]

/** Raw client-computed evidence returned to the core. */
case class ProbeResult(
  id: String,
  fen: Option[String] = None, // Base FEN the probe was run from (critical when probing non-root branches)
  replyLines: Option[List[EngineLine]] = None,
  // Optional metadata that keeps branch probes self-describing.
  purpose: Option[ProbePurpose] = None,
  probedMove: Option[String] = None, // The probed candidate move (UCI)
  depth: Option[Int] = None,         // Depth reached by the client engine
  // Optional contract diagnostics.
  objective: Option[String] = None,
  seedId: Option[String] = None,
  requiredSignals: List[String] = Nil,
  horizon: Option[String] = None,
  candidateMove: Option[String] = None,
  opponentResourceMove: Option[String] = None,
  depthFloor: Option[Int] = None,
  variationHash: Option[String] = None,
  engineConfigFingerprint: Option[String] = None,
  generatedAtEpochMs: Option[Long] = None,
  tablebase: Option[EndgameTablebaseEvidence] = None
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
    val base = validateSignals(result, required, requiredReplyLineCount)
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
    val comparisonFen = request.comparisonFen.map(_.trim).filter(_.nonEmpty)
    val resultComparisonFen = result.tablebase.map(_.comparison.fen.trim).filter(_.nonEmpty)
    val comparisonFenMissing = comparisonFen.nonEmpty && resultComparisonFen.isEmpty
    val comparisonFenMismatch = comparisonFen.exists(expected => resultComparisonFen.exists(_ != expected))
    val comparisonFenInvalid =
      (comparisonFen.toList ++ resultComparisonFen.toList).exists(fen => Fen.read(Standard, Fen.Full(fen)).isEmpty)
    val objectiveMismatch =
      request.objective.flatMap(expected => result.objective.map(_ != expected)).contains(true)
    val seedMismatch =
      request.seedId.flatMap(expected => result.seedId.map(_ != expected)).contains(true)
    val requestMoves =
      request.moves.map(_.trim).filter(_.nonEmpty)
    val candidateMoves = request.candidateMove.map(_.trim).filter(_.nonEmpty).toList
    val allowedMoves =
      if request.purpose.exists(ProbePurpose.isEndgameTablebase) then candidateMoves
      else (candidateMoves ++ requestMoves).distinct
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
        scoredReplyLineCount < requiredReplyLineCount
    val hardReasonBuilder = List.newBuilder[String]
    if fenMissing then hardReasonBuilder += "FEN_UNVERIFIED"
    if requestFenInvalid then hardReasonBuilder += "REQUEST_FEN_INVALID"
    if resultFenInvalid then hardReasonBuilder += "RESULT_FEN_INVALID"
    if fenMismatch then hardReasonBuilder += "FEN_MISMATCH"
    if comparisonFenMissing then hardReasonBuilder += "COMPARISON_FEN_UNVERIFIED"
    if comparisonFenMismatch then hardReasonBuilder += "COMPARISON_FEN_MISMATCH"
    if comparisonFenInvalid then hardReasonBuilder += "COMPARISON_FEN_INVALID"
    if idMismatch then hardReasonBuilder += "ID_MISMATCH"
    if moveMissing then hardReasonBuilder += "PROBED_MOVE_UNVERIFIED"
    if requestMoveInvalid then hardReasonBuilder += "REQUEST_MOVE_INVALID"
    if moveMismatch then hardReasonBuilder += "PROBED_MOVE_MISMATCH"
    if resultMoveInvalid then hardReasonBuilder += "PROBED_MOVE_INVALID"
    if opponentResourceMissing then hardReasonBuilder += "OPPONENT_RESOURCE_UNVERIFIED"
    if opponentResourceMismatch then hardReasonBuilder += "OPPONENT_RESOURCE_MISMATCH"
    if opponentResourceInvalid then hardReasonBuilder += "OPPONENT_RESOURCE_INVALID"
    if purposeContractMissing then hardReasonBuilder += "PURPOSE_CONTRACT_MISSING"
    if depthFloor.nonEmpty && result.depth.isEmpty then hardReasonBuilder += "DEPTH_FLOOR_UNVERIFIED"
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
    else if ProbePurpose.isEndgameTablebase(purpose) then Set("tablebase")
    else Set.empty[String]

  private def hasSignal(signal: String, result: ProbeResult, requiredReplyLineCount: Int): Boolean =
    signal match
      case "replyLines" =>
        result.replyLines.exists(_.count(line => line.moves.nonEmpty && line.depth > 0) >= requiredReplyLineCount)
      case "purpose" =>
        result.purpose.nonEmpty
      case "depth" =>
        result.depth.exists(_ > 0)
      case "variationHash" =>
        result.variationHash.exists(_.trim.nonEmpty)
      case "horizon" =>
        result.horizon.flatMap(ProbeHorizon.plyOffset).nonEmpty
      case "opponentResourceMove" =>
        result.opponentResourceMove.exists(_.trim.nonEmpty)
      case "engineConfigFingerprint" =>
        result.engineConfigFingerprint.exists(_.trim.nonEmpty)
      case "tablebase" =>
        result.tablebase.nonEmpty
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
      root: PositionNodeRef,
      line: CandidateLineNode,
      certifiedHorizonPlyOffset: Int
  ): String =
    variationHash(
      rootFen = root.fen,
      role = line.ref.role,
      rootMove = line.ref.rootMove,
      whitePovEvalCp = line.whitePovEvalCp,
      mate = line.mate,
      depth = line.depth,
      moves = line.line.moves,
      certifiedHorizonPlyOffset = certifiedHorizonPlyOffset
    )

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

  def variationHash(root: PositionNodeRef, line: CandidateLineNode, opponentResourceMove: String): String =
    variationHash(
      rootFen = root.fen,
      role = line.ref.role,
      rootMove = line.ref.rootMove,
      whitePovEvalCp = line.whitePovEvalCp,
      mate = line.mate,
      depth = line.depth,
      moves = line.line.moves,
      opponentResourceMove = opponentResourceMove
    )

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
