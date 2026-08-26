package lila.chessjudgment.model

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

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

enum ProbeObjective(val key: String):
  case BranchReplyMultiPv extends ProbeObjective("branch_reply_multipv")
  case CausalContinuation extends ProbeObjective("causal_continuation")
  case CounterResource extends ProbeObjective("opponent_resource_reply")

object ProbeObjective:
  private val byKey = ProbeObjective.values.map(value => value.key -> value).toMap

  given Reads[ProbeObjective] = Reads:
    case JsString(raw) =>
      byKey.get(raw.trim).fold[JsResult[ProbeObjective]](JsError(s"Unknown probe objective: $raw"))(JsSuccess(_))
    case _ => JsError("Probe objective must be a string id")

  given Writes[ProbeObjective] = Writes(value => JsString(value.key))

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
enum ProbeVariant:
  case BranchReply(requiredHorizonPlyOffset: Int)
  case CausalContinuation(replyMove: String, followUpMove: String)
  case CounterResource(move: String)

case class ProbeRequest(
  id: String,
  fen: String,
  depth: Int,
  multiPv: Int,
  candidateMove: String,
  depthFloor: Int,
  variationHash: String,
  variant: ProbeVariant
):
  def purpose: ProbePurpose = ProbePurpose.ReplyMultipv
  def objective: ProbeObjective = variant match
    case ProbeVariant.BranchReply(_)          => ProbeObjective.BranchReplyMultiPv
    case ProbeVariant.CausalContinuation(_, _) => ProbeObjective.CausalContinuation
    case ProbeVariant.CounterResource(_)      => ProbeObjective.CounterResource
  def moves: List[String] = variant match
    case ProbeVariant.BranchReply(_)        => Nil
    case ProbeVariant.CausalContinuation(reply, followUp) => List(reply, followUp)
    case ProbeVariant.CounterResource(move) => List(move)
  def horizon: Option[String] = variant match
    case ProbeVariant.BranchReply(offset) => Some(ProbeHorizon.renderPlyOffset(offset))
    case _                                => None
  def opponentResourceMove: Option[String] = variant match
    case ProbeVariant.CounterResource(move) => Some(move)
    case _                                  => None
  def continuationMoves: List[String] = variant match
    case ProbeVariant.CausalContinuation(reply, followUp) => List(reply, followUp)
    case _                                      => Nil

object ProbeRequest:
  /** Public transport identity for one complete physical-search contract.
    * The variation hash already owns the exact root, line, search, and variant
    * fields; embedding an occurrence-rich line id here would duplicate that
    * identity and can exceed the wire limit.
    */
  def transportId(objective: ProbeObjective, variationHash: String): String =
    s"probe:${objective.key}:$variationHash"

enum ProbeResolution:
  case EngineSearch(evaluations: List[CandidateLineEvaluation], depth: Int)
  case ExactAutomaticTerminal(evaluation: CandidateLineEvaluation.ExactAutomaticTerminal)

/** Canonical server-bound resolution of an issued probe. */
case class ProbeResult(
  id: String,
  resolution: ProbeResolution
)

enum ProbeAdmissionStatus:
  case Admitted
  case Rejected

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

/** Purpose-aware request/result envelope validation.
  *
  * Board legality and horizon coverage belong to the canonical branch
  * admission that owns the replay. Keeping them out of this envelope check
  * prevents the same engine line from being replayed at every contract gate.
  */
object ProbeContractValidator:
  case class ValidationResult(
      isValid: Boolean,
      reasonCodes: List[String]
  )

  def validateRequest(request: ProbeRequest): ValidationResult =
    validation(requestReasons(request))

  def validateAgainstRequest(
      request: ProbeRequest,
      result: ProbeResult
  ): ValidationResult =
    val reasons = List.newBuilder[String]
    reasons ++= requestReasons(request)
    if request.id != result.id then reasons += "ID_MISMATCH"
    result.resolution match
      case ProbeResolution.EngineSearch(evaluations, depth) =>
        if request.multiPv != evaluations.size then reasons += "REPLY_MULTIPV_COUNT_MISMATCH"
        if evaluations.exists(_.moves.isEmpty) then reasons += "REPLY_LINE_MISSING"
        val rootMoves = evaluations.flatMap(_.moves.headOption).map(EvidenceRef.normalizeMove)
        if rootMoves.distinct.size != rootMoves.size then reasons += "REPLY_ROOT_MOVE_CONFLICT"
        if depth < request.depthFloor then reasons += "DEPTH_FLOOR_UNMET"
      case ProbeResolution.ExactAutomaticTerminal(_) => ()
    validation(reasons.result())

  private def requestReasons(request: ProbeRequest): List[String] =
    val reasons = List.newBuilder[String]
    if request.id.trim.isEmpty then reasons += "REQUEST_ID_MISSING"
    if request.fen.trim.isEmpty then reasons += "REQUEST_FEN_MISSING"
    if request.depth <= 0 then reasons += "REQUEST_DEPTH_INVALID"
    if request.depthFloor <= 0 || request.depthFloor > request.depth then reasons += "DEPTH_FLOOR_INVALID"
    if !validUciMove(request.candidateMove) then reasons += "CANDIDATE_MOVE_INVALID"
    if request.variationHash.trim.isEmpty then reasons += "VARIATION_HASH_MISSING"
    request.variant match
      case ProbeVariant.BranchReply(requiredHorizonPlyOffset) =>
        if request.multiPv <= 0 || request.multiPv > BranchReplyProbeBinding.ReplyMultiPv then
          reasons += "REPLY_MULTIPV_INVALID"
        if requiredHorizonPlyOffset <= 0 then reasons += "HORIZON_INVALID"
      case ProbeVariant.CausalContinuation(reply, followUp) =>
        if request.multiPv != 1 || !validUciMove(reply) || !validUciMove(followUp)
        then reasons += "CAUSAL_CONTINUATION_INVALID"
      case ProbeVariant.CounterResource(resource) =>
        if request.multiPv != 1 || !validUciMove(resource) then reasons += "COUNTER_RESOURCE_INVALID"
    reasons.result()

  private def validation(reasons: List[String]): ValidationResult =
    val distinct = reasons.distinct
    ValidationResult(
      isValid = distinct.isEmpty,
      reasonCodes = if distinct.isEmpty then List("PROBE_CONTRACT_VERIFIED") else distinct
    )

  private def validUciMove(raw: String): Boolean =
    EvidenceRef.normalizeMove(raw).matches("""[a-h][1-8][a-h][1-8][nbrq]?""")

object BranchReplyProbeBinding:
  val ReplyMultiPv = 3
  val Depth = 16
  val DepthFloor = 12

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
        ProbeObjective.CounterResource.key
      ).mkString("||")
    MessageDigest
      .getInstance("SHA-256")
      .digest(raw.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

object CausalContinuationProbeBinding:

  def variationHash(
      rootFen: String,
      role: LineNodeRole,
      rootMove: String,
      whitePovEvalCp: Int,
      mate: Option[Int],
      depth: Int,
      moves: List[String],
      continuationMoves: List[String]
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
        continuationMoves.map(EvidenceRef.normalizeMove).mkString(","),
        ProbeObjective.CausalContinuation.key
      ).mkString("||")
    MessageDigest
      .getInstance("SHA-256")
      .digest(raw.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
