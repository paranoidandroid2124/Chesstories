package lila.chessjudgment.model

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import lila.chessjudgment.model.judgment.{ EvidenceRef, LineNodeRole }
import lila.chessjudgment.model.line.CandidateLineEvaluation
import play.api.libs.json._

enum ProbeKind(val key: String):
  case BranchReply extends ProbeKind("branch_reply")

object ProbeKind:
  given Writes[ProbeKind] = Writes(value => JsString(value.key))

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
  case BranchReply(replyMove: String, requiredHorizonPlyOffset: Int)

case class ProbeRequest(
  id: String,
  fen: String,
  depth: Int,
  candidateMove: String,
  depthFloor: Int,
  variationHash: String,
  variant: ProbeVariant
):
  def kind: ProbeKind = variant match
    case ProbeVariant.BranchReply(_, _) => ProbeKind.BranchReply
  def moves: List[String] = variant match
    case ProbeVariant.BranchReply(replyMove, _) => List(EvidenceRef.normalizeMove(replyMove))
  def horizon: Option[String] = variant match
    case ProbeVariant.BranchReply(_, offset) => Some(ProbeHorizon.renderPlyOffset(offset))

object ProbeRequest:
  /** Public transport identity for one complete physical-search contract.
    * The variation hash already owns the exact root, line, search, and variant
    * fields; embedding an occurrence-rich line id here would duplicate that
    * identity and can exceed the wire limit.
    */
  def transportId(kind: ProbeKind, variationHash: String): String =
    s"probe:${kind.key}:$variationHash"

enum ProbeResolution:
  case EngineSearch(evaluations: List[CandidateLineEvaluation], depth: Int)
  case ExactAutomaticTerminal(evaluation: CandidateLineEvaluation.ExactAutomaticTerminal)

/** Canonical server-bound resolution of an issued probe. */
case class ProbeResult(
  id: String,
  resolution: ProbeResolution
)

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
        if evaluations.size != 1 then reasons += "REPLY_LINE_COUNT_MISMATCH"
        if evaluations.exists(_.moves.isEmpty) then reasons += "REPLY_LINE_MISSING"
        request.variant match
          case ProbeVariant.BranchReply(replyMove, _) =>
            if evaluations.exists(_.moves.headOption.forall(move => !EvidenceRef.sameMove(move, replyMove))) then
              reasons += "REPLY_MOVE_MISMATCH"
        val rootMoves = evaluations.flatMap(_.moves.headOption).map(EvidenceRef.normalizeMove)
        if rootMoves.distinct.size != rootMoves.size then reasons += "REPLY_ROOT_MOVE_CONFLICT"
        if depth < request.depthFloor then reasons += "DEPTH_FLOOR_UNMET"
      case ProbeResolution.ExactAutomaticTerminal(evaluation) =>
        request.variant match
          case ProbeVariant.BranchReply(replyMove, _) =>
            if evaluation.moves match
                case exact :: Nil => !EvidenceRef.sameMove(exact, replyMove)
                case _            => true
            then reasons += "REPLY_MOVE_MISMATCH"
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
      case ProbeVariant.BranchReply(replyMove, requiredHorizonPlyOffset) =>
        if !validUciMove(replyMove) then reasons += "REPLY_SEARCH_INVALID"
        if requiredHorizonPlyOffset <= 0 then reasons += "HORIZON_INVALID"
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
      replyMove: String,
      certifiedHorizonPlyOffset: Int
  ): String =
    digest(variationFields(rootFen, role, rootMove, whitePovEvalCp, mate, depth, moves) :+
      EvidenceRef.normalizeMove(replyMove) :+
      horizon(certifiedHorizonPlyOffset))

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
