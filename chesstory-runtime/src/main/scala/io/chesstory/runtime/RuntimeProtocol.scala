package io.chesstory.runtime

import scala.util.Try

import lila.chessjudgment.analysis.assembly.{
  JudgmentPacketValidator,
  MoveReviewJudgmentOrchestrator,
  RawMoveReviewInput,
  RawOpeningContext
}
import lila.chessjudgment.analysis.opening.{ OpeningRecognitionIndex, OpeningThemePriorIndex }
import lila.chessjudgment.analysis.evaluation.JudgmentThresholds
import lila.chessjudgment.model.{ EndgameTablebaseEvidence, ProbeHorizon, ProbePurpose, ProbeRequest, ProbeResult }
import lila.chessjudgment.model.judgment.MoveMeaningSurface
import lila.chessjudgment.model.strategic.VariationLine
import play.api.libs.json.*

object RuntimeProtocol:
  val RequestSchema = "chesstory.move-meaning.request.v1"
  val ResponseSchema = "chesstory.move-meaning.response.v2"

  final case class Result(httpStatus: Int, body: JsObject)

  private final case class WireVariation(
      moves: List[String],
      scoreCp: Int,
      mate: Option[Int],
      depth: Int
  ):
    def toCore: VariationLine = VariationLine(moves, scoreCp, mate, depth)

  private object WireVariation:
    given Reads[WireVariation] = Json.reads[WireVariation]

  private final case class WireProbeResult(
      id: String,
      fen: String,
      evalCp: Int,
      replyLines: Option[List[WireVariation]],
      deltaVsBaseline: Int,
      purpose: Option[ProbePurpose],
      probedMove: Option[String],
      mate: Option[Int],
      depth: Option[Int],
      candidateMove: Option[String],
      opponentResourceMove: Option[String],
      horizon: Option[String],
      variationHash: Option[String],
      generatedAtEpochMs: Option[Long],
      tablebase: Option[EndgameTablebaseEvidence]
  ):
    def toCore: ProbeResult =
      ProbeResult(
        id = id,
        fen = Some(fen),
        evalCp = evalCp,
        replyLines = replyLines.map(_.map(_.toCore)),
        deltaVsBaseline = deltaVsBaseline,
        purpose = purpose,
        probedMove = probedMove,
        mate = mate,
        depth = depth,
        candidateMove = candidateMove,
        opponentResourceMove = opponentResourceMove,
        horizon = horizon,
        variationHash = variationHash,
        generatedAtEpochMs = generatedAtEpochMs,
        tablebase = tablebase
      )

  private object WireProbeResult:
    given Reads[WireProbeResult] = Json.reads[WireProbeResult]

  private final case class WireInput(
      fen: String,
      playedMoveUci: String,
      variations: List[WireVariation],
      currentEvalCp: Option[Int],
      ply: Option[Int],
      openingContext: Option[RawOpeningContext],
      movePrefixUci: Option[List[String]],
      probeResults: Option[List[WireProbeResult]]
  ):
    def toCore: RawMoveReviewInput =
      RawMoveReviewInput(
        fen = fen,
        playedMoveUci = playedMoveUci,
        variations = variations.map(_.toCore),
        currentEvalCp = currentEvalCp,
        ply = ply,
        openingContext = openingContext,
        movePrefixUci = movePrefixUci.getOrElse(Nil),
        probeResults = probeResults.getOrElse(Nil).map(_.toCore)
      )

  private object WireInput:
    given Reads[WireInput] = Json.reads[WireInput]

  def evaluate(bytes: Array[Byte]): Result =
    Try(Json.parse(bytes)).fold(_ => failure(None, 400, "invalid_json"), evaluate)

  def evaluate(json: JsValue): Result =
    json match
      case obj: JsObject =>
        (obj \ "request_id").validateOpt[String] match
          case JsError(_) => failure(None, 400, "invalid_request_id")
          case JsSuccess(requestId, _) if !requestId.forall(InputLimits.validRequestId) =>
            failure(None, 400, "invalid_request_id")
          case JsSuccess(requestId, _) =>
            if (obj \ "schema_version").asOpt[String] != Some(RequestSchema) then
              failure(requestId, 400, "unsupported_schema_version")
            else
              (obj \ "input").validate[WireInput] match
                case JsError(_) => failure(requestId, 400, "invalid_move_review_input")
                case JsSuccess(input, _) => evaluate(requestId, input.toCore)
      case _ => failure(None, 400, "invalid_request")

  def publicProbeRequestJson(request: ProbeRequest): JsObject =
    Json.obj(
      "id" -> request.id,
      "fen" -> request.fen,
      "moves" -> request.moves,
      "depth" -> request.depth,
      "purpose" -> request.purpose,
      "multiPv" -> request.multiPv,
      "baselineEvalCp" -> request.baselineEvalCp,
      "horizon" -> request.horizon,
      "candidateMove" -> request.candidateMove,
      "depthFloor" -> request.depthFloor,
      "variationHash" -> request.variationHash,
      "comparisonFen" -> request.comparisonFen
    ) ++ Option
      .when(request.opponentResourceMove.nonEmpty)(Json.obj("opponentResourceMove" -> request.opponentResourceMove))
      .getOrElse(Json.obj())

  private def evaluate(requestId: Option[String], raw: RawMoveReviewInput): Result =
    if !InputLimits.accepts(raw) then failure(requestId, 400, "input_limits_exceeded")
    else
      MoveReviewJudgmentOrchestrator.packet(raw) match
        case None => failure(requestId, 422, "move_review_not_buildable")
        case Some(packet) =>
          val validation = JudgmentPacketValidator.validate(packet)
          val engineBacked = InputLimits.engineBacked(raw)
          val moveReview = packet.moveJudgmentView.map: view =>
            val payload = MoveMeaningSurface.publicPayloadJson(view)
            val hasExplanation = (payload \ "explanations").asOpt[JsArray].exists(_.value.nonEmpty)
            val hasEngineMateOutcome =
              (payload \ "verdict" \ "outcome" \ "state").asOpt[String].exists(state =>
                state == "forced_win" || state == "forced_loss"
              )
            val renderable = engineBacked && (hasExplanation || hasEngineMateOutcome)
            Json.obj("renderable" -> renderable) ++ payload
          val hasPlayerFacingReason =
            validation.isValid && moveReview.exists(review => (review \ "renderable").asOpt[Boolean].contains(true))
          val status = if hasPlayerFacingReason then "ready" else "withheld"
          val reason =
            if !engineBacked then Some("insufficient_engine_depth")
            else if !validation.isValid then Some("validation_failed")
            else if !hasPlayerFacingReason then Some("no_player_facing_reason")
            else None
          val probeRequests =
            if engineBacked && validation.isValid then packet.probeRequests.map(publicProbeRequestJson) else Nil
          Result(
            200,
            Json.obj(
              "schema_version" -> ResponseSchema,
              "request_id" -> requestId,
              "ok" -> true,
              "status" -> status,
              "availability" -> Json.obj("state" -> status, "reason" -> reason),
              "probe_requests" -> probeRequests,
              "move_review" -> moveReview
            )
          )

  private[runtime] def failure(requestId: Option[String], httpStatus: Int, error: String): Result =
    Result(
      httpStatus,
      Json.obj(
        "schema_version" -> ResponseSchema,
        "request_id" -> requestId,
        "ok" -> false,
        "status" -> "error",
        "error" -> error
      )
    )

  private object InputLimits:
    private val MaxFenLength = 128
    private val MaxVariations = 16
    private val MaxPvMoves = 80
    private val MaxMovePrefix = 600
    private val MaxProbeResults = 12
    private val MaxReplyLines = 8
    private val MaxTablebaseMoves = 256
    private val MaxDepth = 100
    private val MaxPly = 1000
    private val MaxEval = 100000L
    private val MaxMate = 1000L
    private val MaxText = 256
    private val Uci = "[a-h][1-8][a-h][1-8][nbrq]?".r
    private val RequestId = "[A-Za-z0-9._:-]{1,128}".r

    def validRequestId(value: String): Boolean = RequestId.matches(value)

    def engineBacked(raw: RawMoveReviewInput): Boolean =
      raw.variations
        .groupBy(line => line.moves.headOption.map(_.trim.toLowerCase).getOrElse(""))
        .values
        .forall(lines => lines.exists(line => JudgmentThresholds.engineBackedByDepth(line.depth, line.mate)))

    def accepts(raw: RawMoveReviewInput): Boolean =
      val lines = raw.variations ++ raw.probeResults.flatMap(_.replyLines.getOrElse(Nil))
      raw.fen.nonEmpty && raw.fen.length <= MaxFenLength &&
        validUci(raw.playedMoveUci) &&
        raw.variations.nonEmpty && raw.variations.size <= MaxVariations &&
        raw.movePrefixUci.size <= MaxMovePrefix && raw.movePrefixUci.forall(validUci) &&
        raw.probeResults.size <= MaxProbeResults &&
        raw.probeResults.map(_.id).distinct.size == raw.probeResults.size &&
        raw.probeResults.forall(validProbe) &&
        raw.currentEvalCp.forall(bounded(_, MaxEval)) &&
        raw.ply.forall(ply => ply >= 0 && ply <= MaxPly) &&
        raw.openingContext.forall(validOpening) &&
        lines.forall(validLine)

    private def validLine(line: VariationLine): Boolean =
      line.moves.nonEmpty && line.moves.size <= MaxPvMoves && line.moves.forall(validUci) &&
        bounded(line.scoreCp, MaxEval) && line.mate.forall(mate => mate != 0 && bounded(mate, MaxMate)) &&
        line.depth >= 0 && line.depth <= MaxDepth

    private def validProbe(probe: ProbeResult): Boolean =
      probe.id.nonEmpty && probe.id.length <= MaxText &&
        probe.fen.exists(fen => fen.nonEmpty && fen.length <= MaxFenLength) &&
        bounded(probe.evalCp, MaxEval) && bounded(probe.deltaVsBaseline, MaxEval * 2) &&
        probe.replyLines.forall(_.size <= MaxReplyLines) &&
        probe.depth.forall(depth => depth >= 0 && depth <= MaxDepth) &&
        probe.mate.forall(mate => mate != 0 && bounded(mate, MaxMate)) &&
        probe.probedMove.forall(validUci) && probe.candidateMove.forall(validUci) &&
        probe.opponentResourceMove.forall(validUci) &&
        probe.horizon.forall(value => value.length <= MaxText && ProbeHorizon.plyOffset(value).nonEmpty) &&
        probe.variationHash.forall(hash => hash.nonEmpty && hash.length <= MaxText) &&
        probe.tablebase.forall(validTablebase)

    private def validTablebase(tablebase: EndgameTablebaseEvidence): Boolean =
      val moves = tablebase.legalMoves
      validTablebasePosition(tablebase.terminal) && validTablebasePosition(tablebase.comparison) &&
        moves.nonEmpty && moves.size <= MaxTablebaseMoves &&
        moves.map(_.moveUci.trim.toLowerCase).distinct.size == moves.size &&
        moves.forall(move => validUci(move.moveUci) && validWdl(move.resultingWdl) && move.resultingDtm.forall(validDtm))

    private def validTablebasePosition(position: lila.chessjudgment.model.TablebasePositionEvidence): Boolean =
      position.fen.nonEmpty && position.fen.length <= MaxFenLength && validWdl(position.wdl) && position.dtm.forall(validDtm)

    private def validWdl(wdl: Int): Boolean = wdl >= -2 && wdl <= 2

    private def validDtm(dtm: Int): Boolean = dtm != 0 && bounded(dtm, MaxMate)

    private def validOpening(opening: RawOpeningContext): Boolean =
      List(opening.eco, opening.name, opening.family).flatten.forall(_.length <= MaxText)

    private def validUci(value: String): Boolean = Uci.matches(value.trim.toLowerCase)

    private def bounded(value: Int, limit: Long): Boolean = value.toLong.abs <= limit

object RuntimeResources:
  private val ThemePriorEntries = 43

  def verify(): Unit =
    require(
      OpeningRecognitionIndex.default.allEntries.nonEmpty,
      "scalachess opening database is empty"
    )
    require(
      OpeningThemePriorIndex.default.allEntries.size == ThemePriorEntries,
      "opening theme-prior resource is missing or incomplete"
    )
