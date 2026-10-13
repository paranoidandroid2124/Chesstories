package io.chesstory.runtime

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.concurrent.{ CountDownLatch, ExecutorService, Executors, Semaphore, TimeUnit }
import java.util.concurrent.atomic.AtomicBoolean

import scala.util.control.NonFatal

import com.sun.net.httpserver.{ HttpExchange, HttpServer }
import play.api.libs.json.{ Json, JsObject }

object ChesstoryRuntime:
  private val DefaultHost = "127.0.0.1"
  private val DefaultPort = 8091
  private val DefaultWorkers = math.max(1, math.min(Runtime.getRuntime.availableProcessors, 8))
  private val MaxWorkers = 256
  private val MaximumQueuedRequests = 512
  private val MaxBodyBytes = 1024 * 1024
  private val HealthSchema = "chesstory.runtime-health.v1"
  private val PositionCommentaryJobsPath = "/v1/position-commentary-jobs"
  private val JobId = "^[A-Za-z0-9_-]{32}$".r
  private val RootSearchDepth = 16
  private val ShortJobSearchDepth = 12
  private val JobDeadlineMillis = 3L * 60L * 1000L
  private val MaximumRetainedJobs = 2048
  private val TerminalJobRetentionMillis = 2L * 60L * 1000L

  private lazy val commentaryReady: Unit = RuntimeProtocol.initialize()

  def main(args: Array[String]): Unit =
    val host = sys.env.getOrElse("CHESSTORY_HOST", DefaultHost)
    val port = sys.env.get("CHESSTORY_PORT").fold(DefaultPort)(parsePort)
    val workers = sys.env.get("CHESSTORY_WORKERS").fold(DefaultWorkers)(parseWorkerCount)
    val token = sys.env.get("CHESSTORY_TOKEN").filter(_.nonEmpty)
    val address = new InetSocketAddress(host, port)
    require(address.getAddress != null, s"unresolvable CHESSTORY_HOST: $host")
    require(address.getAddress.isLoopbackAddress || token.nonEmpty, "CHESSTORY_TOKEN is required outside loopback")
    val server = start(address, token, workers)
    val stopped = CountDownLatch(1)
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      try server.stop(1)
      finally stopped.countDown()
    }, "chesstory-runtime-shutdown"))
    println(s"Chesstory runtime listening on http://$host:${server.getAddress.getPort}")
    stopped.await()

  private[runtime] def start(
      address: InetSocketAddress,
      token: Option[String],
      workerCount: Int = DefaultWorkers
  ): RunningRuntimeServer =
    require(workerCount > 0 && workerCount <= MaxWorkers, s"workerCount must be between 1 and $MaxWorkers")
    commentaryReady
    val server = HttpServer.create(address, 256)
    val workers = Semaphore(workerCount)
    val admitted = Semaphore(workerCount + MaximumQueuedRequests)
    val stopping = AtomicBoolean(false)
    val commentaryJobs = CommentaryJobRegistry(MaximumRetainedJobs, TerminalJobRetentionMillis)
    val executor = Executors.newVirtualThreadPerTaskExecutor()
    server.setExecutor(executor)
    server.createContext(
      "/health",
      exchange =>
        completeHttpExchange(
          exchange,
          Json.obj("schema_version" -> HealthSchema, "ok" -> false, "error" -> "internal_error")
        )(respondToHealthRequest)
    )
    server.createContext(
      PositionCommentaryJobsPath,
      exchange =>
        completeHttpExchange(
          exchange,
          PositionCommentaryJobProtocol.jobErrorJson(None, None, None, "internal_error")
        ) { exchange =>
          if !authorized(exchange, token) then
            respond(exchange, 401, PositionCommentaryJobProtocol.jobErrorJson(None, None, None, "unauthorized"))
          else if declaredTooLarge(exchange) then
            respond(exchange, 413, PositionCommentaryJobProtocol.jobErrorJson(None, None, None, "request_too_large"))
          else if !admitted.tryAcquire() then
            respond(
              exchange,
              503,
              PositionCommentaryJobProtocol.jobErrorJson(None, None, None, "runtime_busy")
            )
          else
            val requestReceivedEpochMs = System.currentTimeMillis()
            var workerAcquired = false
            try
              workers.acquire()
              workerAcquired = true
              if !stopping.get() then
                routePositionCommentaryJobRequest(exchange, commentaryJobs, requestReceivedEpochMs)
            catch
              case _: InterruptedException => Thread.currentThread.interrupt()
            finally
              if workerAcquired then workers.release()
              admitted.release()
        }
    )
    try
      server.start()
      RunningRuntimeServer(server, executor, stopping)
    catch
      case NonFatal(error) =>
        stopping.set(true)
        executor.shutdownNow()
        throw error

  private def respondToHealthRequest(exchange: HttpExchange): Unit =
    if exchange.getRequestMethod != "GET" then
      exchange.getResponseHeaders.set("Allow", "GET")
      respond(
        exchange,
        405,
        Json.obj("schema_version" -> HealthSchema, "ok" -> false, "error" -> "method_not_allowed")
      )
    else
      respond(
        exchange,
        200,
        Json.obj("ok" -> true, "schema_version" -> HealthSchema)
      )

  /** The one HTTP prefix router for the browser-executed position-commentary job contract. */
  private def routePositionCommentaryJobRequest(
      exchange: HttpExchange,
      commentaryJobs: CommentaryJobRegistry,
      requestReceivedEpochMs: Long
  ): Unit =
    val rawPath = exchange.getRequestURI.getRawPath
    if rawPath == PositionCommentaryJobsPath then
      createPositionCommentaryJob(exchange, commentaryJobs, requestReceivedEpochMs)
    else if rawPath.startsWith(s"$PositionCommentaryJobsPath/") then
      rawPath.drop(PositionCommentaryJobsPath.length + 1).split("/", -1).toList match
        case List(jobId) =>
          exchange.getRequestMethod match
            case "GET" => readPositionCommentaryJob(exchange, commentaryJobs, jobId)
            case "DELETE" => cancelPositionCommentaryJob(exchange, commentaryJobs, jobId)
            case _ => methodNotAllowedForJob(exchange, "GET, DELETE", Option.when(validJobId(jobId))(jobId))
        case List(jobId, "engine-work-reports") =>
          submitEngineWorkReport(exchange, commentaryJobs, jobId)
        case _ =>
          respond(exchange, 404, PositionCommentaryJobProtocol.jobErrorJson(None, None, None, "job_route_not_found"))
    else
      respond(exchange, 404, PositionCommentaryJobProtocol.jobErrorJson(None, None, None, "job_route_not_found"))

  private def createPositionCommentaryJob(
      exchange: HttpExchange,
      commentaryJobs: CommentaryJobRegistry,
      requestReceivedEpochMs: Long
  ): Unit =
    if exchange.getRequestMethod != "POST" then methodNotAllowedForJob(exchange, "POST", None)
    else
      readPositionCommentaryJsonBody(exchange) match
        case Left(failure) => respond(exchange, failure.httpStatus, PositionCommentaryJobProtocol.jobErrorJson(None, None, None, failure.error))
        case Right(body) =>
          PositionCommentaryJobProtocol.parseJobRequest(body) match
            case Left(failure) =>
              respond(exchange, 400, PositionCommentaryJobProtocol.jobErrorJson(None, None, None, failure.errorCode))
            case Right(jobRequest) =>
              val nowEpochMs = System.currentTimeMillis()
              val policy = CommentaryJobPolicy(
                if jobRequest.remainingBudgetMs.exists(_ <= 10000) then ShortJobSearchDepth else RootSearchDepth,
                requestReceivedEpochMs + math.min(JobDeadlineMillis, jobRequest.remainingBudgetMs.fold(JobDeadlineMillis)(_.toLong)),
                jobRequest.engineProfile,
                investigateChoices = true,
                // A short job cannot afford a second root/focus pair when the
                // preview chooses another reference. Spend its one root pass
                // at one matched depth, leaving room for the move's purpose.
                previewDepth = Option.when(jobRequest.remainingBudgetMs.forall(_ > 10000))(8),
                // Two root candidates suffice for the rating comparison;
                // reserve the short job's assembly budget for reply differences.
                shortJob = jobRequest.remainingBudgetMs.exists(_ <= 10000)
              )
              commentaryJobs.createJob(
                jobRequest.requestId,
                jobRequest.initialFen,
                jobRequest.movePrefixUci,
                jobRequest.currentFen,
                jobRequest.playedMoveUci,
                jobRequest.resultingFen,
                policy,
                nowEpochMs,
                jobRequest.inputLines,
                jobRequest.proposals
              ) match
                case Left(CommentaryJobCreationRejection.RegistryCapacityReached) =>
                  respond(
                    exchange,
                    503,
                    PositionCommentaryJobProtocol.jobErrorJson(
                      Some(jobRequest.requestId),
                      None,
                      Some(jobRequest.engineProfile),
                      "commentary_job_capacity_reached"
                    )
                  )
                case Left(CommentaryJobCreationRejection.JobStartRejected(reason)) =>
                  respond(
                    exchange,
                    400,
                    PositionCommentaryJobProtocol.jobErrorJson(
                      Some(jobRequest.requestId),
                      None,
                      Some(jobRequest.engineProfile),
                      jobStartRejectionCode(reason)
                    )
                  )
                case Right(snapshot) =>
                  exchange.getResponseHeaders.set(
                    "Location",
                    s"$PositionCommentaryJobsPath/${snapshot.jobId}"
                  )
                  respond(exchange, 201, snapshot)

  private def readPositionCommentaryJob(
      exchange: HttpExchange,
      commentaryJobs: CommentaryJobRegistry,
      jobId: String
  ): Unit =
    if !validJobId(jobId) then
      respond(exchange, 400, PositionCommentaryJobProtocol.jobErrorJson(None, None, None, "invalid_job_id"))
    else
      commentaryJobs.expireAndReadJobAt(jobId, System.currentTimeMillis()) match
        case None => respond(exchange, 404, PositionCommentaryJobProtocol.jobErrorJson(None, Some(jobId), None, "job_not_found"))
        case Some(snapshot) => respond(exchange, 200, snapshot)

  private def submitEngineWorkReport(
      exchange: HttpExchange,
      commentaryJobs: CommentaryJobRegistry,
      jobId: String
  ): Unit =
    if exchange.getRequestMethod != "POST" then
      methodNotAllowedForJob(exchange, "POST", Option.when(validJobId(jobId))(jobId))
    else if !validJobId(jobId) then
      respond(exchange, 400, PositionCommentaryJobProtocol.jobErrorJson(None, None, None, "invalid_job_id"))
    else
      readPositionCommentaryJsonBody(exchange) match
        case Left(failure) => respond(exchange, failure.httpStatus, PositionCommentaryJobProtocol.jobErrorJson(None, Some(jobId), None, failure.error))
        case Right(body) =>
          PositionCommentaryJobProtocol.parseEngineWorkReport(body) match
            case Left(failure) =>
              respond(exchange, 400, PositionCommentaryJobProtocol.jobErrorJson(None, Some(jobId), None, failure.errorCode))
            case Right(engineWorkReport) =>
              commentaryJobs.submitEngineWorkReportAt(jobId, engineWorkReport, System.currentTimeMillis()) match
                case Left(EngineWorkReportSubmissionRejection.JobNotFound) =>
                  respond(exchange, 404, PositionCommentaryJobProtocol.jobErrorJson(None, Some(jobId), None, "job_not_found"))
                case Left(EngineWorkReportSubmissionRejection.WorkNotOutstanding) =>
                  respond(exchange, 409, PositionCommentaryJobProtocol.jobErrorJson(None, Some(jobId), Some(engineWorkReport.reportedEngineProfile), "engine_work_not_outstanding"))
                case Right(snapshot) =>
                  respond(exchange, 200, snapshot)

  private def cancelPositionCommentaryJob(
      exchange: HttpExchange,
      commentaryJobs: CommentaryJobRegistry,
      jobId: String
  ): Unit =
    if !validJobId(jobId) then
      respond(exchange, 400, PositionCommentaryJobProtocol.jobErrorJson(None, None, None, "invalid_job_id"))
    else if Option(exchange.getRequestHeaders.getFirst("Prefer")).exists(_.split(",").exists(_.trim == "return=minimal")) then
      // Explicit disposal: the browser already owns its last decoded result. A
      // legacy DELETE without this preference still returns the cancelled snapshot.
      commentaryJobs.releaseJob(jobId)
      exchange.getResponseHeaders.set("Cache-Control", "no-store")
      exchange.getResponseHeaders.set("Preference-Applied", "return=minimal")
      exchange.sendResponseHeaders(204, -1)
    else
      commentaryJobs.cancelJobAt(jobId, System.currentTimeMillis()) match
        case None => respond(exchange, 404, PositionCommentaryJobProtocol.jobErrorJson(None, Some(jobId), None, "job_not_found"))
        case Some(snapshot) => respond(exchange, 200, snapshot)

  private def readPositionCommentaryJsonBody(exchange: HttpExchange): Either[PositionCommentaryJsonBodyFailure, Array[Byte]] =
    if !isJson(exchange) then Left(PositionCommentaryJsonBodyFailure(415, "content_type_must_be_json"))
    else
      val body = exchange.getRequestBody.readNBytes(MaxBodyBytes + 1)
      if body.length > MaxBodyBytes then Left(PositionCommentaryJsonBodyFailure(413, "request_too_large"))
      else Right(body)

  private def jobStartRejectionCode(rejection: CommentaryJobStartRejection): String = rejection match
    case CommentaryJobStartRejection.PositionHistoryInvalid(reason) => reason match
      case lila.chessjudgment.model.line.CanonicalPositionHistoryFailure.InvalidInitialFen =>
        "invalid_initial_fen"
      case lila.chessjudgment.model.line.CanonicalPositionHistoryFailure.InvalidCurrentFen =>
        "invalid_current_fen"
      case lila.chessjudgment.model.line.CanonicalPositionHistoryFailure.IllegalMovePrefix =>
        "invalid_move_prefix_uci"
      case lila.chessjudgment.model.line.CanonicalPositionHistoryFailure.CurrentFenMismatch =>
        "current_fen_mismatch"
      case lila.chessjudgment.model.line.CanonicalPositionHistoryFailure.InitialPlyOutOfRange |
          lila.chessjudgment.model.line.CanonicalPositionHistoryFailure.MovePrefixPlyOutOfRange |
          lila.chessjudgment.model.line.CanonicalPositionHistoryFailure.HalfMoveClockOverflow |
          lila.chessjudgment.model.line.CanonicalPositionHistoryFailure.FullMoveNumberOverflow =>
        "invalid_position_history"
    case CommentaryJobStartRejection.FocusedMoveInvalid => "invalid_focus"
    case CommentaryJobStartRejection.RootSearchDepthInvalid => "invalid_commentary_job_policy"
    case CommentaryJobStartRejection.InvalidProposals => "invalid_proposals"
    case CommentaryJobStartRejection.LocalExpansionLimitInvalid => "invalid_commentary_job_policy"

  private final case class PositionCommentaryJsonBodyFailure(httpStatus: Int, error: String)

  private def completeHttpExchange(
      exchange: HttpExchange,
      internalError: => JsObject
  )(handler: HttpExchange => Unit): Unit =
    try handler(exchange)
    catch
      case NonFatal(error) =>
        System.err.println(s"Chesstory request failed: ${error.getClass.getSimpleName}")
        try
          respond(exchange, 500, internalError)
        catch case NonFatal(_) => ()
    finally exchange.close()

  private def authorized(exchange: HttpExchange, token: Option[String]): Boolean =
    token.forall: value =>
      val expected = s"Bearer $value".getBytes(StandardCharsets.UTF_8)
      val actual = Option(exchange.getRequestHeaders.getFirst("Authorization")).getOrElse("").getBytes(StandardCharsets.UTF_8)
      MessageDigest.isEqual(expected, actual)

  private def isJson(exchange: HttpExchange): Boolean =
    Option(exchange.getRequestHeaders.getFirst("Content-Type"))
      .exists(_.split(";", 2).headOption.exists(_.trim.equalsIgnoreCase("application/json")))

  private def declaredTooLarge(exchange: HttpExchange): Boolean =
    Option(exchange.getRequestHeaders.getFirst("Content-Length"))
      .flatMap(_.toLongOption)
      .exists(_ > MaxBodyBytes)

  private def methodNotAllowedForJob(
      exchange: HttpExchange,
      allowed: String,
      jobId: Option[String]
  ): Unit =
    exchange.getResponseHeaders.set("Allow", allowed)
    respond(exchange, 405, PositionCommentaryJobProtocol.jobErrorJson(None, jobId, None, "method_not_allowed"))

  private def validJobId(jobId: String): Boolean = JobId.matches(jobId)

  private def respond(exchange: HttpExchange, status: Int, body: JsObject): Unit =
    respond(exchange, status, Json.toBytes(body))

  private def respond(exchange: HttpExchange, status: Int, delivery: CommentaryJobDelivery): Unit = delivery match
    case retained: RetainedCommentaryJobResult if acceptsGzip(exchange) =>
      responseHeaders(exchange, gzip = true)
      exchange.sendResponseHeaders(status, retained.compressedLength)
      retained.writeCompressed(exchange.getResponseBody)
    case retained: RetainedCommentaryJobResult => respond(exchange, status, retained.jsonBytes)
    case snapshot: CommentaryJobSnapshot =>
      respond(exchange, status, PositionCommentaryJobProtocol.projectJobSnapshot(snapshot))

  private def respond(exchange: HttpExchange, status: Int, bytes: Array[Byte]): Unit =
    val gzip = bytes.length >= 1024 && acceptsGzip(exchange)
    val delivered = if gzip then RetainedCommentaryJobResult.compress(bytes) else bytes
    responseHeaders(exchange, gzip)
    exchange.sendResponseHeaders(status, delivered.length)
    exchange.getResponseBody.write(delivered)

  private def acceptsGzip(exchange: HttpExchange): Boolean =
    Option(exchange.getRequestHeaders.getFirst("Accept-Encoding")).exists(_.split(",").exists { coding =>
      val parts = coding.toLowerCase(java.util.Locale.ROOT).split(";").map(_.trim)
      parts.headOption.contains("gzip") && parts.tail.find(_.startsWith("q=")).forall(quality =>
        quality.drop(2).toDoubleOption.exists(value => value > 0 && value <= 1))
    })

  private def responseHeaders(exchange: HttpExchange, gzip: Boolean): Unit =
    exchange.getResponseHeaders.set("Content-Type", "application/json; charset=utf-8")
    exchange.getResponseHeaders.set("Cache-Control", "no-store")
    exchange.getResponseHeaders.set("Vary", "Accept-Encoding")
    if gzip then exchange.getResponseHeaders.set("Content-Encoding", "gzip")

  private def parsePort(raw: String): Int =
    raw.toIntOption.filter(port => port > 0 && port <= 65535)
      .getOrElse(throw IllegalArgumentException(s"invalid CHESSTORY_PORT: $raw"))

  private def parseWorkerCount(raw: String): Int =
    raw.toIntOption.filter(workers => workers > 0 && workers <= MaxWorkers)
      .getOrElse(throw IllegalArgumentException(s"invalid CHESSTORY_WORKERS: $raw"))

private[runtime] final class RunningRuntimeServer(
    server: HttpServer,
    executor: ExecutorService,
    stopping: AtomicBoolean
):
  def getAddress: InetSocketAddress = server.getAddress

  def stop(delaySeconds: Int): Unit =
    stopping.set(true)
    try server.stop(delaySeconds)
    finally
      executor.shutdownNow()
      try executor.awaitTermination(math.max(1, delaySeconds).toLong, TimeUnit.SECONDS)
      catch case _: InterruptedException => Thread.currentThread.interrupt()
