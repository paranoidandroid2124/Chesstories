package io.chesstory.runtime

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Locale
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
  private val QueuedRequestsPerWorker = 4
  private val MaxBodyBytes = 1024 * 1024

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
    RuntimeResources.verify()
    val server = HttpServer.create(address, 32)
    val workers = Semaphore(workerCount)
    val admitted = Semaphore(workerCount * (QueuedRequestsPerWorker + 1))
    val stopping = AtomicBoolean(false)
    val executor = Executors.newVirtualThreadPerTaskExecutor()
    server.setExecutor(executor)
    server.createContext("/health", exchange => safely(exchange)(handleHealth))
    server.createContext("/v1/move-meaning", exchange => safely(exchange): exchange =>
      if !admitted.tryAcquire() then
        val result = RuntimeProtocol.failure(None, 503, "runtime_busy")
        respond(exchange, result.httpStatus, result.body)
      else
        var workerAcquired = false
        try
          workers.acquire()
          workerAcquired = true
          if !stopping.get() then handleMoveMeaning(exchange, token)
        catch
          case _: InterruptedException => Thread.currentThread.interrupt()
        finally
          if workerAcquired then workers.release()
          admitted.release()
    )
    try
      server.start()
      RunningRuntimeServer(server, executor, stopping)
    catch
      case NonFatal(error) =>
        stopping.set(true)
        executor.shutdownNow()
        throw error

  private def handleHealth(exchange: HttpExchange): Unit =
    if exchange.getRequestMethod != "GET" then methodNotAllowed(exchange, "GET")
    else
      respond(
        exchange,
        200,
        Json.obj("ok" -> true, "schema_version" -> RuntimeProtocol.ResponseSchema)
      )

  private def handleMoveMeaning(exchange: HttpExchange, token: Option[String]): Unit =
    if exchange.getRequestMethod != "POST" then methodNotAllowed(exchange, "POST")
    else if !authorized(exchange, token) then
      val result = RuntimeProtocol.failure(None, 401, "unauthorized")
      respond(exchange, result.httpStatus, result.body)
    else if !isJson(exchange) then
      val result = RuntimeProtocol.failure(None, 415, "content_type_must_be_json")
      respond(exchange, result.httpStatus, result.body)
    else if declaredTooLarge(exchange) then
      val result = RuntimeProtocol.failure(None, 413, "request_too_large")
      respond(exchange, result.httpStatus, result.body)
    else
      val body = exchange.getRequestBody.readNBytes(MaxBodyBytes + 1)
      if body.length > MaxBodyBytes then
        val result = RuntimeProtocol.failure(None, 413, "request_too_large")
        respond(exchange, result.httpStatus, result.body)
      else
        val result = RuntimeProtocol.evaluate(body)
        respond(exchange, result.httpStatus, result.body)

  private def safely(exchange: HttpExchange)(handler: HttpExchange => Unit): Unit =
    try handler(exchange)
    catch
      case NonFatal(error) =>
        System.err.println(s"Chesstory request failed: ${error.getClass.getSimpleName}")
        try
          val result = RuntimeProtocol.failure(None, 500, "internal_error")
          respond(exchange, result.httpStatus, result.body)
        catch case NonFatal(_) => ()
    finally exchange.close()

  private def authorized(exchange: HttpExchange, token: Option[String]): Boolean =
    token.forall: value =>
      val expected = s"Bearer $value".getBytes(StandardCharsets.UTF_8)
      val actual = Option(exchange.getRequestHeaders.getFirst("Authorization")).getOrElse("").getBytes(StandardCharsets.UTF_8)
      MessageDigest.isEqual(expected, actual)

  private def isJson(exchange: HttpExchange): Boolean =
    Option(exchange.getRequestHeaders.getFirst("Content-Type"))
      .exists(_.toLowerCase(Locale.ROOT).startsWith("application/json"))

  private def declaredTooLarge(exchange: HttpExchange): Boolean =
    Option(exchange.getRequestHeaders.getFirst("Content-Length"))
      .flatMap(_.toLongOption)
      .exists(_ > MaxBodyBytes)

  private def methodNotAllowed(exchange: HttpExchange, allowed: String): Unit =
    exchange.getResponseHeaders.set("Allow", allowed)
    val result = RuntimeProtocol.failure(None, 405, "method_not_allowed")
    respond(exchange, result.httpStatus, result.body)

  private def respond(exchange: HttpExchange, status: Int, body: JsObject): Unit =
    val bytes = Json.stringify(body).getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.set("Content-Type", "application/json; charset=utf-8")
    exchange.getResponseHeaders.set("Cache-Control", "no-store")
    exchange.sendResponseHeaders(status, bytes.length)
    exchange.getResponseBody.write(bytes)

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
