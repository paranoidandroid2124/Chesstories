package controllers

import java.net.URI
import scala.util.Try
import scala.util.control.NonFatal

import play.api.libs.json.Json
import play.api.libs.ws.{ DefaultBodyReadables, DefaultBodyWritables, StandaloneWSClient }

import lila.app.*

object Analyse:
  private[controllers] def positionCommentaryJobsPath(runtimeBase: String): String =
    s"$runtimeBase/v1/position-commentary-jobs"

  private[controllers] def normalizedRuntimeOrigin(raw: String): Option[String] =
    Option(raw).map(_.trim).filter(_.nonEmpty).flatMap(value => Try(URI.create(value)).toOption).flatMap: uri =>
      val scheme = Option(uri.getScheme).map(_.toLowerCase)
      val host = Option(uri.getHost)
      val port = uri.getPort
      val literalLoopback = host.exists: value =>
        value.equalsIgnoreCase("localhost") || value.equalsIgnoreCase("[::1]") ||
          (value.split("\\.", -1).toList match
            case first :: second :: third :: fourth :: Nil =>
              val octets = List(first, second, third, fourth).map: octet =>
                if octet.nonEmpty && octet.forall(_.isDigit) && (octet == "0" || !octet.startsWith("0")) then octet.toIntOption
                else None
              octets.forall(_.exists(value => value >= 0 && value <= 255)) && octets.head.contains(127)
            case _ => false)
      val rootPath = Option(uri.getRawPath).forall(path => path.isEmpty || path == "/")
      val validPort = port == -1 || (port >= 1 && port <= 65535)
      val allowed =
        host.nonEmpty &&
          validPort &&
          rootPath &&
          uri.getRawUserInfo == null &&
          uri.getRawQuery == null &&
          uri.getRawFragment == null &&
          (scheme.contains("https") || (scheme.contains("http") && literalLoopback))
      if allowed then Try(new URI(scheme.get, null, host.get, port, null, null, null).toString).toOption else None

final class Analyse(
    env: Env,
    ws: StandaloneWSClient
) extends LilaController(env):
  import DefaultBodyReadables.*
  import DefaultBodyWritables.*

  private val logger = lila.log("chesstory.move-review")
  private def configured(path: String): Option[String] =
    env.config.getOptional[String](path).map(_.trim).filter(_.nonEmpty)
  private val configuredRuntimeUrl =
    configured("chesstory.runtime.url")
      .orElse(sys.env.get("CHESSTORY_RUNTIME_URL").map(_.trim).filter(_.nonEmpty))
      .getOrElse("http://127.0.0.1:8091")
  private val runtimeBase =
    Analyse.normalizedRuntimeOrigin(configuredRuntimeUrl)
      .getOrElse(throw IllegalArgumentException("chesstory.runtime.url must be an HTTPS or literal-loopback HTTP origin"))
  private val runtimeToken =
    configured("chesstory.runtime.token")
      .orElse(sys.env.get("CHESSTORY_TOKEN").map(_.trim).filter(_.nonEmpty))
  private val runtimeHeaders =
    List("Accept" -> "application/json", "Content-Type" -> "application/json") ++
      runtimeToken.map(value => "Authorization" -> s"Bearer $value")
  // These limit only the Lila-to-runtime transport, never browser engine work.
  private val moveReviewTransportTimeout = 5.seconds
  private val moveReviewJobsPath = Analyse.positionCommentaryJobsPath(runtimeBase)
  private val moveReviewRuntimeEnabled =
    configured("chesstory.moveReview.mode").exists(_.equalsIgnoreCase("runtime"))
  private def moveReviewDisabled =
    NotFound(Json.obj("error" -> "move_review_disabled")).as(JSON).noCache.toFuccess

  def createMoveReviewJob = OpenBodyOf(parse.tolerantText): (bodyCtx: BodyContext[String]) ?=>
    if !moveReviewRuntimeEnabled then moveReviewDisabled
    else
      limit.moveReviewJobCreation(bodyCtx.ip, rateLimitedJson.noCache.toFuccess):
        ws.url(moveReviewJobsPath)
          .withRequestTimeout(moveReviewTransportTimeout)
          .withHttpHeaders(runtimeHeaders*)
          .post(bodyCtx.body.body)
          .map: upstream =>
            Status(upstream.status)(upstream.body[Array[Byte]])
              .as("application/json; charset=utf-8")
              .withHeaders(upstream.header("Retry-After").map("Retry-After" -> _).toList*)
              .noCache
          .recover { case NonFatal(error) =>
            logger.warn(s"Chesstory move-review creation unavailable: ${error.getClass.getSimpleName}")
            BadGateway(Json.obj("error" -> "chesstory_runtime_unavailable")).as(JSON).noCache
          }

  def reportMoveReviewEngineWork(jobId: String) = OpenBodyOf(parse.tolerantText):
    (bodyCtx: BodyContext[String]) ?=>
      if !moveReviewRuntimeEnabled then moveReviewDisabled
      else
        limit.moveReviewJobAccess(bodyCtx.ip, rateLimitedJson.noCache.toFuccess):
          ws.url(s"$moveReviewJobsPath/$jobId/engine-work-reports")
            .withRequestTimeout(moveReviewTransportTimeout)
            .withHttpHeaders(runtimeHeaders*)
            .post(bodyCtx.body.body)
            .map: upstream =>
              Status(upstream.status)(upstream.body[Array[Byte]])
                .as("application/json; charset=utf-8")
                .withHeaders(upstream.header("Retry-After").map("Retry-After" -> _).toList*)
                .noCache
            .recover { case NonFatal(error) =>
              logger.warn(s"Chesstory move-review engine report unavailable: ${error.getClass.getSimpleName}")
              BadGateway(Json.obj("error" -> "chesstory_runtime_unavailable")).as(JSON).noCache
            }

  def cancelMoveReviewJob(jobId: String) = Open:
    if !moveReviewRuntimeEnabled then moveReviewDisabled
    else
      limit.moveReviewJobAccess(ctx.ip, rateLimitedJson.noCache.toFuccess):
        ws.url(s"$moveReviewJobsPath/$jobId")
          .withRequestTimeout(moveReviewTransportTimeout)
          .withHttpHeaders(runtimeHeaders*)
          .delete()
          .map: upstream =>
            Status(upstream.status)(upstream.body[Array[Byte]])
              .as("application/json; charset=utf-8")
              .withHeaders(upstream.header("Retry-After").map("Retry-After" -> _).toList*)
              .noCache
          .recover { case NonFatal(error) =>
            logger.warn(s"Chesstory move-review cancellation unavailable: ${error.getClass.getSimpleName}")
            BadGateway(Json.obj("error" -> "chesstory_runtime_unavailable")).as(JSON).noCache
          }
