package controllers

import scala.util.control.NonFatal

import play.api.libs.json.{ JsValue, Json }
import play.api.libs.ws.{ DefaultBodyReadables, DefaultBodyWritables, StandaloneWSClient }

import lila.app.*

final class Analyse(
    env: Env,
    ws: StandaloneWSClient
) extends LilaController(env):
  import DefaultBodyReadables.*
  import DefaultBodyWritables.*

  private val logger = lila.log("chesstory.adapter")
  private val requestSchema = "chesstory.move-meaning.request.v1"
  private val requestTimeout = 5.seconds
  private val endpoint =
    env.config
      .getOptional[String]("chesstory.runtime.url")
      .orElse(sys.env.get("CHESSTORY_RUNTIME_URL"))
      .map(_.trim)
      .filter(_.nonEmpty)
      .getOrElse("http://127.0.0.1:8091")
      .stripSuffix("/") + "/v1/move-meaning"
  private val token =
    env.config
      .getOptional[String]("chesstory.runtime.token")
      .orElse(sys.env.get("CHESSTORY_TOKEN"))
      .map(_.trim)
      .filter(_.nonEmpty)

  def moveMeaning = OpenBodyOf(parse.json): (bodyCtx: BodyContext[JsValue]) ?=>
    limit.moveMeaning(bodyCtx.ip, rateLimitedJson.toFuccess):
      val envelope = Json.obj(
        "schema_version" -> requestSchema,
        "input" -> bodyCtx.body.body
      )
      val headers =
        List("Accept" -> "application/json", "Content-Type" -> "application/json") ++
          token.map(value => "Authorization" -> s"Bearer $value")
      ws.url(endpoint)
        .withRequestTimeout(requestTimeout)
        .withHttpHeaders(headers*)
        .post(Json.stringify(envelope))
        .map: upstream =>
          Status(upstream.status)(upstream.body[Array[Byte]]).as("application/json; charset=utf-8")
        .recover { case NonFatal(error) =>
          logger.warn(s"Chesstory runtime unavailable: ${error.getClass.getSimpleName}: ${error.getMessage}")
          BadGateway(Json.obj("ok" -> false, "error" -> "chesstory_runtime_unavailable")).as(JSON)
        }
